# Load libraries
library(dplyr)
library(writexl)
library(vars)

# Read datasets
etf <- read.csv("ETFs.csv")
ff  <- read.csv("FF_FUTURE.csv")
dgs <- read.csv("DGS2.csv")
sp500 <- read.csv("SP500.csv")
dji <- read.csv("dji_data.csv")

# Identify date columns
etf_date_col <- names(etf)[grepl("date", names(etf), ignore.case=TRUE)][1]
ff_date_col  <- names(ff)[grepl("date", names(ff), ignore.case=TRUE)][1]
dgs_date_col <- names(dgs)[grepl("observation_date", names(dgs), ignore.case=TRUE)][1]
sp500_date_col <- names(sp500)[grepl("date", names(sp500), ignore.case=TRUE)][1]
dji_date_col <- names(dji)[grepl("date", names(dji), ignore.case=TRUE)][1]

# Convert dates
etf[[etf_date_col]] <- as.Date(etf[[etf_date_col]])
ff[[ff_date_col]]   <- as.Date(ff[[ff_date_col]])
dgs[[dgs_date_col]] <- as.Date(dgs[[dgs_date_col]])
sp500[[sp500_date_col]] <- as.Date(sp500[[sp500_date_col]])
dji[[dji_date_col]] <- as.Date(dji[[dji_date_col]])

# Rename date columns
names(etf)[names(etf)==etf_date_col] <- "date"
names(ff)[names(ff)==ff_date_col] <- "date"
names(dgs)[names(dgs)==dgs_date_col] <- "date"
names(sp500)[names(sp500)==sp500_date_col] <- "date"
names(dji)[names(dji)==dji_date_col] <- "date"

# Merge datasets
merged_data <- etf %>%
  inner_join(ff, by="date") %>%
  inner_join(dgs, by="date") %>%
  inner_join(sp500, by="date") %>%
  inner_join(dji, by="date") %>%
  arrange(date)

# Make sure DGS2 is numeric
merged_data$DGS2 <- as.numeric(merged_data$DGS2)
merged_data$SP500 <- as.numeric(merged_data$SP500)
merged_data$DJI <- as.numeric(merged_data$DJI)

# Construct daily returns
merged_data <- merged_data %>%
  mutate(
    D.SP500 = 100*(log(SP500)-log(lag(SP500))),
    D.DJI = 100*(log(DJI)-log(lag(DJI))),
    D.PSP = 100*(log(PSP)-log(lag(PSP))),
    D.PRIV = 100*(log(PRIV)-log(lag(PRIV))),
    D.PCMM = 100*(log(PCMM)-log(lag(PCMM))),
    D.HYIN = 100*(log(HYIN)-log(lag(HYIN))),
    D.BZID = 100*(log(BZID)-log(lag(BZID)))
  )

# Export merged data
write_xlsx(merged_data, "merged_ETF_FF.xlsx")

# Call variables
D.SP500 <- merged_data$D.SP500
D.DJI <- merged_data$D.DJI
DGS <- merged_data$DGS2
# DGS <- c(NA, diff(merged_data$DGS2))

D.PSP <- merged_data$D.PSP
D.PRIV <- merged_data$D.PRIV
D.PCMM <- merged_data$D.PCMM
D.HYIN <- merged_data$D.HYIN
D.BZID <- merged_data$D.BZID

# Check missing observations
colSums(is.na(merged_data[,c("D.SP500","D.DJI","DGS2","D.PSP",
                             "D.PRIV","D.PCMM","D.HYIN","D.BZID")]))

library(ggplot2)
library(dplyr)

irf.linear <- function(obj) {
  macro <- obj[[1]]
  varname <- obj[[2]]
  
  var_data <- data.frame(DGS=DGS, D.SP500=D.SP500, macro=macro)
  var_data <- var_data[complete.cases(var_data), ]
  
  cat(varname, ":", nrow(var_data), "observations used\n")
  
  rf <- vars::VAR(var_data, p=12, type="const")
  
  res68 <- irf(rf, impulse="DGS", response="macro",
               n.ahead=30, cumulative=TRUE, ci=0.68,
               boot=TRUE, runs=1000, seed=123)
  
  res90 <- irf(rf, impulse="DGS", response="macro",
               n.ahead=30, cumulative=TRUE, ci=0.90,
               boot=TRUE, runs=1000, seed=123)
  
  data.frame(
    horizon=0:30,
    irf=res90$irf$DGS[,"macro"],
    low68=res68$Lower$DGS[,"macro"],
    up68=res68$Upper$DGS[,"macro"],
    low90=res90$Lower$DGS[,"macro"],
    up90=res90$Upper$DGS[,"macro"],
    variable=varname,
    nobs=nrow(var_data)
  )
}

varinfo <- list(
  list(D.PSP, "PSP"),
  list(D.PRIV, "PRIV"),
  list(D.PCMM, "PCMM"),
  list(D.HYIN, "HYIN"),
  list(D.BZID, "BZID")
)

irf_data <- lapply(varinfo, irf.linear) %>%
  bind_rows()

irf_data$variable <- factor(
  irf_data$variable,
  levels=c("PSP","PRIV","PCMM","HYIN","BZID")
)

ymax <- max(abs(c(irf_data$low90, irf_data$up90)), na.rm=TRUE)
ymax <- 1.05*ymax

p <- ggplot(irf_data, aes(x=horizon, y=irf)) +
  geom_ribbon(aes(ymin=low90, ymax=up90, fill="90% CI"), alpha=0.20) +
  geom_ribbon(aes(ymin=low68, ymax=up68, fill="68% CI"), alpha=0.40) +
  geom_line(color="black", linewidth=0.9) +
  geom_hline(yintercept=0, color="gray45", linewidth=0.45) +
  facet_wrap(~variable, ncol=2) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  scale_y_continuous(limits=c(-ymax,ymax)) +
  scale_fill_manual(
    values=c("68% CI"="#4C78A8", "90% CI"="#A9C4DF"),
    breaks=c("68% CI","90% CI"),
    name=NULL
  ) +
  labs(x="Days after shock", y="Cumulative % change") +
  theme_classic(base_size=13) +
  theme(
    strip.background=element_blank(),
    strip.text=element_text(size=13, face="bold"),
    axis.text=element_text(size=10, color="black"),
    legend.position="bottom",
    panel.spacing=unit(1, "lines")
  )

p