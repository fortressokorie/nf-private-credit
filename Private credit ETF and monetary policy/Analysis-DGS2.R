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
bdc <- read.csv("BDC_Index.csv")

# Identify date columns
etf_date_col <- names(etf)[grepl("date", names(etf), ignore.case=TRUE)][1]
ff_date_col  <- names(ff)[grepl("date", names(ff), ignore.case=TRUE)][1]
dgs_date_col <- names(dgs)[grepl("observation_date", names(dgs), ignore.case=TRUE)][1]
sp500_date_col <- names(sp500)[grepl("date", names(sp500), ignore.case=TRUE)][1]
dji_date_col <- names(dji)[grepl("date", names(dji), ignore.case=TRUE)][1]
bdc_date_col <- names(bdc)[grepl("date", names(bdc), ignore.case=TRUE)][1]

# Convert dates
etf[[etf_date_col]] <- as.Date(etf[[etf_date_col]])
ff[[ff_date_col]]   <- as.Date(ff[[ff_date_col]])
dgs[[dgs_date_col]] <- as.Date(dgs[[dgs_date_col]])
sp500[[sp500_date_col]] <- as.Date(sp500[[sp500_date_col]])
dji[[dji_date_col]] <- as.Date(dji[[dji_date_col]])
bdc[[bdc_date_col]] <- as.Date(bdc[[bdc_date_col]], format="%m/%d/%y")

# Rename date columns
names(etf)[names(etf)==etf_date_col] <- "date"
names(ff)[names(ff)==ff_date_col] <- "date"
names(dgs)[names(dgs)==dgs_date_col] <- "date"
names(sp500)[names(sp500)==sp500_date_col] <- "date"
names(dji)[names(dji)==dji_date_col] <- "date"
names(bdc)[names(bdc)==bdc_date_col] <- "date"

# Merge datasets
merged_data <- etf %>%
  inner_join(ff, by="date") %>%
  inner_join(dgs, by="date") %>%
  inner_join(sp500, by="date") %>%
  inner_join(dji, by="date") %>%
  inner_join(bdc, by="date") %>%
  arrange(date)

merged_data$DGS2 <- as.numeric(merged_data$DGS2)
merged_data$SP500 <- as.numeric(merged_data$SP500)
merged_data$DJI <- as.numeric(merged_data$DJI)
merged_data$BDC_Index <- as.numeric(merged_data$BDC_Index)

# Construct daily returns
merged_data <- merged_data %>%
  mutate(
    FF_RATE = 100 - FF_FUTURE,
    D.SP500 = 100*(log(SP500)-log(lag(SP500))),
    D.DJI = 100*(log(DJI)-log(lag(DJI))),
    D.BDC = 100*(log(BDC_Index)-log(lag(BDC_Index))),
    D.PSP = 100*(log(PSP)-log(lag(PSP))),
    D.PRIV = 100*(log(PRIV)-log(lag(PRIV))),
    D.PCMM = 100*(log(PCMM)-log(lag(PCMM))),
    D.HYIN = 100*(log(HYIN)-log(lag(HYIN))),
    D.BZID = 100*(log(BZID)-log(lag(BZID)))
  )

# Export merged data
write_xlsx(merged_data, "merged_ETF_FF.xlsx")

# Call variables
DGS <- merged_data$DGS2
FF <- merged_data$FF_RATE

D.SP500 <- merged_data$D.SP500
D.DJI <- merged_data$D.DJI

# # Total Returns
# D.BDC <- merged_data$D.BDC
# D.PSP <- merged_data$D.PSP
# D.PRIV <- merged_data$D.PRIV
# D.PCMM <- merged_data$D.PCMM
# D.HYIN <- merged_data$D.HYIN
# D.BZID <- merged_data$D.BZID

# Excess Returns
D.BDC <- merged_data$D.BDC-D.SP500
D.PSP <- merged_data$D.PSP-D.SP500
D.PRIV <- merged_data$D.PRIV-D.SP500
D.PCMM <- merged_data$D.PCMM-D.SP500
D.HYIN <- merged_data$D.HYIN-D.SP500
D.BZID <- merged_data$D.BZID-D.SP500

# Check missing observations
colSums(is.na(merged_data[,c(
  "DGS2","FF_RATE",
  "D.SP500","D.DJI","D.BDC","D.PSP",
  "D.PRIV","D.PCMM","D.HYIN","D.BZID"
)]))

# VAR and IRF function
irf.linear <- function(obj, rate) {
  macro <- obj[[1]]
  varname <- obj[[2]]
  
  # # Specification for Total Returns
  # var_data <- data.frame(rate=rate, D.SP500, macro=macro)

  # Specification for Excess Returns
  var_data <- data.frame(rate=rate, macro=macro)
  
  # Drop missing values
  var_data <- var_data[complete.cases(var_data), ]
  
  cat(varname, ":", nrow(var_data), "observations used\n")
  
  rf <- vars::VAR(var_data, p=12, type="const")
  
  res <- irf(rf, impulse="rate", response="macro",
             n.ahead=30, cumulative=TRUE, ci=0.90,
             boot=TRUE)
  
  return(list(res=res, varname=varname, nobs=nrow(var_data)))
}

varinfo <- list(
  list(D.BDC, "BDC Market Index"),
  list(D.PSP, "PSP"),
  list(D.PRIV, "PRIV"),
  list(D.PCMM, "PCMM"),
  list(D.HYIN, "HYIN"),
  list(D.BZID, "BZID")
)

# irfs <- lapply(varinfo, irf.linear, rate=FF)
irfs <- lapply(varinfo, irf.linear, rate=DGS)
names(irfs) <- c("BDC Market","PSP","PRIV","PCMM","HYIN","BZID")

# Common symmetric y-axis
all_values <- unlist(lapply(irfs, function(x)
  c(x$res$irf$rate[,"macro"],
    x$res$Lower$rate[,"macro"],
    x$res$Upper$rate[,"macro"])))

ymax <- max(abs(all_values), na.rm=TRUE)
common_ylim <- c(-ymax, ymax)

par(mfrow=c(3,2))

for (x in irfs) {
  res <- x$res
  varname <- x$varname
  
  irf_est <- res$irf$rate[,"macro"]
  irf_low <- res$Lower$rate[,"macro"]
  irf_up <- res$Upper$rate[,"macro"]
  t <- 0:30
  
  plot(t, irf_est, xlab="Days after shock", ylab="% Change",
       type="l", main=varname, lwd=1, ylim=common_ylim)
  
  lines(t, irf_low, lwd=1, lty=2)
  lines(t, irf_up, lwd=1, lty=2)
  abline(h=0)
}

# Sample sizes
sapply(irfs, function(x) x$nobs)