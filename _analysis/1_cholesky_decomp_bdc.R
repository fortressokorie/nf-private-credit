# Load libraries
library(dplyr)
library(writexl)
library(vars)
library(tidyverse)

# Read datasets
#etf <- read.csv("ETFs.csv")
bdc <- read_csv("_data/bdc_prices_panel.csv")
ff  <- read_csv("Private credit ETF and monetary policy/FF_FUTURE.csv") 
dgs <- read_csv("Private credit ETF and monetary policy/DGS2.csv")
sp500 <- read_csv("Private credit ETF and monetary policy/SP500.csv")
dji <- read_csv("Private credit ETF and monetary policy/dji_data.csv")

# subset bdc dataset to relevant columns 
bdc <- bdc |> 
  dplyr::select(ticker, date, close)

# pivot bdc wide to match etf format 
bdc <- bdc |> 
  pivot_wider(names_from = ticker, values_from = close)

# Identify date columns
#etf_date_col <- names(etf)[grepl("date", names(etf), ignore.case=TRUE)][1]
bdc_date_col <- names(bdc)[grepl("date", names(bdc), ignore.case=TRUE)][1]
ff_date_col  <- names(ff)[grepl("date", names(ff), ignore.case=TRUE)][1]
dgs_date_col <- names(dgs)[grepl("observation_date", names(dgs), ignore.case=TRUE)][1]
sp500_date_col <- names(sp500)[grepl("date", names(sp500), ignore.case=TRUE)][1]
dji_date_col <- names(dji)[grepl("date", names(dji), ignore.case=TRUE)][1]

# Convert dates
#etf[[etf_date_col]] <- as.Date(etf[[etf_date_col]])
bdc[[bdc_date_col]] <- as.Date(bdc[[bdc_date_col]])
ff[[ff_date_col]]   <- as.Date(ff[[ff_date_col]])
dgs[[dgs_date_col]] <- as.Date(dgs[[dgs_date_col]])
sp500[[sp500_date_col]] <- as.Date(sp500[[sp500_date_col]])
dji[[dji_date_col]] <- as.Date(dji[[dji_date_col]])

# Rename date columns
#names(etf)[names(etf)==etf_date_col] <- "date"
names(bdc)[names(bdc)==bdc_date_col] <- "date"
names(ff)[names(ff)==ff_date_col] <- "date"
names(dgs)[names(dgs)==dgs_date_col] <- "date"
names(sp500)[names(sp500)==sp500_date_col] <- "date"
names(dji)[names(dji)==dji_date_col] <- "date"

# Merge datasets
merged_data <- bdc %>%
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
    D.ARCC = 100*(log(ARCC)-log(lag(ARCC))),
    D.BBDC = 100*(log(BBDC)-log(lag(BBDC))),
    D.BCSF = 100*(log(BCSF)-log(lag(BCSF))),
    D.BXSL = 100*(log(BXSL)-log(lag(BXSL))),
    D.CCAP = 100*(log(CCAP)-log(lag(CCAP))),
    D.CGBD = 100*(log(CGBD)-log(lag(CGBD))),
    D.CION = 100*(log(CION)-log(lag(CION))),
    D.CSWC = 100*(log(CSWC)-log(lag(CSWC))),
    D.EQS = 100*(log(EQS)-log(lag(EQS))),
    D.FDUS = 100*(log(FDUS)-log(lag(FDUS))),
    D.FSK = 100*(log(FSK)-log(lag(FSK))),
    D.GAIN = 100*(log(GAIN)-log(lag(GAIN))),
    D.GBDC = 100*(log(GBDC)-log(lag(GBDC))),
    D.GECC = 100*(log(GECC)-log(lag(GECC))),
    D.GLAD = 100*(log(GLAD)-log(lag(GLAD))),
    D.GSBD = 100*(log(GSBD)-log(lag(GSBD))),
    D.HRZN = 100*(log(HRZN)-log(lag(HRZN))),
    D.HTGC = 100*(log(HTGC)-log(lag(HTGC))),
    D.ICMB = 100*(log(ICMB)-log(lag(ICMB))),
    D.KBDC = 100*(log(KBDC)-log(lag(KBDC))),
    D.LIEN = 100*(log(LIEN)-log(lag(LIEN))),
    D.MAIN = 100*(log(MAIN)-log(lag(MAIN))),
    D.MFIC = 100*(log(MFIC)-log(lag(MFIC))),
    D.MSDL = 100*(log(MSDL)-log(lag(MSDL))),
    D.MSIF = 100*(log(MSIF)-log(lag(MSIF))),
    D.NCDL = 100*(log(NCDL)-log(lag(NCDL))),
    D.NMFC = 100*(log(NMFC)-log(lag(NMFC))),
    D.NSLR = 100*(log(NSLR)-log(lag(NSLR))),
    D.OBDC = 100*(log(OBDC)-log(lag(OBDC))),
    D.OCSL = 100*(log(OCSL)-log(lag(OCSL))),
    D.OFS = 100*(log(OFS)-log(lag(OFS))),
    D.OXSQ = 100*(log(OXSQ)-log(lag(OXSQ))),
    D.PFLT = 100*(log(PFLT)-log(lag(PFLT))),
    D.PFX = 100*(log(PFX)-log(lag(PFX))),
    D.PNNT = 100*(log(PNNT)-log(lag(PNNT))),
    D.PSBD = 100*(log(PSBD)-log(lag(PSBD))),
    D.PSEC = 100*(log(PSEC)-log(lag(PSEC))),
    D.RAND = 100*(log(RAND)-log(lag(RAND))),
    D.RWAY = 100*(log(RWAY)-log(lag(RWAY))),
    D.SAR = 100*(log(SAR)-log(lag(SAR))),
    D.SCM = 100*(log(SCM)-log(lag(SCM))),
    D.SLRC = 100*(log(SLRC)-log(lag(SLRC))),
    D.TCPC = 100*(log(TCPC)-log(lag(TCPC))),
    D.TPVG = 100*(log(TPVG)-log(lag(TPVG))),
    D.TRIN = 100*(log(TRIN)-log(lag(TRIN))),
    D.TSLX = 100*(log(TSLX)-log(lag(TSLX))),
    D.WHF = 100*(log(WHF)-log(lag(WHF))),
  )

# Export merged data
write_xlsx(merged_data, "_data/merged_BDC_FF.xlsx")

# Call variables
D.SP500 <- merged_data$D.SP500
D.DJI <- merged_data$D.DJI
DGS <- merged_data$DGS2
# DGS <- c(NA, diff(merged_data$DGS2))

D.ARCC <- merged_data$D.ARCC
D.BBDC <- merged_data$D.BBDC
D.BCSF <- merged_data$D.BCSF
D.BXSL <- merged_data$D.BXSL
D.CCAP <- merged_data$D.CCAP
D.CGBD <- merged_data$D.CGBD
D.CION <- merged_data$D.CION
D.CSWC <- merged_data$D.CSWC
D.EQS <- merged_data$D.EQS
D.FDUS <- merged_data$D.FDUS
D.FSK <- merged_data$D.FSK
D.GAIN <- merged_data$D.GAIN
D.GBDC <- merged_data$D.GBDC
D.GECC <- merged_data$D.GECC
D.GLAD <- merged_data$D.GLAD
D.GSBD <- merged_data$D.GSBD
D.HRZN <- merged_data$D.HRZN
D.HTGC <- merged_data$D.HTGC
D.ICMB <- merged_data$D.ICMB
D.KBDC <- merged_data$D.KBDC
D.LIEN <- merged_data$D.LIEN
D.MAIN <- merged_data$D.MAIN
D.MFIC <- merged_data$D.MFIC
D.MSDL <- merged_data$D.MSDL
D.MSIF <- merged_data$D.MSIF
D.NCDL <- merged_data$D.NCDL
D.NMFC <- merged_data$D.NMFC
D.NSLR <- merged_data$D.NSLR
D.OBDC <- merged_data$D.OBDC
D.OCSL <- merged_data$D.OCSL
D.OFS <- merged_data$D.OFS
D.OXSQ <- merged_data$D.OXSQ
D.PFLT <- merged_data$D.PFLT
D.PFX <- merged_data$D.PFX
D.PNNT <- merged_data$D.PNNT
D.PSBD <- merged_data$D.PSBD
D.PSEC <- merged_data$D.PSEC
D.RAND <- merged_data$D.RAND
D.RWAY <- merged_data$D.RWAY
D.SAR <- merged_data$D.SAR
D.SCM <- merged_data$D.SCM
D.SLRC <- merged_data$D.SLRC
D.TCPC <- merged_data$D.TCPC
D.TPVG <- merged_data$D.TPVG
D.TRIN <- merged_data$D.TRIN
D.TSLX <- merged_data$D.TSLX
D.WHF <- merged_data$D.WHF

# Check missing observations
# colSums(is.na(merged_data[,c("D.SP500","D.DJI","DGS2","D.PSP",
#                              "D.PRIV","D.PCMM","D.HYIN","D.BZID")]))

# Check missing observations
colSums(is.na(merged_data[,c("D.SP500","D.DJI","DGS2",
                             "D.ARCC","D.BBDC","D.BCSF","D.BXSL","D.CCAP",
                             "D.CGBD","D.CION","D.CSWC","D.EQS","D.FDUS",
                             "D.FSK","D.GAIN","D.GBDC","D.GECC","D.GLAD",
                             "D.GSBD","D.HRZN","D.HTGC","D.ICMB","D.KBDC",
                             "D.LIEN","D.MAIN","D.MFIC","D.MSDL","D.MSIF",
                             "D.NCDL","D.NMFC","D.NSLR","D.OBDC","D.OCSL",
                             "D.OFS","D.OXSQ","D.PFLT","D.PFX","D.PNNT",
                             "D.PSBD","D.PSEC","D.RAND","D.RWAY","D.SAR",
                             "D.SCM","D.SLRC","D.TCPC","D.TPVG","D.TRIN",
                             "D.TSLX","D.WHF")]))

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
  list(D.ARCC, "ARCC"),
  list(D.BBDC, "BBDC"),
  list(D.BCSF, "BCSF"),
  list(D.BXSL, "BXSL"),
  list(D.CCAP, "CCAP"),
  list(D.CGBD, "CGBD"),
  list(D.CION, "CION"),
  list(D.CSWC, "CSWC"),
  list(D.EQS, "EQS"),
  list(D.FDUS, "FDUS"),
  list(D.FSK, "FSK"),
  list(D.GAIN, "GAIN"),
  list(D.GBDC, "GBDC"),
  list(D.GECC, "GECC"),
  list(D.GLAD, "GLAD"),
  list(D.GSBD, "GSBD"),
  list(D.HRZN, "HRZN"),
  list(D.HTGC, "HTGC"),
  list(D.ICMB, "ICMB"),
  list(D.KBDC, "KBDC"),
  list(D.LIEN, "LIEN"),
  list(D.MAIN, "MAIN"),
  list(D.MFIC, "MFIC"),
  list(D.MSDL, "MSDL"),
  list(D.MSIF, "MSIF"),
  list(D.NCDL, "NCDL"),
  list(D.NMFC, "NMFC"),
  list(D.NSLR, "NSLR"),
  list(D.OBDC, "OBDC"),
  list(D.OCSL, "OCSL"),
  list(D.OFS, "OFS"),
  list(D.OXSQ, "OXSQ"),
  list(D.PFLT, "PFLT"),
  list(D.PFX, "PFX"),
  list(D.PNNT, "PNNT"),
  list(D.PSBD, "PSBD"),
  list(D.PSEC, "PSEC"),
  list(D.RAND, "RAND"),
  list(D.RWAY, "RWAY"),
  list(D.SAR, "SAR"),
  list(D.SCM, "SCM"),
  list(D.SLRC, "SLRC"),
  list(D.TCPC, "TCPC"),
  list(D.TPVG, "TPVG"),
  list(D.TRIN, "TRIN"),
  list(D.TSLX, "TSLX"),
  list(D.WHF, "WHF")
)

irf_data <- lapply(varinfo, irf.linear) %>%
  bind_rows()

irf_data$variable <- factor(
  irf_data$variable,
  levels=c("ARCC","BBDC","BCSF","BXSL","CCAP","CGBD","CION","CSWC","EQS","FDUS",
           "FSK","GAIN","GBDC","GECC","GLAD","GSBD","HRZN","HTGC","ICMB","KBDC",
           "LIEN","MAIN","MFIC","MSDL","MSIF","NCDL","NMFC","NSLR","OBDC","OCSL",
           "OFS","OXSQ","PFLT","PFX","PNNT","PSBD","PSEC","RAND","RWAY","SAR",
           "SCM","SLRC","TCPC","TPVG","TRIN","TSLX","WHF")
)

# Export irf data
write_xlsx(irf_data, "_data/ird_bdc_output.xlsx")

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
