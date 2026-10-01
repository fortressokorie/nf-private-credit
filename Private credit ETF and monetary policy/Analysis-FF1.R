# Load libraries
library(dplyr)
library(writexl)
library(vars)

# Read datasets
etf <- read.csv("ETFs.csv")
ff  <- read.csv("FF_FUTURE.csv")

# Identify date columns
etf_date_col <- names(etf)[grepl("date", names(etf), ignore.case=TRUE)][1]
ff_date_col  <- names(ff)[grepl("date", names(ff), ignore.case=TRUE)][1]

# Convert dates
etf[[etf_date_col]] <- as.Date(etf[[etf_date_col]])
ff[[ff_date_col]]   <- as.Date(ff[[ff_date_col]])

# Rename date columns
names(etf)[names(etf)==etf_date_col] <- "date"
names(ff)[names(ff)==ff_date_col] <- "date"

# Merge datasets
merged_data <- etf %>%
  inner_join(ff, by="date") %>%
  arrange(date)

# Construct implied Fed Funds rate and ETF returns
merged_data <- merged_data %>%
  mutate(
    FF_RATE = 100 - FF_FUTURE,
    D.PSP  = 100*(log(PSP)-log(lag(PSP))),
    D.PRIV = 100*(log(PRIV)-log(lag(PRIV))),
    D.PCMM = 100*(log(PCMM)-log(lag(PCMM))),
    D.HYIN = 100*(log(HYIN)-log(lag(HYIN))),
    D.BZID = 100*(log(BZID)-log(lag(BZID)))
  )

# Export merged data
write_xlsx(merged_data, "merged_ETF_FF.xlsx")

# Call variables
FF <- merged_data$FF_RATE
D.PSP <- merged_data$D.PSP
D.PRIV <- merged_data$D.PRIV
D.PCMM <- merged_data$D.PCMM
D.HYIN <- merged_data$D.HYIN
D.BZID <- merged_data$D.BZID

# Check missing observations
colSums(is.na(merged_data[,c("FF_RATE","D.PSP","D.PRIV",
                             "D.PCMM","D.HYIN","D.BZID")]))

# VAR and IRF function
irf.linear <- function(obj) {
  macro <- obj[[1]]
  varname <- obj[[2]]
  
  var_data <- data.frame(FF=FF, macro=macro)
  var_data <- var_data[complete.cases(var_data), ]
  
  cat(varname, ":", nrow(var_data), "observations used\n")
  
  rf <- vars::VAR(var_data, p=12, type="const")
  
  res <- irf(rf, impulse="FF", response="macro",
             n.ahead=30, cumulative=TRUE, ci=0.90)
  
  return(list(res=res, varname=varname, nobs=nrow(var_data)))
}

# ETF list
varinfo <- list(
  list(D.PSP, "PSP"),
  list(D.PRIV, "PRIV"),
  list(D.PCMM, "PCMM"),
  list(D.HYIN, "HYIN"),
  list(D.BZID, "BZID")
)

# Estimate IRFs
irfs <- lapply(varinfo, irf.linear)
names(irfs) <- c("PSP","PRIV","PCMM","HYIN","BZID")

# Common symmetric y-axis
all_values <- unlist(lapply(irfs, function(x)
  c(x$res$irf$FF[,"macro"],
    x$res$Lower$FF[,"macro"],
    x$res$Upper$FF[,"macro"])))

ymax <- max(abs(all_values), na.rm=TRUE)
common_ylim <- c(-ymax, ymax)

# Plot IRFs
par(mfrow=c(3,2))

for (x in irfs) {
  res <- x$res
  varname <- x$varname
  
  irf_est <- res$irf$FF[,"macro"]
  irf_low <- res$Lower$FF[,"macro"]
  irf_up <- res$Upper$FF[,"macro"]
  t <- 0:30
  
  plot(t, irf_est, xlab="Days after shock", ylab="% Change",
       type="l", main=varname, lwd=1, lty=1, ylim=common_ylim)
  
  lines(t, irf_low, lwd=1, lty=2)
  lines(t, irf_up, lwd=1, lty=2)
  abline(h=0)
}

# Display sample sizes
sapply(irfs, function(x) x$nobs)
