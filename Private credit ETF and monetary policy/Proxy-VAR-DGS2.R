# Load libraries
library(dplyr)
library(writexl)
library(vars)
library(sovereign)
library(tidyr)
library(purrr)
library(ggplot2)

# Read datasets
etf <- read.csv("ETFs.csv")
ff  <- read.csv("FF_FUTURE.csv")
dgs <- read.csv("DGS2.csv")
sp500 <- read.csv("SP500.csv")
dji <- read.csv("dji_data.csv")
bdc <- read.csv("BDC_Index.csv")
mps <- read.csv("monetary-policy-surprises.csv")

# Identify date columns
etf_date_col <- names(etf)[grepl("date", names(etf), ignore.case=TRUE)][1]
ff_date_col  <- names(ff)[grepl("date", names(ff), ignore.case=TRUE)][1]
dgs_date_col <- names(dgs)[grepl("observation_date", names(dgs), ignore.case=TRUE)][1]
sp500_date_col <- names(sp500)[grepl("date", names(sp500), ignore.case=TRUE)][1]
dji_date_col <- names(dji)[grepl("date", names(dji), ignore.case=TRUE)][1]
bdc_date_col <- names(bdc)[grepl("date", names(bdc), ignore.case=TRUE)][1]
mps_date_col <- names(mps)[grepl("date", names(mps), ignore.case=TRUE)][1]

# Convert dates
etf[[etf_date_col]] <- as.Date(etf[[etf_date_col]])
ff[[ff_date_col]]   <- as.Date(ff[[ff_date_col]])
dgs[[dgs_date_col]] <- as.Date(dgs[[dgs_date_col]])
sp500[[sp500_date_col]] <- as.Date(sp500[[sp500_date_col]])
dji[[dji_date_col]] <- as.Date(dji[[dji_date_col]])
bdc[[bdc_date_col]] <- as.Date(bdc[[bdc_date_col]], format="%m/%d/%y")
mps[[mps_date_col]] <- as.Date(mps[[mps_date_col]], format="%m/%d/%y")

# Rename date columns
names(etf)[names(etf)==etf_date_col] <- "date"
names(ff)[names(ff)==ff_date_col] <- "date"
names(dgs)[names(dgs)==dgs_date_col] <- "date"
names(sp500)[names(sp500)==sp500_date_col] <- "date"
names(dji)[names(dji)==dji_date_col] <- "date"
names(bdc)[names(bdc)==bdc_date_col] <- "date"
names(mps)[names(mps)==mps_date_col] <- "date"

# Merge datasets
merged_data <- etf %>%
  inner_join(ff, by="date") %>%
  inner_join(dgs, by="date") %>%
  inner_join(sp500, by="date") %>%
  inner_join(dji, by="date") %>%
  inner_join(bdc, by="date") %>%
  left_join(mps, by="date") %>%
  arrange(date)

merged_data[is.na(merged_data)] <- 0

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

MPS <- merged_data$ED_PC1_scaled

# Excess Returns
D.BDC <- merged_data$D.BDC-D.SP500
D.PSP <- merged_data$D.PSP-D.SP500
D.PRIV <- merged_data$D.PRIV-D.SP500
D.PCMM <- merged_data$D.PCMM-D.SP500
D.HYIN <- merged_data$D.HYIN-D.SP500
D.BZID <- merged_data$D.BZID-D.SP500


var_irf_cumulative <- function(var, horizon = 30, CI = c(0.05, 0.95),
                               bootstrap.num = 500, seed = 1234) {
  
  if (var$structure != "IV") stop("This function is written for structure = 'IV'.")
  
  set.seed(seed)
  
  coef         <- var$model$coef
  residuals    <- var$residuals[[1]]
  data         <- var$data
  p            <- var$model$p
  freq         <- var$model$freq
  type         <- var$model$type
  instrument   <- var$instrument
  instrumented <- var$instrumented
  
  regressors <- colnames(dplyr::select(data, -date))
  K <- length(regressors)
  
  p.lower <- CI[1]
  p.upper <- CI[2]
  
  # --------------------------------------------------
  # Function to extract IV IRFs
  # --------------------------------------------------
  
  get_irf <- function(v) {
    
    B <- as.matrix(sovereign:::solve_B(v, report_iv = FALSE))
    coef.v <- v$model$coef
    
    phi <- coef.v[, !names(coef.v) %in% "y" &
                    !grepl("const", names(coef.v)) &
                    !grepl("trend", names(coef.v)), drop = FALSE]
    
    irf.mat <- sovereign:::IRF_solve(
      Phi = phi, B = B, lag = horizon, structure = "IV"
    )
    
    irf.df <- data.frame(t(irf.mat))
    
    keep.rows <- (1:horizon) * K - K + 1
    irf.df <- irf.df[keep.rows, , drop = FALSE]
    
    irf.df$shock   <- instrumented
    irf.df$horizon <- 0:(horizon - 1)
    
    colnames(irf.df) <- c(regressors, "shock", "horizon")
    
    irf.df %>%
      tidyr::pivot_longer(
        cols = all_of(regressors),
        names_to = "target",
        values_to = "response"
      )
  }
  
  # --------------------------------------------------
  # Cumulative point estimates
  # --------------------------------------------------
  
  point.irf <- get_irf(var) %>%
    dplyr::group_by(shock, target) %>%
    dplyr::arrange(horizon, .by_group = TRUE) %>%
    dplyr::mutate(response = cumsum(response)) %>%
    dplyr::ungroup()
  
  # --------------------------------------------------
  # Bootstrap
  # --------------------------------------------------
  
  bootstrap.irfs <- lapply(1:bootstrap.num, function(b) {
    
    U <- residuals[, -c(1, 2)]
    r <- sample(c(-1, 1), size = nrow(U), replace = TRUE)
    U <- sweep(U, MARGIN = 1, STATS = r, FUN = "*")
    
    instrument.bag <- instrument
    instrument.bag[, -1] <- instrument.bag[, -1] * r
    
    Y <- data.frame(matrix(
      NA, nrow = nrow(data), ncol = length(regressors)
    ))
    
    colnames(Y) <- regressors
    Y[1:p, ] <- data[1:p, regressors]
    
    for (i in (p + 1):nrow(data)) {
      
      X <- Y[(i - p):(i - 1), ]
      X <- data.frame(stats::embed(as.matrix(X), dimension = p))
      
      if (type %in% c("const", "both")) X$const <- 1
      if (type %in% c("trend", "both")) X$trend <- 1:nrow(X)
      
      X.hat <- t(as.matrix(coef[, -1]) %*% t(as.matrix(X)))
      Y[i, ] <- X.hat - U[i, ]
    }
    
    Y$date <- data$date
    
    var.bag <- sovereign::VAR(
      data = Y, p = p, horizon = 1, freq = freq, type = type
    )
    
    var.bag$structure    <- "IV"
    var.bag$instrumented <- instrumented
    var.bag$instrument   <- instrument.bag
    
    temp.irf <- get_irf(var.bag) %>%
      dplyr::group_by(shock, target) %>%
      dplyr::arrange(horizon, .by_group = TRUE) %>%
      dplyr::mutate(response = cumsum(response)) %>%
      dplyr::ungroup()
    
    temp.irf$draw <- b
    
    return(temp.irf)
  })
  
  # --------------------------------------------------
  # Confidence intervals
  # --------------------------------------------------
  
  boot.all <- dplyr::bind_rows(bootstrap.irfs)
  
  ci <- boot.all %>%
    dplyr::group_by(shock, target, horizon) %>%
    dplyr::summarise(
      response.lower = quantile(response, probs = p.lower, na.rm = TRUE),
      response.upper = quantile(response, probs = p.upper, na.rm = TRUE),
      response.mean  = mean(response, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --------------------------------------------------
  # Combine point estimates and confidence intervals
  # --------------------------------------------------
  
  results <- point.irf %>%
    dplyr::left_join(ci, by = c("shock", "target", "horizon")) %>%
    dplyr::mutate(
      response.adjust = response - response.mean,
      response.lower  = response.lower + response.adjust,
      response.upper  = response.upper + response.adjust
    ) %>%
    dplyr::select(
      target, shock, horizon,
      response.lower, response, response.upper
    ) %>%
    dplyr::arrange(target, shock, horizon)
  
  return(results)
}

var.irf.cum <- var_irf_cumulative(
  var,
  horizon = 30,
  CI = c(0.05, 0.95),
  bootstrap.num = 500,
  seed = 1234
)

irf.bdc.cum <- var.irf.cum %>%
  dplyr::filter(shock == "DGS", target == "D.BDC")

ggplot(irf.bdc.cum, aes(x = horizon, y = response)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(
    aes(ymin = response.lower, ymax = response.upper),
    alpha = 0.20
  ) +
  geom_line(linewidth = 1) +
  theme_light() +
  labs(title = "Stock prices", x = "Horizon", y = "")