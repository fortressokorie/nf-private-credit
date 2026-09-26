# =============================================================================
# BDC quarterly fundamentals from SEC XBRL
# Run with: source("bdc_fundamentals.R")  or  Rscript bdc_fundamentals.R
# =============================================================================

# -----------------------------------------------------------------------------
# WHAT THIS BUILDS
# -----------------------------------------------------------------------------

# A quarterly panel (one row per BDC per quarter end) of:
#
# * net investment income (total and per share) and dividends per share, giving dividend coverage
# * investments at fair value vs. cost
# * leverage (debt ÷ net assets, and total liabilities ÷ net assets as a backup)
# * supporting items: net assets, NAV per share, shares, total assets/liabilities,
#   total investment income, realized/unrealized gains, net increase in net assets, fees
#
# All data comes from the SEC's companyfacts API: one request per company that returns
# every number the company has tagged in its 10-Qs and 10-Ks.
#
# How the SEC data is structured (why the code looks the way it does):
#
# * Balance-sheet items (net assets, fair value, debt) are point-in-time values at a date.
# * Income-statement items (NII, dividends, fees) cover a period. 10-Qs report the quarter
#   (and year-to-date); 10-Ks usually report only the full year. So Q4 is calculated as
#   full year minus the 9-month year-to-date figure and flagged in q4_derived.
# * Funds don't all use the same tag for the same item, so each variable has a ranked list of
#   candidate tags. The first one a fund actually uses is taken, and the tag used is recorded.
# * Each number appears in several filings (as a prior-period comparison). The original report
#   (earliest filing) is kept, and report_filed records when it became public.

library(tidyverse)
library(httr)
library(jsonlite)
library(writexl)

out_dir    <- "/Users/fortressokorie/nf-private-credit/_data"
sec_ua  <- "St. Olaf College research Fortress Okorie okoriekosi@gmail.com"  # SEC requires a contact

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. TICKER LIST
# -----------------------------------------------------------------------------

# get BDC universe from cefdata.com----------------------------------------------
cef_url  <- "https://cefdata.com/bdc-universe"
cef_page <- read_html(cef_url)

# Tickers: every link to an individual fund page (/funds/<ticker>) inside the table
get_tickers <- function(page) {
  page |>
    html_elements("table a[href*='/funds/']") |>
    html_attr("href") |>
    str_extract("(?<=/funds/)[A-Za-z.\\-]+") |>
    toupper() |>
    unique()
}

bdc_tickers <- get_tickers(cef_page)


# -----------------------------------------------------------------------------
# 2. VARIABLES AND CANDIDATE XBRL TAGS
# -----------------------------------------------------------------------------

# Tags are tried in order; the first one with data for a given quarter is used.
# To add a variable or a tag, just add a row / a tag here.

var_map <- tribble(
  ~var,                     ~type,      ~unit,        ~tags,
  # --- balance sheet (point-in-time) ---
  "investments_fair_value", "instant",  "USD",        c("InvestmentOwnedAtFairValue"),
  "investments_cost",       "instant",  "USD",        c("InvestmentOwnedAtCost"),
  "total_assets",           "instant",  "USD",        c("Assets"),
  "total_liabilities",      "instant",  "USD",        c("Liabilities"),
  "total_debt",             "instant",  "USD",        c("LongTermDebt", "DebtInstrumentCarryingAmount",
                                                        "DebtLongtermAndShorttermCombinedAmount",
                                                        "LongTermDebtNoncurrent"),
  "net_assets",             "instant",  "USD",        c("StockholdersEquity", "AssetsNet"),
  "shares_outstanding",     "instant",  "shares",     c("SharesOutstanding", "CommonStockSharesOutstanding"),
  "nav_per_share",          "instant",  "USD/shares", c("NetAssetValuePerShare"),
  # --- income statement (period flows) ---
  "total_investment_income","duration", "USD",        c("GrossInvestmentIncomeOperating",
                                                        "InvestmentIncomeInterestAndDividend", "Revenues"),
  "net_investment_income",  "duration", "USD",        c("NetInvestmentIncome"),
  "nii_per_share",          "duration", "USD/shares", c("InvestmentCompanyInvestmentIncomeLossPerShare",
                                                        "NetInvestmentIncomePerShare"),
  "dividends_per_share",    "duration", "USD/shares", c("CommonStockDividendsPerShareDeclared",
                                                        "InvestmentCompanyDistributionToShareholdersPerShare",
                                                        "CommonStockDividendsPerShareCashPaid"),
  "realized_gain_loss",     "duration", "USD",        c("RealizedInvestmentGainsLosses",
                                                        "DebtAndEquitySecuritiesRealizedGainLoss"),
  "unrealized_gain_loss",   "duration", "USD",        c("UnrealizedGainLossOnInvestments",
                                                        "DebtAndEquitySecuritiesUnrealizedGainLoss"),
  "net_increase_net_assets","duration", "USD",        c("NetIncomeLoss"),
  "management_fees",        "duration", "USD",        c("ManagementFeeExpense"),
  "incentive_fees",         "duration", "USD",        c("IncentiveFeeExpense")
)

# -----------------------------------------------------------------------------
# 3. DOWNLOAD COMPANY FACTS FROM SEC
# -----------------------------------------------------------------------------

sec_get <- function(url) {
  r <- GET(url, user_agent(sec_ua))
  Sys.sleep(0.15)                                  # SEC limit: 10 requests/second
  if (status_code(r) != 200) return(NULL)
  fromJSON(content(r, as = "text", encoding = "UTF-8"))
}

cik_map <- sec_get("https://www.sec.gov/files/company_tickers.json") |>
  bind_rows() |>
  transmute(ticker = toupper(ticker), cik = sprintf("%010d", as.integer(cik_str)))

# Pull every candidate tag for one company into a long table
get_company_facts <- function(tk) {
  cik <- cik_map$cik[cik_map$ticker == tk][1]
  if (is.na(cik)) { message("No CIK for ", tk); return(NULL) }
  j <- sec_get(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json"))
  if (is.null(j)) { message("No company facts for ", tk); return(NULL) }
  gaap <- j$facts$`us-gaap`

  pmap(var_map, \(var, type, unit, tags) {
    imap(tags, \(tag, priority) {
      d <- gaap[[tag]]$units[[unit]]
      if (is.null(d)) return(NULL)
      d <- as_tibble(d)
      if (!"start" %in% names(d)) d$start <- NA_character_
      d |> transmute(var, type, tag, priority, start = as.Date(start), end = as.Date(end),
                     val, form, filed = as.Date(filed))
    }) |> list_rbind()
  }) |>
    list_rbind() |>
    mutate(ticker = tk, cik = cik, .before = 1)
}

facts_raw <- map(bdc_tickers, \(tk) { message(tk); get_company_facts(tk) }) |> list_rbind()

facts <- facts_raw |> filter(form %in% c("10-Q", "10-K", "10-Q/A", "10-K/A"))

# -----------------------------------------------------------------------------
# 4. BUILD QUARTERLY VALUES
# -----------------------------------------------------------------------------

# Keep the original report of each fact (earliest filing)
first_report <- function(d, ...) d |> arrange(filed) |> distinct(..., .keep_all = TRUE)

# --- point-in-time items: keep month-end dates only ---
instants <- facts |>
  filter(type == "instant", mday(end + 1) == 1) |>
  first_report(ticker, var, tag, end) |>
  mutate(derived = FALSE)

# --- period items ---
dur <- facts |>
  filter(type == "duration", !is.na(start)) |>
  mutate(days = as.numeric(end - start)) |>
  first_report(ticker, var, tag, start, end)

quarters <- dur |> filter(between(days, 80, 100)) |> mutate(derived = FALSE)

# Q4 = full fiscal year minus 9-month year-to-date with the same start date
annual <- dur |> filter(between(days, 350, 380))
ytd9   <- dur |> filter(between(days, 260, 290))

q4 <- annual |>
  inner_join(ytd9 |> select(ticker, var, tag, start, val9 = val),
             by = c("ticker", "var", "tag", "start")) |>
  mutate(start = end - 91, val = val - val9, derived = TRUE) |>
  select(-val9)

durations <- bind_rows(quarters, q4) |> select(-days)

# One value per company-variable-quarter: best-ranked tag, then a directly reported
# quarter over a derived Q4
fund_long <- bind_rows(instants, durations) |>
  arrange(ticker, var, end, priority, derived, filed) |>
  distinct(ticker, var, end, .keep_all = TRUE) |>
  rename(period_end = end)

fund <- fund_long |>
  group_by(ticker, cik, period_end) |>
  mutate(report_filed = max(filed), q4_derived = any(derived)) |>
  ungroup() |>
  pivot_wider(id_cols = c(ticker, cik, period_end, report_filed, q4_derived),
              names_from = var, values_from = val) |>
  # drop rows that are only stray point-in-time facts
  filter(!is.na(net_assets) | !is.na(net_investment_income))

# make sure every variable column exists even if no fund used its tags
for (v in setdiff(var_map$var, names(fund))) fund[[v]] <- NA_real_

# -----------------------------------------------------------------------------
# 5. DERIVED MEASURES
# -----------------------------------------------------------------------------

bdc_fundamentals <- fund |>
  mutate(
    calendar_quarter      = paste0(year(period_end), "Q", quarter(period_end)),
    # fill NII per share from totals when not tagged (uses period-end shares, so approximate)
    nii_per_share_source  = if_else(!is.na(nii_per_share), "reported",
                              if_else(!is.na(net_investment_income / shares_outstanding),
                                      "computed", NA_character_)),
    nii_per_share         = coalesce(nii_per_share, net_investment_income / shares_outstanding),
    nav_per_share         = coalesce(nav_per_share, net_assets / shares_outstanding),
    dividend_coverage     = nii_per_share / dividends_per_share,
    fv_to_cost            = investments_fair_value / investments_cost,
    unrealized_appreciation = investments_fair_value - investments_cost,
    debt_to_equity        = total_debt / net_assets,
    liabilities_to_equity = coalesce(total_liabilities, total_assets - net_assets) / net_assets,
    nii_return_on_nav_ann = 4 * net_investment_income / net_assets
  ) |>
  select(ticker, cik, calendar_quarter, period_end, report_filed, q4_derived,
         net_investment_income, nii_per_share, nii_per_share_source, dividends_per_share,
         dividend_coverage, nii_return_on_nav_ann,
         investments_fair_value, investments_cost, fv_to_cost, unrealized_appreciation,
         total_debt, total_liabilities, total_assets, net_assets,
         debt_to_equity, liabilities_to_equity,
         nav_per_share, shares_outstanding,
         total_investment_income, realized_gain_loss, unrealized_gain_loss,
         net_increase_net_assets, management_fees, incentive_fees) |>
  arrange(ticker, period_end)

glimpse(bdc_fundamentals)

# -----------------------------------------------------------------------------
# 6. COVERAGE CHECKS
# -----------------------------------------------------------------------------

# Look at these before using the data: which variables each fund actually reports, and
# which tag was used.

coverage <- bdc_fundamentals |>
  group_by(ticker) |>
  summarise(first_quarter = min(period_end), last_quarter = max(period_end), n_quarters = n(),
            across(c(net_investment_income, nii_per_share, dividends_per_share,
                     investments_fair_value, investments_cost, total_debt, net_assets),
                   \(x) round(mean(!is.na(x)), 2), .names = "pct_{.col}"))
coverage |> print(n = Inf, width = Inf)

print(setdiff(bdc_tickers, unique(bdc_fundamentals$ticker)))   # no SEC data found

tag_usage <- fund_long |> count(ticker, var, tag, name = "n_quarters")

# Sanity checks: flag rows that look wrong
checks <- bdc_fundamentals |>
  filter(dividend_coverage < 0.5 | dividend_coverage > 2 |
         fv_to_cost < 0.5 | fv_to_cost > 1.5 |
         debt_to_equity > 3 | liabilities_to_equity > 3) |>
  select(ticker, period_end, q4_derived, dividend_coverage, fv_to_cost,
         debt_to_equity, liabilities_to_equity)
print(checks, n = Inf)

# -----------------------------------------------------------------------------
# 7. DATA DICTIONARY
# -----------------------------------------------------------------------------

dictionary <- tribble(
  ~variable, ~description, ~units, ~notes,
  "ticker", "Exchange ticker", "Text", "",
  "cik", "SEC Central Index Key", "10-digit text", "",
  "calendar_quarter", "Calendar quarter of period_end", "e.g. 2025Q3", "Funds with non-December fiscal years still map to the calendar quarter.",
  "period_end", "Quarter-end date the figures refer to", "Date", "",
  "report_filed", "Date the last of this row's figures was first filed with the SEC", "Date", "Join to daily prices on this date (not period_end) to avoid look-ahead bias.",
  "q4_derived", "TRUE if any quarterly flow in the row was calculated as full year minus 9-month YTD", "Logical", "Derived per-share Q4 values are approximate.",
  "net_investment_income", "Net investment income (investment income minus expenses, before gains/losses) for the quarter", "USD", "Main earnings measure for BDCs.",
  "nii_per_share", "NII per share for the quarter", "USD per share", "See nii_per_share_source.",
  "nii_per_share_source", "'reported' = tagged by fund; 'computed' = NII / period-end shares", "Text", "",
  "dividends_per_share", "Dividends declared per share in the quarter", "USD per share", "May include specials; some funds tag paid rather than declared.",
  "dividend_coverage", "nii_per_share / dividends_per_share", "Ratio", "<1 = dividend not covered by NII.",
  "nii_return_on_nav_ann", "4 × NII / net assets", "Decimal (0.10 = 10%)", "Annualized earnings yield on NAV.",
  "investments_fair_value", "Total investment portfolio at fair value", "USD", "",
  "investments_cost", "Total investment portfolio at amortized cost", "USD", "",
  "fv_to_cost", "investments_fair_value / investments_cost", "Ratio", "<1 = portfolio marked below cost (credit stress).",
  "unrealized_appreciation", "Fair value minus cost", "USD", "Negative = cumulative net unrealized depreciation.",
  "total_debt", "Total borrowings", "USD", "Tagging varies the most across funds; check tag_usage.",
  "total_liabilities", "Total liabilities", "USD", "",
  "total_assets", "Total assets", "USD", "",
  "net_assets", "Net assets (equity)", "USD", "= NAV × shares.",
  "debt_to_equity", "total_debt / net_assets", "Ratio", "Regulatory cap is 2.0 for most BDCs (asset coverage ≥150%).",
  "liabilities_to_equity", "total_liabilities / net_assets", "Ratio", "Backup leverage measure; slightly above debt_to_equity because it includes payables.",
  "nav_per_share", "Net asset value per share", "USD per share", "Filled from net_assets / shares if not tagged.",
  "shares_outstanding", "Common shares outstanding at period end", "Shares", "",
  "total_investment_income", "Total investment income (interest, dividends, fees)", "USD", "",
  "realized_gain_loss", "Net realized gains (losses) on investments", "USD", "Patchy tagging.",
  "unrealized_gain_loss", "Net change in unrealized appreciation (depreciation)", "USD", "Patchy tagging.",
  "net_increase_net_assets", "Net increase in net assets resulting from operations (NII + gains/losses)", "USD", "",
  "management_fees", "Base management fee expense", "USD", "Patchy; internally managed BDCs (e.g. MAIN, CSWC) have none.",
  "incentive_fees", "Incentive fee expense", "USD", "Patchy tagging."
)

# -----------------------------------------------------------------------------
# 8. SAVE
# -----------------------------------------------------------------------------

write_csv(bdc_fundamentals, file.path(out_dir, "bdc_fundamentals.csv"))

write_xlsx(
  list(data_dictionary = dictionary,
       fundamentals_quarterly = bdc_fundamentals,
       coverage = coverage,
       tag_usage = tag_usage,
       checks = checks),
  file.path(out_dir, "bdc_fundamentals.xlsx")
)

out_dir_2    <- "/Users/fortressokorie/nf-private-credit/_dictionaries"
write.csv(dictionary, file.path(out_dir_2, "bdc_fundamentals_sec_dictionary.csv"))

# -----------------------------------------------------------------------------
# OPTIONAL: ATTACH TO THE DAILY PRICE PANEL
# -----------------------------------------------------------------------------

# Adds the latest *publicly available* fundamentals to each trading day.

if (FALSE) {  # optional: set to TRUE to run
  panel <- read_csv(file.path(out_dir, "bdc_daily_panel.csv"))

  panel_fund <- panel |>
    left_join(bdc_fundamentals |> select(-cik),
              by = join_by(ticker, closest(date >= report_filed))) |>
    mutate(dividend_yield_ann = 4 * dividends_per_share / close_unadj)
}
