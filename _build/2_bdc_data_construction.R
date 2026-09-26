# load necessary packages-------------------------------------------------------
library(quantmod)
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(writexl)

# Objectives--------------------------------------------------------------------
# 1. **Universe** — every BDC ticker listed on cefdata.com/bdc-universe (plus its current NAV snapshot).
# 2. **Daily prices** — Open, High, Low, Close, Volume, Adjusted close from Yahoo Finance via `quantmod`.
# 3. **Historical NAV** — quarterly NAV per share from SEC EDGAR XBRL filings (Yahoo does *not* carry BDC NAV history).
# 4. **Daily price-to-NAV** — daily close divided by the most recent NAV.
# 5. **Current P/NAV snapshots** — from cefdata, bdcinvestor.com, and Yahoo's "Book Value" field, for cross-checking.

# set up------------------------------------------------------------------------
out_dir    <- "/Users/fortressokorie/nf-private-credit/_data"
start_date <- "2010-01-01"
end_date   <- Sys.Date()

# SEC requires a descriptive User-Agent with a contact email
sec_ua <- "St. Olaf College research Fortress Okorie okoriekosi@gmail.com"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


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

# cross-check bdc universe
if (length(bdc_tickers) < 45) {
  warning("Could not read the full cefdata list; using hard-coded fallback list.")
  fallback <- c("ARCC","BBDC","BCSF","BXSL","CCAP","CGBD","CION","CSWC","EQS","FDUS",
                "FSK","GAIN","GBDC","GECC","GLAD","GSBD","HRZN","HTGC","ICMB","KBDC",
                "LIEN","MAIN","MFIC","MRCC","MSDL","MSIF","NCDL","NMFC","NSLR","OBDC",
                "OCSL","OFS","OXSQ","PFLT","PFX","PNNT","PSBD","PSEC","RAND","RWAY",
                "SAR","SCM","SLRC","TCPC","TPVG","TRIN","TSLX","WHF")
  bdc_tickers <- union(bdc_tickers, fallback)
}

bdc_tickers <- sort(bdc_tickers)
length(bdc_tickers)
bdc_tickers

# daily values from Yahoo Finance-----------------------------------------------
get_yahoo <- function(tk) {
  x <- tryCatch(
    getSymbols(tk, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE),
    error = function(e) { message("Yahoo failed for ", tk, ": ", conditionMessage(e)); NULL }
  )
  if (is.null(x)) return(NULL)
  tibble(
    ticker    = tk,
    date      = index(x),
    open      = as.numeric(Op(x)),
    high      = as.numeric(Hi(x)),
    low       = as.numeric(Lo(x)),
    close     = as.numeric(Cl(x)),
    volume    = as.numeric(Vo(x)),
    adj_close = as.numeric(Ad(x))
  )
}

prices <- map(bdc_tickers, \(tk) { Sys.sleep(1); get_yahoo(tk) }) |> list_rbind()

prices |> group_by(ticker) |> summarise(first = min(date), last = max(date), n = n()) |> print(n = Inf)
setdiff(bdc_tickers, unique(prices$ticker))   # tickers Yahoo didn't return

# Yahoo treats BDCs as ordinary stocks, so it has no NAV field.
# The closest thing is the current **Book Value per share** (for a BDC, book value per share is essentially NAV per share) 
# and **Price/Book**. Pulled here only as a snapshot/cross-check.


# Historical NAV per share from SEC EDGAR (free, official)---------------------------

# BDCs tag `us-gaap:NetAssetValuePerShare` in their 10-Q/10-K XBRL. This gives a quarterly
# NAV history going back to roughly 2009–2011 for most funds.

sec_get <- function(url) {
  r <- GET(url, user_agent(sec_ua))
  Sys.sleep(0.15)                       # SEC limit is 10 requests/second
  if (status_code(r) != 200) return(NULL)
  fromJSON(content(r, as = "text", encoding = "UTF-8"))
}

# Ticker -> CIK map
cik_map <- sec_get("https://www.sec.gov/files/company_tickers.json") |>
  bind_rows() |>
  transmute(ticker = toupper(ticker), cik = sprintf("%010d", as.integer(cik_str)), sec_name = title)

get_sec_nav <- function(tk) {
  cik <- cik_map$cik[cik_map$ticker == tk][1]
  if (is.na(cik)) { message("No CIK for ", tk); return(NULL) }
  j <- sec_get(paste0("https://data.sec.gov/api/xbrl/companyconcept/CIK", cik,
                      "/us-gaap/NetAssetValuePerShare.json"))
  if (is.null(j) || is.null(j$units[["USD/shares"]])) { message("No NAV tag for ", tk); return(NULL) }
  as_tibble(j$units[["USD/shares"]]) |>
    filter(form %in% c("10-Q", "10-K", "10-Q/A", "10-K/A")) |>
    transmute(ticker = tk, cik, period_end = as.Date(end), nav = val,
              filed = as.Date(filed), form)
}

nav_raw <- map(bdc_tickers, get_sec_nav) |> list_rbind()

# Each NAV shows up in several filings (as a prior-period comparative).
# Keep the ORIGINAL report for each quarter end (earliest filing date).
nav_q <- nav_raw |>
  arrange(ticker, period_end, filed) |>
  distinct(ticker, period_end, .keep_all = TRUE)

nav_q |> group_by(ticker) |> summarise(first = min(period_end), last = max(period_end), n = n()) |> print(n = Inf)
setdiff(bdc_tickers, unique(nav_q$ticker))    # tickers with no SEC NAV history

# Daily Price to Nav------------------------------------------------------------
# Two versions, since which one is "right" depends on the research question:
# `nav_period` / `p_nav_period` — NAV for the most recent **quarter end** on or before the date
# (what the fund was worth; uses information not public until the 10-Q is filed).
# `nav_known` / `p_nav_known` — most recent NAV **already filed with the SEC** as of that date
# (what investors could actually see; no look-ahead — better for return/event studies).

nav_by_period <- nav_q |> select(ticker, period_end, nav_period = nav)
nav_by_filed  <- nav_q |> select(ticker, filed, nav_known = nav, nav_known_period = period_end)

bdc_panel <- prices |>
  left_join(nav_by_period, by = join_by(ticker, closest(date >= period_end))) |>
  left_join(nav_by_filed,  by = join_by(ticker, closest(date >= filed))) |>
  mutate(p_nav_period = close / nav_period,
         p_nav_known  = close / nav_known) |>
  select(ticker, date, open, high, low, close, volume, adj_close,
         nav_period, nav_period_end = period_end, p_nav_period,
         nav_known, nav_known_period, nav_filed = filed, p_nav_known) |>
  arrange(ticker, date)

glimpse(bdc_panel)

# snapshot for cross-checking----------------------------------------------------
latest_panel <- bdc_panel |> group_by(ticker) |> slice_max(date, n = 1) |> ungroup() |>
  select(ticker, last_date = date, close, nav_known, nav_known_period, p_nav_known)

# write datasets out------------------------------------------------------------
write_csv(bdc_panel, file.path(out_dir, "bdc_daily_panel.csv"))   # one long file, all tickers

write_csv(prices, file.path(out_dir, "bdc_prices_panel.csv"))

write_csv(nav_raw, file.path(out_dir, "nav_raw.csv"))

write_csv(nav_by_period, file.path(out_dir, "nav_by_period.csv"))

write_csv(nav_by_filed, file.path(out_dir, "nav_by_filed.csv"))

write_xlsx(
  list(daily_panel = bdc_panel,
       nav_quarterly = nav_q),
  file.path(out_dir, "bdc_dataset.xlsx")
)

