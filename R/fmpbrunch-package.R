#' fmpbrunch: Minimal FMP Client (v3 & stable)
#'
#' @description
#' Lightweight R client for FinancialModelingPrep. Supports prices,
#' metrics (key/ratios/growth/ev), profiles, and various quote/list endpoints—
#' all returned as tidy tibbles.
#'
#' @details
#' **Functions:**
#' - `fmp_set_key()`   – Set API key (alias: `set_fmp_key()` – deprecated)
#' - `fmp_prices()`    – Historical quotes (v3 & stable)
#' - `fmp_metrics()`   – Metrics (key/ratios/growth/ev; optional TTM; v3 & stable)
#' - `fmp_profile()`   – Company profiles (v3 & stable)
#' - **Bulk/CSV (stable):**
#'   - `fmp_profiles_bulk()` / `fmp_profiles_bulk_all()` – Company profiles by parts / all
#'   - `fmp_metrics_ttm_bulk(type = c("key","ratios"))`  – Key-Metrics-TTM or Ratios-TTM
#'   - `fmp_eod_bulk(date)`                               – Full EOD dump for a date
#' - **Quotes & Lists:**
#'   - FX:         `fmp_fx_quotes()`         – All available forex pairs
#'   - Crypto:     `fmp_crypto_quotes()`     – All available crypto quotes
#'   - Commodities:`fmp_commodity_quotes()`  – All available commodity quotes
#'   - Exchanges:  `fmp_exchanges()`         – Available exchanges/venues
#'   - Stocks:     `fmp_stock_list()`        – Full stock ticker list
#'
#' **Quick Start**
#' \preformatted{
#' library(fmpbrunch)
#' fmp_set_key("YOUR_KEY")  # falls back to Sys.getenv("FMP_API_KEY")
#'
#' # Prices & metrics
#' fmp_prices(c("AAPL","MSFT"), from = "2025-10-15", version = "stable")
#' fmp_metrics("AAPL", metric = "key", ttm = TRUE, version = "stable")
#' fmp_profile("AAPL", version = "stable")
#'
#' # Quotes & lists
#' fmp_fx_quotes("v3")
#' fmp_crypto_quotes("v3")
#' fmp_commodity_quotes("stable")
#' fmp_exchanges("stable")
#' fmp_stock_list("v3")
#'
#' # Bulk/CSV (stable)
#' fmp_profiles_bulk(0:2)                           # specific parts
#' fmp_profiles_bulk_all(max_parts = 100)           # auto until empty
#' fmp_metrics_ttm_bulk("key")                      # or "ratios"
#' fmp_eod_bulk("2024-10-22")                       # full-day dump
#' }
#'
#' @seealso
#' [`fmp_set_key()`], [`set_fmp_key()`],
#' [`fmp_prices()`], [`fmp_metrics()`], [`fmp_profile()`],
#' [`fmp_profiles_bulk()`], [`fmp_profiles_bulk_all()`],
#' [`fmp_metrics_ttm_bulk()`], [`fmp_eod_bulk()`],
#' [`fmp_fx_quotes()`], [`fmp_crypto_quotes()`], [`fmp_commodity_quotes()`],
#' [`fmp_exchanges()`], [`fmp_stock_list()`]
#'
#' @docType package
#' @name fmpbrunch
#' @aliases fmpbrunch-package
#' @keywords internal
"_PACKAGE"
