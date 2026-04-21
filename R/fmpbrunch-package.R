#' fmpbrunch: Minimal FMP Client (v3 & stable)
#'
#' @description
#' Lightweight R client for FinancialModelingPrep. Supports prices,
#' metrics, profiles, news, quotes, screeners, bulk CSV endpoints,
#' and various list endpoints — all returned as tidy tibbles.
#'
#' @details
#' **Functions:**
#' - `fmp_set_key()` – Set API key (alias: `set_fmp_key()` – deprecated)
#'
#' - **Prices & market data**
#'   - `fmp_prices()` – Historical quotes (v3 & stable)
#'   - `fmp_prices_divadj()` – Dividend-adjusted EOD prices (stable)
#'   - `fmp_quote()` – Real-time stock quote(s) (stable)
#'
#' - **Fundamentals & company data**
#'   - `fmp_metrics()` – Metrics (`key` / `ratios` / `growth` / `ev`; v3 & stable)
#'   - `fmp_profile()` – Company profiles (v3 & stable)
#'   - `fmp_earnings()` – Earnings reports (stable)
#'   - `fmp_dividends()` – Dividend history (stable)
#'   - `fmp_company_screener()` – Company screener (stable)
#'
#' - **News**
#'   - `fmp_news()` – News/articles from one stable news endpoint
#'   - `fmp_news_bulk()` – Combined news from multiple stable endpoints
#'
#' - **Quotes & lists**
#'   - `fmp_fx_quotes()` – All available forex pairs
#'   - `fmp_crypto_quotes()` – All available crypto quotes
#'   - `fmp_commodity_quotes()` – All available commodity quotes
#'   - `fmp_exchanges()` – Available exchanges/venues
#'   - `fmp_stock_list()` – Full stock ticker list
#'   - `fmp_index_constituents()` – Constituents of major US indices
#'
#' - **Bulk/CSV (stable)**
#'   - `fmp_profiles_bulk()` / `fmp_profiles_bulk_all()` – Company profiles by parts / all
#'   - `fmp_metrics_ttm_bulk()` – Bulk TTM key-metrics or ratios
#'   - `fmp_eod_bulk()` – Full EOD dump for a date
#'   - `fmp_income_statement_bulk()` – Bulk income statement (standard / growth)
#'   - `fmp_balance_sheet_bulk()` – Bulk balance sheet (standard / growth)
#'   - `fmp_cash_flow_bulk()` – Bulk cash flow statement (standard / growth)
#'
#' @section Quick Start:
#' \preformatted{
#' library(fmpbrunch)
#'
#' # Use key from environment...
#' Sys.setenv(FMP_API_KEY = "YOUR_KEY")
#'
#' # ...or set it explicitly for the session
#' fmp_set_key("YOUR_KEY")
#'
#' # Prices & metrics
#' fmp_prices(c("AAPL","MSFT"), from = "2025-10-15", version = "stable")
#' fmp_metrics("AAPL", metric = "key", ttm = TRUE, version = "stable")
#' fmp_profile("AAPL", version = "stable")
#' fmp_prices_divadj(c("AAPL","MSFT"), from = "2024-10-01")
#'
#' # Quotes & lists
#' fmp_fx_quotes("v3")
#' fmp_crypto_quotes("v3")
#' fmp_commodity_quotes("stable")
#' fmp_exchanges("stable")
#' fmp_stock_list("v3")
#' fmp_index_constituents("dowjones")
#'
#' # News
#' fmp_news("general", limit = 10)
#' fmp_news_bulk(endpoints = c("general","stock"), pages = 0:1, limit = 10)
#'
#' # Bulk/CSV (stable)
#' fmp_profiles_bulk(0:2)
#' fmp_profiles_bulk_all(max_parts = 100)
#' fmp_metrics_ttm_bulk("key")
#' fmp_eod_bulk("2024-10-22")
#' fmp_income_statement_bulk(2025, "Q1", variant = "standard")
#' }
#'
#' @seealso
#' [`fmp_set_key()`], [`set_fmp_key()`],
#' [`fmp_prices()`], [`fmp_prices_divadj()`], [`fmp_quote()`],
#' [`fmp_metrics()`], [`fmp_profile()`], [`fmp_earnings()`], [`fmp_dividends()`],
#' [`fmp_company_screener()`],
#' [`fmp_news()`], [`fmp_news_bulk()`],
#' [`fmp_profiles_bulk()`], [`fmp_profiles_bulk_all()`],
#' [`fmp_metrics_ttm_bulk()`], [`fmp_eod_bulk()`],
#' [`fmp_income_statement_bulk()`], [`fmp_balance_sheet_bulk()`], [`fmp_cash_flow_bulk()`],
#' [`fmp_fx_quotes()`], [`fmp_crypto_quotes()`], [`fmp_commodity_quotes()`],
#' [`fmp_exchanges()`], [`fmp_stock_list()`], [`fmp_index_constituents()`]
#'
#' @docType package
#' @name fmpbrunch
#' @aliases fmpbrunch-package
#' @keywords internal
"_PACKAGE"
