#' fmpbrunch: Minimal FMP Client (v3 & stable)
#'
#' @description
#' Leichter R-Client für FinancialModelingPrep. Unterstützt Kurse,
#' Metrics (key/ratios/growth/ev) und Profile – alles als tidy tibbles.
#'
#' @details
#' **Funktionen:**
#' - `fmp_prices()`  – historical quotes (v3 & stable)
#' - `fmp_metrics()` – Metrics (key/ratios/growth/ev; TTM optional; v3 & stable)
#' - `fmp_profile()` – Company profiles (v3 & stable)
#' - FX: `fmp_fx_quotes()` - All available forex pairs
#' - Crypto: `fmp_crypto_quotes()` -All available crypto quotes
#' - Commodities: `fmp_commodity_quotes()` - All available commodity quotes
#' - Exchanges: `fmp_exchanges()` - All available exchanges
#'
#' **Quick Start**
#' \preformatted{
#' library(fmpbrunch)
#' set_fmp_key("YOUR_KEY")
#' fmp_prices(c("AAPL","MSFT"), from = "2025-10-15", version = "stable")
#' fmp_metrics("AAPL", metric = "key", ttm = TRUE, version = "stable")
#' fmp_profile("AAPL", version = "stable")
#' fmp_fx_quotes("v3")
#' fmp_crypto_quotes("v3")
#' fmp_commodity_quotes("stable")
#' fmp_exchanges("stable")
#' }
#'
#' @seealso [`fmp_prices()`], [`fmp_metrics()`], [`fmp_profile()`],
#'  [`fmp_fx_quotes()`], [`fmp_crypto_quotes()`], [`fmp_commodity_quotes()`]
#'
#' @docType package
#' @name fmpbrunch
#' @aliases fmpbrunch-package
#' @keywords internal
"_PACKAGE"
