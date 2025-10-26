#' fmpbrunch: Minimal FMP Client (v3 & stable)
#'
#' @description
#' Leichter R-Client für FinancialModelingPrep. Unterstützt Kurse,
#' Metrics (key/ratios/growth/ev) und Profile – alles als tidy tibbles.
#'
#' @details
#' **Funktionen:**
#' - `fmp_prices()`  – historische Preise (v3 & stable)
#' - `fmp_metrics()` – Kennzahlen (key/ratios/growth/ev; TTM optional; v3 & stable)
#' - `fmp_profile()` – Firmenprofile (v3 & stable)
#' - FX: `fmp_fx_quotes()` - Alle verfügbaren Währungspaare und aktuelle Kurse
#' - Crypto: `fmp_crypto_quotes()` - Alle verfügbaren Crypto Währungen und aktuelle Kurse
#' - Commodities: `fmp_commodity_quotes()` - Alle verfügbaren Rohstoffe und aktuelle Kurese
#' - Exchanges: `fmp_exchanges()` - Alle Verfügbaren Börsen
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
#' @seealso [`fmp_prices()`], [`fmp_metrics()`], [`fmp_profile()`]
#'
#' @docType package
#' @name fmpbrunch
#' @aliases fmpbrunch-package
#' @keywords internal
"_PACKAGE"
