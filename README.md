
# fmpbrunch

Minimal R client for FinancialModelingPrep (v3 & stable).  
Get prices, metrics (key/ratios/growth/EV), profiles,
FX/crypto/commodities quotes, and exchange lists — as tidy tibbles.

# Install

install.packages(“pak”)

pak::pak(“Boersenbrunch/fmpbrunch”)

# Quickstart

library(fmpbrunch)

# Set your FMP key

Sys.setenv(FMP_API_KEY = “YOUR_KEY”) or via

fmp_set_key(“YOUR_KEY”)

# Currently avaialble endpoints

Prices

fmp_prices(c(“AAPL”,“MSFT”), from = “2025-10-15”, version = “stable”)

Metrics

fmp_metrics(“AAPL”, metric = “key”, ttm = TRUE, version = “stable”)

Profiles

fmp_profile(c(“AAPL”,“MSFT”), version = “stable”)

FX all avaialble pairs

fmp_fx_quotes(“stable”)

All avaialble Cryptos

fmp_crypto_quotes(“stable”)

All avaialble Commodities

fmp_commodity_quotes(“stable”)

All avaialble exchanges

fmp_exchanges(“stable”)

# License

MIT © Börsenbrunch
