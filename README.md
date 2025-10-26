
# fmpbrunch

Minimal R client for FinancialModelingPrep (v3 & stable).  
Get prices, metrics (key/ratios/growth/EV), profiles,
FX/crypto/commodities quotes, and exchange lists — as tidy tibbles.

## Install

\`\`\`r \# install.packages(“pak”) pak::pak(“Boersenbrunch/fmpbrunch”)

\`\`\`r

## Quickstart

library(fmpbrunch)

Sys.setenv(FMP_API_KEY = “YOUR_KEY”) \# or set_fmp_key(“YOUR_KEY”)
set_fmp_key()

# Prices

fmp_prices(c(“AAPL”,“MSFT”), from = “2025-10-15”, version = “stable”)
\|\> head()

# Metrics

fmp_metrics(“AAPL”, metric = “key”, ttm = TRUE, version = “stable”) \|\>
head()

# Profiles

fmp_profile(c(“AAPL”,“MSFT”), version = “stable”) \|\> head()

# FX / Crypto / Commodities / Exchanges

fmp_fx_quotes(“stable”) \|\> head() fmp_crypto_quotes(“stable”) \|\>
head() fmp_commodity_quotes(“stable”) \|\> head()
fmp_exchanges(“stable”) \|\> head()

# License

MIT © Martin Kiontke
