# --- API Abfragen für Financial Modeling Prep (v3 & stable) -------------

#' @importFrom dplyr bind_rows arrange group_by slice_head ungroup mutate summarise desc
#' @importFrom tibble as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom stats setNames
#' @importFrom curl curl new_handle handle_setheaders
NULL



.fmp_key <- new.env(parent = emptyenv())
.get_key <- function() {
  k <- .fmp_key$key; if (is.null(k) || !nzchar(k)) k <- Sys.getenv("FMP_API_KEY")
  if (!nzchar(k)) stop("API-Key fehlt: fmp_set_key(YOUR KEY) aufrufen.", call. = FALSE)
  k
}
.enc <- function(x) utils::URLencode(as.character(x), reserved = TRUE)
.qs  <- function(lst) paste0(names(lst), "=", vapply(lst, .enc, ""), collapse = "&")
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Helfer zum robusten Parsen (vereinheitlicht Keys, NULL -> NA) ----------
.rectify_records <- function(x) {
  # data.frame? -> tibble zurück
  if (inherits(x, "data.frame")) return(tibble::as_tibble(x))
  # named list mit skalaren Werten -> eine Zeile
  if (is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))) {
    keys <- names(x)
    row  <- setNames(lapply(keys, function(k) if (is.null(x[[k]])) NA else x[[k]]), keys)
    return(tibble::as_tibble(row))
  }
  # Liste von Records (jeweils named list / df)
  if (is.list(x) && length(x) > 0) {
    # Sammle alle Keys
    keyset <- unique(unlist(lapply(x, function(rec) names(rec) %||% colnames(rec)), use.names = FALSE))
    keyset <- keyset[nzchar(keyset)]
    if (!length(keyset)) return(tibble::tibble())
    rows <- lapply(x, function(rec) {
      # in named list konvertieren
      if (inherits(rec, "data.frame")) rec <- as.list(rec[1, , drop = FALSE])
      vals <- lapply(keyset, function(k) {
        v <- if (is.null(rec[[k]])) NA else rec[[k]]
        # atomarisieren (erste Komponente), um DataFrame-Kollisionen zu vermeiden
        if (is.list(v)) v <- if (length(v)) v[[1]] else NA
        v
      })
      as.data.frame(setNames(vals, keyset), stringsAsFactors = FALSE)
    })
    return(suppressWarnings(dplyr::bind_rows(rows)))
  }
  tibble::tibble()
}


.parse_metrics_any <- function(js, sym = NULL, metric = NULL) {
  cand <- NULL
  if (!is.null(js$data)) cand <- js$data
  if (is.null(cand) && identical(metric, "ratios") && !is.null(js$ratios)) cand <- js$ratios
  if (is.null(cand) && identical(metric, "growth") && !is.null(js$growth)) cand <- js$growth
  if (is.null(cand) && identical(metric, "key")    && !is.null(js$metrics)) cand <- js$metrics
  if (is.null(cand) && identical(metric, "ev")     && !is.null(js$enterpriseValues)) cand <- js$enterpriseValues
  if (is.null(cand) && !is.null(js$financials)) cand <- js$financials   # zusätzliche Chance
  if (is.null(cand)) cand <- js

  df <- .rectify_records(cand)
  if (NROW(df) && !"symbol" %in% names(df)) {
    sym_val <- if (!is.null(sym)) sym else if (!is.null(js$symbol)) js$symbol else NULL
    if (!is.null(sym_val)) df$symbol <- sym_val
  }
  for (dc in c("date","reportedDate","fillingDate")) {
    if (dc %in% names(df)) suppressWarnings(df[[dc]] <- tryCatch(as.Date(df[[dc]]), error=function(e) df[[dc]]))
  }
  df
}


# ---- Mini-Fetcher (ohne fill/nullValue) --------------------------------------
.fetch_json <- function(url) jsonlite::fromJSON(url, simplifyVector = TRUE)


#' Set FMP API key
#'
#' Setzt den API-Key für Abfragen (liest sonst FMP_API_KEY aus der Umgebung).
#' @export
#' @examples
#' \dontrun{
#' Sys.setenv(FMP_API_KEY = "YOUR_KEY")
#' fmp_set_key("YOUR_KEY")
#' }
fmp_set_key <- function(key = NULL) {
  if (is.null(key) || !nzchar(key)) key <- Sys.getenv("FMP_API_KEY")
  if (!nzchar(key)) stop("API-Key fehlt: fmp_set_key('...') oder Sys.setenv(FMP_API_KEY='...')", call. = FALSE)
  .fmp_key$key <- key; invisible(key)
}

#' @name fmp_set_key
#' @rdname fmp_set_key
#' @export
#' @usage set_fmp_key(key = NULL)
#' @details `set_fmp_key()` is deprecated. Use `fmp_set_key()` instead.
set_fmp_key <- function(key = NULL) {
  .Deprecated("fmp_set_key")
  fmp_set_key(key)
}

# --- PRICES (v3 & stable)----------------------


#' Get historical prices (v3 & stable)
#'
#' Holt Kurs-Zeitreihen. Bei v3: Symbol im Pfad, Serverfilter `from/to`.
#' Bei stable: 1 Request pro Symbol, Clientfilter nach Datum.
#' @param symbols Character-Vektor mit Tickers
#' @param from,to Optionales Datum "YYYY-MM-DD"
#' @param version "v3" oder "stable"
#' @return Tibble mit Spalten wie symbol, date, open, high, low, close, volume
#' @export
fmp_prices <- function(symbols, from = NULL, to = NULL, version = c("v3","stable")) {
  version <- match.arg(version)
  key <- .get_key()
  stopifnot(length(symbols) >= 1)

  # ---- v3: Symbol im Pfad, from/to serverseitig filtern ---------------------
  if (version == "v3") {
    fetch_v3 <- function(sym) {
      base <- sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s", sym)
      p <- list(apikey = key); if (!is.null(from)) p$from <- from; if (!is.null(to)) p$to <- to
      url <- paste0(base, "?", .qs(p))
      js  <- jsonlite::fromJSON(url, simplifyVector = TRUE)
      df  <- js$historical
      if (is.null(df) || NROW(df) == 0) return(tibble::tibble())
      df$symbol <- sym
      tibble::as_tibble(df)
    }
    out <- dplyr::bind_rows(lapply(symbols, fetch_v3))
    if ("date" %in% names(out)) suppressWarnings(out$date <- as.Date(out$date))
    return(dplyr::arrange(out, symbol, date))
  }

  # ---- stable: 1 Request PRO Symbol (Kommaliste liefert nix) ----------------
  fetch_stable <- function(sym) {
    base <- "https://financialmodelingprep.com/stable/historical-price-eod/full"
    p <- list(symbol = sym, apikey = key)  # NUR symbol & apikey
    url <- paste0(base, "?", .qs(p))
    js  <- jsonlite::fromJSON(url, simplifyVector = TRUE)

    # Formen: js$data (data.frame) ODER js$data (list) -> nimm, was da ist
    if (!is.null(js$data) && inherits(js$data, "data.frame")) {
      df <- tibble::as_tibble(js$data)
    } else if (!is.null(js$data) && is.list(js$data)) {
      # falls doch Liste, flachziehen
      parts <- lapply(js$data, function(x) if (inherits(x, "data.frame")) tibble::as_tibble(x) else tibble::tibble())
      df <- dplyr::bind_rows(parts)
    } else if (inherits(js, "data.frame")) {
      df <- tibble::as_tibble(js)
    } else {
      df <- tibble::tibble()
    }

    if (NROW(df) == 0) return(df)
    if (!"symbol" %in% names(df)) df$symbol <- sym
    if ("date" %in% names(df)) suppressWarnings(df$date <- as.Date(df$date))

    # Datumsfilter CLIENT-seitig
    if (!is.null(from)) df <- df[df$date >= as.Date(from), , drop = FALSE]
    if (!is.null(to))   df <- df[df$date <= as.Date(to),   , drop = FALSE]
    df
  }

  out <- dplyr::bind_rows(lapply(symbols, fetch_stable))
  if (NROW(out)) out <- dplyr::arrange(out, symbol, date)
  out
}







# --- METRICS (v3 & stable)  --------------
# Nutzung:
# fmp_metrics(c("AAPL","MSFT"), metric="key",    ttm=FALSE, period="annual", limit=20, version="v3")
# fmp_metrics(c("AAPL","MSFT"), metric="ratios", ttm=TRUE,                 version="stable")
# fmp_metrics("AAPL",            metric="growth", ttm=FALSE, period="quarter", limit=8, version="v3")
# fmp_metrics("AAPL",            metric="ev",     ttm=FALSE, period="annual",  limit=20, version="stable")



#' Get financial metrics (key / ratios / growth / ev) – v3 & stable
#'
#' Liefert Kennzahlen je nach `metric`. `ttm=TRUE` nutzt TTM-Endpoints (wo vorhanden).
#' @param symbols Character-Vektor
#' @param metric "key","ratios","growth","ev"
#' @param ttm Logical, TTM-Variante (falls vorhanden)
#' @param period "annual" oder "quarter" (nicht bei stable/growth)
#' @param limit Integer, max. Zeilen pro Symbol (bei Bedarf clientseitig)
#' @param version "v3" oder "stable"
#' @return Tibble mit Kennzahlen
#' @export
fmp_metrics <- function(symbols,
                        metric  = c("key","ratios","growth","ev"),
                        ttm     = FALSE,
                        period  = c("annual","quarter"),
                        limit   = 40,
                        version = c("v3","stable")) {

  metric  <- match.arg(metric)
  version <- match.arg(version)
  period  <- match.arg(period)
  key     <- .get_key()
  stopifnot(length(symbols) >= 1)

  # v3: fixe Pfade
  map_v3 <- list(
    key    = list(path = "key-metrics",       ttm = "key-metrics-ttm"),
    ratios = list(path = "ratios",            ttm = "ratios-ttm"),
    growth = list(path = "financial-growth",  ttm = NULL),
    ev     = list(path = "enterprise-values", ttm = NULL)
  )

  # stable: mehrere Kandidatenpfade (wir testen nacheinander)
  map_st <- list(
    key    = list(path = "key-metrics",       ttm = "key-metrics-ttm"),
    ratios = list(path = "ratios",            ttm = "ratios-ttm"),
    growth = list(path = "financial-growth",  ttm = NULL),   # <— WICHTIG
    ev     = list(path = "enterprise-values", ttm = NULL)
  )
  # Hilfsfunktion: baue URL
  build_url <- function(base, params) paste0(base, if (length(params)) paste0("?", .qs(params)) else "")

  if (version == "v3") {
    fetch_v3 <- function(sym) {
      ent <- map_v3[[metric]]
      path <- if (ttm && !is.null(ent$ttm)) ent$ttm else ent$path
      base <- sprintf("https://financialmodelingprep.com/api/v3/%s/%s", path, sym)
      p    <- list(apikey = key)
      if (!ttm) { p$period <- period; p$limit <- limit }
      js <- .fetch_json(build_url(base, p))
      .parse_metrics_any(js, sym, metric)
    }
    out <- dplyr::bind_rows(lapply(symbols, fetch_v3))
    if ("symbol" %in% names(out)) {
      out <- if ("date" %in% names(out)) dplyr::arrange(out, symbol, dplyr::desc(date)) else dplyr::arrange(out, symbol)
    }
    return(out)
  }

  # ---------- STABLE: probiere Kandidatenpfade der Reihe nach -----------------
  fetch_st <- function(sym) {
    key <- .get_key()

    if (metric == "growth") {
      # Stable Growth akzeptiert nur symbol+apikey (kein period/limit)
      url <- paste0("https://financialmodelingprep.com/stable/financial-growth",
                    "?symbol=", utils::URLencode(sym, reserved = TRUE),
                    "&apikey=", utils::URLencode(key, reserved = TRUE))
      js <- jsonlite::fromJSON(url, simplifyVector = TRUE)
      # einfache, robuste Faltung
      if (!is.null(js$data) && inherits(js$data, "data.frame")) {
        df <- tibble::as_tibble(js$data)
      } else if (inherits(js, "data.frame")) {
        df <- tibble::as_tibble(js)
      } else if (is.list(js) && length(js) > 0) {
        parts <- lapply(js, function(x) as.data.frame(x, stringsAsFactors = FALSE))
        df <- tibble::as_tibble(dplyr::bind_rows(parts))
      } else {
        df <- tibble::tibble()
      }
      if (nrow(df) && !"symbol" %in% names(df)) df$symbol <- sym
      if ("date" %in% names(df)) suppressWarnings(df$date <- as.Date(df$date))
      return(df)
    }

    # alle anderen Metrics (key/ratios/ev): bisherige stabile Logik
    path <- if (ttm && metric != "ev") switch(metric,
                                              key    = "key-metrics-ttm",
                                              ratios = "ratios-ttm",
                                              ev     = "enterprise-values"  # ttm gibt's i.d.R. nicht
    ) else switch(metric,
                  key    = "key-metrics",
                  ratios = "ratios",
                  ev     = "enterprise-values"
    )

    base <- paste0("https://financialmodelingprep.com/stable/", path)
    p <- list(symbol = sym, apikey = key)
    if (!ttm && metric %in% c("key","ratios")) {
      p$period <- period
      p$limit  <- limit
    }
    url <- paste0(base, "?", paste0(names(p), "=", vapply(p, utils::URLencode, "", reserved = TRUE), collapse = "&"))
    js  <- jsonlite::fromJSON(url, simplifyVector = TRUE)
    .parse_metrics_any(js, sym, metric)
  }

  out <- dplyr::bind_rows(lapply(symbols, fetch_st))
  if ("symbol" %in% names(out)) {
    out <- if ("date" %in% names(out)) dplyr::arrange(out, symbol, dplyr::desc(date)) else dplyr::arrange(out, symbol)
  }
  out
}


# --- PROFILE (v3 & stable) ----------------------------------------------------

# kleiner Parser für Profile (nimmt $data, sonst flach; ergänzt symbol)
.parse_profile_any <- function(js, sym = NULL) {
  cand <- if (!is.null(js$data)) js$data else js
  df   <- .rectify_records(cand)
  if (NROW(df) && !"symbol" %in% names(df)) {
    sym_val <- if (!is.null(sym)) sym else if (!is.null(js$symbol)) js$symbol else NULL
    if (!is.null(sym_val)) df$symbol <- sym_val
  }
  # weiches Date-Parsing für typische Felder
  for (dc in c("ipoDate","date","founded")) {
    if (dc %in% names(df)) {
      suppressWarnings(df[[dc]] <- tryCatch(as.Date(df[[dc]]), error = function(e) df[[dc]]))
    }
  }
  df
}

#' Get company profiles (v3 & stable)
#'
#' Holt Firmen-Profile für ein oder mehrere Symbole.
#' - v3:     `/api/v3/profile/<SYMBOL>`
#' - stable: `/stable/profile?symbol=<SYMBOL>`
#'
#' @param symbols Character-Vektor mit Tickers (mind. 1)
#' @param version "v3" oder "stable"
#' @return Tibble mit Profilfeldern (u. a. companyName, industry, sector, website, ipoDate, ...), inkl. `symbol`
#' @export
fmp_profile <- function(symbols, version = c("v3","stable")) {
  version <- match.arg(version)
  key <- .get_key()
  stopifnot(length(symbols) >= 1)

  if (version == "v3") {
    fetch_v3 <- function(sym) {
      base <- sprintf("https://financialmodelingprep.com/api/v3/profile/%s", sym)
      url  <- paste0(base, "?", .qs(list(apikey = key)))
      js   <- jsonlite::fromJSON(url, simplifyVector = TRUE)
      # v3 liefert oft direkt ein data.frame (Zeile) oder eine Liste von Zeilen
      if (inherits(js, "data.frame")) {
        df <- tibble::as_tibble(js)
      } else {
        df <- .parse_profile_any(js, sym)
      }
      if (NROW(df) && !"symbol" %in% names(df)) df$symbol <- sym
      df
    }
    out <- dplyr::bind_rows(lapply(symbols, fetch_v3))
  } else {
    fetch_st <- function(sym) {
      base <- "https://financialmodelingprep.com/stable/profile"
      url  <- paste0(base, "?", .qs(list(symbol = sym, apikey = key)))
      js   <- jsonlite::fromJSON(url, simplifyVector = TRUE)
      df   <- .parse_profile_any(js, sym)
      if (NROW(df) && !"symbol" %in% names(df)) df$symbol <- sym
      df
    }
    out <- dplyr::bind_rows(lapply(symbols, fetch_st))
  }

  if (NROW(out) && "symbol" %in% names(out)) out <- dplyr::arrange(out, symbol)
  out
}




# --- FOREX QUOTES (v3 & stable) ----------------------------------------------


.parse_fx_any <- function(js) {
  cand <- NULL
  for (nm in c("data","forex","forexList","items","quotes","results")) {
    if (!is.null(js[[nm]])) { cand <- js[[nm]]; break }
  }
  if (is.null(cand)) cand <- js
  df <- .rectify_records(cand)

  # symbol/pair/ticker -> symbol
  if (!"symbol" %in% names(df)) {
    for (alt in c("symbolName","pair","ticker","code")) {
      if (alt %in% names(df)) { df$symbol <- df[[alt]]; break }
    }
  }

  # numerische Felder weich casten
  for (nm in intersect(c("price","rate","bid","ask","open","high","low","changes","change","last","mid"), names(df))) {
    suppressWarnings(df[[nm]] <- as.numeric(df[[nm]]))
  }

  # Zeitfeld (optional) → POSIXct
  for (tc in c("timestamp","updated","time","date","lastUpdated")) {
    if (tc %in% names(df)) {
      v <- df[[tc]]
      if (is.numeric(v)) {
        df$time <- as.POSIXct(ifelse(v > 1e12, v/1000, v), origin = "1970-01-01", tz = "UTC")
      } else {
        suppressWarnings(df$time <- as.POSIXct(v, tz = "UTC"))
      }
      break
    }
  }
  tibble::as_tibble(df)
}

#' Get all Forex quotes (v3 & stable)
#'
#' Holt alle verfügbaren FX-Paare mit aktuellen Quotes.
#' v3: /api/v3/quotes/forex
#' stable: /stable/batch-forex-quotes
#' @param version "v3" oder "stable"
#' @return Tibble mit mind. `symbol`, optional `price`, `bid`, `ask`, `time`, …
#' @export
fmp_fx_quotes <- function(version = c("v3","stable")) {
  version <- match.arg(version)
  key <- .get_key()

  if (version == "v3") {
    url <- paste0("https://financialmodelingprep.com/api/v3/quotes/forex?",
                  .qs(list(apikey = key)))
  } else {
    url <- paste0("https://financialmodelingprep.com/stable/batch-forex-quotes?",
                  .qs(list(apikey = key)))
  }

  js  <- jsonlite::fromJSON(url, simplifyVector = TRUE)
  out <- .parse_fx_any(js)

  # Falls nur "rate" existiert, als "price" spiegeln
  if (nrow(out) && !"price" %in% names(out) && "rate" %in% names(out)) {
    out$price <- out$rate
  }
  if (nrow(out) && "symbol" %in% names(out)) out <- dplyr::arrange(out, symbol)
  out
}

# --- EXCHANGES (v3 & stable) --------------------------------------------------

.parse_exchanges_any <- function(js) {
  # 1) schon ein data.frame?
  if (inherits(js, "data.frame")) {
    df <- tibble::as_tibble(js)

    # 2) Liste mit $data-Container
  } else if (is.list(js) && !is.null(js$data)) {
    df <- .rectify_records(js$data)

    # 3) allgemeine Liste von Records
  } else if (is.list(js)) {
    df <- .rectify_records(js)

    # 4) atomarer Vektor (z. B. nur Char-Vektoren)
  } else if (is.atomic(js)) {
    df <- tibble::tibble(name = as.character(js))

  } else {
    df <- tibble::tibble()
  }

  # etwas Kosmetik: häufige Felder auf "name"/"acronym" mappen (nur wenn vorhanden)
  rename_map <- c(
    exchangeShortName = "acronym",
    mic               = "acronym",
    code              = "acronym",
    #  exchange          = "name",         # nur mappen, wenn "name" noch fehlt
    exchangeName      = "name"
  )
  for (k in names(rename_map)) {
    new <- rename_map[[k]]
    if (k %in% names(df) && !new %in% names(df)) df[[new]] <- df[[k]]
  }
  if (!"name" %in% names(df) && "exchange" %in% names(df)) df$name <- df$exchange

  # sortieren, wenn sinnvoll
  if ("acronym" %in% names(df)) df <- dplyr::arrange(df, acronym)
  else if ("name" %in% names(df)) df <- dplyr::arrange(df, name)

  tibble::as_tibble(df)
}


#' Get list of available exchanges (v3 & stable)
#'
#' Liefert die vollständige Liste aller verfügbaren Börsen/Handelsplätze.
#' - v3:     `/api/v3/exchanges-list`
#' - stable: `/stable/available-exchanges`
#'
#' @param version Zeichenkette: `"v3"` oder `"stable"`.
#' @return Tibble mit Feldern wie `name`, `acronym` (MIC/Short), `country`, `currency`, etc. – je nach Endpoint.
#' @examples
#' \dontrun{
#' fmp_set_key("YOUR_KEY")
#' ex_v3 <- fmp_exchanges("v3")
#' ex_st <- fmp_exchanges("stable")
#' head(ex_v3)
#' }
#' @export
fmp_exchanges <- function(version = c("v3","stable")) {
  version <- match.arg(version)
  key <- .get_key()

  url <- if (version == "v3") {
    paste0("https://financialmodelingprep.com/api/v3/exchanges-list?",
           .qs(list(apikey = key)))
  } else {
    paste0("https://financialmodelingprep.com/stable/available-exchanges?",
           .qs(list(apikey = key)))
  }

  js <- jsonlite::fromJSON(url, simplifyVector = TRUE)
  .parse_exchanges_any(js)
}


# --- CRYPTO QUOTES (v3 & stable) ---------------------------------------------

.parse_crypto_any <- function(js) {
  # Liste der Records finden (data / quotes / results / …)
  cand <- NULL
  for (nm in c("data","crypto","items","quotes","results","list")) {
    if (!is.null(js[[nm]])) { cand <- js[[nm]]; break }
  }
  if (is.null(cand)) cand <- js

  df <- .rectify_records(cand)

  # symbol/ticker/name angleichen
  if (!"symbol" %in% names(df)) {
    for (alt in c("symbolName","ticker","code","pair","name")) {
      if (alt %in% names(df)) { df$symbol <- df[[alt]]; break }
    }
  }

  # numerische Standardfelder weich casten (falls vorhanden)
  for (nm in intersect(c(
    "price","changes","change","dayHigh","dayLow","open","previousClose",
    "marketCap","volume","avgVolume","circulatingSupply","beta"
  ), names(df))) {
    suppressWarnings(df[[nm]] <- as.numeric(df[[nm]]))
  }

  # Timestamp -> POSIXct (falls vorhanden)
  for (tc in c("timestamp","updated","time","date","lastUpdated")) {
    if (tc %in% names(df)) {
      v <- df[[tc]]
      if (is.numeric(v)) {
        df$time <- as.POSIXct(ifelse(v > 1e12, v/1000, v), origin = "1970-01-01", tz = "UTC")
      } else {
        suppressWarnings(df$time <- as.POSIXct(v, tz = "UTC"))
      }
      break
    }
  }

  tibble::as_tibble(df)
}

#' Get all Crypto quotes (v3 & stable)
#'
#' Liefert alle verfügbaren Krypto-Assets mit aktuellen Quotes.
#' v3:    /api/v3/quotes/crypto
#' stable:/stable/batch-crypto-quotes
#'
#' @param version "v3" oder "stable"
#' @return Tibble (mind. `symbol`, optional `price`, `changes`, `marketCap`, `time`, …)
#' @examples
#' \dontrun{
#' set_fmp_key("YOUR_KEY")
#' crypto_v3     <- fmp_crypto_quotes("v3")
#' crypto_stable <- fmp_crypto_quotes("stable")
#' head(crypto_stable)
#' }
#' @export
fmp_crypto_quotes <- function(version = c("v3","stable")) {
  version <- match.arg(version)
  key <- .get_key()

  if (version == "v3") {
    url <- paste0("https://financialmodelingprep.com/api/v3/quotes/crypto?",
                  .qs(list(apikey = key)))
  } else {
    url <- paste0("https://financialmodelingprep.com/stable/batch-crypto-quotes?",
                  .qs(list(apikey = key)))
  }

  js  <- jsonlite::fromJSON(url, simplifyVector = TRUE)
  out <- .parse_crypto_any(js)

  if (nrow(out) && "symbol" %in% names(out)) out <- dplyr::arrange(out, symbol)
  out
}

# --- COMMODITY QUOTES (v3 & stable) ------------------------------------------

.parse_commodity_any <- function(js) {
  # Finde die Liste – manche Antworten kommen unter data/quotes/etc.
  cand <- NULL
  for (nm in c("data","items","quotes","results","list","commodities")) {
    if (!is.null(js[[nm]])) { cand <- js[[nm]]; break }
  }
  if (is.null(cand)) cand <- js

  df <- .rectify_records(cand)

  # Symbol vereinheitlichen
  if (!"symbol" %in% names(df)) {
    for (alt in c("symbolName","ticker","code","name")) {
      if (alt %in% names(df)) { df$symbol <- df[[alt]]; break }
    }
  }

  # Häufige numerische Felder weich casten (nur falls vorhanden)
  for (nm in intersect(c(
    "price","changes","change","dayHigh","dayLow","open","previousClose",
    "yearHigh","yearLow","volume","avgVolume","marketCap"
  ), names(df))) {
    suppressWarnings(df[[nm]] <- as.numeric(df[[nm]]))
  }

  # Zeitfeld → POSIXct (falls vorhanden)
  for (tc in c("timestamp","updated","time","date","lastUpdated")) {
    if (tc %in% names(df)) {
      v <- df[[tc]]
      if (is.numeric(v)) {
        df$time <- as.POSIXct(ifelse(v > 1e12, v/1000, v), origin = "1970-01-01", tz = "UTC")
      } else {
        suppressWarnings(df$time <- as.POSIXct(v, tz = "UTC"))
      }
      break
    }
  }

  tibble::as_tibble(df)
}

#' Get all Commodity quotes (v3 & stable)
#'
#' Liefert alle verfügbaren Rohstoff-Quotes (Gold, Öl, Kupfer, Weizen, …).
#' v3:     /api/v3/quotes/commodity
#' stable: /stable/batch-commodity-quotes
#'
#' @param version "v3" oder "stable"
#' @return Tibble mit mind. `symbol`, optional `price`, `changes`, `time`, …
#' @examples
#' \dontrun{
#' set_fmp_key("YOUR_KEY")
#' cmd_v3     <- fmp_commodity_quotes("v3")
#' cmd_stable <- fmp_commodity_quotes("stable")
#' head(cmd_stable)
#' }
#' @export
fmp_commodity_quotes <- function(version = c("v3","stable")) {
  version <- match.arg(version)
  key <- .get_key()

  if (version == "v3") {
    url <- paste0("https://financialmodelingprep.com/api/v3/quotes/commodity?",
                  .qs(list(apikey = key)))
  } else {
    url <- paste0("https://financialmodelingprep.com/stable/batch-commodity-quotes?",
                  .qs(list(apikey = key)))
  }

  js  <- jsonlite::fromJSON(url, simplifyVector = TRUE)
  out <- .parse_commodity_any(js)

  if (nrow(out) && "symbol" %in% names(out)) out <- dplyr::arrange(out, symbol)
  out
}

# --- STOCKS LIST (v3 & stable) -----------------------------------------------

.parse_stock_list_any <- function(js) {
  cand <- if (!is.null(js$data)) js$data else js
  df   <- .rectify_records(cand)

  # Harmonisierung: Namen & typische Felder
  if (!"symbol" %in% names(df)) {
    for (alt in c("ticker","code","symbolName")) {
      if (alt %in% names(df)) { df$symbol <- df[[alt]]; break }
    }
  }
  if (!"name" %in% names(df) && "companyName" %in% names(df)) df$name <- df$companyName
  if (!"exchange" %in% names(df)) {
    for (alt in c("exchangeShortName","exchangeName","stockExchange")) {
      if (alt %in% names(df)) { df$exchange <- df[[alt]]; break }
    }
  }
  # weiche Numerik
  for (nm in intersect(c("price","changes","change","marketCap"), names(df))) {
    suppressWarnings(df[[nm]] <- as.numeric(df[[nm]]))
  }

  # sinnvolle Sortierung
  if ("symbol" %in% names(df)) df <- dplyr::arrange(df, symbol)

  tibble::as_tibble(df)
}

#' Get full stocks list (v3 & stable)
#'
#' Liefert die komplette Liste aller verfügbaren Aktien (Ticker, Name, Börse, …).
#' - v3:     `/api/v3/stock/list`
#' - stable: `/stable/stock-list`
#'
#' @param version Zeichenkette: `"v3"` oder `"stable"`.
#' @return Tibble mit Spalten wie `symbol`, `name`, `exchange`, optional `price`, `type`, `currency`, `isin`, `cusip` (abhängig vom Endpoint).
#' @examples
#' \dontrun{
#' fmp_set_key()  # liest FMP_API_KEY aus der Umgebung
#' sl_v3 <- fmp_stock_list("v3")
#' sl_st <- fmp_stock_list("stable")
#' head(sl_v3)
#' }
#' @export
fmp_stock_list <- function(version = c("v3","stable")) {
  version <- match.arg(version)
  key <- .get_key()

  url <- if (version == "v3") {
    paste0("https://financialmodelingprep.com/api/v3/stock/list?",
           .qs(list(apikey = key)))
  } else {
    paste0("https://financialmodelingprep.com/stable/stock-list?",
           .qs(list(apikey = key)))
  }

  js <- jsonlite::fromJSON(url, simplifyVector = TRUE)
  .parse_stock_list_any(js)
}


# --- Bulk: Company Profiles (stable) -------------------------------------------



#' Get company profiles in bulk (stable )
#' @param parts   Integer-Vektor, z.B. 0:2
#' @param retries Anzahl Retries pro Part (Default 3)
#' @param pause   Grundpause zwischen Retries in Sekunden (Default 0.6)
#' @return Tibble mit Profilfeldern (oder leer, mit Warnung bei Limit)
#' @export
fmp_profiles_bulk <- function(parts = 0L, retries = 3L, pause = 0.6) {
  key <- .get_key()

  is_limit_json <- function(raw) {
    # wenn Response wie {"Error Message":"Limit Reach..."} ist
    head_txt <- rawToChar(raw[seq_len(min(length(raw), 512))])
    grepl('"Error Message"\\s*:\\s*"Limit Reach', head_txt, ignore.case = TRUE, perl = TRUE)
  }

  fetch_csv_file <- function(url, retries, pause) {
    attempt <- 0L
    repeat {
      attempt <- attempt + 1L
      res <- try(curl::curl_fetch_memory(url), silent = TRUE)
      if (inherits(res, "try-error")) {
        Sys.sleep(runif(1, 0.2, 0.6) * attempt * pause)
        if (attempt > retries) return(list(df = tibble::tibble(), limited = FALSE))
        next
      }

      # Rate-Limit JSON?
      if (is_limit_json(res$content)) {
        if (attempt > retries) {
          return(list(df = tibble::tibble(), limited = TRUE))
        }
        Sys.sleep(runif(1, 0.8, 1.4) * attempt * pause)  # etwas länger warten
        next
      }

      # sonst CSV in Tempfile schreiben & robust einlesen
      tf <- tempfile(fileext = ".csv")
      on.exit(unlink(tf), add = TRUE)
      con <- file(tf, open = "wb"); writeBin(res$content, con); close(con)

      df <- tryCatch(
        utils::read.csv(tf, stringsAsFactors = FALSE, check.names = FALSE,
                        blank.lines.skip = TRUE, na.strings = c("", "NA")),
        error = function(e) {
          tryCatch(
            utils::read.csv(tf, stringsAsFactors = FALSE, check.names = FALSE,
                            fill = TRUE, comment.char = "", na.strings = c("", "NA")),
            error = function(e2) data.frame()
          )
        }
      )
      return(list(df = tibble::as_tibble(df), limited = FALSE))
    }
  }

  make_url <- function(p) {
    paste0(
      "https://financialmodelingprep.com/stable/profile-bulk?",
      .qs(list(part = as.integer(p), apikey = key, datatype = "csv"))
    )
  }

  any_limited <- FALSE
  fetch_part <- function(p) {
    res <- fetch_csv_file(make_url(p), retries, pause)
    if (res$limited) any_limited <<- TRUE
    df <- res$df
    if (!ncol(df)) return(tibble::tibble())

    if (!"symbol" %in% names(df)) {
      for (alt in c("ticker","code","symbolName")) {
        if (alt %in% names(df)) { df$symbol <- df[[alt]]; break }
      }
    }
    for (dc in c("ipoDate","date","founded")) {
      if (dc %in% names(df)) {
        suppressWarnings(df[[dc]] <- tryCatch(as.Date(df[[dc]]), error = function(e) df[[dc]]))
      }
    }
    df
  }

  out <- dplyr::bind_rows(lapply(parts, fetch_part))
  if (nrow(out) && "symbol" %in% names(out)) out <- dplyr::arrange(out, symbol)

  if (any_limited && !nrow(out)) {
    warning("FMP rate-limit erreicht (profile-bulk). Später erneut versuchen oder Intervall vergrößern.")
  }
  out
}






#' Get ALL company profiles in bulk (auto parts)
#' @param start_part Erster Part (Default 0)
#' @param max_parts  Sicherheitslimit (Default 100)
#' @param retries    Retries je Part (an fmp_profiles_bulk weitergereicht)
#' @param pause      Basis-Pause für Backoff (an fmp_profiles_bulk weitergereicht)
#' @param sleep_between Pause (Sek.) zwischen Parts (Fair Use)
#' @param verbose    Fortschritt anzeigen?
#' @return Tibble mit allen Profilen; Attribut 'parts_fetched' mit Integer-Vektor
#' @export
fmp_profiles_bulk_all <- function(start_part = 0L,
                                  max_parts  = 100L,
                                  retries    = 3L,
                                  pause      = 0.6,
                                  sleep_between = 0.4,
                                  verbose    = TRUE) {
  stopifnot(start_part >= 0L, max_parts >= 1L)
  out   <- list()
  parts <- integer(0)

  for (p in seq.int(start_part, start_part + max_parts - 1L)) {
    if (verbose) message(sprintf("[profiles-bulk] fetching part %d ...", p))
    df <- fmp_profiles_bulk(parts = p, retries = retries, pause = pause)
    if (!nrow(df)) {
      if (verbose) message(sprintf("[profiles-bulk] part %d empty → stopping.", p))
      break
    }
    out[[length(out) + 1L]] <- df
    parts <- c(parts, p)
    Sys.sleep(sleep_between)
  }

  res <- dplyr::bind_rows(out)
  attr(res, "parts_fetched") <- parts
  res
}

# --- Bulk: Company Metrics (stable) -------------------------------------------


#' Get bulk TTM metrics (stable)
#'
#' Lädt die großen Bulk-CSV-Exports für **Key-Metrics-TTM** ODER **Ratios-TTM**.
#' Intern robust: CSV erzwingen, Rate-Limit-JSON erkennen, Retries mit Backoff, Newline-Fix.
#'
#' @param type    Charakter: "key" (Key-Metrics-TTM) oder "ratios" (Ratios-TTM)
#' @param retries Anzahl Retries bei Rate-Limit/Netzwerkproblemen (Default 3)
#' @param pause   Basis-Pause in Sekunden für Backoff-Jitter (Default 0.6)
#' @return Tibble mit allen Spalten, die FMP liefert (inkl. `symbol`). Bei TTM i.d.R. **ohne** Datums-Spalte.
#' @examples
#' \dontrun{
#' fmp_set_key("YOUR_KEY")
#' k <- fmp_metrics_ttm_bulk("key")
#' r <- fmp_metrics_ttm_bulk("ratios")
#' }
#' @export
fmp_metrics_ttm_bulk <- function(type = c("key", "ratios"),
                                 retries = 3L,
                                 pause   = 0.6) {
  key   <- .get_key()
  type  <- match.arg(type)

  path <- switch(type,
                 key    = "key-metrics-ttm-bulk",
                 ratios = "ratios-ttm-bulk")

  make_url <- function() {
    paste0("https://financialmodelingprep.com/stable/", path, "?",
           .qs(list(apikey = key, datatype = "csv")))
  }

  is_limit_json <- function(raw) {
    txt <- rawToChar(raw[seq_len(min(length(raw), 512))])
    grepl('"Error Message"\\s*:\\s*"Limit Reach', txt, ignore.case = TRUE, perl = TRUE)
  }

  fetch_csv_file <- function(url, retries, pause) {
    attempt <- 0L
    repeat {
      attempt <- attempt + 1L
      res <- try(curl::curl_fetch_memory(url), silent = TRUE)
      if (inherits(res, "try-error")) {
        Sys.sleep(runif(1, 0.2, 0.6) * attempt * pause)
        if (attempt > retries) return(tibble::tibble())
        next
      }

      # Rate-Limit-JSON statt CSV?
      if (is_limit_json(res$content)) {
        if (attempt > retries) return(tibble::tibble())
        Sys.sleep(runif(1, 0.8, 1.4) * attempt * pause)
        next
      }

      # → CSV in Tempfile
      tf <- tempfile(fileext = ".csv")
      on.exit(unlink(tf), add = TRUE)
      con <- file(tf, open = "wb"); writeBin(res$content, con); close(con)

      # Newline-Fix (verhindert "unvollständige letzte Zeile")
      if (length(res$content) > 0L) {
        last_byte <- res$content[length(res$content)]
        if (!last_byte %in% as.raw(c(0x0A, 0x0D))) {
          con2 <- file(tf, open = "ab"); writeBin(as.raw(0x0A), con2); close(con2)
        }
      }

      # Robust einlesen
      df <- suppressWarnings(tryCatch(
        utils::read.csv(tf, stringsAsFactors = FALSE, check.names = FALSE,
                        blank.lines.skip = TRUE, na.strings = c("", "NA")),
        error = function(e) {
          tryCatch(
            utils::read.csv(tf, stringsAsFactors = FALSE, check.names = FALSE,
                            fill = TRUE, comment.char = "", na.strings = c("", "NA")),
            error = function(e2) data.frame()
          )
        }
      ))
      return(tibble::as_tibble(df))
    }
  }

  url <- make_url()
  out <- fetch_csv_file(url, retries = retries, pause = pause)

  if (!ncol(out)) return(out)

  # symbol sicherstellen
  if (!"symbol" %in% names(out)) {
    for (alt in c("ticker","code","symbolName")) {
      if (alt %in% names(out)) { out$symbol <- out[[alt]]; break }
    }
  }

  # evtl. Datumsfelder (meist gibt's bei TTM keins, aber just in case)
  for (dc in c("date","reportedDate","fillingDate","calendarYear")) {
    if (dc %in% names(out)) {
      suppressWarnings(out[[dc]] <- tryCatch(as.Date(out[[dc]]), error = function(e) out[[dc]]))
    }
  }

  if ("symbol" %in% names(out)) out <- dplyr::arrange(out, symbol)
  out
}


# --- Bulk: EOD Data (stable) -------------------------------------------

#' Get bulk EOD prices for a given date
#'
#' Lädt den kompletten EOD-Dump für ein Datum (alle verfügbaren Symbole).
#' Robust: CSV wird erzwungen, Rate-Limit-JSON erkannt, Retries mit Backoff,
#' Newline-Fix gegen "unvollständige letzte Zeile".
#'
#' @param date   Zeichenkette "YYYY-MM-DD" (Pflicht)
#' @param retries Anzahl Retries bei Limit/Netzwerkproblemen (Default 3)
#' @param pause   Basis-Pause (Sek.) für Backoff-Jitter (Default 0.6)
#' @return Tibble mit EOD-Feldern (symbol, date, open, high, low, close, volume, ...).
#' @examples
#' \dontrun{
#' fmp_set_key("YOUR_KEY")
#' eod <- fmp_eod_bulk("2024-10-22")
#' }
#' @export
fmp_eod_bulk <- function(date, retries = 3L, pause = 0.6) {
  stopifnot(is.character(date), length(date) == 1L, nchar(date) >= 8L)
  key <- .get_key()

  make_url <- function() {
    paste0(
      "https://financialmodelingprep.com/stable/eod-bulk?",
      .qs(list(date = date, apikey = key, datatype = "csv"))
    )
  }

  is_limit_json <- function(raw) {
    txt <- rawToChar(raw[seq_len(min(length(raw), 512))])
    grepl('"Error Message"\\s*:\\s*"Limit Reach', txt, ignore.case = TRUE, perl = TRUE)
  }

  fetch_csv_file <- function(url, retries, pause) {
    attempt <- 0L
    repeat {
      attempt <- attempt + 1L
      res <- try(curl::curl_fetch_memory(url), silent = TRUE)
      if (inherits(res, "try-error")) {
        Sys.sleep(runif(1, 0.2, 0.6) * attempt * pause)
        if (attempt > retries) return(tibble::tibble())
        next
      }

      if (is_limit_json(res$content)) {
        if (attempt > retries) return(tibble::tibble())
        Sys.sleep(runif(1, 0.8, 1.4) * attempt * pause)
        next
      }

      tf <- tempfile(fileext = ".csv")
      on.exit(unlink(tf), add = TRUE)
      con <- file(tf, open = "wb"); writeBin(res$content, con); close(con)

      # Newline-Fix gegen "unvollständige letzte Zeile"
      if (length(res$content) > 0L) {
        last_byte <- res$content[length(res$content)]
        if (!last_byte %in% as.raw(c(0x0A, 0x0D))) {
          con2 <- file(tf, open = "ab"); writeBin(as.raw(0x0A), con2); close(con2)
        }
      }

      df <- suppressWarnings(tryCatch(
        utils::read.csv(tf, stringsAsFactors = FALSE, check.names = FALSE,
                        blank.lines.skip = TRUE, na.strings = c("", "NA")),
        error = function(e) {
          tryCatch(
            utils::read.csv(tf, stringsAsFactors = FALSE, check.names = FALSE,
                            fill = TRUE, comment.char = "", na.strings = c("", "NA")),
            error = function(e2) data.frame()
          )
        }
      ))
      return(tibble::as_tibble(df))
    }
  }

  url <- make_url()
  out <- fetch_csv_file(url, retries, pause)
  if (!ncol(out)) return(out)

  # Spalten harmonisieren (falls vorhanden)
  # häufig: symbol, date, open, high, low, close, adjClose, volume, exchange, ...
  num_cols <- intersect(
    c("open","high","low","close","adjClose","volume","unadjustedVolume","change","changePercent","vwap"),
    names(out)
  )
  for (nm in num_cols) suppressWarnings(out[[nm]] <- as.numeric(out[[nm]]))
  if ("date" %in% names(out)) suppressWarnings(out$date <- tryCatch(as.Date(out$date), error=function(e) out$date))

  if ("symbol" %in% names(out)) out <- dplyr::arrange(out, symbol)
  out
}

