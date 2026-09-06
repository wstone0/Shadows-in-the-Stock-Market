# ---------------------------------------------------------------------------
# 01_build_aggregates.R
#
# Reads the ~1 GB daily OHLC panel once and writes small aggregate tables that
# the Quarto report reads. Keeping the heavy pass out of the .qmd means the
# report renders in seconds instead of minutes, and every number in the write-up
# has a single, inspectable source.
#
# Run from the PROJECT ROOT:  Rscript code/01_build_aggregates.R
# ---------------------------------------------------------------------------

library(data.table)
setDTthreads(0)

outdir <- "data/aggregates"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

message("reading panel ...")
d <- fread(
  "data/finished_data.csv",
  select = c("Date", "stock_name", "Open", "High", "Low", "Close", "Adj.Close",
             "Volume", "volume_range",
             "Open_cat", "High_cat", "Low_cat", "Close_cat", "Adj.Close_cat"),
  showProgress = FALSE
)

# The cleaning script stored prices as character; the *_cat columns are the last
# two decimal digits as a string ("0", "05", "60"). Coerce both back to numbers.
cat_cols   <- c("Open_cat", "High_cat", "Low_cat", "Close_cat", "Adj.Close_cat")
price_cols <- c("Open", "High", "Low", "Close", "Adj.Close")
for (cc in cat_cols)   set(d, j = cc, value = suppressWarnings(as.integer(d[[cc]])))
for (cc in price_cols) set(d, j = cc, value = suppressWarnings(as.numeric(d[[cc]])))

d[, range_usd := High - Low]
# Adjustment only bites on stocks that paid a dividend or split during the window.
d[, adjusted := abs(Close - Adj.Close) > 0.005]

types <- c("Open", "High", "Low", "Close")
is_round <- function(x) x %in% c(0, 50)

# ---------------------------------------------------------------------------
# 0. provenance
# ---------------------------------------------------------------------------
fwrite(data.table(
  n_rows       = nrow(d),
  n_stocks     = uniqueN(d$stock_name),
  date_min     = as.character(min(d$Date)),
  date_max     = as.character(max(d$Date)),
  share_adjusted   = mean(d$adjusted),
  share_high_eq_low = mean(d$range_usd < 1e-9, na.rm = TRUE),
  share_narrow_range = mean(d$range_usd < 0.20, na.rm = TRUE),
  built_at     = as.character(Sys.time())
), file.path(outdir, "provenance.csv"))

# ---------------------------------------------------------------------------
# 1. full last-two-digit distribution, by price type
#    (drives the replication figure and the four-way comparison)
# ---------------------------------------------------------------------------
dist <- rbindlist(lapply(types, function(ty) {
  cc <- paste0(ty, "_cat")
  t  <- d[!is.na(get(cc)), .(n = .N), by = c(cc)]
  setnames(t, cc, "cent")
  t[, `:=`(type = ty, share = n / sum(n))][]
}))
fwrite(dist[order(type, cent)], file.path(outdir, "cent_distribution.csv"))

# ---------------------------------------------------------------------------
# 2. placebo: split/dividend adjustment should destroy the cent grid
#    Restricted to rows where the adjustment actually changes the price.
# ---------------------------------------------------------------------------
plac <- rbindlist(list(
  d[adjusted == TRUE & !is.na(Close_cat),
    .(n = .N), by = .(cent = Close_cat)][, series := "Close (as traded)"],
  d[adjusted == TRUE & !is.na(`Adj.Close_cat`),
    .(n = .N), by = .(cent = `Adj.Close_cat`)][, series := "Adjusted close"]
))
plac[, share := n / sum(n), by = series]
fwrite(plac[order(series, cent)], file.path(outdir, "placebo_adjustment.csv"))

fwrite(data.table(
  subset = c("adjustment applies", "adjustment applies",
             "no adjustment",      "no adjustment"),
  series = c("Close (as traded)", "Adjusted close",
             "Close (as traded)", "Adjusted close"),
  p_round = c(
    mean(is_round(d[adjusted == TRUE]$Close_cat)),
    mean(is_round(d[adjusted == TRUE]$`Adj.Close_cat`)),
    mean(is_round(d[adjusted == FALSE]$Close_cat)),
    mean(is_round(d[adjusted == FALSE]$`Adj.Close_cat`))
  ),
  n = c(sum(d$adjusted), sum(d$adjusted), sum(!d$adjusted), sum(!d$adjusted))
), file.path(outdir, "placebo_summary.csv"))

# ---------------------------------------------------------------------------
# 3. headline roundness rates by price type, under three sample filters
# ---------------------------------------------------------------------------
filters <- list(
  "All stock-days"                    = quote(rep(TRUE, .N)),
  "Range >= $0.20"                    = quote(range_usd >= 0.20),
  "Range >= $0.20 & volume >= 50k"    = quote(range_usd >= 0.20 & Volume >= 50000)
)

rates <- rbindlist(lapply(names(filters), function(fl) {
  s <- d[eval(filters[[fl]])]
  rbindlist(lapply(types, function(ty) {
    x <- s[[paste0(ty, "_cat")]]; x <- x[!is.na(x)]
    data.table(filter = fl, type = ty, n = length(x),
               p_00 = mean(x == 0), p_50 = mean(x == 50),
               p_round = mean(is_round(x)),
               p_mod10 = mean(x %% 10 == 0),
               p_mod25 = mean(x %% 25 == 0))
  }))
}))
fwrite(rates, file.path(outdir, "roundness_rates.csv"))

# ---------------------------------------------------------------------------
# 4. stock-level paired tests
#    The stock is the unit of observation, which handles within-stock
#    dependence without needing clustered standard errors on 9.9M rows.
# ---------------------------------------------------------------------------
paired <- rbindlist(lapply(names(filters), function(fl) {
  s  <- d[eval(filters[[fl]])]
  sl <- s[, .(nday = .N,
              Open  = mean(is_round(Open_cat)),
              High  = mean(is_round(High_cat)),
              Low   = mean(is_round(Low_cat)),
              Close = mean(is_round(Close_cat))),
          by = stock_name][nday >= 250]
  rbindlist(lapply(list(c("High","Open"), c("Low","Open"),
                        c("High","Close"), c("Low","Close")), function(p) {
    v  <- sl[[p[1]]] - sl[[p[2]]]
    tt <- t.test(v)
    data.table(filter = fl, comparison = paste(p[1], "-", p[2]),
               n_stocks = nrow(sl), mean_delta = mean(v),
               ci_lo = tt$conf.int[1], ci_hi = tt$conf.int[2],
               t_stat = unname(tt$statistic), share_positive = mean(v > 0))
  }))
}))
fwrite(paired, file.path(outdir, "paired_tests.csv"))

# ---------------------------------------------------------------------------
# 5. the .99 / .01 asymmetry, by price type
#    Under any symmetric null the log ratio is zero. Opens and closes are the
#    control series: they have no reason to be asymmetric.
# ---------------------------------------------------------------------------
asym <- rbindlist(lapply(names(filters)[1:2], function(fl) {
  s  <- d[eval(filters[[fl]])]
  sl <- s[, .(nday = .N,
              Open_99  = mean(Open_cat == 99),  Open_01  = mean(Open_cat == 1),
              High_99  = mean(High_cat == 99),  High_01  = mean(High_cat == 1),
              Low_99   = mean(Low_cat == 99),   Low_01   = mean(Low_cat == 1),
              Close_99 = mean(Close_cat == 99), Close_01 = mean(Close_cat == 1)),
          by = stock_name][nday >= 250]
  rbindlist(lapply(types, function(ty) {
    a <- sl[[paste0(ty, "_99")]]; b <- sl[[paste0(ty, "_01")]]
    keep <- a > 0 & b > 0
    v  <- log(a[keep] / b[keep]); tt <- t.test(v)
    data.table(filter = fl, type = ty, n_stocks = sum(keep),
               log_ratio = mean(v), ci_lo = tt$conf.int[1], ci_hi = tt$conf.int[2],
               ratio = exp(mean(v)))
  }))
}))
fwrite(asym, file.path(outdir, "asymmetry.csv"))

# distribution in the neighbourhood of a round dollar, for the asymmetry figure
nbhd <- dist[cent %in% c(90:99, 0:10)]
nbhd[, offset := ifelse(cent >= 90, cent - 100, cent)]   # -10 .. +10 around .00
fwrite(nbhd[order(type, offset)], file.path(outdir, "round_neighbourhood.csv"))

# ---------------------------------------------------------------------------
# 6. the two confounds the original write-up worried about
# ---------------------------------------------------------------------------
d[, price_bin := cut(Close,
                     breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 1000, Inf),
                     labels = c("<$1","$1-5","$5-10","$10-25","$25-50",
                                "$50-100","$100-250","$250-1k",">$1k"))]

fwrite(d[!is.na(price_bin), .(n = .N,
                              Open  = mean(is_round(Open_cat),  na.rm = TRUE),
                              High  = mean(is_round(High_cat),  na.rm = TRUE),
                              Low   = mean(is_round(Low_cat),   na.rm = TRUE),
                              Close = mean(is_round(Close_cat), na.rm = TRUE)),
         by = price_bin][order(price_bin)],
       file.path(outdir, "by_price_bin.csv"))

fwrite(d[, .(n = .N,
             Open  = mean(is_round(Open_cat),  na.rm = TRUE),
             High  = mean(is_round(High_cat),  na.rm = TRUE),
             Low   = mean(is_round(Low_cat),   na.rm = TRUE),
             Close = mean(is_round(Close_cat), na.rm = TRUE)),
         by = volume_range][order(volume_range)],
       file.path(outdir, "by_volume_range.csv"))

message("wrote aggregates to ", outdir)

# ---------------------------------------------------------------------------
# 7. THE SHADOW LAW
#
# `.99` is not a special digit. If highs stall one tick below wherever resting
# sell orders sit, then EVERY attracting level should show excess mass one tick
# below it and a deficit one tick above, scaled by how hard that level pulls.
#
#   mass(c)   = share of prints landing exactly on cent value c
#   shadow(c) = share(c-1) - share(c+1)        [mod 100]
#
# Levels are restricted to multiples of 5 so that neither neighbour is itself a
# magnet. Inference is two-stage: fit the slope of shadow on mass WITHIN each
# stock across the 20 levels, then average those slopes across stocks. That
# treats the stock as the unit and sidesteps dependence between levels.
# ---------------------------------------------------------------------------
LEVELS <- seq(0, 95, 5)

shadow_slopes <- function(catcol) {
  cnt <- d[!is.na(get(catcol)), .(n = .N), by = c("stock_name", catcol)]
  setnames(cnt, catcol, "cent")
  tot <- cnt[, .(N = sum(n)), by = stock_name][N >= 250]
  cnt <- cnt[stock_name %in% tot$stock_name]
  cnt <- cnt[CJ(stock_name = tot$stock_name, cent = 0:99), on = .(stock_name, cent)]
  cnt[is.na(n), n := 0L]
  cnt <- tot[cnt, on = "stock_name"]
  cnt[, share := n / N]

  wide <- dcast(cnt, stock_name ~ cent, value.var = "share")
  gc(verbose = FALSE)
  col <- function(k) as.matrix(wide[, as.character(k %% 100), with = FALSE])

  M <- do.call(cbind, lapply(LEVELS, function(k) col(k)))          # magnet mass
  A <- do.call(cbind, lapply(LEVELS, function(k) col(k - 1) - col(k + 1)))  # shadow

  Mc <- M - rowMeans(M); Ac <- A - rowMeans(A)
  denom <- rowSums(Mc^2)
  slope <- ifelse(denom > 0, rowSums(Mc * Ac) / denom, NA_real_)
  data.table(stock_name = wide$stock_name, slope = slope)
}

slopes <- rbindlist(lapply(types, function(ty) {
  s  <- shadow_slopes(paste0(ty, "_cat"))
  s  <- s[is.finite(slope)]
  tt <- t.test(s$slope)
  data.table(type = ty, n_stocks = nrow(s),
             mean_slope = mean(s$slope), median_slope = median(s$slope),
             ci_lo = tt$conf.int[1],
             ci_hi = tt$conf.int[2], t_stat = unname(tt$statistic),
             share_correct_sign = mean(if (ty == "Low") s$slope < 0 else s$slope > 0))
}))
fwrite(slopes, file.path(outdir, "shadow_slopes.csv"))

# Pooled level-by-level view, for the scatter figure.
pooled <- rbindlist(lapply(types, function(ty) {
  v <- dist[type == ty][order(cent)]$share
  data.table(type = ty, cent = 0:99,
             mass   = v,
             shadow = v[((0:99) - 1) %% 100 + 1] - v[((0:99) + 1) %% 100 + 1])
}))
fwrite(pooled[cent %in% LEVELS], file.path(outdir, "shadow_levels.csv"))

message("wrote shadow tables")
