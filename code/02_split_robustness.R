# ---------------------------------------------------------------------------
# 02_split_robustness.R
#
# The OHLC columns from Yahoo are back-adjusted for splits. A 4:1 split divides
# historical prices by four, which moves them onto a quarter-cent grid; the
# cleaning script then rounds them back into pennies. About 12% of stock-days
# are affected, so the cent digit on those rows is a rounding of a price that
# never traded at that cent.
#
# This re-estimates the shadow law on the clean subset only: stock-days where
# ALL FOUR of open, high, low and close sit exactly on the penny grid in the
# raw feed, before any rounding.
#
# Raw prices are stored as float32 widened to float64, so the tolerance for
# "on the grid" has to scale with magnitude rather than being a fixed epsilon.
#
# Run from the PROJECT ROOT:  Rscript code/02_split_robustness.R
# ---------------------------------------------------------------------------

library(data.table)
setDTthreads(0)

outdir <- "data/aggregates"
LEVELS <- seq(0, 95, 5)
types  <- c("Open", "High", "Low", "Close")

message("reading raw wide panel ...")
raw <- fread("data/All_Tickers_5_Year_Data.csv", showProgress = FALSE)

long <- melt(raw, id.vars = "Date", variable.name = "key", value.name = "px",
             variable.factor = FALSE, na.rm = TRUE)
rm(raw); gc(verbose = FALSE)

long[, stock_name := sub("_.*$", "", key)]
long[, field      := sub("^[^_]*_", "", key)]
long <- long[field %in% types]
long[, key := NULL]

# on the penny grid, with float32 headroom that grows with price
long[, cents := px * 100]
long[, on_grid := abs(cents - round(cents)) <= (cents * 2e-7 + 1e-4)]
long[, cent := round(cents) %% 100]

wide <- dcast(long, Date + stock_name ~ field,
              value.var = c("cent", "on_grid"))
rm(long); gc(verbose = FALSE)

ok <- paste0("on_grid_", types)
wide[, clean := Reduce(`&`, lapply(ok, function(c) wide[[c]] %in% TRUE))]

message(sprintf("stock-days: %s total, %s clean (%.1f%%)",
                format(nrow(wide), big.mark = ","),
                format(sum(wide$clean), big.mark = ","),
                100 * mean(wide$clean)))

clean <- wide[clean == TRUE]
rm(wide); gc(verbose = FALSE)

# ---- same two-stage estimator as 01_build_aggregates.R, clean rows only ----
slopes_for <- function(ty) {
  cc  <- paste0("cent_", ty)
  cnt <- clean[!is.na(get(cc)), .(n = .N), by = c("stock_name", cc)]
  setnames(cnt, cc, "cent")
  tot <- cnt[, .(N = sum(n)), by = stock_name][N >= 250]
  cnt <- cnt[stock_name %in% tot$stock_name]
  cnt <- cnt[CJ(stock_name = tot$stock_name, cent = 0:99), on = .(stock_name, cent)]
  cnt[is.na(n), n := 0L]
  cnt <- tot[cnt, on = "stock_name"]
  cnt[, share := n / N]

  w   <- dcast(cnt, stock_name ~ cent, value.var = "share")
  col <- function(k) as.matrix(w[, as.character(k %% 100), with = FALSE])
  M   <- do.call(cbind, lapply(LEVELS, function(k) col(k)))
  A   <- do.call(cbind, lapply(LEVELS, function(k) col(k - 1) - col(k + 1)))

  Mc <- M - rowMeans(M); Ac <- A - rowMeans(A)
  den <- rowSums(Mc^2)
  s <- (rowSums(Mc * Ac) / den)[den > 0]
  s <- s[is.finite(s)]
  tt <- t.test(s)
  data.table(type = ty, n_stocks = length(s), mean_slope = mean(s),
             median_slope = median(s),
             ci_lo = tt$conf.int[1], ci_hi = tt$conf.int[2],
             t_stat = unname(tt$statistic),
             share_correct_sign = mean(if (ty == "Low") s < 0 else s > 0))
}

res <- rbindlist(lapply(types, slopes_for))
fwrite(res, file.path(outdir, "shadow_slopes_clean.csv"))
print(res)
message("wrote shadow_slopes_clean.csv")
