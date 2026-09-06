# Shadows in the Stock Market

How resting limit orders shape daily highs and lows.

**[Read the analysis →](https://wstone0.github.io/projects/212_final_project/stock_analysis.html)**

A 2024 statistics class project, revisited in 2026 with the controls the original
design was missing. Original work with Alan Schulz Diaz; the revision is mine.

## What it finds

Daily stock prices cluster on round cent values about three times as often as
chance predicts — a replication of Osborne (1962) and Harris (1991) on 9.9M
stock-days.

Daily highs and lows, however, are *not* rounder than ordinary prices. Benchmarked
against opens and closes from the same stock on the same day, they are slightly
less round. What sets extremes apart is something else: **every attracting cent
value casts a shadow.** Highs pile up one tick below it, lows one tick above,
scaling linearly with how hard that level pulls. The within-stock slope is +0.075
for highs and −0.078 for lows, against zero for both benchmark series.

That is the price-level signature of limit orders resting at salient prices —
the structure Osler (2003) identified in FX order books, recovered here from
daily bars.

## Reproducing

```bash
Rscript code/01_build_aggregates.R
Rscript code/02_split_robustness.R
quarto render stock_analysis.qmd
```

The report reads only the small summary tables in `data/aggregates/`, which are
committed, so it renders without the source panel. Regenerating those tables
needs `data/finished_data.csv` — a ~1 GB daily OHLC panel pulled from Yahoo
Finance covering 10,070 tickers, October 2019 to October 2024. It is gitignored;
`code/Cleaning.R` documents how it was built.

| Path | What it is |
|---|---|
| `stock_analysis.qmd` | The report. |
| `code/01_build_aggregates.R` | One pass over the panel; writes the summary tables. |
| `code/02_split_robustness.R` | Re-estimates the result on stock-days untouched by corporate-action adjustment. |
| `data/aggregates/` | Those tables. Every figure and number traces to one of them. |
| `code/*_shiny/` | The two interactive apps linked from the report. |

## A note on the data

Yahoo's OHLC columns are back-adjusted for splits and spin-offs, which moves
about 12% of stock-days off the penny grid before rounding. `02_split_robustness.R`
re-estimates everything on the 77.6% of days untouched by this; the effect gets
stronger, not weaker.
