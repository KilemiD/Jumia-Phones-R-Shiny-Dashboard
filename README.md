# Jumia Phones Dashboard

Smartphone prices and specifications from [Jumia
Kenya](https://www.jumia.co.ke/smartphones/), scraped with Python, cleaned,
and explored in R Shiny. Filter by brand, RAM and storage to see the most and
least expensive handsets, the brand mix, and how average price moves with each
specification.

**Live app:** https://danielkilemi.shinyapps.io/Jumia_Phones_Dashboard/

## Running it

```r
shiny::runApp()
```

Needs: `shiny`, `shinydashboard`, `dplyr`, `readr`, `ggplot2`, `treemapify`,
`highcharter`, `scales`, `htmltools`.

## Layout

```
app.R                  packages, sourcing, page assembly — nothing else
R/
  theme.R              highcharter themes and the sparkline value box
  data.R               loading, cleaning, and the one filter function
  charts.R             the five charts
  mod_explorer.R       filters and outputs, as a Shiny module
www/JMIA_BIG.png       the logo — Shiny only serves static files from www/
Jumia Phones Cleanest Data v3.csv
```

## What changed, and why

**The app could not start.** Line 331 of the old `app.R` was:

```r
source("R-scripts/99-shiny-helpers.R")
```

There is no `R-scripts/` folder in this repository, and `source()` on a
missing file is a hard error — so the app failed before the UI was defined.
Whatever that file held, nothing in `app.R` referenced it; the line is gone.

**The logo never loaded.** `tags$img(src = "JMIA_BIG.png")` only works for
files under `www/`, and the PNG sat in the repository root. It has been moved
to `www/JMIA_BIG.png`.

**Nokia was invisible.** The brand dropdown hard-coded ten brands. The data
has thirteen, so `nokia` (70 listings), `poco` (4) and `blackview` (2) could
not be selected at all. The dropdown is now built from the data.

**"Least expensive" showed the wrong number.** The old code did:

```r
arrange(desc(Price)) %>% top_n(-7)      # the 7 cheapest, still in DESC order
... %>% pull(Price) %>% first()         # so first() is the DEAREST of those 7
```

The headline read Ksh 6,399 when the cheapest phone on Jumia was Ksh 6,340.
`slice_min()` returns them cheapest-first, so the headline is now the actual
minimum.

**RAM and storage sorted as text.** `"8GB"` sorted between `"6GB"` and
`"12GB"` because they were compared as strings. Both are now ordered factors
sorted by their number, so the axes read 1GB → 12GB and 16GB → 256GB.

**Picking a brand was a one-way door.** The cascading dropdowns replaced the
RAM and STORAGE choices with a raw, unsorted, duplicated column slice that had
no `"ALL"` entry — so once you chose a brand you could not get back to all
sizes without reloading. The choices are now derived, de-duplicated, sorted,
and always keep `ALL`.

### The restructure

The old file was 1,656 lines, and most of that was one shape repeated. Each of
the six outputs opened with the same eight-branch `if`/`else if` chain —
every combination of the three filters being `"ALL"` or not — and each branch
repeated the whole chart with a slightly different `filter()` in front of it.
Forty-eight copies of five charts.

Treating `"ALL"` as *no filter* removes the branching entirely. One
`filter_phones()` feeds one reactive dataset, and each chart is written once:

```r
filtered <- reactive({ filter_phones(phones, input$brand, input$ram, input$rom) })
```

Dead weight removed along the way: `DBI` and `RMySQL` were loaded but the data
comes from a CSV; `reshape2`, `ggfittext`, `RColorBrewer`, `DT`, `tibble` and
`purrr` were loaded and never used. `hc_theme_sparkline2()` and
`dropdownButtonp()` were defined and never called — and `dropdownButtonp()`
passed `status` twice, so calling it would have failed. Also gone: a leftover
`#setwd("C:/Users/John/Desktop/jumia stuff/...")`.

Every filter combination now guards against matching nothing, showing "No
phones match this combination" instead of an error mid-dashboard.

## Redeploying

`Jumia_Phones_Dashboard.dcf` records the shinyapps.io app (`appId` 7862962,
account `danielkilemi`). From RStudio use the Publish button, or:

```r
rsconnect::deployApp()
```

`R/` and `www/` must both go up with the app.
