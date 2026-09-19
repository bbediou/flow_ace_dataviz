# FLOW ACE data visualisation

Two-page ACE dashboard: dataset overview (games played, play-count and play-order heatmaps) and per-task analysis (raw, aggregated, correlations, EF composite scores).

An R Shiny app compiled to WebAssembly with [shinylive](https://posit-dev.github.io/r-shinylive/)
and served as a static site from GitHub Pages. No Shiny server is involved: R runs
in the visitor's browser via webR.

- App source: `app/`
- Build and deploy: `.github/workflows/deploy-shinylive.yaml`

## Layout

```
app/
  app.R
  APPS_fcts/     helper functions actually source()d by app.R
  APPS_data/     one folder per dataset (Clinic, Flow_t1, Flow_t2, School)
```

## Local preview

```r
install.packages("shinylive")
shinylive::export("app", "site")
httpuv::runStaticServer("site")
```

## Packages

Everything the app needs is downloaded into the visitor's browser on first load,
so the dependency list is kept as small as possible:

shiny, bslib, shinyWidgets, shinycssloaders, plotly, dplyr, ggplot2, tidyr,
stringr, purrr, ggsci, ggpubr, lavaan

Adding a package here adds megabytes to every visitor's first load. Packages must
have a WebAssembly build on [repo.r-wasm.org](https://repo.r-wasm.org); pure-R and
most common packages do, but not all of CRAN.
