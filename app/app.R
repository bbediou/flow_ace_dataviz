# ==============================================================================
# ACE — Data Visualization Dashboard (v2)
#
#   Page 1  "Dataset Overview"  -> pick dataset, summary value-boxes,
#                                  games-played plots, games matrix (counts /
#                                  play-order heatmaps).
#   Page 2  "Task Analysis"     -> pick task + participants, then explore plots
#                                  grouped into Raw data / Aggregated / Correlations.
#                                  Now also includes EF composite-score views and
#                                  a CSV download of the aggregated EF data.
#
# New in v2 (vs ACE_dataviz_selectdata.R):
#   (1) Overview: participant x game heatmap of play COUNTS (tile + text).
#   (2) Overview: participant x game heatmap of play ORDER (tile + text),
#       derived from the earliest timestamp of each task per participant.
#   (3) Raw data: EF composite scores per participant (boxplot OR radar).
#   (4) Aggregated: group EF composites (boxplot OR radar, mean +/- SD) and a
#       correlation matrix between per-task EF scores and EF composites.
#   (5) Download button: aggregated data file with EF scores as CSV.
#
# All data wrangling and plotting reuses the helper functions in ./APPS_fcts/.
# Data is read from ./APPS_data/<dataset>/ so the app is self-contained and
# deployable to shinyapps.io.
# ==============================================================================

# 1. Required packages --------------------------------------------------------
# Loaded directly. Under shinylive there is nothing to install: the visitor's
# browser fetches these as WebAssembly builds from repo.r-wasm.org. The previous
# install.packages(repos = "https://cloud.r-project.org") bootstrap cannot work
# here, because CRAN serves no WebAssembly binaries.
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)
library(ggsci)
library(ggpubr)
library(lavaan)   # EFA (lavaan::efa) + CFA (lavaan::cfa) of the EF structure

rm(list = ls())

# Load helper functions — must come AFTER rm()
source("./APPS_fcts/ACE_plots_fcts_light.R")
source("./APPS_fcts/ACE_functions.R")

# `%nin%` was previously provided by Hmisc (dropped to keep deps light).
`%nin%` <- function(x, table) !(x %in% table)

# Static, globally-defined lists ----------------------------------------------
list_of_datasets <- c("Flow_t1", "Flow_t2")#,"Clinic", "School")
list_of_tasks    <- c("BACKWARDSSPATIALSPAN", "BOXED", "FILTER", "FLANKER",
                      "SAAT_IMPULSIVE", "SAAT_SUSTAINED", "STROOP",
                      "TASKSWITCH_V2", "TNT")

# Variable menus reused in several places
agg_var_choices <- c(
    "Mean RT (correct)"        = "rt_corr_mean",
    "RT SD"                    = "rt_corr_sd",
    "Accuracy (% correct)"     = "pcorr",
    "Rate Correct Score"       = "rcs",
    "Max Object Count"         = "maxobjectcount",
    "D-prime (psycho)"         = "dprime_psycho",
    "D-prime (corrected)"      = "dprime_corr",
    "Capacity K"               = "k"
)

distrib_var_choices <- c("pcorr", "rcs", "rt_corr_mean", "rt_corr_median",
                         "n_trials", "maxobjectcount", "dprime_psycho",
                         "dprime_corr", "k")

# EF composite columns (executive-function summary scores) --------------------
EF_COMPOSITES <- c("IC_composite", "CF_composite", "WM_composite")
# Per-task EF scores that feed the composites (used in the correlation matrix
# and as the indicators for the EFA / CFA)
EF_TASK_COLS  <- c("IC_stroop_mean", "IC_flanker_mean", "IC_saat_mean",
                   "WM_boxed_mean", "WM_filter_mean", "WM_backwardsspan_mean",
                   "CF_tnt_mean", "CF_switch_mean")

# CFA measurement model — each task loads on its expected EF construct
EF_CFA_MODEL <- '
  IC =~ IC_stroop_mean + IC_flanker_mean + IC_saat_mean
  CF =~ CF_tnt_mean + CF_switch_mean
  WM =~ WM_boxed_mean + WM_filter_mean + WM_backwardsspan_mean
'

# Short task labels (used in the play-order heatmaps)
task_abbr <- c(
    BACKWARDSSPATIALSPAN = "BSS",  BOXED = "Boxed",  FILTER = "Filter",
    FLANKER = "Flanker",  SAAT_IMPULSIVE = "SAATi",  SAAT_SUSTAINED = "SAATs",
    STROOP = "Stroop",  TASKSWITCH_V2 = "Switch",  TNT = "TNT"
)

# Recommended performance metric per task = the metric that feeds the EF
# composite (source: R_scripts/ace_processing_Harmony.R). This one is
# auto-selected and flagged with a star in the "By condition" menu.
recommended_metric <- c(
    BACKWARDSSPATIALSPAN = "maxobjectcount",   # WM_backwardsspan
    BOXED                = "rcs",              # WM_boxed
    FILTER               = "k",                # WM_filter
    FLANKER              = "rcs",              # IC_flanker
    SAAT_IMPULSIVE       = "rcs",              # IC_saat
    SAAT_SUSTAINED       = "rt_corr_sd",       # SA (sustained RT variability)
    STROOP               = "rcs",              # IC_stroop
    TASKSWITCH_V2        = "rcs",              # CF_switch
    TNT                  = "rcs"               # CF_tnt
)

SPINNER_COL <- "#0dc5c1"

# Small helper for the "no data" placeholder
no_data <- function(msg = "No data to display") {
    div(class = "text-muted text-center p-5",
        style = "font-size:1.1rem;",
        icon("circle-info"), " ", msg)
}

# ------------------------------------------------------------------------------
# TEXT-CONTRAST HELPERS
# Pick black or white geom_text() so labels stay readable on any tile fill:
# reproduce the fill colour for each value, measure its luminance, invert.
# ------------------------------------------------------------------------------
.rel_lum <- function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    as.numeric(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ])
}
contrast_col <- function(hex) ifelse(.rel_lum(hex) < 0.5, "white", "black")

# hex colour for each value along a 2+ colour gradient (mirrors scale_fill_gradient*)
ramp_hex <- function(x, colours, limits = range(x, na.rm = TRUE)) {
    d <- diff(limits); if (!is.finite(d) || d == 0) d <- 1
    t <- (pmin(pmax(x, limits[1]), limits[2]) - limits[1]) / d
    t[!is.finite(t)] <- 0.5
    m <- grDevices::colorRamp(colours)(t)
    grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
}
# hex colour for each value along a viridis scale (mirrors scale_fill_viridis_c)
viridis_hex <- function(x, option = "D", direction = 1, limits = range(x, na.rm = TRUE)) {
    d <- diff(limits); if (!is.finite(d) || d == 0) d <- 1
    t <- (x - limits[1]) / d; t[!is.finite(t)] <- 0.5
    if (direction == -1) t <- 1 - t
    viridisLite::viridis(256, option = option)[pmax(1, pmin(256, round(t * 255) + 1))]
}

# ------------------------------------------------------------------------------
# LOCAL PLOT HELPERS (defined here so shared APPS_fcts files stay untouched)
# ------------------------------------------------------------------------------

# (1) Participant x game heatmap of play counts
plot_games_heatmap <- function(gs) {
    gs$.tcol <- contrast_col(ramp_hex(gs$PlayCount, c("#f7fbff", "#08519c")))
    ggplot(gs, aes(x = GameName, y = Sid, fill = PlayCount,
                   text = paste0(Sid, "<br>", GameName, ": ", PlayCount, " play(s)"))) +
        geom_tile(color = "grey92") +
        geom_text(aes(label = PlayCount, color = .tcol), size = 2.4, show.legend = FALSE) +
        scale_color_identity() +
        scale_fill_gradient(low = "#f7fbff", high = "#08519c") +
        labs(x = "Game", y = "Participant", fill = "Plays") +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# (2a) Play order — PER PARTICIPANT sequence.
#      x = order position (1st, 2nd, ...), y = participant, cell = game played.
plot_games_order_seq <- function(ord) {
    ord   <- ord %>% mutate(game = dplyr::recode(task, !!!as.list(task_abbr)))
    games <- sort(unique(ord$game))
    pal   <- ggsci::pal_d3("category20")(length(games)); names(pal) <- games
    ord$.tcol <- contrast_col(pal[ord$game])
    ggplot(ord, aes(x = factor(play_order), y = Sid, fill = game,
                    text = paste0(Sid, "<br>Position ", play_order, ": ", game))) +
        geom_tile(color = "grey92") +
        geom_text(aes(label = game, color = .tcol), size = 2.2, show.legend = FALSE) +
        scale_color_identity() +
        scale_fill_manual(values = pal) +
        labs(x = "Play order (position)", y = "Participant", fill = "Game") +
        theme_minimal(base_size = 11)
}

# (2b) Play order — TASK POSITION counts.
#      x = order position, y = task, cell = # participants who played it there.
plot_games_order_pos <- function(ord) {
    cnt <- ord %>%
        mutate(game = dplyr::recode(task, !!!as.list(task_abbr))) %>%
        count(game, play_order, name = "n")
    cnt$.tcol <- contrast_col(viridis_hex(cnt$n, option = "D", direction = -1))
    ggplot(cnt, aes(x = factor(play_order), y = game, fill = n,
                    text = paste0(game, "<br>Position ", play_order, ": ",
                                  n, " participant(s)"))) +
        geom_tile(color = "grey92") +
        geom_text(aes(label = n, color = .tcol), size = 2.8, show.legend = FALSE) +
        scale_color_identity() +
        scale_fill_viridis_c(direction = -1, option = "D") +
        labs(x = "Play order (position)", y = "Game (task)",
             fill = "# participants") +
        theme_minimal(base_size = 11)
}

# (3)/(4) EF composite boxplot — group boxplot + individual points connected
#         across the three composites (parallel-coordinates style), plus a
#         group-mean diamond.
plot_ef_box <- function(df, comps = EF_COMPOSITES) {
    comps <- intersect(comps, names(df))
    long  <- df[, c("Sid", comps)] %>%
        tidyr::pivot_longer(-Sid, names_to = "composite", values_to = "score") %>%
        dplyr::mutate(composite = factor(composite, levels = comps)) %>%
        dplyr::filter(is.finite(score))
    p <- ggplot(long, aes(x = composite, y = score)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        geom_boxplot(width = .45, outlier.shape = NA, fill = "#cfe8f3", alpha = .6) +
        geom_line(aes(group = Sid), color = "#2C7FB8", alpha = .25, linewidth = .4) +
        geom_point(aes(group = Sid,
                       text = paste0(Sid, "<br>", composite, ": ", round(score, 2))),
                   size = 3.5, alpha = .6, color = "#2C7FB8") +
        stat_summary(fun = mean, geom = "point", shape = 23,
                     size = 4.5, fill = "#d7301f", color = "black") +
        labs(x = NULL, y = "Composite score (z)",
             caption = "Red diamond = group mean") +
        theme_classic()
    ggplotly(p, tooltip = "text")
}

# (3) EF radar — one polygon per participant
ef_radar_individual <- function(df, comps = EF_COMPOSITES) {
    comps <- intersect(comps, names(df))
    p <- plot_ly(type = "scatterpolar", mode = "lines+markers")
    for (i in seq_len(nrow(df))) {
        r <- as.numeric(df[i, comps])
        p <- add_trace(p,
                       r     = c(r, r[1]),
                       theta = c(comps, comps[1]),
                       name  = df$Sid[i],
                       hoverinfo = "text",
                       text  = paste0(df$Sid[i], "<br>", c(comps, comps[1]),
                                      ": ", round(c(r, r[1]), 2)))
    }
    p %>% layout(title = "EF composite scores per participant",
                 polar = list(radialaxis = list(zeroline = TRUE)))
}

# (4) EF radar — group mean +/- SD
ef_radar_group <- function(df, comps = EF_COMPOSITES) {
    comps <- intersect(comps, names(df))
    m  <- vapply(comps, function(cc) mean(df[[cc]], na.rm = TRUE), numeric(1))
    s  <- vapply(comps, function(cc) sd(df[[cc]],   na.rm = TRUE), numeric(1))
    th <- c(comps, comps[1])
    plot_ly(type = "scatterpolar", mode = "lines+markers") %>%
        add_trace(r = c(m + s, (m + s)[1]), theta = th, name = "Mean + SD",
                  line = list(dash = "dot", color = "#9ecae1"), fill = "none",
                  hoverinfo = "skip") %>%
        add_trace(r = c(m - s, (m - s)[1]), theta = th, name = "Mean - SD",
                  line = list(dash = "dot", color = "#9ecae1"), fill = "none",
                  hoverinfo = "skip") %>%
        add_trace(r = c(m, m[1]), theta = th, name = "Mean",
                  line = list(width = 3, color = "#08519c"),
                  marker = list(size = 7, color = "#08519c"),
                  hoverinfo = "text",
                  text = paste0(th, ": ", round(c(m, m[1]), 2),
                                " &plusmn; ", round(c(s, s[1]), 2))) %>%
        layout(title = "Group EF composites (mean &plusmn; SD)",
               polar = list(radialaxis = list(zeroline = TRUE)))
}

# (4) Correlation matrix: per-task EF scores vs EF composites, reordered by
#     hierarchical clustering and partitioned into k clusters (default 3, to
#     surface the IC / CF / WM constructs). Cluster blocks are boxed.
ef_corr_plot <- function(df, k = 3) {
    cols <- intersect(c(EF_TASK_COLS, EF_COMPOSITES), names(df))
    cols <- cols[vapply(cols, function(cc) sum(is.finite(df[[cc]])) > 2, logical(1))]
    validate(need(length(cols) >= 3, "Not enough EF columns to cluster/correlate"))
    M <- suppressWarnings(cor(as.matrix(df[, cols]), use = "pairwise.complete.obs"))

    # --- hierarchical clustering on correlation distance -----------------------
    Mc <- M; Mc[!is.finite(Mc)] <- 0                    # impute for clustering only
    hc  <- hclust(as.dist(1 - Mc), method = "ward.D2")
    lev <- colnames(M)[hc$order]                        # dendrogram order
    k   <- min(k, length(lev))
    cl  <- cutree(hc, k = k)[lev]                        # cluster id per position

    # cluster boundaries (clusters are contiguous in dendrogram order)
    rects <- data.frame(lo = numeric(0), hi = numeric(0))
    start <- 1
    for (i in 2:(length(cl) + 1)) {
        if (i > length(cl) || cl[i] != cl[i - 1]) {
            rects <- rbind(rects, data.frame(lo = start - 0.5, hi = (i - 1) + 0.5))
            start <- i
        }
    }

    long <- as.data.frame(as.table(M))
    names(long) <- c("Var1", "Var2", "r")
    long$xi <- match(long$Var1, lev)
    long$yi <- match(long$Var2, lev)
    long$.tcol <- contrast_col(ramp_hex(long$r, c("#B2182B", "white", "#2166AC"),
                                        limits = c(-1, 1)))

    p <- ggplot(long, aes(x = xi, y = yi, fill = r,
                          text = paste0(Var1, " vs ", Var2, "<br>r = ", round(r, 2)))) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(r, 2), color = .tcol), size = 2.3, show.legend = FALSE) +
        scale_color_identity() +
        geom_rect(data = rects, inherit.aes = FALSE,
                  aes(xmin = lo, xmax = hi, ymin = lo, ymax = hi),
                  fill = NA, color = "black", linewidth = .8) +
        scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                             midpoint = 0, limits = c(-1, 1)) +
        scale_x_continuous(breaks = seq_along(lev), labels = lev, expand = c(0, 0)) +
        scale_y_continuous(breaks = seq_along(lev), labels = lev, expand = c(0, 0)) +
        labs(x = NULL, y = NULL, fill = "r",
             title = paste0("EF task ↔ composite correlations (", k,
                            " hierarchical clusters)")) +
        theme_minimal(base_size = 10) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
}

# --- Factor analysis of the EF structure -------------------------------------
# Indicator matrix = the per-task EF scores that feed the composites.
ef_fa_data <- function(df) {
    ind <- intersect(EF_TASK_COLS, names(df))
    d <- as.data.frame(df[, ind, drop = FALSE])
    d[rowSums(!is.na(d)) > 0, , drop = FALSE]   # drop fully-empty rows
}

# Shared loadings heatmap (indicator x factor)
loadings_heatmap <- function(long, title) {
    long$factor <- factor(long$factor)
    # Clamp the COLOUR to [-1, 1] so tile fill (and thus the text-contrast
    # decision) stay bounded even when improper/Heywood loadings exceed 1.
    # The printed label still shows the true loading.
    long$fill_v <- pmin(pmax(long$loading, -1), 1)
    long$.tcol  <- contrast_col(ramp_hex(long$fill_v,
                                         c("#B2182B", "white", "#2166AC"),
                                         limits = c(-1, 1)))
    cap <- if (any(abs(long$loading) > 1, na.rm = TRUE))
        "Colour capped at ±1 (loadings can exceed 1 in improper solutions)" else NULL
    ggplotly(
        ggplot(long, aes(x = factor, y = indicator, fill = fill_v,
                         text = paste0(indicator, " → ", factor,
                                       "<br>loading = ", round(loading, 2)))) +
            geom_tile(color = "white") +
            geom_text(aes(label = sprintf("%.2f", loading), color = .tcol),
                      size = 3, show.legend = FALSE) +
            scale_color_identity() +
            scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                                 midpoint = 0, limits = c(-1, 1),
                                 oob = scales::squish, na.value = "grey90") +
            labs(x = NULL, y = NULL, fill = "loading", title = title, caption = cap) +
            theme_minimal(base_size = 10),
        tooltip = "text")
}

# Exploratory FA (oblimin rotation, FIML for missing data). Returns fit or NULL.
ef_efa_fit <- function(df, nf = 3) {
    d <- ef_fa_data(df)
    if (nrow(d) < 10 || ncol(d) < nf + 1) return(NULL)
    tryCatch(suppressWarnings(
        lavaan::efa(data = d, nfactors = nf, rotation = "oblimin", missing = "ml")),
        error = function(e) NULL)
}

ef_efa_plot <- function(fit) {
    L <- as.matrix(unclass(fit$loadings))
    long <- as.data.frame(as.table(L))
    names(long) <- c("indicator", "factor", "loading")
    loadings_heatmap(long, "EFA loadings (3 factors, oblimin rotation)")
}

ef_efa_print <- function(fit, df) {
    d  <- ef_fa_data(df)
    L  <- as.matrix(unclass(fit$loadings))
    ss <- colSums(L^2)
    pv <- ss / nrow(L)
    ev <- tryCatch(eigen(cor(d, use = "pairwise.complete.obs"))$values,
                   error = function(e) NA_real_)
    cat("Exploratory Factor Analysis  (oblimin rotation, FIML)\n")
    cat("Indicators:", nrow(L), " | Factors: 3 | n =", nrow(d), "\n\n")
    cat("SS loadings    :", paste(sprintf("%.2f", ss), collapse = "   "), "\n")
    cat("Prop. variance :", paste(sprintf("%.2f", pv), collapse = "   "), "\n")
    if (all(is.finite(ev)))
        cat("\nEigenvalues (corr matrix):", paste(sprintf("%.2f", ev), collapse = "  "),
            "\n(# eigenvalues > 1 is a rough guide to the number of factors)\n")
    cat("\nRead the heatmap: each task should load highest on its expected",
        "\nfactor (IC = stroop/flanker/saat, CF = tnt/switch,",
        "\nWM = boxed/filter/backwardsspan).\n")
}

# Confirmatory FA (each task -> expected construct, FIML). Returns fit or NULL.
ef_cfa_fit <- function(df) {
    d <- ef_fa_data(df)
    if (nrow(d) < 10) return(NULL)
    tryCatch(suppressWarnings(
        lavaan::cfa(EF_CFA_MODEL, data = d, std.lv = TRUE, missing = "fiml")),
        error = function(e) NULL)
}

ef_cfa_plot <- function(fit) {
    se <- lavaan::standardizedSolution(fit)
    se <- se[se$op == "=~", ]
    long <- data.frame(indicator = se$rhs,
                       factor    = factor(se$lhs, levels = c("IC", "CF", "WM")),
                       loading   = se$est.std)
    loadings_heatmap(long, "CFA standardized loadings (expected structure)")
}

ef_cfa_print <- function(fit) {
    cat("Confirmatory Factor Analysis  (3-factor, FIML)\n")
    cat("Model:\n")
    cat("  IC =~ stroop + flanker + saat\n")
    cat("  CF =~ tnt + switch\n")
    cat("  WM =~ boxed + filter + backwardsspan\n")
    cat("Converged:", lavaan::lavInspect(fit, "converged"),
        " | n =", lavaan::lavInspect(fit, "nobs"), "\n\n")
    fm <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue",
                                     "cfi", "tli", "rmsea", "srmr"))
    cat("Fit indices:\n"); print(round(fm, 3))
    cat("Guide: CFI/TLI > .90 good; RMSEA < .08 acceptable; SRMR < .08 good.\n")
    se <- lavaan::standardizedSolution(fit)
    fc <- se[se$op == "~~" & se$lhs != se$rhs &
             se$lhs %in% c("IC", "CF", "WM") & se$rhs %in% c("IC", "CF", "WM"), ]
    if (nrow(fc)) {
        cat("\nLatent factor correlations:\n")
        print(fc[, c("lhs", "rhs", "est.std", "pvalue")], row.names = FALSE)
    }
    cat("\nNote: with small samples these estimates are unstable — interpret with care.\n")
}


# USER INTERFACE ==============================================================
ui <- page_navbar(
    title = tagList(icon("brain"), "ACE Data Visualization Dashboard"),
    id    = "main_nav",
    theme = bs_theme(
        version    = 5,
        bootswatch = "flatly",
        primary    = "#2C7FB8",
        base_font  = font_google("Inter", local = FALSE)
    ),
    fillable = FALSE,
    underline = TRUE,

    # ---- PAGE 1: DATASET OVERVIEW -------------------------------------------
    nav_panel(
        title = "Dataset Overview",
        icon  = icon("database"),

        layout_columns(
            col_widths  = c(4, 8),
            row_heights = "auto",

            card(
                card_header(icon("table-list"), " Dataset"),
                selectInput("dataset", "Select dataset",
                            choices = list_of_datasets, width = "100%"),
                helpText("Summary statistics, games played and all task ",
                         "analyses update automatically when you switch dataset.")
            ),

            uiOutput("overview_valueboxes") %>%
                withSpinner(color = SPINNER_COL, proxy.height = "140px")
        ),

        card(
            full_screen = TRUE,
            card_header(
                class = "d-flex justify-content-between align-items-center",
                tagList(icon("gamepad"), " Games played"),
                radioGroupButtons(
                    "ngames_view", label = NULL,
                    choices = c("Distribution" = "hist",
                                "Per participant" = "subj"),
                    selected = "hist", size = "sm", status = "primary"
                )
            ),
            uiOutput("panel_ngames_played") %>% withSpinner(color = SPINNER_COL)
        ),

        # (1)+(2) NEW: participant x game matrix (counts / play order)
        card(
            full_screen = TRUE,
            card_header(
                class = "d-flex justify-content-between align-items-center",
                tagList(icon("border-all"), " Games matrix (participant × game)"),
                radioGroupButtons(
                    "games_matrix_view", label = NULL,
                    choices = c("Play counts"          = "count",
                                "Order · per participant" = "seq",
                                "Order · task positions"  = "pos"),
                    selected = "count", size = "sm", status = "primary"
                )
            ),
            uiOutput("panel_games_matrix") %>% withSpinner(color = SPINNER_COL)
        )
    ),

    # ---- PAGE 2: TASK ANALYSIS ----------------------------------------------
    nav_panel(
        title = "Task Analysis",
        icon  = icon("chart-line"),

        layout_sidebar(
            sidebar = sidebar(
                width = 320,
                title = tagList(icon("sliders"), " Selection"),
                selectInput("data", "Select task", choices = list_of_tasks,
                            width = "100%"),
                uiOutput("picker_IDs"),
                hr(),
                helpText("All participants are selected by default. Use the ",
                         "menu above to focus on a subset.")
            ),

            navset_card_tab(
                id = "analysis_tabs",

                # ---------- RAW DATA -------------------------------------------
                nav_panel(
                    title = tagList(icon("braille"), " Raw data"),
                    navset_pill(
                        nav_panel("N trials — per subject",
                                  uiOutput("panel_ntrials.subj") %>%
                                      withSpinner(color = SPINNER_COL)),
                        nav_panel("N trials — distribution",
                                  uiOutput("panel_ntrials.distrib") %>%
                                      withSpinner(color = SPINNER_COL)),
                        nav_panel("Raw RT — per subject",
                                  uiOutput("panel_rt.trials") %>%
                                      withSpinner(color = SPINNER_COL)),
                        nav_panel("Raw RT — distribution",
                                  uiOutput("panel_rt.distrib") %>%
                                      withSpinner(color = SPINNER_COL)),
                        nav_panel("Response windows",
                                  uiOutput("panel_response.window") %>%
                                      withSpinner(color = SPINNER_COL))
                    )
                ),

                # ---------- AGGREGATED PERFORMANCE -----------------------------
                nav_panel(
                    title = tagList(icon("chart-column"), " Aggregated performance"),
                    navset_pill(
                        nav_panel(
                            "By condition",
                            div(class = "mb-3",
                                selectInput("agg_var", "Performance metric:",
                                            choices = agg_var_choices,
                                            selected = "rt_corr_mean",
                                            width = "320px")),
                            uiOutput("panel_agg_bycond") %>%
                                withSpinner(color = SPINNER_COL)
                        ),
                        nav_panel(
                            "Distribution",
                            div(class = "d-flex gap-3 flex-wrap mb-3",
                                radioGroupButtons(
                                    "keep_cond", "Keep experimental conditions?",
                                    choices = c("Yes", "No"), selected = "Yes",
                                    status = "primary", size = "sm"),
                                selectInput("Xvar", "Variable to plot:",
                                            choices = distrib_var_choices,
                                            selected = "pcorr", width = "260px")),
                            uiOutput("panel_agg_distrib") %>%
                                withSpinner(color = SPINNER_COL)
                        )
                    )
                ),

                # ---------- CORRELATIONS ---------------------------------------
                nav_panel(
                    title = tagList(icon("circle-nodes"), " Correlations"),
                    navset_pill(
                        nav_panel(
                            "Performance vs Age",
                            div(class = "mb-3",
                                selectInput("corrage_var", "Performance metric:",
                                            choices = agg_var_choices,
                                            selected = "rt_corr_mean",
                                            width = "320px")),
                            uiOutput("panel_corrwithage") %>%
                                withSpinner(color = SPINNER_COL)
                        ),
                        nav_panel(
                            "Game score vs Age",
                            plotlyOutput("plot_task.corrwithage.gamescore",
                                         height = "500px") %>%
                                withSpinner(color = SPINNER_COL)
                        ),
                        nav_panel(
                            "Performance vs Game score",
                            div(class = "mb-3",
                                selectInput("Yvar", "Performance metric:",
                                            choices = c("rt_corr_mean", "rt_corr_sd",
                                                        "rcs", "dprime_psycho",
                                                        "dprime_corr", "k",
                                                        "maxobjectcount"),
                                            selected = "rt_corr_mean",
                                            width = "320px")),
                            plotlyOutput("gamescore_corr", height = "500px") %>%
                                withSpinner(color = SPINNER_COL)
                        ),
                        nav_panel(
                            "X – Y (MICAH scoring)",
                            div(class = "d-flex gap-3 flex-wrap mb-3",
                                selectInput("Xvar_corr", "X variable:",
                                            choices = c("n_trials", "rt_corr_mean",
                                                        "rt_corr_median", "pcorr",
                                                        "rcs", "dprime_psycho",
                                                        "dprime_corr", "maxobjectcount",
                                                        "k"),
                                            selected = "rt_corr_mean", width = "260px"),
                                selectInput("Yvar_corr", "Y variable:",
                                            choices = c("n_trials", "rt_corr_mean",
                                                        "rt_corr_median", "pcorr",
                                                        "rcs", "dprime_psycho",
                                                        "dprime_corr", "dprime",
                                                        "maxobjectcount", "k"),
                                            selected = "rt_corr_median", width = "260px")),
                            plotlyOutput("plot_corr_XY", height = "500px") %>%
                                withSpinner(color = SPINNER_COL)
                        )
                    )
                )
            )
        )
    ),

    # ---- PAGE 3: EXECUTIVE FUNCTION -----------------------------------------
    # EF composite scores combine ALL tasks and are participant-level, so this
    # page has its own participant picker and no task selector.
    nav_panel(
        title = "Executive Function",
        icon  = icon("brain"),

        layout_sidebar(
            sidebar = sidebar(
                width = 320,
                title = tagList(icon("sliders"), " Selection"),
                uiOutput("picker_IDs_ef"),
                hr(),
                downloadButton("download_ef", "Download EF data (CSV)",
                               class = "btn-sm btn-primary w-100"),
                helpText("Downloads the full aggregated data file with EF ",
                         "composite scores for the selected dataset."),
                hr(),
                helpText("EF composites combine all tasks, so there is no task ",
                         "selector here. All participants are selected by default.")
            ),

            navset_card_tab(
                id = "ef_tabs",

                # ---------- COMPOSITE SCORES -----------------------------------
                nav_panel(
                    title = tagList(icon("chart-column"), " Composite scores"),
                    div(class = "mb-3",
                        radioGroupButtons(
                            "ef_view", "Display:",
                            choices = c("Boxplot (participants + group)" = "box",
                                        "Radar · participants"           = "radar_ind",
                                        "Radar · group (mean ± SD)"       = "radar_grp"),
                            selected = "box", status = "primary", size = "sm")),
                    uiOutput("panel_ef") %>% withSpinner(color = SPINNER_COL)
                ),

                # ---------- STRUCTURE: corr / EFA / CFA ------------------------
                nav_panel(
                    title = tagList(icon("circle-nodes"), " Structure (corr / EFA / CFA)"),
                    navset_pill(
                        nav_panel(
                            "Correlations (clustered)",
                            helpText("Correlations between per-task EF scores and ",
                                     "the IC / CF / WM composites, reordered by ",
                                     "hierarchical clustering (3 clusters, boxed)."),
                            plotlyOutput("plot_ef_corr", height = "600px") %>%
                                withSpinner(color = SPINNER_COL)
                        ),
                        nav_panel(
                            "Exploratory (EFA)",
                            helpText("Exploratory factor analysis (3 factors, ",
                                     "oblimin rotation, FIML) on the 8 task scores ",
                                     "that feed the composites — does the expected ",
                                     "IC / CF / WM structure emerge on its own?"),
                            plotlyOutput("ef_efa_plot", height = "460px") %>%
                                withSpinner(color = SPINNER_COL),
                            verbatimTextOutput("ef_efa_text")
                        ),
                        nav_panel(
                            "Confirmatory (CFA)",
                            helpText("Confirmatory factor analysis: does each task ",
                                     "load on its expected construct? Latent factors ",
                                     "are defined by the task indicators; the ",
                                     "composites are their observed proxies."),
                            plotlyOutput("ef_cfa_plot", height = "460px") %>%
                                withSpinner(color = SPINNER_COL),
                            verbatimTextOutput("ef_cfa_text")
                        )
                    )
                )
            )
        )
    ),

    nav_spacer(),
    nav_item(
        tags$span(class = "navbar-text small text-muted", "MICAH analytics")
    )
)


# SERVER =======================================================================
server <- function(input, output, session) {

    # ---------------------------------------------------------------------------
    # DATASET LOADER
    # ---------------------------------------------------------------------------
    getDataSet <- reactive({
        data_path <- case_when(
            input$dataset == "Clinic"  ~ "APPS_data/Clinic/",
            input$dataset == "School"  ~ "APPS_data/School/",
            input$dataset == "Flow_t1" ~ "APPS_data/Flow_t1/",
            input$dataset == "Flow_t2" ~ "APPS_data/Flow_t2/"
        )
        data_file <- case_when(
            input$dataset == "Clinic"  ~ "ace_data_Harmony.RData",
            input$dataset == "School"  ~ "ace_data_Harmony.RData",
            input$dataset == "Flow_t1" ~ "ace_data_flow_t1.RData",
            input$dataset == "Flow_t2" ~ "ace_data_flow_t2.RData"
        )
        demographics_file <- case_when(
            input$dataset == "Clinic"  ~ "ace_demographics_Harmony.RDS",
            input$dataset == "School"  ~ "ace_demographics_Harmony.RDS",
            input$dataset == "Flow_t1" ~ "ace_demographics_flow_t1.RDS",
            input$dataset == "Flow_t2" ~ "ace_demographics_flow_t2.RDS"
        )
        gamescores_file <- case_when(
            input$dataset == "Clinic"  ~ "ace_games_data_Harmony.RDS",
            input$dataset == "School"  ~ "ace_games_data_Harmony.RDS",
            input$dataset == "Flow_t1" ~ "ace_games_data_flow_t1.RDS",
            input$dataset == "Flow_t2" ~ "ace_games_data_flow_t2.RDS"
        )
        EF_file <- case_when(
            input$dataset == "Clinic"  ~ "ace_final_merged_df_EF_Harmony.RDS",
            input$dataset == "School"  ~ "ace_final_merged_df_EF_Harmony.RDS",
            input$dataset == "Flow_t1" ~ "ace_final_merged_df_EF_flow_t1.RDS",
            input$dataset == "Flow_t2" ~ "ace_final_merged_df_EF_flow_t2.RDS"
        )

        e <- new.env()
        load(paste0(data_path, data_file), envir = e)

        # EF file — resilient load. Prefer the expected name; if it is missing
        # (e.g. Flow_t2 currently ships an EF file misnamed "..._flow_t1.RDS"),
        # fall back to any ace_final_merged_df_EF*.RDS in the folder. If none is
        # found, ef_data stays NULL and the EF panels show a friendly message.
        ef_path <- paste0(data_path, EF_file)
        if (!file.exists(ef_path)) {
            alt <- list.files(data_path,
                              pattern = "^ace_final_merged_df_EF.*\\.RDS$",
                              full.names = TRUE)
            ef_path <- if (length(alt)) alt[[1]] else NA_character_
        }
        ef_data <- if (!is.na(ef_path) && file.exists(ef_path))
            readRDS(ef_path) else NULL

        demographics <- readRDS(paste0(data_path, demographics_file))
        gamesScores  <- readRDS(paste0(data_path, gamescores_file)) %>%
            mutate(task = ifelse(task == "bss", "backwardsspatialspan", task))

        list_of_IDs <- unique(demographics$Sid)

        list(env          = e,
             demographics = demographics,
             gamesScores  = gamesScores,
             ef_data      = ef_data,          # <- now returned (was loaded but dropped)
             list_of_IDs  = list_of_IDs)
    })

    # ---------------------------------------------------------------------------
    # DYNAMIC PARTICIPANT PICKER — ALL participants selected by default
    # ---------------------------------------------------------------------------
    output$picker_IDs <- renderUI({
        ids <- getDataSet()$list_of_IDs
        shinyWidgets::pickerInput(
            inputId  = "ID_select",
            label    = "Participants to display:",
            choices  = ids,
            selected = ids,
            options  = list(
                `min-options`          = 1,
                `actions-box`          = TRUE,
                `live-search`          = TRUE,
                size                   = 12,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE,
            width    = "100%"
        )
    })

    # Selected IDs with a safe fallback to "all" (picker not yet rendered)
    selected_ids <- reactive({
        sel <- input$ID_select
        if (is.null(sel)) getDataSet()$list_of_IDs else sel
    })

    # Independent participant picker for the Executive Function page
    output$picker_IDs_ef <- renderUI({
        ids <- getDataSet()$list_of_IDs
        shinyWidgets::pickerInput(
            inputId  = "ID_select_ef",
            label    = "Participants to display:",
            choices  = ids,
            selected = ids,
            options  = list(
                `min-options`          = 1,
                `actions-box`          = TRUE,
                `live-search`          = TRUE,
                size                   = 12,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE,
            width    = "100%"
        )
    })

    ef_selected_ids <- reactive({
        sel <- input$ID_select_ef
        if (is.null(sel)) getDataSet()$list_of_IDs else sel
    })

    # ---------------------------------------------------------------------------
    # HELPER: Sid cleaning
    # ---------------------------------------------------------------------------
    clean_sids <- function(df) {
        df %>%
            mutate(
                Sid = gsub(".*\\_(.*)\\_.*", "\\1", Participant.Id),
                Sid = gsub(x = Sid, pattern = "harmony-test2_", replacement = ""),
                Sid = gsub(x = Sid, pattern = "flow_t1_", replacement = ""),
                Sid = gsub(x = Sid, pattern = "flow_t2_", replacement = ""),
                Sid = gsub(x = Sid, pattern = " ",              replacement = ""),
                Sid = str_replace_all(Sid, "[^[:alnum:]]", "")
            )
    }

    # ---------------------------------------------------------------------------
    # REACTIVE DATA FRAMES
    # ---------------------------------------------------------------------------
    getData <- reactive({
        ds      <- getDataSet()
        df_name <- paste0(tolower(input$data), "_processed")
        get(df_name, envir = ds$env) %>%
            filter(Session.Type == "Real") %>%
            clean_sids() %>%
            filter(Sid %in% input$ID_select)
    })

    aggData_cond <- reactive({
        ds      <- getDataSet()
        df_name <- paste0(tolower(input$data), "_agg_cond")
        get(df_name, envir = ds$env) %>%
            clean_sids() %>%
            filter(Sid %in% input$ID_select)
    })

    aggData <- reactive({
        ds      <- getDataSet()
        df_name <- paste0(tolower(input$data), "_agg")
        get(df_name, envir = ds$env) %>%
            clean_sids() %>%
            filter(Sid %in% input$ID_select)
    })

    mergeData_cond <- reactive({ merge(aggData_cond(), getDataSet()$demographics) })
    mergeData      <- reactive({ merge(aggData(),      getDataSet()$demographics) })

    # EF composite data, Sid-normalized to match the participant picker
    efData <- reactive({
        ef <- getDataSet()$ef_data
        if (is.null(ef) || nrow(ef) == 0) return(data.frame())
        ef %>%
            clean_sids() %>%
            filter(Sid %in% ef_selected_ids())
    })

    # Per-participant total games played (used by overview plots)
    gamesPerSubject <- reactive({
        getDataSet()$gamesScores %>%
            group_by(Sid) %>%
            dplyr::summarize(n_games_played = sum(PlayCount, na.rm = TRUE),
                             .groups = "drop")
    })

    # Play ORDER: for each participant, rank the tasks by earliest timestamp
    gamesOrder <- reactive({
        ds <- getDataSet()
        purrr::map_dfr(list_of_tasks, function(tk) {
            obj <- paste0(tolower(tk), "_processed")
            if (!exists(obj, envir = ds$env, inherits = FALSE)) return(NULL)
            df <- get(obj, envir = ds$env)
            if (!"datetime" %in% names(df)) return(NULL)
            df %>%
                clean_sids() %>%
                mutate(.t = suppressWarnings(as.POSIXct(datetime))) %>%
                group_by(Sid) %>%
                dplyr::summarize(t0 = suppressWarnings(min(.t, na.rm = TRUE)),
                                 .groups = "drop") %>%
                mutate(task = tk)
        }) %>%
            filter(is.finite(as.numeric(t0))) %>%
            group_by(Sid) %>%
            mutate(play_order = rank(t0, ties.method = "first")) %>%
            ungroup()
    })

    # ===========================================================================
    # PAGE 1 — DATASET OVERVIEW
    # ===========================================================================

    output$overview_valueboxes <- renderUI({
        demo <- getDataSet()$demographics
        gs   <- getDataSet()$gamesScores

        n_part <- length(unique(demo$Sid))

        if ("Age" %in% names(demo)) {
            ages <- demo %>% distinct(Sid, .keep_all = TRUE) %>% pull(Age)
            ages <- suppressWarnings(as.numeric(ages))
            ages <- ages[is.finite(ages)]
        } else {
            ages <- numeric(0)
        }
        age_mean  <- if (length(ages)) sprintf("%.1f yrs", mean(ages)) else "n/a"
        age_range <- if (length(ages)) sprintf("%g – %g", min(ages), max(ages)) else "n/a"

        total_games <- if (!is.null(gs) && nrow(gs))
            format(sum(gs$PlayCount, na.rm = TRUE), big.mark = ",") else "0"

        layout_column_wrap(
            width = 1/2, fill = FALSE,
            value_box("Participants", n_part, showcase = icon("users"), theme = "primary"),
            value_box("Mean age", age_mean, showcase = icon("cake-candles"), theme = "info"),
            value_box("Age range", age_range, showcase = icon("arrows-left-right"), theme = "secondary"),
            value_box("Total games played", total_games, showcase = icon("gamepad"), theme = "success")
        )
    })

    output$panel_ngames_played <- renderUI({
        gs <- getDataSet()$gamesScores
        if (is.null(gs) || nrow(gs) == 0) return(no_data("No games-played data"))
        if (identical(input$ngames_view, "subj"))
            plotlyOutput("plot_ngames.sids", height = "560px")
        else
            plotlyOutput("plot_ngames.distrib", height = "480px")
    })

    output$plot_ngames.distrib <- renderPlotly({
        p1 <- gamesPerSubject() %>%
            ggplot(aes(x = n_games_played)) +
            geom_histogram(col = "white", fill = "#2C7FB8") +
            labs(x = "Number of games played", y = "Number of participants",
                 title = "Distribution of games played") +
            theme_bw()
        ggplotly(p1)
    })

    output$plot_ngames.sids <- renderPlotly({
        p2 <- gamesPerSubject() %>%
            ggplot(aes(x = reorder(Sid, n_games_played), y = n_games_played, text = Sid)) +
            geom_segment(aes(xend = Sid, y = 0, yend = n_games_played), col = "grey70") +
            geom_point(col = "#2C7FB8", size = 2) +
            coord_flip() +
            labs(x = "Participant", y = "Number of games played",
                 title = "Games played per participant") +
            theme_bw()
        ggplotly(p2, tooltip = "text")
    })

    # (1)+(2) Games matrix
    output$panel_games_matrix <- renderUI({
        switch(input$games_matrix_view,
               seq = plotlyOutput("plot_games_order_seq", height = "700px"),
               pos = plotlyOutput("plot_games_order_pos", height = "480px"),
               plotlyOutput("plot_games_heatmap", height = "700px"))
    })

    # NOTE: the Overview page is dataset-level, so these use ALL participants of
    # the current dataset — NOT the Task-Analysis participant picker (which lives
    # on page 2 and would otherwise leave these empty after switching datasets).
    output$plot_games_heatmap <- renderPlotly({
        ids <- getDataSet()$list_of_IDs
        gs  <- getDataSet()$gamesScores %>% filter(Sid %in% ids)
        validate(need(nrow(gs) > 0, "No games data for this dataset"))
        ggplotly(plot_games_heatmap(gs), tooltip = "text")
    })

    output$plot_games_order_seq <- renderPlotly({
        ord <- gamesOrder() %>% filter(Sid %in% getDataSet()$list_of_IDs)
        validate(need(nrow(ord) > 0,
                      "No play-order data available (no task timestamps found)"))
        ggplotly(plot_games_order_seq(ord), tooltip = "text")
    })

    output$plot_games_order_pos <- renderPlotly({
        ord <- gamesOrder() %>% filter(Sid %in% getDataSet()$list_of_IDs)
        validate(need(nrow(ord) > 0,
                      "No play-order data available (no task timestamps found)"))
        ggplotly(plot_games_order_pos(ord), tooltip = "text")
    })

    # ===========================================================================
    # PAGE 2 — TASK ANALYSIS
    # ===========================================================================

    ## ---- Raw data panels -----------------------------------------------------
    output$panel_ntrials.subj <- renderUI({
        if (nrow(getData()) == 0) no_data() else plotlyOutput("plot_ntrials.subj", height = "560px")
    })
    output$panel_ntrials.distrib <- renderUI({
        if (nrow(getData()) == 0) no_data() else plotlyOutput("plot_ntrials.distrib", height = "480px")
    })
    output$panel_rt.trials <- renderUI({
        if (nrow(getData()) == 0) no_data() else plotlyOutput("plot_rt.trials", height = "560px")
    })
    output$panel_rt.distrib <- renderUI({
        if (nrow(getData()) == 0) no_data() else plotlyOutput("plot_rt.distrib", height = "480px")
    })
    output$panel_response.window <- renderUI({
        if (is.null(getData()$Response.Window) || nrow(getData()) == 0)
            no_data("No response-window data for this task")
        else plotlyOutput("plot_rw.trials", height = "560px")
    })

    output$plot_ntrials.subj    <- renderPlotly({ plot_ntrials.subj(getData()) })
    output$plot_ntrials.distrib <- renderPlotly({ plot_ntrials.distrib(getData()) })
    output$plot_rt.trials       <- renderPlotly({ plot_rt.trials(getData()) })
    output$plot_rt.distrib      <- renderPlotly({ plot_rt.distrib(getData()) })
    output$plot_rw.trials       <- renderPlotly({ plot_rw.trials(getData()) })

    ## ---- Aggregated performance ----------------------------------------------
    output$panel_agg_bycond <- renderUI({
        df <- aggData_cond()
        v  <- input$agg_var
        if (nrow(df) == 0) {
            no_data()
        } else if (v %nin% colnames(df)) {
            no_data(paste0("Metric '", v, "' is not available for ", input$data))
        } else if (all(is.na(df[[v]]))) {
            no_data(paste0("Metric '", v, "' has no values for ", input$data))
        } else {
            plotlyOutput("plot_agg_bycond", height = "560px")
        }
    })
    output$plot_agg_bycond <- renderPlotly({ plot_task.perf.var(aggData_cond(), input$agg_var) })

    output$panel_agg_distrib <- renderUI({
        if (nrow(aggData()) == 0) {
            no_data()
        } else if (input$Xvar %nin% colnames(aggData())) {
            no_data(paste0("Variable '", input$Xvar, "' doesn't exist in ", input$data))
        } else {
            plotlyOutput("plot_agg_distrib", height = "560px")
        }
    })
    output$plot_agg_distrib <- renderPlotly({
        if (input$keep_cond == "Yes") {
            df_agg <- aggData_cond()
            p <- ggplot(df_agg, aes_string(x = input$Xvar, fill = "Condition")) +
                geom_histogram() + ggsci::scale_fill_bmj() + theme_minimal()
        } else {
            df_agg <- aggData()
            p <- ggplot(df_agg, aes_string(x = input$Xvar)) +
                geom_histogram(fill = "#2C7FB8") + theme_minimal()
        }
        ggplotly(p)
    })

    # (3)+(4) EF composites — consolidated (boxplot / radars)
    output$panel_ef <- renderUI({
        df    <- efData()
        comps <- intersect(EF_COMPOSITES, names(df))
        if (nrow(df) == 0 || length(comps) == 0)
            return(no_data("No EF composite data for this dataset / selection"))
        switch(input$ef_view,
               radar_ind = plotlyOutput("plot_ef_radar_ind", height = "620px"),
               radar_grp = plotlyOutput("plot_ef_radar_grp", height = "620px"),
               plotlyOutput("plot_ef_box", height = "560px"))
    })
    output$plot_ef_box       <- renderPlotly({ plot_ef_box(efData()) })
    output$plot_ef_radar_ind <- renderPlotly({ ef_radar_individual(efData()) })
    output$plot_ef_radar_grp <- renderPlotly({ ef_radar_group(efData()) })

    # EF task <-> composite correlations (Correlations tab)
    output$plot_ef_corr <- renderPlotly({
        df <- efData()
        validate(need(nrow(df) > 2, "Need at least 3 participants for correlations"))
        ef_corr_plot(df, k = 3)
    })

    # EFA / CFA of the EF structure -------------------------------------------
    FA_MIN_N <- 10   # soft floor; results below ~50 are unstable

    efaFit <- reactive({
        validate(need(nrow(ef_fa_data(efData())) >= FA_MIN_N,
                      paste0("Need at least ", FA_MIN_N,
                             " participants with EF data for factor analysis.")))
        ef_efa_fit(efData(), nf = 3)
    })
    cfaFit <- reactive({
        validate(need(nrow(ef_fa_data(efData())) >= FA_MIN_N,
                      paste0("Need at least ", FA_MIN_N,
                             " participants with EF data for factor analysis.")))
        ef_cfa_fit(efData())
    })

    output$ef_efa_plot <- renderPlotly({
        fit <- efaFit(); validate(need(!is.null(fit),
            "EFA did not converge for this selection — try selecting more participants."))
        ef_efa_plot(fit)
    })
    output$ef_efa_text <- renderPrint({
        fit <- efaFit(); validate(need(!is.null(fit), "EFA did not converge."))
        ef_efa_print(fit, efData())
    })
    output$ef_cfa_plot <- renderPlotly({
        fit <- cfaFit(); validate(need(!is.null(fit),
            "CFA did not converge for this selection — try selecting more participants."))
        ef_cfa_plot(fit)
    })
    output$ef_cfa_text <- renderPrint({
        fit <- cfaFit(); validate(need(!is.null(fit), "CFA did not converge."))
        ef_cfa_print(fit)
    })

    # Auto-select (and star) the recommended performance metric when the task
    # changes — the metric each task contributes to its EF composite.
    observeEvent(input$data, {
        rec <- recommended_metric[[input$data]]
        ch  <- agg_var_choices
        if (!is.null(rec) && rec %in% ch) {
            names(ch)[ch == rec] <- paste0("★ ", names(ch)[ch == rec], " (recommended)")
            sel <- rec
        } else {
            sel <- "rt_corr_mean"
        }
        updateSelectInput(session, "agg_var", choices = ch, selected = sel)
    }, ignoreInit = FALSE)

    ## ---- Correlations --------------------------------------------------------
    output$panel_corrwithage <- renderUI({
        df <- mergeData_cond()
        v  <- input$corrage_var
        if (nrow(df) == 0) {
            no_data()
        } else if (v %nin% colnames(df)) {
            no_data(paste0("Metric '", v, "' is not available for ", input$data))
        } else if (all(is.na(df[[v]]))) {
            no_data(paste0("Metric '", v, "' has no values for ", input$data))
        } else {
            plotlyOutput("plot_corrwithage", height = "560px")
        }
    })
    output$plot_corrwithage <- renderPlotly({
        plot_task.corrwithage.var(mergeData_cond(), input$corrage_var)
    })

    output$plot_task.corrwithage.gamescore <- renderPlotly({
        tmp <- merge(getDataSet()$gamesScores, getDataSet()$demographics) %>%
            filter(Sid %in% input$ID_select) %>%
            filter(task == tolower(input$data))
        validate(need(nrow(tmp) > 0, "No game-score data for this task / selection"))
        p <- tmp %>%
            ggplot(aes(x = Age, y = GameScore)) +
            geom_point(aes(text = Sid), position = position_jitter(width = .1),
                       alpha = .3, size = 3) +
            geom_smooth(method = "glm") +
            ggpubr::stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = ",")),
                             method = "spearman", p.accuracy = 0.001, r.accuracy = 0.01,
                             digits = 2, na.rm = TRUE, label.x = 14,
                             label.y = .8 * max(tmp$GameScore, na.rm = TRUE),
                             output.type = "text") +
            labs(title = input$data) + theme_classic()
        ggplotly(p)
    })

    output$gamescore_corr <- renderPlotly({
        tmp_gs     <- getDataSet()$gamesScores %>% filter(task == tolower(input$data))
        tmp_merged <- merge(aggData_cond(), tmp_gs)
        validate(need(nrow(tmp_merged) > 0, "No matching game-score data"))
        p <- ggplot(tmp_merged, aes_string(x = "GameScore", y = input$Yvar)) +
            geom_point(alpha = .2, size = 3) + geom_smooth(method = "glm") + theme_classic()
        ggplotly(p)
    })

    output$plot_corr_XY <- renderPlotly({
        validate(need(nrow(aggData_cond()) > 0, "No data to plot"))
        p <- aggData_cond() %>%
            ggplot(aes_string(x = input$Xvar_corr, y = input$Yvar_corr, fill = "Condition")) +
            geom_point(alpha = .2, size = 3) + geom_smooth(method = "glm") + theme_classic()
        ggplotly(p)
    })

    # (5) Download aggregated EF data as CSV
    output$download_ef <- downloadHandler(
        filename = function() paste0("ACE_EF_aggregated_", input$dataset, "_",
                                     Sys.Date(), ".csv"),
        content  = function(file) {
            ef <- getDataSet()$ef_data
            validate(need(!is.null(ef) && nrow(ef) > 0,
                          "No EF data available for this dataset"))
            utils::write.csv(ef, file, row.names = FALSE)
        }
    )
}

shinyApp(ui = ui, server = server)
