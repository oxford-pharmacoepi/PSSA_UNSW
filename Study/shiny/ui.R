pairLabel <- function(index, marker, markerType) {
  suffix <- dplyr::if_else(markerType == "proxy", "Drug proxy", "Condition")
  paste0(index, "  \u2192  ", marker, "  \u00b7  ", suffix)
}

pairChoices <- protocolPairs |>
  dplyr::mutate(label = pairLabel(
    .data$index_cohort_name, .data$marker_cohort_name, .data$marker_type
  )) |>
  dplyr::arrange(.data$marker_type, .data$index_cohort_name, .data$marker_cohort_name) |>
  dplyr::select("label", "pair_id") |>
  tibble::deframe()

analysisChoices <- choices$sequence_ratios_analysis_id
evidenceClassChoices <- c(
  "Positive controls" = "positive",
  "Negative controls" = "negative",
  "New potential signals" = "unknown"
)
sequenceCdmChoices <- sort(unique(data[["sequence_ratios"]]$cdm_name))
sequenceCdmChoices <- sequenceCdmChoices[!is.na(sequenceCdmChoices)]
temporalCdmChoices <- sort(unique(data[["temporal_symmetry"]]$cdm_name))
temporalCdmChoices <- temporalCdmChoices[!is.na(temporalCdmChoices)]
characteristicsSplit <- data[["summarise_characteristics"]] |>
  omopgenerics::splitAll()
characteristicsCdmChoices <- sort(unique(characteristicsSplit$cdm_name))
characteristicsCdmChoices <- characteristicsCdmChoices[!is.na(characteristicsCdmChoices)]
characteristicsCohortChoices <- characteristicsSplit |>
  dplyr::distinct(.data$cohort_name) |>
  dplyr::arrange(.data$cohort_name) |>
  dplyr::mutate(label = protocolDisplayName(.data$cohort_name)) |>
  dplyr::select("label", "cohort_name") |>
  tibble::deframe()
defaultCdm <- if ("CPRD GOLD" %in% sequenceCdmChoices) {
  "CPRD GOLD"
} else {
  sequenceCdmChoices[[1]]
}
comparisonDefaultA <- defaultCdm
comparisonDefaultB <- if ("PBS_OMOP" %in% sequenceCdmChoices) {
  "PBS_OMOP"
} else {
  comparisonAlternatives <- setdiff(sequenceCdmChoices, comparisonDefaultA)
  if (length(comparisonAlternatives) > 0) {
    comparisonAlternatives[[1]]
  } else {
    comparisonDefaultA
  }
}
temporalDefaultPairs <- c(
  protocolPairs$pair_id[match("diagnosis", protocolPairs$marker_type)],
  protocolPairs$pair_id[match("proxy", protocolPairs$marker_type)]
)
temporalDefaultPairs <- temporalDefaultPairs[!is.na(temporalDefaultPairs)]

filterPicker <- function(inputId, label, choices, selected, multiple = TRUE) {
  shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = list(
      `actions-box` = multiple,
      `live-search` = TRUE,
      size = 10,
      `selected-text-format` = "count > 2"
    )
  )
}

downloadMenu <- function(outputId, label) {
  bslib::popover(
    shiny::icon("download"),
    shiny::downloadButton(outputId, label = label),
    title = "Download"
  )
}

ui <- bslib::page_navbar(
  title = shiny::tags$span(
    class = "brand-lockup",
    shiny::tags$img(src = "ohdsi_logo.svg", height = "38px", alt = "OHDSI"),
    shiny::tags$span("PSSA")
  ),
  window_title = "PSSA safety signal report",
  selected = "Evidence",
  header = shiny::includeCSS("www/custom.css"),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0b6e69",
    secondary = "#52606d",
    bg = "#f5f7f9",
    fg = "#172b35",
    `font-size-base` = "0.94rem"
  ),
  bslib::nav_panel(
    title = "Overview",
    value = "Overview",
    icon = shiny::icon("circle-info"),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box(
        title = "Databases",
        value = shiny::textOutput("overview_cdm_count", inline = TRUE),
        showcase = shiny::icon("database"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Protocol pairs",
        value = shiny::textOutput("overview_pair_count", inline = TRUE),
        showcase = shiny::icon("arrow-right-arrow-left"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Drug proxies",
        value = shiny::textOutput("overview_proxy_count", inline = TRUE),
        showcase = shiny::icon("pills"),
        theme = "teal"
      ),
      bslib::value_box(
        title = "Analysis designs",
        value = shiny::textOutput("overview_analysis_count", inline = TRUE),
        showcase = shiny::icon("sliders"),
        theme = "secondary"
      )
    ),
    backgroundCard("background.md")
  ),
  bslib::nav_panel(
    title = "Characteristics",
    value = "Characteristics",
    icon = shiny::icon("users"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        class = "protocol-sidebar",
        shiny::tags$h5("Filter characteristics"),
        filterPicker(
          "characteristics_cdm_name", "Database",
          choices = characteristicsCdmChoices,
          selected = characteristicsCdmChoices[[1]],
          multiple = FALSE
        ),
        filterPicker(
          "characteristics_cohort_name", "Drug cohort",
          choices = characteristicsCohortChoices,
          selected = unname(characteristicsCohortChoices)
        ),
        shiny::tags$p(
          class = "filter-note",
          "Baseline characteristics are available for CPRD GOLD and Australian PBS; PBS statin characteristics were unavailable in the supplied export."
        ),
        width = 330,
        open = "desktop"
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::value_box(
          title = "Database",
          value = shiny::textOutput("characteristics_database", inline = TRUE),
          showcase = shiny::icon("database"), theme = "primary"
        ),
        bslib::value_box(
          title = "Cohorts shown",
          value = shiny::textOutput("characteristics_cohort_count", inline = TRUE),
          showcase = shiny::icon("layer-group"), theme = "teal"
        ),
        bslib::value_box(
          title = "Cohort memberships shown",
          value = shiny::textOutput("characteristics_subject_count", inline = TRUE),
          showcase = shiny::icon("users"), theme = "secondary"
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          shiny::tags$div(
            shiny::tags$strong("Baseline cohort characteristics"),
            shiny::tags$span(
              class = "card-subtitle",
              "Counts, age, sex, observation time, and cohort timing from CohortCharacteristics."
            )
          ),
          downloadMenu("characteristics_download", "Download filtered CSV")
        ),
        gt::gt_output("characteristics_table") |>
          shinycssloaders::withSpinner()
      )
    )
  ),
  bslib::nav_panel(
    title = "Evidence",
    value = "Evidence",
    icon = shiny::icon("chart-line"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        class = "protocol-sidebar",
        shiny::tags$h5("Filter evidence"),
        filterPicker(
          "sequence_cdm_name", "Database",
          choices = sequenceCdmChoices,
          selected = defaultCdm,
          multiple = FALSE
        ),
        filterPicker(
          "sequence_marker_type", "Marker type",
          choices = c("Condition" = "diagnosis", "Drug proxy" = "proxy"),
          selected = c("diagnosis", "proxy")
        ),
        filterPicker(
          "sequence_evidence_class", "Evidence class",
          choices = evidenceClassChoices,
          selected = unname(evidenceClassChoices)
        ),
        filterPicker(
          "sequence_pair_id", "Protocol pair",
          choices = pairChoices,
          selected = unname(pairChoices)
        ),
        filterPicker(
          "sequence_analysis_id", "Analysis design",
          choices = analysisChoices,
          selected = unname(analysisChoices)
        ),
        shiny::radioButtons(
          "sequence_estimate_type", "Estimate",
          choices = c("Adjusted" = "adjusted", "Crude" = "crude"),
          selected = "adjusted",
          inline = TRUE
        ),
        shiny::tags$p(
          class = "filter-note",
          "Filters update the table and plots automatically."
        ),
        width = 330,
        open = "desktop"
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::value_box(
          title = "Estimates shown",
          value = shiny::textOutput("sequence_estimate_count", inline = TRUE),
          showcase = shiny::icon("table-list"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Validated",
          value = shiny::textOutput("sequence_validated_count", inline = TRUE),
          showcase = shiny::icon("circle-check"),
          theme = "success"
        ),
        bslib::value_box(
          title = "CI includes 1",
          value = shiny::textOutput("sequence_inconclusive_count", inline = TRUE),
          showcase = shiny::icon("circle-half-stroke"),
          theme = "warning"
        )
      ),
      bslib::navset_card_tab(
        id = "sequence_view",
        bslib::nav_panel(
          title = "Evidence table",
          icon = shiny::icon("table"),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Protocol evidence"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Observed direction and validation use the complete 95% CI."
                )
              ),
              downloadMenu("sequence_table_download", "Download filtered CSV")
            ),
            DT::DTOutput("sequence_protocol_table") |>
              shinycssloaders::withSpinner()
          )
        ),
        bslib::nav_panel(
          title = "Design performance",
          icon = shiny::icon("gauge-high"),
          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            bslib::value_box(
              title = "Sensitivity",
              value = shiny::textOutput("performance_primary_sensitivity", inline = TRUE),
              showcase = shiny::icon("bullseye"), theme = "primary"
            ),
            bslib::value_box(
              title = "Specificity",
              value = shiny::textOutput("performance_primary_specificity", inline = TRUE),
              showcase = shiny::icon("shield-halved"), theme = "success"
            ),
            bslib::value_box(
              title = "PPV",
              value = shiny::textOutput("performance_primary_ppv", inline = TRUE),
              showcase = shiny::icon("circle-check"), theme = "info"
            ),
            bslib::value_box(
              title = "NPV",
              value = shiny::textOutput("performance_primary_npv", inline = TRUE),
              showcase = shiny::icon("circle-minus"), theme = "warning"
            )
          ),
          shiny::tags$p(
            class = "filter-note",
            "Primary-design summaries use benchmark controls selected in the sidebar. PPV and NPV use conclusive above-1 and below-1 calls; inconclusive intervals are reported separately."
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Performance across analysis designs"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Compare windows, blackout periods, and prior-observation requirements."
                )
              ),
              downloadMenu("performance_download", "Download performance CSV")
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::tags$div(
                class = "performance-panel",
                shiny::tags$h6("Observation windows"),
                plotly::plotlyOutput("performance_windows_plot", height = "520px") |>
                  shinycssloaders::withSpinner()
              ),
              shiny::tags$div(
                class = "performance-panel",
                shiny::tags$h6("Blackout periods"),
                plotly::plotlyOutput("performance_blackouts_plot", height = "520px") |>
                  shinycssloaders::withSpinner()
              )
            ),
            DT::DTOutput("performance_table") |>
              shinycssloaders::withSpinner()
          )
        ),
        bslib::nav_panel(
          title = "Databases",
          icon = shiny::icon("code-compare"),
          shiny::tags$div(
            class = "plot-controls comparison-controls",
            shiny::selectInput(
              "comparison_cdm_a", "Database A",
              choices = sequenceCdmChoices,
              selected = comparisonDefaultA
            ),
            shiny::selectInput(
              "comparison_cdm_b", "Database B",
              choices = sequenceCdmChoices,
              selected = comparisonDefaultB
            )
          ),
          shiny::tags$div(
            class = "comparison-metrics",
            shiny::tags$div(
              class = "comparison-metric",
              shiny::tags$p("Matched estimates"),
              shiny::textOutput("comparison_matched_count")
            ),
            shiny::tags$div(
              class = "comparison-metric comparison-metric-success",
              shiny::tags$p("Direction agreement"),
              shiny::textOutput("comparison_direction_agreement")
            ),
            shiny::tags$div(
              class = "comparison-metric comparison-metric-warning",
              shiny::tags$p("Median fold difference"),
              shiny::textOutput("comparison_median_fold")
            )
          ),
          bslib::card(
            full_screen = TRUE,
            fill = FALSE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Matched sequence ratios"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Points on the diagonal agree exactly; dashed lines mark a ratio of 1."
                )
              )
            ),
            plotly::plotlyOutput("sequence_database_plot", height = "640px") |>
              shinycssloaders::withSpinner()
          ),
          bslib::card(
            full_screen = TRUE,
            fill = FALSE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Agreement details"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Filter Agreement to Discordant to review differences first."
                )
              ),
              downloadMenu("comparison_table_download", "Download comparison CSV")
            ),
            DT::DTOutput("sequence_database_table") |>
              shinycssloaders::withSpinner()
          )
        ),
        bslib::nav_panel(
          title = "Primary forest",
          icon = shiny::icon("grip-lines"),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Paired condition and proxy forest"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Condition and corresponding drug-proxy estimates share a row."
                )
              )
            ),
            plotly::plotlyOutput("sequence_primary_plot", height = "720px") |>
              shinycssloaders::withSpinner()
          )
        ),
        bslib::nav_panel(
          title = "Sensitivity map",
          icon = shiny::icon("border-all"),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Design sensitivity"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Normal colour range is 0\u20134; orange and magenta diamonds flag high and extreme outliers."
                )
              )
            ),
            plotly::plotlyOutput("sequence_sensitivity_plot", height = "820px") |>
              shinycssloaders::withSpinner()
          )
        ),
        bslib::nav_panel(
          title = "Custom plot",
          icon = shiny::icon("chart-line"),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Configurable sequence-ratio view"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Facet or colour by analysis to separate repeated pair estimates."
                )
              )
            ),
            shiny::tags$div(
              class = "plot-controls plot-controls-grid",
              shiny::selectInput(
                "sequence_plot_facet", "Facet by",
                choices = c(
                  "None" = "none", "Analysis design" = "analysis_short",
                  "Marker type" = "marker_label",
                  "Expected association" = "expected_association"
                ),
                selected = "none"
              ),
              shiny::selectInput(
                "sequence_plot_colour", "Colour by",
                choices = c(
                  "Analysis design" = "analysis_short",
                  "Marker type" = "marker_label",
                  "Expected association" = "expected_association",
                  "Observed direction" = "observed_direction"
                ),
                selected = "analysis_short"
              ),
              shiny::checkboxInput(
                "sequence_plot_show_ci", "Show 95% confidence intervals",
                value = FALSE
              )
            ),
            plotly::plotlyOutput("sequence_package_plot", height = "720px") |>
              shinycssloaders::withSpinner()
          )
        )
      )
    )
  ),
  bslib::nav_panel(
    title = "Snapshot",
    value = "Snapshot",
    icon = shiny::icon("database"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::tags$div(
          shiny::tags$strong("CDM snapshot"),
          shiny::tags$span(
            class = "card-subtitle",
            "Database size, observation period, domains, and source metadata."
          )
        ),
        downloadMenu("snapshot_table_download", "Download snapshot table")
      ),
      gt::gt_output("snapshot_table") |>
        shinycssloaders::withSpinner()
    )
  ),
  bslib::nav_panel(
    title = "Timing",
    value = "Timing",
    icon = shiny::icon("clock-rotate-left"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        class = "protocol-sidebar",
        shiny::tags$h5("Filter timing profiles"),
        filterPicker(
          "temporal_cdm_name", "Database",
          choices = temporalCdmChoices,
          selected = if (defaultCdm %in% temporalCdmChoices) {
            defaultCdm
          } else {
            temporalCdmChoices[[1]]
          },
          multiple = FALSE
        ),
        filterPicker(
          "temporal_pair_id", "Protocol pair",
          choices = pairChoices,
          selected = temporalDefaultPairs
        ),
        filterPicker(
          "temporal_analysis_id", "Analysis design",
          choices = choices$temporal_symmetry_analysis_id,
          selected = unname(choices$temporal_symmetry_analysis_id)
        ),
        shiny::tags$p(
          class = "filter-note",
          "Two representative pairs are selected initially to keep the timing plot readable."
        ),
        width = 330,
        open = "desktop"
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "Temporal profile",
          icon = shiny::icon("chart-area"),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::tags$div(
                shiny::tags$strong("Marker timing around index initiation"),
                shiny::tags$span(
                  class = "card-subtitle",
                  "Negative days precede the index medicine; positive days follow it."
                )
              )
            ),
            plotly::plotlyOutput("temporal_profile_plot", height = "760px") |>
              shinycssloaders::withSpinner()
          )
        ),
        bslib::nav_panel(
          title = "Timing table",
          icon = shiny::icon("table"),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::tags$strong("Temporal counts"),
              downloadMenu("temporal_table_download", "Download filtered CSV")
            ),
            DT::DTOutput("temporal_protocol_table") |>
              shinycssloaders::withSpinner()
          )
        )
      )
    )
  ),
  bslib::nav_spacer(),
  bslib::nav_item(
    bslib::popover(
      shiny::icon("download"),
      shiny::downloadButton("download_raw", "Download all results"),
      title = "Raw data"
    )
  ),
  bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
)
