server <- function(input, output, session) {
  output$overview_cdm_count <- shiny::renderText({
    length(unique(stats::na.omit(data[["sequence_ratios"]]$cdm_name)))
  })
  output$overview_pair_count <- shiny::renderText(nrow(protocolPairs))
  output$overview_proxy_count <- shiny::renderText(
    sum(protocolPairs$marker_type == "proxy")
  )
  output$overview_analysis_count <- shiny::renderText(
    length(unique(unname(choices$sequence_ratios_analysis_id)))
  )

  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      data |>
        omopgenerics::bind() |>
        omopgenerics::exportSummarisedResult(fileName = file)
    }
  )

  getSnapshotTable <- shiny::reactive({
    data[["summarise_omop_snapshot"]] |>
      OmopSketch::tableOmopSnapshot()
  })
  output$snapshot_table <- gt::render_gt(getSnapshotTable())
  output$snapshot_table_download <- shiny::downloadHandler(
    filename = "cdm_snapshot.html",
    content = function(file) gt::gtsave(getSnapshotTable(), file)
  )

  characteristicsResult <- shiny::reactive({
    shiny::req(input$characteristics_cdm_name, input$characteristics_cohort_name)
    data[["summarise_characteristics"]] |>
      dplyr::filter(
        .data$cdm_name == input$characteristics_cdm_name
      ) |>
      omopgenerics::filterGroup(
        .data$cohort_name %in% input$characteristics_cohort_name
      ) |>
      dplyr::mutate(
        group_level = protocolDisplayName(.data$group_level)
      )
  })

  characteristicsData <- shiny::reactive({
    characteristicsResult() |>
      omopgenerics::splitAll() |>
      dplyr::mutate(
        Cohort = protocolDisplayName(.data$cohort_name),
        Characteristic = dplyr::if_else(
          is.na(.data$variable_level) | .data$variable_level == "",
          .data$variable_name,
          paste0(.data$variable_name, ": ", .data$variable_level)
        ),
        Estimate = dplyr::case_when(
          .data$estimate_name == "percentage" ~ paste0(.data$estimate_value, "%"),
          TRUE ~ .data$estimate_value
        )
      ) |>
      dplyr::select("Cohort", "Characteristic", "estimate_name", "Estimate") |>
      tidyr::pivot_wider(names_from = "estimate_name", values_from = "Estimate") |>
      dplyr::rename_with(
        ~ dplyr::recode(.x, count="Count", percentage="Percentage", mean="Mean",
                        sd="SD", q25="Q1", median="Median", q75="Q3")
      )
  })

  characteristicsGt <- shiny::reactive({
    result <- characteristicsResult()
    CohortCharacteristics::tableCharacteristics(
      result,
      type = "gt",
      header = "cohort_name",
      hide = c(
        "cdm_name",
        omopgenerics::additionalColumns(result),
        omopgenerics::settingsColumns(result)
      )
    )
  })

  output$characteristics_cohort_count <- shiny::renderText({
    format(length(unique(characteristicsData()$Cohort)), big.mark = ",")
  })
  output$characteristics_database <- shiny::renderText({
    shiny::req(input$characteristics_cdm_name)
    dplyr::recode(input$characteristics_cdm_name, PBS_OMOP = "Australian PBS")
  })
  output$characteristics_subject_count <- shiny::renderText({
    x <- characteristicsData() |>
      dplyr::filter(.data$Characteristic == "Number subjects") |>
      dplyr::pull(dplyr::any_of("Count"))
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || all(is.na(x))) return("Not available")
    format(sum(x, na.rm = TRUE), big.mark = ",", scientific = FALSE)
  })
  output$characteristics_table <- gt::render_gt(characteristicsGt())
  output$characteristics_download <- shiny::downloadHandler(
    filename = "pssa_baseline_characteristics.csv",
    content = function(file) readr::write_csv(characteristicsData(), file)
  )

  selectedSequencePairs <- shiny::reactive({
    shiny::req(
      input$sequence_pair_id, input$sequence_marker_type,
      input$sequence_evidence_class
    )
    protocolPairs |>
      dplyr::filter(
        .data$pair_id %in% input$sequence_pair_id,
        .data$marker_type %in% input$sequence_marker_type,
        .data$expected_association %in% input$sequence_evidence_class
      )
  })

  selectedPerformancePairs <- shiny::reactive({
    shiny::req(input$sequence_pair_id, input$sequence_marker_type)
    protocolPairs |>
      dplyr::filter(
        .data$pair_id %in% input$sequence_pair_id,
        .data$marker_type %in% input$sequence_marker_type,
        .data$include_in_benchmark,
        .data$expected_association %in% c("positive", "negative")
      )
  })

  getSequenceRatiosData <- shiny::reactive({
    shiny::req(
      input$sequence_cdm_name, input$sequence_analysis_id,
      input$sequence_estimate_type
    )
    filterProtocolPairIds(
      result = data[["sequence_ratios"]] |>
        dplyr::filter(
          .data$cdm_name == input$sequence_cdm_name,
          .data$variable_name == input$sequence_estimate_type,
          .data$estimate_name %in% c("point_estimate", "lower_CI", "upper_CI")
        ) |>
        omopgenerics::filterSettings(
          .data$analysis_id %in% input$sequence_analysis_id
        ),
      pairs = selectedSequencePairs(),
      indexColumn = "index_cohort_name",
      markerColumn = "marker_cohort_name"
    )
  })

  sequenceEvidence <- shiny::reactive({
    sequenceRatioEvidenceData(getSequenceRatiosData())
  })

  performanceEvidence <- shiny::reactive({
    shiny::req(
      input$sequence_cdm_name, input$sequence_analysis_id,
      input$sequence_estimate_type
    )
    filterProtocolPairIds(
      result = data[["sequence_ratios"]] |>
        dplyr::filter(
          .data$cdm_name == input$sequence_cdm_name,
          .data$variable_name == input$sequence_estimate_type,
          .data$estimate_name %in% c("point_estimate", "lower_CI", "upper_CI")
        ) |>
        omopgenerics::filterSettings(
          .data$analysis_id %in% input$sequence_analysis_id
        ),
      pairs = selectedPerformancePairs(),
      indexColumn = "index_cohort_name",
      markerColumn = "marker_cohort_name"
    ) |>
      sequenceRatioEvidenceData()
  })

  performanceMetrics <- shiny::reactive({
    sequencePerformanceMetrics(performanceEvidence())
  })

  performanceEvidenceAllDatabases <- shiny::reactive({
    shiny::req(input$sequence_analysis_id, input$sequence_estimate_type)
    filterProtocolPairIds(
      result = data[["sequence_ratios"]] |>
        dplyr::filter(
          .data$variable_name == input$sequence_estimate_type,
          .data$estimate_name %in% c("point_estimate", "lower_CI", "upper_CI")
        ) |>
        omopgenerics::filterSettings(
          .data$analysis_id %in% input$sequence_analysis_id
        ),
      pairs = selectedPerformancePairs(),
      indexColumn = "index_cohort_name",
      markerColumn = "marker_cohort_name"
    ) |>
      sequenceRatioEvidenceData()
  })

  performanceMetricsAllDatabases <- shiny::reactive({
    sequencePerformanceMetrics(performanceEvidenceAllDatabases())
  })

  primaryForestEvidence <- shiny::reactive({
    shiny::req(input$sequence_cdm_name, input$sequence_estimate_type)
    filterProtocolPairIds(
      result = data[["sequence_ratios"]] |>
        dplyr::filter(
          .data$cdm_name == input$sequence_cdm_name,
          .data$variable_name == input$sequence_estimate_type,
          .data$estimate_name %in% c("point_estimate", "lower_CI", "upper_CI")
        ) |>
        omopgenerics::filterSettings(
          .data$analysis_id == "primary"
        ),
      pairs = selectedSequencePairs(),
      indexColumn = "index_cohort_name",
      markerColumn = "marker_cohort_name"
    ) |>
      sequenceRatioEvidenceData()
  })

  output$sequence_estimate_count <- shiny::renderText(
    format(nrow(sequenceEvidence()), big.mark = ",")
  )
  output$sequence_validated_count <- shiny::renderText(
    sum(sequenceEvidence()$validated_status == "Validated", na.rm = TRUE)
  )
  output$sequence_inconclusive_count <- shiny::renderText(
    sum(sequenceEvidence()$observed_direction == "Includes 1", na.rm = TRUE)
  )

  output$sequence_protocol_table <- DT::renderDT({
    sequenceEvidence() |>
      sequenceRatioEvidenceTable()
  })

  output$sequence_table_download <- shiny::downloadHandler(
    filename = "pssa_sequence_evidence.csv",
    content = function(file) {
      sequenceEvidence() |>
        sequenceRatioEvidenceExport() |>
        readr::write_csv(file)
    }
  )

  primaryMetric <- shiny::reactive({
    performanceMetrics() |>
      dplyr::filter(.data$analysis_id == "primary") |>
      dplyr::summarise(
        sensitivity = weightedMetric(.data$sensitivity, .data$n_positive),
        specificity = weightedMetric(.data$specificity, .data$n_negative),
        false_positive_rate = weightedMetric(.data$false_positive_rate, .data$n_negative),
        false_negative_rate = weightedMetric(.data$false_negative_rate, .data$n_positive),
        ppv = ifelse(
          sum(.data$true_positive + .data$false_positive, na.rm = TRUE) > 0,
          sum(.data$true_positive, na.rm = TRUE) /
            sum(.data$true_positive + .data$false_positive, na.rm = TRUE),
          NA_real_
        ),
        npv = ifelse(
          sum(.data$true_negative + .data$positive_below_1, na.rm = TRUE) > 0,
          sum(.data$true_negative, na.rm = TRUE) /
            sum(.data$true_negative + .data$positive_below_1, na.rm = TRUE),
          NA_real_
        )
      )
  })
  formatPrimaryMetric <- function(column) {
    value <- primaryMetric()[[column]]
    if (length(value) == 0 || is.na(value)) return("Not available")
    scales::percent(value, accuracy = 0.1)
  }
  output$performance_primary_sensitivity <- shiny::renderText(
    formatPrimaryMetric("sensitivity")
  )
  output$performance_primary_specificity <- shiny::renderText(
    formatPrimaryMetric("specificity")
  )
  output$performance_primary_ppv <- shiny::renderText(
    formatPrimaryMetric("ppv")
  )
  output$performance_primary_npv <- shiny::renderText(
    formatPrimaryMetric("npv")
  )
  output$performance_windows_plot <- plotly::renderPlotly({
    performanceMetricsAllDatabases() |>
      cohortSymmetryPerformancePlot("windows") |>
      plotly::ggplotly(tooltip = c("text"))
  })
  output$performance_blackouts_plot <- plotly::renderPlotly({
    performanceMetricsAllDatabases() |>
      cohortSymmetryPerformancePlot("blackouts") |>
      plotly::ggplotly(tooltip = c("text"))
  })
  output$performance_table <- DT::renderDT({
    performanceMetrics() |>
      sequencePerformanceTable()
  })
  output$performance_download <- shiny::downloadHandler(
    filename = "pssa_design_performance.csv",
    content = function(file) readr::write_csv(performanceMetrics(), file)
  )

  comparisonEvidence <- shiny::reactive({
    shiny::req(
      input$comparison_cdm_a, input$comparison_cdm_b,
      input$sequence_analysis_id, input$sequence_estimate_type
    )
    shiny::validate(shiny::need(
      input$comparison_cdm_a != input$comparison_cdm_b,
      "Choose two different databases."
    ))

    allDatabaseEvidence <- filterProtocolPairIds(
      result = data[["sequence_ratios"]] |>
        dplyr::filter(
          .data$cdm_name %in% c(input$comparison_cdm_a, input$comparison_cdm_b),
          .data$variable_name == input$sequence_estimate_type,
          .data$estimate_name %in% c("point_estimate", "lower_CI", "upper_CI")
        ) |>
        omopgenerics::filterSettings(
          .data$analysis_id %in% input$sequence_analysis_id
        ),
      pairs = selectedSequencePairs(),
      indexColumn = "index_cohort_name",
      markerColumn = "marker_cohort_name"
    ) |>
      sequenceRatioEvidenceData()

    sequenceRatioDatabaseComparisonData(
      allDatabaseEvidence,
      databaseA = input$comparison_cdm_a,
      databaseB = input$comparison_cdm_b
    )
  })

  output$comparison_matched_count <- shiny::renderText(
    format(nrow(comparisonEvidence()), big.mark = ",")
  )
  output$comparison_direction_agreement <- shiny::renderText({
    comparison <- comparisonEvidence()
    comparable <- !is.na(comparison$direction_agrees)
    if (!any(comparable)) return("Not available")
    scales::percent(mean(comparison$direction_agrees[comparable]), accuracy = 1)
  })
  output$comparison_median_fold <- shiny::renderText({
    fold <- comparisonEvidence()$fold_difference
    fold <- fold[is.finite(fold)]
    if (length(fold) == 0) return("Not available")
    paste0(format(round(stats::median(fold), 2), nsmall = 2), "×")
  })
  output$sequence_database_plot <- plotly::renderPlotly({
    comparisonEvidence() |>
      cohortSymmetryDatabaseComparisonPlot(
        databaseA = input$comparison_cdm_a,
        databaseB = input$comparison_cdm_b
      ) |>
      plotly::ggplotly(tooltip = c("text"))
  })
  output$sequence_database_table <- DT::renderDT({
    comparisonEvidence() |>
      sequenceRatioDatabaseComparisonTable(
        databaseA = input$comparison_cdm_a,
        databaseB = input$comparison_cdm_b
      )
  })
  output$comparison_table_download <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "pssa_database_comparison_",
        gsub("[^A-Za-z0-9]+", "_", input$comparison_cdm_a), "_vs_",
        gsub("[^A-Za-z0-9]+", "_", input$comparison_cdm_b), ".csv"
      )
    },
    content = function(file) readr::write_csv(comparisonEvidence(), file)
  )

  output$sequence_primary_plot <- plotly::renderPlotly({
    primaryForestEvidence() |>
      cohortSymmetryPrimaryForestPlot(protocolPairs = protocolPairs) |>
      plotly::ggplotly(tooltip = c("text"))
  })

  output$sequence_sensitivity_plot <- plotly::renderPlotly({
    sequenceEvidence() |>
      cohortSymmetrySensitivityMap() |>
      plotly::ggplotly(tooltip = c("text"))
  })

  output$sequence_package_plot <- plotly::renderPlotly({
    shiny::req(input$sequence_plot_colour)
    sequenceEvidence() |>
      cohortSymmetryConfigurablePlot(
        facetBy = input$sequence_plot_facet,
        colourBy = input$sequence_plot_colour,
        showCI = isTRUE(input$sequence_plot_show_ci)
      ) |>
      plotly::ggplotly(tooltip = c("text"))
  })

  selectedTemporalPairs <- shiny::reactive({
    shiny::req(input$temporal_pair_id)
    protocolPairs |>
      dplyr::filter(.data$pair_id %in% input$temporal_pair_id)
  })

  getTemporalSymmetryData <- shiny::reactive({
    shiny::req(input$temporal_cdm_name, input$temporal_analysis_id)
    filterProtocolPairIds(
      result = data[["temporal_symmetry"]] |>
        dplyr::filter(
          .data$cdm_name == input$temporal_cdm_name,
          .data$variable_name == "temporal_symmetry",
          .data$estimate_name == "count"
        ) |>
        omopgenerics::filterSettings(
          .data$analysis_id %in% input$temporal_analysis_id
        ),
      pairs = selectedTemporalPairs(),
      indexColumn = "index_name",
      markerColumn = "marker_name"
    )
  })

  temporalEvidence <- shiny::reactive({
    temporalSymmetryEvidenceData(getTemporalSymmetryData())
  })

  output$temporal_profile_plot <- plotly::renderPlotly({
    temporalEvidence() |>
      cohortSymmetryTemporalProfilePlot() |>
      plotly::ggplotly(tooltip = c("text"))
  })

  output$temporal_protocol_table <- DT::renderDT({
    temporalEvidence() |>
      temporalSymmetryEvidenceTable()
  })

  output$temporal_table_download <- shiny::downloadHandler(
    filename = "pssa_temporal_counts.csv",
    content = function(file) readr::write_csv(temporalEvidence(), file)
  )
}
