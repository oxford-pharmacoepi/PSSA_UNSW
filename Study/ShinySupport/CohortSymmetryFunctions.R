addProtocolPairMetadata <- function(data) {
  if (!exists("protocolPairs", inherits = TRUE)) return(data)

  pairs <- get("protocolPairs", inherits = TRUE)
  if (all(c("index_cohort_name", "marker_cohort_name") %in% names(data))) {
    data <- dplyr::left_join(
      data, pairs,
      by = c("index_cohort_name", "marker_cohort_name")
    )
  } else if (all(c("index_name", "marker_name") %in% names(data))) {
    data <- dplyr::left_join(
      data, pairs,
      by = c(
        "index_name" = "index_cohort_name",
        "marker_name" = "marker_cohort_name"
      )
    )
  }

  if ("include_in_benchmark" %in% names(data) &&
      !"benchmark_status" %in% names(data)) {
    data <- data |>
      dplyr::mutate(
        benchmark_status = dplyr::case_when(
          is.na(.data$include_in_benchmark) ~ "Unknown",
          .data$include_in_benchmark ~ "Yes",
          TRUE ~ "No"
        )
      )
  }

  data
}

classifySequenceRatioEvidence <- function(data) {
  required <- c(
    "point_estimate", "lower_CI", "upper_CI", "expected_direction"
  )
  for (column in setdiff(required, names(data))) {
    data[[column]] <- NA_character_
  }

  data |>
    dplyr::mutate(
      point_estimate = suppressWarnings(as.numeric(.data$point_estimate)),
      lower_CI = suppressWarnings(as.numeric(.data$lower_CI)),
      upper_CI = suppressWarnings(as.numeric(.data$upper_CI)),
      invalid_interval = !is.na(.data$point_estimate) &
        !is.na(.data$lower_CI) & !is.na(.data$upper_CI) &
        (.data$lower_CI > .data$point_estimate |
          .data$point_estimate > .data$upper_CI),
      observed_direction = dplyr::case_when(
        is.na(.data$point_estimate) | is.na(.data$lower_CI) |
          is.na(.data$upper_CI) ~ "Unavailable",
        .data$invalid_interval ~ "Invalid interval",
        .data$lower_CI > 1 ~ "Above 1",
        .data$upper_CI < 1 ~ "Below 1",
        TRUE ~ "Includes 1"
      ),
      validated_status = dplyr::case_when(
        is.na(.data$expected_direction) |
          .data$expected_direction == "unknown" ~ "Not assessed",
        .data$observed_direction == "Unavailable" ~ "Unavailable",
        .data$observed_direction == "Invalid interval" ~ "Invalid interval",
        .data$expected_direction == "above_1" &
          .data$observed_direction == "Above 1" ~ "Validated",
        .data$expected_direction == "not_above_1" &
          .data$observed_direction == "Below 1" ~ "Validated",
        .data$observed_direction == "Includes 1" ~ "Inconclusive (CI includes 1)",
        TRUE ~ "Not validated"
      )
    ) |>
    dplyr::select(-"invalid_interval")
}

cohortSymmetryResultType <- function(result) {
  resultTypes <- unique(omopgenerics::settings(result)$result_type)
  if (any(resultTypes == "temporal_symmetry")) {
    "temporal"
  } else {
    "sequence"
  }
}

cohortSymmetryTable <- function(result,
                                header = character(),
                                group = character(),
                                hide = character()) {
  resultType <- cohortSymmetryResultType(result)
  tableData <- result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    addProtocolPairMetadata() |>
    omopgenerics::pivotEstimates()

  if (identical(resultType, "temporal")) {
    tableData <- tableData |>
      dplyr::mutate(
        pair = paste(.data$index_name, .data$marker_name, sep = " -> ")
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "cdm_name", "pair", "analysis_label", "analysis_id", "marker_type",
          "variable_level", "count", "expected_direction", "benchmark_status"
        ))
      ) |>
      dplyr::rename(
        analysis = "analysis_label",
        marker = "marker_type",
        day = "variable_level",
        temporal_count = "count",
        protocol_expectation = "expected_direction",
        benchmark_pair = "benchmark_status"
      ) |>
      dplyr::arrange(
        .data$pair, .data$analysis,
        suppressWarnings(as.numeric(.data$day))
      )
  } else {
    tableData <- tableData |>
      classifySequenceRatioEvidence() |>
      dplyr::mutate(
        pair = paste(
          .data$index_cohort_name, .data$marker_cohort_name, sep = " -> "
        ),
        estimate = dplyr::case_when(
          !is.na(.data$point_estimate) & !is.na(.data$lower_CI) &
            !is.na(.data$upper_CI) ~ paste0(
              .data$point_estimate, " [", .data$lower_CI, ", ",
              .data$upper_CI, "]"
            ),
          !is.na(.data$point_estimate) ~ as.character(.data$point_estimate),
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "cdm_name", "pair", "analysis_label", "analysis_id", "marker_type",
          "variable_name", "estimate", "expected_direction",
          "observed_direction", "validated_status", "benchmark_status"
        ))
      ) |>
      dplyr::rename(
        analysis = "analysis_label",
        marker = "marker_type",
        estimate_type = "variable_name",
        protocol_expectation = "expected_direction",
        validation = "validated_status",
        benchmark_pair = "benchmark_status"
      ) |>
      dplyr::arrange(.data$pair, .data$analysis, .data$estimate_type)
  }

  if ("analysis" %in% names(tableData)) {
    tableData <- tableData |>
      dplyr::mutate(
        analysis = dplyr::coalesce(.data$analysis, .data$analysis_id)
      ) |>
      dplyr::select(-dplyr::any_of("analysis_id"))
  }

  gt::gt(tableData) |>
    gt::sub_missing(columns = dplyr::everything(), missing_text = "\u2014")
}

cohortSymmetryPlot <- function(result,
                               x = "index_cohort_name",
                               facet = "cdm_name",
                               colour = "variable_name") {
  if (cohortSymmetryResultType(result) == "temporal") {
    return(
      CohortSymmetry::plotTemporalSymmetry(
        result = result,
        scales = "free"
      )
    )
  }

  availableFacets <- c(
    "cdm_name",
    "cohort_date_range",
    "combination_window",
    "confidence_interval",
    "days_prior_observation",
    "index_marker_gap",
    "moving_average_restriction",
    "washout_window"
  )
  facet <- facet[facet %in% availableFacets]
  if (length(facet) == 0) {
    facet <- NULL
  }

  CohortSymmetry::plotSequenceRatios(
    result = result,
    onlyASR = TRUE,
    facet = facet
  )
}

cohortSymmetryBenchmarkData <- function(result) {
  result |>
    omopgenerics::tidy() |>
    addProtocolPairMetadata() |>
    dplyr::filter(
      !is.na(.data$pair_id),
      .data$variable_level == "sequence_ratio",
      .data$variable_name == "adjusted"
    ) |>
    classifySequenceRatioEvidence() |>
    dplyr::mutate(
      window_days = suppressWarnings(as.integer(.data$window_days)),
      blackout_days = suppressWarnings(as.integer(.data$blackout_days)),
      protocol_prior_observation = suppressWarnings(
        as.integer(.data$protocol_prior_observation)
      ),
      is_primary = tolower(.data$is_primary) == "true",
      include_in_benchmark = tolower(.data$include_in_benchmark) == "true",
      pair = paste(
        .data$index_cohort_name, .data$marker_cohort_name, sep = " \u2192 "
      )
    )
}

cohortSymmetryBenchmarkTable <- function(result) {
  cohortSymmetryBenchmarkData(result) |>
    dplyr::select(
      "pair",
      "marker_type",
      "tier",
      "expected_association",
      "expected_direction",
      "include_in_benchmark",
      "analysis_id",
      "analysis_label",
      "changed_parameter",
      "window_days",
      "blackout_days",
      "protocol_prior_observation",
      "point_estimate",
      "lower_CI",
      "upper_CI",
      "observed_direction",
      "validated_status"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$expected_association),
      .data$pair,
      dplyr::desc(.data$analysis_id)
    ) |>
    gt::gt() |>
    gt::cols_label(
      pair = "Index \u2192 marker",
      marker_type = "Marker",
      tier = "Tier",
      expected_association = "Expected",
      expected_direction = "Expected direction",
      include_in_benchmark = "Benchmark control",
      analysis_id = "Analysis",
      analysis_label = "Design",
      changed_parameter = "Changed",
      window_days = "Window",
      blackout_days = "Blackout",
      protocol_prior_observation = "Prior history",
      point_estimate = "aSR",
      lower_CI = "Lower CI",
      upper_CI = "Upper CI",
      observed_direction = "Observed direction (95% CI)",
      validated_status = "Validation"
    ) |>
    gt::fmt_number(
      columns = c("point_estimate", "lower_CI", "upper_CI"),
      decimals = 2
    ) |>
    gt::sub_missing(
      columns = dplyr::everything(),
      missing_text = "\u2014"
    ) |>
    gt::tab_header(
      title = "Protocol control results",
      subtitle = "Adjusted sequence ratios across the selected analysis settings"
    )
}

cohortSymmetryBenchmarkPlot <- function(result) {
  plotData <- cohortSymmetryBenchmarkData(result) |>
    dplyr::filter(.data$is_primary, !is.na(.data$point_estimate))

  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = stats::reorder(.data$pair, .data$point_estimate),
      y = .data$point_estimate,
      ymin = .data$lower_CI,
      ymax = .data$upper_CI,
      colour = .data$expected_association,
      shape = .data$marker_type
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.35)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x = NULL,
      y = "Adjusted sequence ratio (log scale)",
      colour = "Expected",
      shape = "Marker type",
      title = "Primary analysis: protocol controls and signals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

cohortSymmetrySensitivityPlot <- function(result) {
  plotData <- cohortSymmetryBenchmarkData(result) |>
    dplyr::filter(!is.na(.data$point_estimate)) |>
    dplyr::mutate(
      analysis_label = factor(
        .data$analysis_label,
        levels = c(
          "Primary: 365d window / 7d blackout / 365d history",
          "Window: 30 days",
          "Window: 60 days",
          "Window: 90 days",
          "Window: 180 days",
          "Blackout: 0 days",
          "Blackout: 1 day",
          "Blackout: 14 days",
          "Prior observation: 180 days"
        )
      )
    )

  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$analysis_label,
      y = .data$point_estimate,
      ymin = .data$lower_CI,
      ymax = .data$upper_CI,
      colour = .data$changed_parameter
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_pointrange() +
    ggplot2::facet_wrap(ggplot2::vars(.data$pair), scales = "free_y") +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x = NULL,
      y = "Adjusted sequence ratio (log scale)",
      colour = "Parameter changed",
      title = "One-at-a-time sensitivity analyses"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )
}
