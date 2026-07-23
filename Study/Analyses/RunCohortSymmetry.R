if (!requireNamespace("CohortSymmetry", quietly = TRUE)) {
  stop(
    "The CohortSymmetry package is required to run the PSSA. ",
    "Install it in the study environment before running RunStudy.R."
  )
}

analysisSettings <- readr::read_csv(
  file.path(studyPath, "inst", "analysis_settings.csv"),
  col_types = readr::cols(
    analysis_id = readr::col_character(),
    analysis_label = readr::col_character(),
    changed_parameter = readr::col_character(),
    window_days = readr::col_integer(),
    blackout_days = readr::col_integer(),
    days_prior_observation = readr::col_integer(),
    washout_window = readr::col_integer(),
    index_marker_gap = readr::col_double(),
    moving_average_restriction = readr::col_integer(),
    timescale = readr::col_character(),
    is_primary = readr::col_logical()
  )
)

if (!exists("pssaCohortPairs")) {
  stop("Study cohorts must be instantiated before running CohortSymmetry.")
}
if (sum(analysisSettings$is_primary) != 1) {
  stop("analysis_settings.csv must contain exactly one primary analysis.")
}
if (any(analysisSettings$blackout_days >= analysisSettings$window_days)) {
  stop("Every blackout_days value must be smaller than window_days.")
}

primarySetting <- analysisSettings |>
  dplyr::filter(.data$is_primary)
if (primarySetting$changed_parameter != "primary") {
  stop("The primary analysis must use changed_parameter = primary.")
}
designFields <- c(
  "window_days",
  "blackout_days",
  "days_prior_observation",
  "washout_window",
  "index_marker_gap",
  "moving_average_restriction",
  "timescale"
)
expectedChangedField <- c(
  window = "window_days",
  blackout = "blackout_days",
  prior_observation = "days_prior_observation"
)

for (settingIndex in which(!analysisSettings$is_primary)) {
  setting <- analysisSettings[settingIndex, ]
  changedFields <- designFields[
    vapply(
      designFields,
      \(field) !identical(setting[[field]], primarySetting[[field]]),
      logical(1)
    )
  ]
  expectedField <- unname(expectedChangedField[setting$changed_parameter])

  if (
    length(expectedField) != 1 ||
      is.na(expectedField) ||
      length(changedFields) != 1 ||
      changedFields != expectedField
  ) {
    stop(
      setting$analysis_id,
      " must change only its declared parameter from the primary design."
    )
  }
}

addProtocolSettings <- function(result, pair, setting) {
  resultSettings <- omopgenerics::settings(result) |>
    dplyr::mutate(
      pair_id = pair$pair_id,
      index_label = pair$index_label,
      marker_label = pair$marker_label,
      marker_type = pair$marker_type,
      tier = as.character(pair$tier),
      expected_association = pair$expected_association,
      expected_direction = pair$expected_direction,
      include_in_benchmark = as.character(pair$include_in_benchmark),
      analysis_id = setting$analysis_id,
      analysis_label = setting$analysis_label,
      changed_parameter = setting$changed_parameter,
      window_days = as.character(setting$window_days),
      blackout_days = as.character(setting$blackout_days),
      protocol_prior_observation = as.character(setting$days_prior_observation),
      incident_washout_days = as.character(setting$washout_window),
      is_primary = as.character(setting$is_primary),
      protocol_note = pair$protocol_note
    )

  omopgenerics::newSummarisedResult(
    result,
    settings = resultSettings
  )
}

sequenceRatioResults <- list()
temporalSymmetryResults <- list()
resultIndex <- 0L

for (pairIndex in seq_len(nrow(pssaCohortPairs))) {
  pair <- pssaCohortPairs[pairIndex, ]

  for (settingIndex in seq_len(nrow(analysisSettings))) {
    setting <- analysisSettings[settingIndex, ]
    resultIndex <- resultIndex + 1L
    resultName <- paste(pair$pair_id, setting$analysis_id, sep = "_")
    sequenceCohortName <- paste0("pssa_sequence_", resultIndex)

    omopgenerics::logMessage(
      paste(
        "Generating sequence cohort",
        resultName,
        paste0("(", resultIndex, "/", nrow(pssaCohortPairs) * nrow(analysisSettings), ")")
      )
    )

    cdm <- CohortSymmetry::generateSequenceCohortSet(
      cdm = cdm,
      indexTable = pair$index_table,
      indexId = pair$index_id,
      markerTable = pair$marker_table,
      markerId = pair$marker_id,
      name = sequenceCohortName,
      washoutWindow = setting$washout_window,
      daysPriorObservation = setting$days_prior_observation,
      indexMarkerGap = setting$index_marker_gap,
      combinationWindow = c(setting$blackout_days, setting$window_days),
      movingAverageRestriction = setting$moving_average_restriction
    )

    sequenceRatioResults[[resultName]] <- CohortSymmetry::summariseSequenceRatios(
      cohort = cdm[[sequenceCohortName]]
    ) |>
      addProtocolSettings(pair = pair, setting = setting)

    temporalSymmetryResults[[resultName]] <- CohortSymmetry::summariseTemporalSymmetry(
      cohort = cdm[[sequenceCohortName]],
      timescale = setting$timescale
    ) |>
      addProtocolSettings(pair = pair, setting = setting)

    cdm <- CDMConnector::dropTable(
      cdm = cdm,
      name = sequenceCohortName
    )
  }
}

sequenceRatioResult <- do.call(
  omopgenerics::bind,
  sequenceRatioResults
)
temporalSymmetryResult <- do.call(
  omopgenerics::bind,
  temporalSymmetryResults
)

omopgenerics::exportSummarisedResult(
  sequenceRatioResult,
  fileName = "sequence_ratios_{cdm_name}_{date}.csv",
  path = summarisedResultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)

omopgenerics::exportSummarisedResult(
  temporalSymmetryResult,
  fileName = "temporal_symmetry_{cdm_name}_{date}.csv",
  path = summarisedResultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)
