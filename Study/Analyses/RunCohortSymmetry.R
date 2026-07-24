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

addProtocolSettings <- function(result, setting) {
  resultSettings <- omopgenerics::settings(result) |>
    dplyr::mutate(
      analysis_id = setting$analysis_id,
      analysis_label = setting$analysis_label,
      changed_parameter = setting$changed_parameter,
      window_days = as.character(setting$window_days),
      blackout_days = as.character(setting$blackout_days),
      protocol_prior_observation = as.character(setting$days_prior_observation),
      incident_washout_days = as.character(setting$washout_window),
      is_primary = as.character(setting$is_primary)
    )

  omopgenerics::newSummarisedResult(
    result,
    settings = resultSettings
  )
}

sequenceRatioResults <- list()
temporalSymmetryResults <- list()

# Generating broad cohort sets avoids the cohort-definition-ID mismatch seen
# when creating a temporary set for each protocol pair. The Shiny report then
# retains only the prespecified pairs in analysis_pairs.csv.
cohortSetTypes <- list(
  diagnosis = list(
    index_table = "pssa_drug_cohorts",
    marker_table = "pssa_condition_cohorts"
  ),
  proxy = list(
    index_table = "pssa_drug_cohorts",
    marker_table = "pssa_drug_cohorts"
  )
)

for (settingIndex in seq_len(nrow(analysisSettings))) {
  setting <- analysisSettings[settingIndex, ]
  for (setType in names(cohortSetTypes)) {
    cohortSet <- cohortSetTypes[[setType]]
    resultId <- paste(setting$analysis_id, setType, sep = "_")
    sequenceCohortName <- paste0("pssa_sequence_", resultId)
    omopgenerics::logMessage(
      paste(
        "Generating sequence cohort",
        resultId
      )
    )

    cdm <- CohortSymmetry::generateSequenceCohortSet(
      cdm = cdm,
      indexTable = cohortSet$index_table,
      indexId = NULL,
      markerTable = cohortSet$marker_table,
      markerId = NULL,
      name = sequenceCohortName,
      washoutWindow = setting$washout_window,
      daysPriorObservation = setting$days_prior_observation,
      indexMarkerGap = setting$index_marker_gap,
      combinationWindow = c(setting$blackout_days, setting$window_days),
      movingAverageRestriction = setting$moving_average_restriction
    )

    sequenceRatioResults[[resultId]] <- CohortSymmetry::summariseSequenceRatios(
      cohort = cdm[[sequenceCohortName]]
    ) |>
      addProtocolSettings(setting = setting)

    temporalSymmetryResults[[resultId]] <- CohortSymmetry::summariseTemporalSymmetry(
      cohort = cdm[[sequenceCohortName]],
      timescale = setting$timescale
    ) |>
      addProtocolSettings(setting = setting)

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
