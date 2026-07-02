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
    prior_window = readr::col_integer(),
    post_window = readr::col_integer(),
    washout_window = readr::col_integer(),
    interval = readr::col_integer(),
    days_prior_observation = readr::col_integer(),
    moving_avg_restriction = readr::col_integer()
  )
)

if (!exists("pssaCohortPairs")) {
  stop("Study cohorts must be instantiated before running CohortSymmetry.")
}

sequenceRatioResults <- list()
adjustedSequenceRatioResults <- list()

for (pairIndex in seq_len(nrow(pssaCohortPairs))) {
  pair <- pssaCohortPairs[pairIndex, ]
  for (settingIndex in seq_len(nrow(analysisSettings))) {
    setting <- analysisSettings[settingIndex, ]
    resultName <- paste(pair$pair_id, setting$analysis_id, sep = "_")
    sequenceCohortName <- paste0("sequencecohort_", pairIndex, "_", settingIndex)
    
    cdm <- CohortSymmetry::generateSequenceCohortSet(
      cdm = cdm,
      indexTable = pair$index_table,
      indexId = pair$index_id,
      markerTable = pair$marker_table,
      markerId = pair$marker_id,
      name = sequenceCohortName,
      washoutWindow = setting$washout_window,
      daysPriorObservation = setting$days_prior_observation,
      indexMarkerGap = setting$interval,
      combinationWindow = c(setting$prior_window, setting$post_window),
      movingAverageRestriction = setting$moving_avg_restriction
    )

    omopgenerics::logMessage(
      paste("Running sequence ratio", resultName)
    )
    sequenceRatioResults[[resultName]] <- CohortSymmetry::summariseSequenceRatios(
      cdm = cdm,
      name = sequenceCohortName
    )

    # adjusted too?
  }
}

sequenceRatioResult <- do.call(omopgenerics::bind, sequenceRatioResults)

omopgenerics::exportSummarisedResult(
  sequenceRatioResult,
  fileName = "sequence_ratio_{cdm_name}_{date}.csv",
  path = resultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)
