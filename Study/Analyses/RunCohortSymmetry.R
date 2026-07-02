source(file.path(studyPath, "Codelists.R"))

codelistInputs <- readStudyCodelists(path = file.path(studyPath, "inst", "mock_codelists.csv"))

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

analysisPairs <- codelistInputs$analysis_pairs

sequenceRatioResults <- list()
adjustedSequenceRatioResults <- list()

indexCohortSettings <- omopgenerics::settings(cdm[[indexTableName]]) |>
  dplyr::select("cohort_definition_id", "cohort_name")

markerCohortSettings <- omopgenerics::settings(cdm[[markerTableName]]) |>
  dplyr::select("cohort_definition_id", "cohort_name")

getCohortId <- function(cohortSettings, cohortName) {
  cohortId <- cohortSettings |>
    dplyr::filter(.data$cohort_name == .env$cohortName) |>
    dplyr::pull("cohort_definition_id")
  if (length(cohortId) != 1) {
    stop(
      "Expected exactly one cohort_definition_id for cohort_name ",
      cohortName,
      ". Found: ",
      paste(cohortId, collapse = ", ")
    )
  }
  cohortId
}

for (pairIndex in seq_len(nrow(analysisPairs))) {
  for (settingIndex in seq_len(nrow(analysisSettings))) {
    pair <- analysisPairs[pairIndex, ]
    setting <- analysisSettings[settingIndex, ]
    resultName <- paste(pair$pair_id, setting$analysis_id, sep = "_")
    
    cdm$sequencecohort <- CohortSymmetry::generateSequenceCohortSet(
      cdm = cdm,
      indexTable = indexTableName,
      indexId = getCohortId(indexCohortSettings, pair$drug_cohort_name),
      markerTable = markerTableName,
      markerId = getCohortId(markerCohortSettings, pair$diagnosis_cohort_name),
      name = "sequencecohort",
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
      name = "sequencecohort"
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
