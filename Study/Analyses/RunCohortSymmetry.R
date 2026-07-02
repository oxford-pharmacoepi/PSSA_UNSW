source(file.path(studyPath, "Codelists.R"))

codelistInputs <- readStudyCodelists(path = here::here("inst", "mock_codelists.csv"))

if (!requireNamespace("CohortSymmetry", quietly = TRUE)) {
  stop(
    "The CohortSymmetry package is required to run the PSSA. ",
    "Install it in the study environment before running RunStudy.R."
  )
}

analysisSettings <- readr::read_csv(
  here::here("inst", "analysis_settings.csv"),
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

cohortSettings <- omopgenerics::settings(cdm[["pssa_study_cohorts"]]) |>
  dplyr::select(cohort_definition_id, cohort_name)

analysisPairs <- codelistInputs$analysis_pairs

sequenceRatioResults <- list()
adjustedSequenceRatioResults <- list()

for (pairIndex in seq_len(nrow(analysisPairs))) {
  for (settingIndex in seq_len(nrow(analysisSettings))) {
    pair <- analysisPairs[pairIndex, ]
    setting <- analysisSettings[settingIndex, ]
    resultName <- paste(pair$pair_id, setting$analysis_id, sep = "_")
    
    cdm$sequencecohort <- CohortSymmetry::generateSequenceCohortSet(
      cdm = cdm,
      indexTable = cohortTableName,
      indexId = cohortSettings |>
        dplyr::filter(cohort_name == pair |>
                        dplyr::pull(drug_cohort_name)
        ) |>
        dplyr::pull(cohort_definition_id),
      markerTable = cohortTableName,
      markerId = cohortSettings |>
        dplyr::filter(cohort_name == pair |>
                        dplyr::pull(diagnosis_cohort_name)
        ) |>
        dplyr::pull(cohort_definition_id),
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
