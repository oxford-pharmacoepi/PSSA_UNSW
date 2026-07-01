source(file.path(studyPath, "Codelists.R"))

if (!requireNamespace("CohortSymmetry", quietly = TRUE)) {
  stop(
    "The CohortSymmetry package is required to run the PSSA. ",
    "Install it in the study environment before running RunStudy.R."
  )
}

codelistInputs <- readStudyCodelists(pssaSettings$codelistFile)

analysisSettings <- readr::read_csv(
  pssaSettings$analysisSettingsFile,
  col_types = readr::cols(
    analysis_id = readr::col_character(),
    prior_window = readr::col_integer(),
    post_window = readr::col_integer(),
    washout_window = readr::col_integer(),
    interval = readr::col_integer(),
    min_cell_count = readr::col_integer()
  )
)

cohortSettings <- omopgenerics::settings(cdm[[cohortTableName]]) |>
  dplyr::collect() |>
  dplyr::left_join(
    codelistInputs$codelist_specification,
    by = "cohort_name"
  )

cohortIdLookup <- cohortSettings |>
  dplyr::distinct(
    .data$cohort_name,
    .data$cohort_definition_id
  )

analysisPairs <- codelistInputs$analysis_pairs |>
  dplyr::left_join(
    cohortIdLookup |>
      dplyr::select(
        drug_cohort_name = "cohort_name",
        drug_cohort_id = "cohort_definition_id"
      ),
    by = "drug_cohort_name"
  ) |>
  dplyr::left_join(
    cohortIdLookup |>
      dplyr::select(
        diagnosis_cohort_name = "cohort_name",
        diagnosis_cohort_id = "cohort_definition_id"
      ),
    by = "diagnosis_cohort_name"
  )

if (any(is.na(analysisPairs$drug_cohort_id)) || any(is.na(analysisPairs$diagnosis_cohort_id))) {
  stop("Could not map all prespecified codelist pairs to instantiated cohort IDs.")
}

sequenceRatioResults <- list()
adjustedSequenceRatioResults <- list()

for (pairIndex in seq_len(nrow(analysisPairs))) {
  for (settingIndex in seq_len(nrow(analysisSettings))) {
    pair <- analysisPairs[pairIndex, ]
    setting <- analysisSettings[settingIndex, ]
    resultName <- paste(pair$pair_id, setting$analysis_id, sep = "_")

    omopgenerics::logMessage(
      paste("Running sequence ratio", resultName),
      logFile = logFile
    )
    sequenceRatioResults[[resultName]] <- CohortSymmetry::summariseSequenceRatios(
      cdm = cdm,
      cohortTable = cohortTableName,
      targetCohortId = pair$drug_cohort_id,
      outcomeCohortId = pair$diagnosis_cohort_id,
      priorWindow = setting$prior_window,
      postWindow = setting$post_window,
      washoutWindow = setting$washout_window,
      interval = setting$interval,
      minCellCount = setting$min_cell_count
    )

    omopgenerics::logMessage(
      paste("Running adjusted sequence ratio", resultName),
      logFile = logFile
    )
    adjustedSequenceRatioResults[[resultName]] <- CohortSymmetry::summariseAdjustedSequenceRatios(
      cdm = cdm,
      cohortTable = cohortTableName,
      targetCohortId = pair$drug_cohort_id,
      outcomeCohortId = pair$diagnosis_cohort_id,
      priorWindow = setting$prior_window,
      postWindow = setting$post_window,
      washoutWindow = setting$washout_window,
      interval = setting$interval,
      minCellCount = setting$min_cell_count
    )
  }
}

sequenceRatioResult <- do.call(omopgenerics::bind, sequenceRatioResults)
adjustedSequenceRatioResult <- do.call(omopgenerics::bind, adjustedSequenceRatioResults)

omopgenerics::exportSummarisedResult(
  sequenceRatioResult,
  fileName = "sequence_ratio_{cdm_name}_{date}.csv",
  path = resultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)

omopgenerics::exportSummarisedResult(
  adjustedSequenceRatioResult,
  fileName = "adjusted_sequence_ratio_{cdm_name}_{date}.csv",
  path = resultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)

pssaResult <- omopgenerics::importSummarisedResult(
  path = resultsFolder,
  recursive = FALSE
)

omopgenerics::exportSummarisedResult(
  pssaResult,
  fileName = "pssa_results_{cdm_name}_{date}.csv",
  path = resultsFolder,
  minCellCount = minCellCount,
  logFile = logFile
)

OmopViewer::exportStaticApp(
  result = pssaResult,
  directory = file.path(resultsFolder, "shiny"),
  title = "PSSA UNSW",
  report = TRUE,
  open = FALSE
)
