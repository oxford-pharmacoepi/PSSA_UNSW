source(file.path(studyPath, "Codelists.R"))

getCohortId <- function(cohortTable) {
  cohortId <- omopgenerics::settings(cohortTable) |>
    dplyr::pull("cohort_definition_id")
  if (length(cohortId) != 1) {
    stop(
      "Expected exactly one cohort_definition_id in ",
      omopgenerics::tableName(cohortTable),
      ". Found: ",
      paste(cohortId, collapse = ", ")
    )
  }
  cohortId
}

omopgenerics::logMessage("Instantiating study cohorts")
codelistInputs <- readStudyCodelists(path = file.path(studyPath, "inst", "mock_codelists.csv"))
analysisPairs <- codelistInputs$analysis_pairs
cohortPairs <- vector("list", nrow(analysisPairs))

for (pairIndex in seq_len(nrow(analysisPairs))) {
  pair <- analysisPairs[pairIndex, ]
  indexTableName <- paste0("pssa_drug_cohort_", pairIndex)
  markerTableName <- paste0("pssa_diagnosis_cohort_", pairIndex)

  omopgenerics::logMessage(paste("Instantiating cohorts for", pair$pair_id))

  cdm[[indexTableName]] <- CohortConstructor::conceptCohort(
    cdm = cdm,
    conceptSet = codelistInputs$codelists[pair$drug_cohort_name],
    name = indexTableName
  )

  cdm[[markerTableName]] <- CohortConstructor::conceptCohort(
    cdm = cdm,
    conceptSet = codelistInputs$codelists[pair$diagnosis_cohort_name],
    name = markerTableName
  )

  cohortPairs[[pairIndex]] <- tibble::tibble(
    pair_id = pair$pair_id,
    index_table = indexTableName,
    index_id = getCohortId(cdm[[indexTableName]]),
    marker_table = markerTableName,
    marker_id = getCohortId(cdm[[markerTableName]])
  )
}

pssaCohortPairs <- dplyr::bind_rows(cohortPairs)
