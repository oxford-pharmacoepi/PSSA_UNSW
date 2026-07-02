instantiatePairCohorts <- function(cdm, codelists, pair, indexTableName, markerTableName) {
  omopgenerics::logMessage(paste("Instantiating cohorts for", pair$pair_id))

  cdm[[indexTableName]] <- CohortConstructor::conceptCohort(
    cdm = cdm,
    conceptSet = codelists[pair$drug_cohort_name],
    name = indexTableName
  )

  cdm[[markerTableName]] <- CohortConstructor::conceptCohort(
    cdm = cdm,
    conceptSet = codelists[pair$diagnosis_cohort_name],
    name = markerTableName
  )

  cdm
}
