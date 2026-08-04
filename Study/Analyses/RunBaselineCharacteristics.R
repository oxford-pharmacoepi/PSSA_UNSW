if (!requireNamespace("CohortCharacteristics", quietly = TRUE)) {
  stop(
    "runBaselineCharacteristics is TRUE but the CohortCharacteristics package is not installed."
  )
}

if (!exists("pssaCohortPairs") || !exists("cdm")) {
  stop("Study cohorts must be instantiated before running baseline characteristics.")
}

if (!"pssa_drug_cohorts" %in% names(cdm)) {
  stop("Expected pssa_drug_cohorts to exist before running baseline characteristics.")
}

indexCohortNames <- unique(pssaCohortPairs$index_cohort_name)
indexCohortIds <- purrr::map_int(
  indexCohortNames,
  \(cohortName) getCohortId(cdm$pssa_drug_cohorts, cohortName)
)

markerCohortNames <- unique(pssaCohortPairs$marker_cohort_name)
markerCohortIds <- purrr::map_int(
  markerCohortNames,
  \(cohortName) getCohortId(cdm$pssa_drug_cohorts, cohortName)
)
names(markerCohortIds) <- markerCohortNames

ageGroups <- list(
  c(0, 49),
  c(50, 59),
  c(60, 69),
  c(70, 79),
  c(80, 89),
  c(90, 150)
)

priorDrugFlags <- lapply(
  names(markerCohortIds),
  \(cohortName) {
    list(
      targetCohortTable = "pssa_drug_cohorts",
      targetCohortId = markerCohortIds[[cohortName]],
      window = list(c(-180, -1)),
      nameStyle = paste0("prior_", cohortName)
    )
  }
)
names(priorDrugFlags) <- paste0("Prior ", names(markerCohortIds))

omopgenerics::logMessage("Running baseline characteristics for index drug cohorts")
baselineCharacteristics <- CohortCharacteristics::summariseCharacteristics(
  cohort = cdm$pssa_drug_cohorts,
  cohortId = indexCohortIds,
  counts = TRUE,
  demographics = TRUE,
  ageGroup = ageGroups,
  cohortIntersectFlag = priorDrugFlags
)

omopgenerics::exportSummarisedResult(
  baselineCharacteristics,
  fileName = "baseline_characteristics_{cdm_name}_{date}.csv",
  path = summarisedResultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)
