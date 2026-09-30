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

ageGroups <- list(
  c(0, 49),
  c(50, 59),
  c(60, 69),
  c(70, 79),
  c(80, 89),
  c(90, 150)
)

cdm[["pssa_drug_cohorts_first"]] <- cdm$pssa_drug_cohorts |>
  CohortConstructor::requirePriorObservation(365) |>
  CohortConstructor::requireIsFirstEntry() |>
  dplyr::compute(temporary = FALSE, overwrite = TRUE, name = "pssa_drug_cohorts_first")

omopgenerics::logMessage("Running baseline characteristics for index drug cohorts")
baselineCharacteristics <- CohortCharacteristics::summariseCharacteristics(
  cohort = cdm$pssa_drug_cohorts_first,
  cohortId = NULL,
  counts = TRUE,
  demographics = TRUE,
  ageGroup = ageGroups
)

omopgenerics::exportSummarisedResult(
  baselineCharacteristics,
  fileName = "baseline_characteristics_{cdm_name}_{date}.csv",
  path = summarisedResultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)
