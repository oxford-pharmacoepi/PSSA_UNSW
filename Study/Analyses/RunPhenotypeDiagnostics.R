if (!requireNamespace("PhenotypeR", quietly = TRUE)) {
  stop(
    "runPhenotypeDiagnostics is TRUE but the PhenotypeR package is not installed."
  )
}

phenotypeResults <- list()

for (cohortTableName in pssaCohortTables) {
  omopgenerics::logMessage(
    paste("Running PhenotypeR codelist diagnostics for", cohortTableName)
  )
  phenotypeResults[[paste0(cohortTableName, "_codelist")]] <-
    PhenotypeR::codelistDiagnostics(
      cohort = cdm[[cohortTableName]],
      achillesCodeUse = !is.null(achillesSchema),
      orphanCodeUse = !is.null(achillesSchema),
      cohortCodeUse = TRUE,
      drugDiagnostics = identical(cohortTableName, "pssa_drug_cohorts"),
      drugDiagnosticsSample = phenotypeSample
    )

  omopgenerics::logMessage(
    paste("Running PhenotypeR cohort diagnostics for", cohortTableName)
  )
  phenotypeResults[[paste0(cohortTableName, "_cohort")]] <-
    PhenotypeR::cohortDiagnostics(
      cohort = cdm[[cohortTableName]],
      cohortCount = TRUE,
      cohortCharacteristics = TRUE,
      largeScaleCharacteristics = runLargeScaleCharacteristics,
      compareCohorts = TRUE,
      cohortSurvival = FALSE,
      cohortSample = phenotypeSample,
      matchedSample = 0
    )
}

phenotypeResult <- do.call(
  omopgenerics::bind,
  phenotypeResults
)

omopgenerics::exportSummarisedResult(
  phenotypeResult,
  fileName = "phenotype_diagnostics_{cdm_name}_{date}.csv",
  path = summarisedResultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)
