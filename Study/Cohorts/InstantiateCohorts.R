source(file.path(studyPath, "Codelists.R"))

codelistInputs <- readStudyCodelists(pssaSettings$codelistFile)
allCodelists <- codelistInputs$codelists

if (length(allCodelists) == 0 || any(lengths(allCodelists) == 0)) {
  stop("Every prespecified drug and diagnosis codelist must contain at least one concept ID.")
}

cdm <- CDMConnector::generateConceptCohortSet(
  cdm = cdm,
  conceptSet = allCodelists,
  name = cohortTableName,
  limit = "all",
  requiredObservation = requiredObservation,
  end = "observation_period_end_date",
  overwrite = TRUE
)

cohortCounts <- CDMConnector::cohortCount(cdm[[cohortTableName]]) |>
  dplyr::collect()

exportStudySummary(
  x = cohortCounts,
  resultType = "cohort_counts",
  fileName = "cohort_counts_{cdm_name}_{date}.csv",
  group = setdiff(names(cohortCounts), c("number_records", "number_subjects")),
  estimates = intersect(c("number_records", "number_subjects"), names(cohortCounts))
)

omopgenerics::exportConceptSetExpression(
  x = allCodelists,
  path = codelistExportFolder,
  type = "json"
)
