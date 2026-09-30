if (!requireNamespace("CohortCharacteristics", quietly = TRUE)) {
  stop(
    "runBaselineCharacteristics is TRUE but CohortCharacteristics is not installed."
  )
}

if (!exists("cdm") || !exists("db")) {
  stop("The CDM reference and open database connection must already exist.")
}

if (!"pssa_drug_cohorts" %in% names(cdm)) {
  stop("Expected pssa_drug_cohorts before running baseline characteristics.")
}

ageGroups <- list(
  c(0, 49),
  c(50, 59),
  c(60, 69),
  c(70, 79),
  c(80, 89),
  c(90, 150)
)

sourceCohort <- cdm$pssa_drug_cohorts
observationPeriod <- cdm$observation_period |>
  dplyr::select(
    subject_id = .data$person_id,
    .data$observation_period_start_date,
    .data$observation_period_end_date
  )

cohortIds <- sourceCohort |>
  dplyr::distinct(.data$cohort_definition_id) |>
  dplyr::arrange(.data$cohort_definition_id) |>
  dplyr::collect() |>
  dplyr::pull(.data$cohort_definition_id)

if (length(cohortIds) == 0) {
  stop("No cohort IDs were found in pssa_drug_cohorts.")
}

omopgenerics::logMessage(
  paste0("Preparing baseline characteristics for ", length(cohortIds), " drug cohorts")
)

firstCohortTables <- vector("list", length(cohortIds))
for (i in seq_along(cohortIds)) {
  currentCohortId <- cohortIds[[i]]
  intermediateTableName <- paste0("pssa_first_", currentCohortId)

  omopgenerics::logMessage(
    paste0("Processing cohort ", currentCohortId, " (", i, " of ", length(cohortIds), ")")
  )

  firstCohortTables[[i]] <- sourceCohort |>
    dplyr::filter(.data$cohort_definition_id == .env$currentCohortId) |>
    dplyr::inner_join(observationPeriod, by = "subject_id") |>
    dplyr::filter(
      .data$observation_period_start_date <= .data$cohort_start_date - 365L,
      .data$observation_period_end_date >= .data$cohort_start_date
    ) |>
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$cohort_start_date,
      .data$cohort_end_date
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::filter(
      .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
    ) |>
    dplyr::filter(
      .data$cohort_end_date == min(.data$cohort_end_date, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::compute(
      name = intermediateTableName,
      temporary = FALSE,
      overwrite = TRUE
    )
}

omopgenerics::logMessage("Combining separately processed baseline cohorts")
combinedCohorts <- firstCohortTables[[1]]
if (length(firstCohortTables) > 1) {
  for (i in 2:length(firstCohortTables)) {
    combinedCohorts <- dplyr::union_all(combinedCohorts, firstCohortTables[[i]])
  }
}

combinedCohorts |>
  dplyr::compute(
    name = "pssa_drug_cohorts_first",
    temporary = FALSE,
    overwrite = TRUE
  )

cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  cdmName = dbName,
  achillesSchema = achillesSchema,
  cohortTables = c("pssa_drug_cohorts", "pssa_drug_cohorts_first")
)

cohortCounts <- cdm$pssa_drug_cohorts_first |>
  dplyr::group_by(.data$cohort_definition_id) |>
  dplyr::summarise(
    number_records = dplyr::n(),
    number_subjects = dplyr::n_distinct(.data$subject_id),
    .groups = "drop"
  ) |>
  dplyr::collect()
print(cohortCounts)

omopgenerics::logMessage("Running baseline characteristics for drug cohorts")
baselineCharacteristics <- CohortCharacteristics::summariseCharacteristics(
  cohort = cdm$pssa_drug_cohorts_first,
  cohortId = NULL,
  counts = TRUE,
  demographics = TRUE,
  ageGroup = ageGroups
)

omopgenerics::logMessage("Exporting baseline characteristics")
omopgenerics::exportSummarisedResult(
  baselineCharacteristics,
  fileName = "baseline_characteristics_{cdm_name}_{date}.csv",
  path = summarisedResultsFolder,
  minCellCount = minCellCount,
  logFile = NULL
)

omopgenerics::logMessage("Baseline characteristics completed successfully")
