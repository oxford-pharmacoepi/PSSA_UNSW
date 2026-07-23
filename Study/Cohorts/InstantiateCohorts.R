source(file.path(studyPath, "Codelists.R"))

getCohortId <- function(cohortTable, cohortName) {
  cohortId <- omopgenerics::settings(cohortTable) |>
    dplyr::filter(.data$cohort_name == .env$cohortName) |>
    dplyr::pull("cohort_definition_id")

  if (length(cohortId) != 1) {
    stop(
      "Expected exactly one cohort_definition_id for ",
      cohortName,
      " in ",
      omopgenerics::tableName(cohortTable),
      ". Found: ",
      paste(cohortId, collapse = ", ")
    )
  }
  cohortId
}

omopgenerics::logMessage("Reading protocol codelists and analysis pairs")
codelistInputs <- readStudyCodelists(
  cohortFolder = file.path(studyPath, "Cohorts"),
  pairPath = file.path(studyPath, "inst", "analysis_pairs.csv")
)

drugCohortNames <- codelistInputs$cohort_specification |>
  dplyr::filter(.data$cohort_domain == "drug") |>
  dplyr::pull("cohort_name")
conditionCohortNames <- codelistInputs$cohort_specification |>
  dplyr::filter(.data$cohort_domain == "condition") |>
  dplyr::pull("cohort_name")

omopgenerics::logMessage("Resolving drug concept expressions")
drugCodelists <- codelistInputs$concept_sets[drugCohortNames] |>
  CodelistGenerator::asCodelist(cdm = cdm)

omopgenerics::logMessage("Instantiating drug cohorts with DrugUtilisation")
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "pssa_drug_cohorts",
  conceptSet = drugCodelists,
  gapEra = 1
)

omopgenerics::logMessage("Instantiating condition cohorts with CohortConstructor")
cdm$pssa_condition_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = codelistInputs$concept_sets[conditionCohortNames],
  name = "pssa_condition_cohorts",
  exit = "event_start_date",
  overlap = "keep"
)

analysisPairs <- codelistInputs$analysis_pairs |>
  dplyr::mutate(
    index_table = "pssa_drug_cohorts",
    marker_table = dplyr::if_else(
      .data$marker_domain == "drug",
      "pssa_drug_cohorts",
      "pssa_condition_cohorts"
    ),
    index_id = purrr::map_int(
      .data$index_cohort_name,
      \(cohortName) getCohortId(cdm$pssa_drug_cohorts, cohortName)
    ),
    marker_id = purrr::map2_int(
      .data$marker_table,
      .data$marker_cohort_name,
      \(tableName, cohortName) getCohortId(cdm[[tableName]], cohortName)
    )
  )

pssaCohortPairs <- analysisPairs
pssaCohortTables <- c("pssa_drug_cohorts", "pssa_condition_cohorts")
