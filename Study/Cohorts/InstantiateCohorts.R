source(file.path(studyPath, "Codelists.R"))

omopgenerics::logMessage("Instantiating study cohorts")
codelistInputs <- readStudyCodelists(path = file.path(studyPath, "inst", "mock_codelists.csv"))

drugCohorts <- codelistInputs$codelist_specification |>
  dplyr::filter(.data$codelist_type == "drug") |>
  dplyr::distinct(.data$cohort_name) |>
  dplyr::pull("cohort_name")

diagnosisCohorts <- codelistInputs$codelist_specification |>
  dplyr::filter(.data$codelist_type == "diagnosis") |>
  dplyr::distinct(.data$cohort_name) |>
  dplyr::pull("cohort_name")

cdm[[indexTableName]] <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = codelistInputs$codelists[drugCohorts],
  name = indexTableName
)

cdm[[markerTableName]] <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = codelistInputs$codelists[diagnosisCohorts],
  name = markerTableName
)
