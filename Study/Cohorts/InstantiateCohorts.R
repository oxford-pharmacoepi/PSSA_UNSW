logMessage("Importing simple concept set expressions")
cohort_concepts <- omopgenerics::importConceptSetExpression(path = here::here("Cohorts"), type = "csv")
cdm$pssa_study_cohorts <- CohortConstructor::conceptCohort(
  cdm,
  cohort_concepts,
  "pssa_study_cohorts"
)
