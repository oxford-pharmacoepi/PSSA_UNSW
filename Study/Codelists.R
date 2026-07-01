readStudyCodelists <- function(path) {
  codelistCsv <- readr::read_csv(
    path,
    col_types = readr::cols(
      pair_id = readr::col_character(),
      cohort_name = readr::col_character(),
      codelist_type = readr::col_character(),
      concept_id = readr::col_integer(),
      include_descendants = readr::col_logical(),
      concept_name = readr::col_character()
    )
  )

  requiredColumns <- c(
    "pair_id",
    "cohort_name",
    "codelist_type",
    "concept_id",
    "include_descendants"
  )
  missingColumns <- setdiff(requiredColumns, names(codelistCsv))
  if (length(missingColumns) > 0) {
    stop("Missing codelist columns: ", paste(missingColumns, collapse = ", "))
  }

  invalidTypes <- setdiff(unique(codelistCsv$codelist_type), c("drug", "diagnosis"))
  if (length(invalidTypes) > 0) {
    stop("codelist_type must be drug or diagnosis. Found: ", paste(invalidTypes, collapse = ", "))
  }

  conceptSets <- split(codelistCsv, codelistCsv$cohort_name) |>
    lapply(function(x) {
      x <- dplyr::distinct(
        x,
        .data$concept_id,
        .data$include_descendants
      )
      tibble::tibble(
        concept_id = x$concept_id,
        descendants = x$include_descendants,
        excluded = FALSE
      )
    })

  codelists <- conceptSets |>
    CodelistGenerator::newConceptSetExpression()

  codelistSpecification <- codelistCsv |>
    dplyr::distinct(
      .data$pair_id,
      .data$cohort_name,
      .data$codelist_type
    )

  analysisPairs <- codelistSpecification |>
    tidyr::pivot_wider(
      id_cols = "pair_id",
      names_from = "codelist_type",
      values_from = "cohort_name"
    ) |>
    dplyr::rename(
      drug_cohort_name = "drug",
      diagnosis_cohort_name = "diagnosis"
    )

  if (any(is.na(analysisPairs$drug_cohort_name)) || any(is.na(analysisPairs$diagnosis_cohort_name))) {
    stop("Each pair_id must have one drug cohort and one diagnosis cohort.")
  }

  list(
    codelists = codelists,
    codelist_specification = codelistSpecification,
    analysis_pairs = analysisPairs
  )
}
