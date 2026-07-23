readStudyCodelists <- function(cohortFolder, pairPath) {
  cohortFiles <- list.files(
    cohortFolder,
    pattern = "[.]csv$",
    full.names = TRUE
  )
  if (length(cohortFiles) == 0) {
    stop("No cohort CSV files found in ", cohortFolder, ".")
  }

  codelistCsv <- cohortFiles |>
    purrr::map_dfr(function(cohortFile) {
      cohortRows <- readr::read_csv(
        cohortFile,
        col_types = readr::cols(
          cohort_name = readr::col_character(),
          cohort_label = readr::col_character(),
          cohort_domain = readr::col_character(),
          concept_id = readr::col_integer(),
          include_descendants = readr::col_logical(),
          concept_name = readr::col_character(),
          vocabulary_id = readr::col_character(),
          concept_code = readr::col_character(),
          review_status = readr::col_character(),
          phenotype_note = readr::col_character()
        )
      )

      expectedCohortName <- tools::file_path_sans_ext(basename(cohortFile))
      if (
        nrow(cohortRows) == 0 ||
          any(cohortRows$cohort_name != expectedCohortName)
      ) {
        stop(
          basename(cohortFile),
          " must contain one or more rows with cohort_name = ",
          expectedCohortName,
          "."
        )
      }
      cohortRows
    })

  pairCsv <- readr::read_csv(
    pairPath,
    col_types = readr::cols(
      pair_id = readr::col_character(),
      index_cohort_name = readr::col_character(),
      marker_cohort_name = readr::col_character(),
      marker_type = readr::col_character(),
      tier = readr::col_integer(),
      expected_association = readr::col_character(),
      expected_direction = readr::col_character(),
      include_in_benchmark = readr::col_logical(),
      protocol_note = readr::col_character()
    )
  )

  requiredCodelistColumns <- c(
    "cohort_name",
    "cohort_label",
    "cohort_domain",
    "concept_id",
    "include_descendants"
  )
  missingCodelistColumns <- setdiff(requiredCodelistColumns, names(codelistCsv))
  if (length(missingCodelistColumns) > 0) {
    stop(
      "Missing codelist columns: ",
      paste(missingCodelistColumns, collapse = ", ")
    )
  }

  requiredPairColumns <- c(
    "pair_id",
    "index_cohort_name",
    "marker_cohort_name",
    "marker_type",
    "tier",
    "expected_association",
    "expected_direction",
    "include_in_benchmark"
  )
  missingPairColumns <- setdiff(requiredPairColumns, names(pairCsv))
  if (length(missingPairColumns) > 0) {
    stop(
      "Missing analysis-pair columns: ",
      paste(missingPairColumns, collapse = ", ")
    )
  }

  invalidDomains <- setdiff(
    unique(codelistCsv$cohort_domain),
    c("drug", "condition")
  )
  if (length(invalidDomains) > 0) {
    stop(
      "cohort_domain must be drug or condition. Found: ",
      paste(invalidDomains, collapse = ", ")
    )
  }

  invalidMarkerTypes <- setdiff(
    unique(pairCsv$marker_type),
    c("diagnosis", "proxy")
  )
  if (length(invalidMarkerTypes) > 0) {
    stop(
      "marker_type must be diagnosis or proxy. Found: ",
      paste(invalidMarkerTypes, collapse = ", ")
    )
  }

  invalidAssociations <- setdiff(
    unique(pairCsv$expected_association),
    c("positive", "negative", "emerging")
  )
  if (length(invalidAssociations) > 0) {
    stop(
      "expected_association must be positive, negative, or emerging. Found: ",
      paste(invalidAssociations, collapse = ", ")
    )
  }

  invalidDirections <- setdiff(
    unique(pairCsv$expected_direction),
    c("above_1", "not_above_1", "unknown")
  )
  if (length(invalidDirections) > 0) {
    stop(
      "expected_direction must be above_1, not_above_1, or unknown. Found: ",
      paste(invalidDirections, collapse = ", ")
    )
  }

  inconsistentDirections <- pairCsv |>
    dplyr::filter(
      (.data$expected_association == "positive" &
        .data$expected_direction != "above_1") |
        (.data$expected_association == "negative" &
          .data$expected_direction != "not_above_1") |
        (.data$expected_association == "emerging" &
          .data$expected_direction != "unknown")
    )
  if (nrow(inconsistentDirections) > 0) {
    stop(
      "Expected association and direction disagree for: ",
      paste(inconsistentDirections$pair_id, collapse = ", ")
    )
  }

  cohortIdentity <- codelistCsv |>
    dplyr::distinct(
      .data$cohort_name,
      .data$cohort_label,
      .data$cohort_domain
    )

  inconsistentCohorts <- cohortIdentity |>
    dplyr::count(.data$cohort_name) |>
    dplyr::filter(.data$n > 1)
  if (nrow(inconsistentCohorts) > 0) {
    stop(
      "Cohort metadata is inconsistent for: ",
      paste(inconsistentCohorts$cohort_name, collapse = ", ")
    )
  }

  cohortSpecification <- codelistCsv |>
    dplyr::group_by(
      .data$cohort_name,
      .data$cohort_label,
      .data$cohort_domain
    ) |>
    dplyr::summarise(
      review_status = dplyr::if_else(
        any(.data$review_status == "provisional_broad"),
        "provisional_broad",
        "reviewed_seed"
      ),
      phenotype_note = paste(
        unique(.data$phenotype_note),
        collapse = "; "
      ),
      .groups = "drop"
    )

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
        excluded = FALSE,
        mapped = FALSE
      )
    }) |>
    CodelistGenerator::newConceptSetExpression()

  definedCohorts <- unique(codelistCsv$cohort_name)
  missingIndexCohorts <- setdiff(pairCsv$index_cohort_name, definedCohorts)
  missingMarkerCohorts <- setdiff(pairCsv$marker_cohort_name, definedCohorts)
  if (length(c(missingIndexCohorts, missingMarkerCohorts)) > 0) {
    stop(
      "Analysis pairs reference undefined cohorts: ",
      paste(
        unique(c(missingIndexCohorts, missingMarkerCohorts)),
        collapse = ", "
      )
    )
  }

  pairDomains <- pairCsv |>
    dplyr::left_join(
      cohortSpecification |>
        dplyr::select(
          index_cohort_name = "cohort_name",
          index_label = "cohort_label",
          index_domain = "cohort_domain"
        ),
      by = "index_cohort_name"
    ) |>
    dplyr::left_join(
      cohortSpecification |>
        dplyr::select(
          marker_cohort_name = "cohort_name",
          marker_label = "cohort_label",
          marker_domain = "cohort_domain"
        ),
      by = "marker_cohort_name"
    )

  if (any(pairDomains$index_domain != "drug")) {
    stop("Every protocol index cohort must have cohort_domain = drug.")
  }
  if (any(pairDomains$marker_type == "diagnosis" & pairDomains$marker_domain != "condition")) {
    stop("Diagnosis markers must have cohort_domain = condition.")
  }
  if (any(pairDomains$marker_type == "proxy" & pairDomains$marker_domain != "drug")) {
    stop("Drug proxies must have cohort_domain = drug.")
  }

  list(
    concept_sets = conceptSets,
    cohort_specification = cohortSpecification,
    analysis_pairs = pairDomains,
    codelist_csv = codelistCsv
  )
}
