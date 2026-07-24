# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_log_file = list(result_type = "summarise_log_file"),
  sequence_ratios = list(result_type = "sequence_ratios"),
  temporal_symmetry = list(result_type = "temporal_symmetry")
)

source(file.path(getwd(), "functions.R"))

pairsFile <- file.path(getwd(), "data", "analysis_pairs.csv")
if (!file.exists(pairsFile)) {
  pairsFile <- file.path(getwd(), "..", "inst", "analysis_pairs.csv")
}
protocolPairs <- readr::read_csv(
  pairsFile,
  show_col_types = FALSE
) |>
  dplyr::select(
    "pair_id", "index_cohort_name", "marker_cohort_name", "marker_type",
    "tier", "expected_association", "expected_direction",
    "include_in_benchmark", "protocol_note"
  ) |>
  dplyr::mutate(
    expected_association = dplyr::recode(
      .data$expected_association,
      emerging = "unknown"
    )
  )

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

sequenceProtocolPairs <- data[["sequence_ratios"]] |>
  omopgenerics::splitAll() |>
  dplyr::distinct(.data$index_cohort_name, .data$marker_cohort_name) |>
  dplyr::inner_join(
    protocolPairs,
    by = c("index_cohort_name", "marker_cohort_name")
  )
temporalProtocolPairs <- data[["temporal_symmetry"]] |>
  omopgenerics::splitAll() |>
  dplyr::distinct(.data$index_name, .data$marker_name) |>
  dplyr::inner_join(
    protocolPairs,
    by = c(
      "index_name" = "index_cohort_name",
      "marker_name" = "marker_cohort_name"
    )
  )

# edit choices and values of interest
choices <- values
choices$sequence_ratios_index_cohort_name <-
  unique(sequenceProtocolPairs$index_cohort_name)
choices$sequence_ratios_marker_cohort_name <-
  unique(sequenceProtocolPairs$marker_cohort_name)
choices$temporal_symmetry_index_name <-
  unique(temporalProtocolPairs$index_name)
choices$temporal_symmetry_marker_name <-
  unique(temporalProtocolPairs$marker_name)
selected <- getSelected(values)

# Keep stable analysis IDs as picker values while showing descriptive labels.
analysisLookup <- omopgenerics::settings(result) |>
  dplyr::select(dplyr::any_of(c("analysis_id", "analysis_label"))) |>
  dplyr::filter(!is.na(.data$analysis_id), !is.na(.data$analysis_label)) |>
  dplyr::distinct()
labelAnalysisChoices <- function(ids) {
  labels <- analysisLookup$analysis_label[
    match(ids, analysisLookup$analysis_id)
  ]
  labels[is.na(labels)] <- ids[is.na(labels)]
  stats::setNames(ids, labels)
}
choices$sequence_ratios_analysis_id <-
  labelAnalysisChoices(choices$sequence_ratios_analysis_id)
choices$temporal_symmetry_analysis_id <-
  labelAnalysisChoices(choices$temporal_symmetry_analysis_id)

# Start large panels with a useful, fast view. Users can expand any picker to
# compare additional cohort pairs or sensitivity analyses.
firstChoice <- function(x) {
  if (length(x) == 0) character() else x[[1]]
}

selected$sequence_ratios_index_cohort_name <-
  firstChoice(sequenceProtocolPairs$index_cohort_name)
selected$sequence_ratios_marker_cohort_name <-
  firstChoice(sequenceProtocolPairs$marker_cohort_name)
selected$sequence_ratios_variable_name <-
  intersect(c("adjusted", "crude"), choices$sequence_ratios_variable_name)
selected$sequence_ratios_estimate_name <-
  intersect(
    c("point_estimate", "lower_CI", "upper_CI"),
    choices$sequence_ratios_estimate_name
  )
selected$sequence_ratios_analysis_id <-
  if ("primary" %in% unname(choices$sequence_ratios_analysis_id)) {
    "primary"
  } else {
    firstChoice(choices$sequence_ratios_analysis_id)
  }

selected$temporal_symmetry_index_name <-
  firstChoice(temporalProtocolPairs$index_name)
selected$temporal_symmetry_marker_name <-
  firstChoice(temporalProtocolPairs$marker_name)
selected$temporal_symmetry_analysis_id <-
  if ("primary" %in% unname(choices$temporal_symmetry_analysis_id)) {
    "primary"
  } else {
    firstChoice(choices$temporal_symmetry_analysis_id)
  }

save(
  data, choices, selected, values, protocolPairs,
  file = file.path(getwd(), "data", "shinyData.RData")
)

rm(
  result, values, choices, selected, resultList, data, protocolPairs,
  sequenceProtocolPairs, temporalProtocolPairs, pairsFile
)
