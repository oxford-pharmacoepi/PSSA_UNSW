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

validateSourceDrugLookup <- function(sourceDrugLookup) {
  requiredColumns <- c("item_code", "drug_name", "form_strength", "atc_code")
  missingColumns <- setdiff(requiredColumns, names(sourceDrugLookup))
  if (length(missingColumns) > 0) {
    stop(
      "Source drug lookup is missing columns: ",
      paste(missingColumns, collapse = ", ")
    )
  }

  sourceDrugLookup |>
    dplyr::mutate(
      item_code = trimws(as.character(.data$item_code)),
      drug_name = as.character(.data$drug_name),
      form_strength = as.character(.data$form_strength),
      atc_code = toupper(trimws(as.character(.data$atc_code)))
    ) |>
    dplyr::filter(
      !is.na(.data$item_code),
      .data$item_code != "",
      !is.na(.data$atc_code),
      .data$atc_code != ""
    ) |>
    dplyr::distinct()
}

readProxyAnalysisPairs <- function(pairPath) {
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
  ) |>
    dplyr::filter(.data$marker_type == "proxy")

  if (nrow(pairCsv) == 0) {
    stop("No proxy rows found in analysis_pairs.csv.")
  }

  pairCsv
}

protocolDrugAtcPrefixes <- tibble::tribble(
  ~cohort_name, ~atc_prefix,
  "ace_inhibitors", "C09AA",
  "amiodarone", "C01BD01",
  "anticoagulants", "B01AA",
  "anticoagulants", "B01AB",
  "anticoagulants", "B01AE",
  "anticoagulants", "B01AF",
  "anticoagulants", "B01AX",
  "antidepressants", "N06A",
  "antiemetics", "A04A",
  "antiepileptics", "N03A",
  "antifungals", "G01AF",
  "antifungals", "G01AG",
  "antifungals", "J02AC",
  "antigout_medicines", "M04A",
  "antimigraine_medicines", "N02C",
  "antiparkinson_medicines", "N04",
  "antipsychotics", "N05A",
  "antitussives", "R05D",
  "antitussives", "R05F",
  "bisphosphonates", "M05BA",
  "bisphosphonates", "M05BB",
  "dhp_calcium_channel_blockers", "C08CA",
  "glp1_receptor_agonists", "A10BJ",
  "jak_inhibitors", "L04AF",
  "laxatives", "A06A",
  "loop_diuretics", "C03C",
  "nsaids", "M01A",
  "ophthalmological_medicines", "S01",
  "opioids", "N02A",
  "organic_nitrates", "C01DA",
  "osteoporosis_medicines", "M05B",
  "osteoporosis_medicines", "H05AA",
  "proton_pump_inhibitors", "A02BC",
  "sglt2_inhibitors", "A10BK",
  "statins", "C10AA",
  "systemic_hormonal_contraceptives", "G03A",
  "thyroid_hormones", "H03AA"
)

buildSourceDrugMembers <- function(
    sourceDrugLookup,
    drugCohorts,
    atcPrefixes = protocolDrugAtcPrefixes) {
  requiredDrugCohorts <- drugCohorts |>
    dplyr::distinct(.data$cohort_name)

  missingMappings <- setdiff(
    requiredDrugCohorts$cohort_name,
    unique(atcPrefixes$cohort_name)
  )
  if (length(missingMappings) > 0) {
    stop(
      "Add ATC prefixes for these drug cohorts in protocolDrugAtcPrefixes: ",
      paste(missingMappings, collapse = ", ")
    )
  }

  extraMappings <- setdiff(
    unique(atcPrefixes$cohort_name),
    requiredDrugCohorts$cohort_name
  )
  if (length(extraMappings) > 0) {
    warning(
      "Ignoring ATC prefix rows for cohorts not present in the study: ",
      paste(extraMappings, collapse = ", ")
    )
  }

  mappedCohorts <- requiredDrugCohorts |>
    dplyr::inner_join(atcPrefixes, by = "cohort_name")

  sourceDrugMembers <- tidyr::crossing(
    mappedCohorts,
    sourceDrugLookup |>
      dplyr::select(
        .data$item_code,
        .data$drug_name,
        .data$form_strength,
        .data$atc_code
      )
  ) |>
    dplyr::filter(startsWith(.data$atc_code, .data$atc_prefix)) |>
    dplyr::distinct()

  cohortsWithNoMembers <- mappedCohorts |>
    dplyr::distinct(.data$cohort_name) |>
    dplyr::anti_join(
      sourceDrugMembers |>
        dplyr::distinct(.data$cohort_name),
      by = "cohort_name"
    )

  if (nrow(cohortsWithNoMembers) > 0) {
    stop(
      "No source-code rows matched the configured ATC prefixes for: ",
      paste(cohortsWithNoMembers$cohort_name, collapse = ", ")
    )
  }

  sourceDrugMembers
}

createSourceDrugCohorts <- function(
    cdm,
    sourceDrugMembers,
    drugCohorts,
    gapEra = 1,
    targetTableName = "pssa_drug_cohorts") {
  drugCohortSet <- drugCohorts |>
    dplyr::distinct(.data$cohort_name) |>
    dplyr::arrange(.data$cohort_name) |>
    dplyr::mutate(
      cohort_definition_id = dplyr::row_number(),
      gap_era = as.character(gapEra)
    ) |>
    dplyr::select(
      .data$cohort_definition_id,
      .data$cohort_name,
      .data$gap_era
    )

  sourceCodeMap <- sourceDrugMembers |>
    dplyr::distinct(.data$cohort_name, .data$item_code) |>
    dplyr::inner_join(
      drugCohortSet |>
        dplyr::select(.data$cohort_definition_id, .data$cohort_name),
      by = "cohort_name"
    ) |>
    dplyr::select(.data$cohort_definition_id, .data$item_code)

  rawTableName <- omopgenerics::uniqueTableName(prefix = omopgenerics::tmpPrefix())

  cdm[[rawTableName]] <- cdm$drug_exposure |>
    dplyr::mutate(
      drug_source_value = as.character(.data$drug_source_value),
      cohort_end_date = dplyr::coalesce(
        .data$drug_exposure_end_date,
        .data$drug_exposure_start_date
      )
    ) |>
    dplyr::inner_join(
      sourceCodeMap,
      by = c("drug_source_value" = "item_code"),
      copy = TRUE
    ) |>
    dplyr::transmute(
      cohort_definition_id = .data$cohort_definition_id,
      subject_id = .data$person_id,
      cohort_start_date = .data$drug_exposure_start_date,
      cohort_end_date = .data$cohort_end_date
    ) |>
    dplyr::distinct() |>
    dplyr::compute(
      name = rawTableName,
      temporary = FALSE,
      logPrefix = "PSSA_source_drug_cohorts_raw_"
    )

  cdm[[rawTableName]] <- omopgenerics::newCohortTable(
    cdm[[rawTableName]],
    cohortSetRef = drugCohortSet
  )

  cdm[[targetTableName]] <- DrugUtilisation::erafyCohort(
    cdm[[rawTableName]],
    gapEra = gapEra,
    name = targetTableName,
    nameStyle = "{cohort_name}"
  )

  cdm[[rawTableName]] <- NULL
  cdm
}

sourceDrugLookupPath <- get0(
  "sourceDrugLookupPath",
  ifnotfound = file.path(studyPath, "inst", "source_drug_lookup.csv")
)
sourceDrugGapEra <- get0("sourceDrugGapEra", ifnotfound = 1)

analysisPairPath <- file.path(studyPath, "inst", "analysis_pairs.csv")

omopgenerics::logMessage("Reading proxy-only analysis pairs for drug-only source data")
analysisPairsInput <- readProxyAnalysisPairs(analysisPairPath)

drugCohorts <- dplyr::bind_rows(
  analysisPairsInput |>
    dplyr::transmute(cohort_name = .data$index_cohort_name),
  analysisPairsInput |>
    dplyr::transmute(cohort_name = .data$marker_cohort_name)
) |>
  dplyr::distinct()

if (!file.exists(sourceDrugLookupPath)) {
  stop(
    "Source drug lookup file not found: ",
    sourceDrugLookupPath,
    ". Set sourceDrugLookupPath before sourcing InstantiateSourceDrugCohorts.R."
  )
}

omopgenerics::logMessage("Reading source-code drug lookup")
sourceDrugLookup <- readr::read_csv(
  sourceDrugLookupPath,
  show_col_types = FALSE
) |>
  validateSourceDrugLookup()

omopgenerics::logMessage("Building source-code membership lists from ATC prefixes")
sourceDrugMembers <- buildSourceDrugMembers(
  sourceDrugLookup = sourceDrugLookup,
  drugCohorts = drugCohorts
)

omopgenerics::logMessage("Instantiating drug cohorts from drug_source_value")
cdm <- createSourceDrugCohorts(
  cdm = cdm,
  sourceDrugMembers = sourceDrugMembers,
  drugCohorts = drugCohorts,
  gapEra = sourceDrugGapEra,
  targetTableName = "pssa_drug_cohorts"
)

analysisPairs <- analysisPairsInput |>
  dplyr::mutate(
    index_table = "pssa_drug_cohorts",
    marker_table = "pssa_drug_cohorts",
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
pssaCohortTables <- "pssa_drug_cohorts"
