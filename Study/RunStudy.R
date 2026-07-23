runId <- format(Sys.time(), "%Y%m%d_%H%M%S")
resultsFolder <- file.path(studyPath, "Results", dbName, runId)
summarisedResultsFolder <- file.path(resultsFolder, "summarised_result")
codelistResultsFolder <- file.path(resultsFolder, "codelists")
dir.create(summarisedResultsFolder, recursive = TRUE, showWarnings = FALSE)
dir.create(codelistResultsFolder, recursive = TRUE, showWarnings = FALSE)

omopgenerics::createLogFile(
  logFile = file.path(resultsFolder, "log_{date}_{time}")
)

omopgenerics::logMessage("Creating CDM reference")
cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  cdmName = dbName,
  achillesSchema = achillesSchema
)

omopgenerics::logMessage("Summarising the OMOP CDM snapshot")
snapshotResult <- OmopSketch::summariseOmopSnapshot(cdm)
omopgenerics::exportSummarisedResult(
  snapshotResult,
  fileName = "cdm_snapshot_{cdm_name}_{date}.csv",
  path = summarisedResultsFolder,
  minCellCount = minCellCount
)

source(file.path(studyPath, "Cohorts", "InstantiateCohorts.R"))

CodelistGenerator::exportConceptSetExpression(
  codelistInputs$concept_sets,
  path = codelistResultsFolder,
  type = "csv"
)

if (runPhenotypeDiagnostics) {
  omopgenerics::logMessage("Running phenotype diagnostics")
  source(file.path(studyPath, "Analyses", "RunPhenotypeDiagnostics.R"))
}

omopgenerics::logMessage("Running CohortSymmetry analyses")
source(file.path(studyPath, "Analyses", "RunCohortSymmetry.R"))

omopgenerics::logMessage("Importing this run's summarised results")
allResults <- omopgenerics::importSummarisedResult(
  path = summarisedResultsFolder
)

finalResultsFile <- paste0("pssa_safety_", dbName, ".csv")
omopgenerics::exportSummarisedResult(
  allResults,
  fileName = finalResultsFile,
  minCellCount = minCellCount,
  path = resultsFolder,
  logFile = NULL
)

omopgenerics::logMessage("Creating the complete Shiny report")
source(file.path(studyPath, "ShinySupport", "CohortSymmetryPanel.R"))
panelDetails <- OmopViewer::panelDetailsFromResult(allResults)
resultTypes <- unique(omopgenerics::settings(allResults)$result_type)

if ("sequence_ratios" %in% resultTypes) {
  panelDetails$sequence_ratios <- cohortSymmetryPanel(
    "sequence_ratios",
    title = "Sequence ratios",
    includeProtocolBenchmark = TRUE
  )
}
if ("temporal_symmetry" %in% resultTypes) {
  panelDetails$temporal_symmetry <- cohortSymmetryPanel(
    "temporal_symmetry",
    title = "Temporal symmetry"
  )
}

shinyDirectory <- file.path(studyPath, "shiny")
if (dir.exists(shinyDirectory)) {
  unlink(shinyDirectory, recursive = TRUE)
}

OmopViewer::exportStaticApp(
  allResults,
  directory = studyPath,
  title = "GNN-PSSA safety signal report",
  background = TRUE,
  summary = TRUE,
  report = TRUE,
  panelDetails = panelDetails,
  open = FALSE
)

appendCohortSymmetryShinyFunctions(shinyDirectory)
file.copy(
  from = file.path(studyPath, "ShinySupport", "background.md"),
  to = file.path(shinyDirectory, "background.md"),
  overwrite = TRUE
)

omopgenerics::logMessage(
  paste("Study results written to", resultsFolder)
)
