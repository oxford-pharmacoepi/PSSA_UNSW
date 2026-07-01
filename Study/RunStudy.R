resultsFolder <- file.path(studyPath, "Results")
dir.create(resultsFolder, showWarnings = FALSE, recursive = TRUE)

omopgenerics::createLogFile(
  logFile = file.path(resultsFolder, "log_{date}_{time}.txt")
)
logFile <- getOption("omopgenerics.logFile")
omopgenerics::logMessage("Log created", logFile = logFile)

omopgenerics::logMessage("Creating CDM reference", logFile = logFile)
cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  cdmName = dbName,
  achillesSchema = achillesSchema
)

snapshotResult <- CDMConnector::snapshot(cdm)
omopgenerics::exportSummarisedResult(
  snapshotResult,
  fileName = "cdm_snapshot_{cdm_name}_{date}.csv",
  path = resultsFolder,
  minCellCount = minCellCount
)

omopgenerics::logMessage("Instantiating study cohorts", logFile = logFile)
source(file.path(studyPath, "Cohorts", "InstantiateCohorts.R"))

omopgenerics::logMessage("Running CohortSymmetry analysis", logFile = logFile)
source(file.path(studyPath, "Analyses", "RunCohortSymmetry.R"))

omopgenerics::logMessage("Exporting results", logFile = logFile)
zipFile <- file.path(resultsFolder, paste0("Results_", CDMConnector::cdmName(cdm), ".zip"))
filesToZip <- setdiff(
  list.files(resultsFolder, full.names = TRUE, recursive = TRUE, include.dirs = FALSE),
  zipFile
)
utils::zip(zipfile = zipFile, files = filesToZip)

omopgenerics::logMessage(paste("Results exported to", zipFile), logFile = logFile)
