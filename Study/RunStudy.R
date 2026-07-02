resultsFolder <- here::here("Results")
if(!dir.exists(resultsFolder)) {
  dir.create(resultsFolder)
}

createLogFile(logFile = here::here("Results", "log_{date}_{time}"))

omopgenerics::logMessage("Creating CDM reference")
cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  cdmName = dbName,
  achillesSchema = achillesSchema
)

snapshotResult <- OmopSketch::summariseOmopSnapshot(cdm)
omopgenerics::exportSummarisedResult(
  snapshotResult,
  fileName = "cdm_snapshot_{cdm_name}_{date}.csv",
  path = resultsFolder,
  minCellCount = minCellCount
)

source(file.path(studyPath, "Cohorts", "InstantiateCohorts.R"))

omopgenerics::logMessage("Running CohortSymmetry analysis")
source(file.path(studyPath, "Analyses", "RunCohortSymmetry.R"))

# omopgenerics::logMessage("Exporting results")
# zipFile <- file.path(resultsFolder, paste0("Results_", CDMConnector::cdmName(cdm), ".zip"))
# filesToZip <- setdiff(
#   list.files(resultsFolder, full.names = TRUE, recursive = TRUE, include.dirs = FALSE),
#   zipFile
# )
#utils::zip(zipfile = zipFile, files = filesToZip)

#omopgenerics::logMessage(paste("Results exported to", zipFile))

logMessage("Creating one final csv file from all results")
all_results <- importSummarisedResult(path = resultsFolder)
exportSummarisedResult(all_results,
                       fileName = paste0("pssa_",cdm_name,".csv"),
                       minCellCount = minCellCount,
                       path = resultsFolder)

csv_files <- list.files(path = resultsFolder, patter = ".csv")
final_file_name <- glue::glue("pssa_{cdm_name}.csv")
files_to_delete <- csv_files[csv_files != final_file_name]
files_to_delete <- file.path(resultsFolder, files_to_delete)
unlink(files_to_delete)

# Create shiny app
OmopViewer::exportStaticApp(
  all_results,
  directory = here::here()
)

