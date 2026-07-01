library(CDMConnector)
library(CodelistGenerator)
library(DBI)
library(dplyr)
library(omopgenerics)
library(readr)

studyPath <- normalizePath(
  if (file.exists("RunStudy.R")) {
    "."
  } else if (file.exists(file.path("Study", "RunStudy.R"))) {
    "Study"
  } else {
    stop("Run CodeToRun.R from the repository root or from the Study directory.")
  },
  mustWork = TRUE
)

dbName <- "UNSW"

# Example only. Replace with the site's DBI connection call.
# db <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = Sys.getenv("CDM_DBNAME"),
#   host = Sys.getenv("CDM_HOST"),
#   port = as.integer(Sys.getenv("CDM_PORT")),
#   user = Sys.getenv("CDM_USER"),
#   password = Sys.getenv("CDM_PASSWORD")
# )
db <- DBI::dbConnect("...")

cdmSchema <- "..."
writePrefix <- "pssa_unsw"
writeSchema <- c(schema = "...", prefix = writePrefix)
achillesSchema <- NULL

minCellCount <- 6

cohortTableName <- "pssa_study_cohorts"
requiredObservation <- c(365, 365)

pssaSettings <- list(
  cohortTableName = cohortTableName,
  codelistFile = file.path(studyPath, "inst", "mock_codelists.csv"),
  analysisSettingsFile = file.path(studyPath, "inst", "analysis_settings.csv"),
  minCellCount = minCellCount
)

source(file.path(studyPath, "RunStudy.R"))

cli::cli_alert_success("Study finished")
