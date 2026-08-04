library(CDMConnector)
library(CodelistGenerator)
library(CohortConstructor)
library(CohortSymmetry)
library(DBI)
library(dplyr)
library(DrugUtilisation)
library(omopgenerics)
library(OmopSketch)
library(OmopViewer)
library(purrr)
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

# Replace this example with the site's DBI connection call.
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
writePrefix <- "pssa_safety"
writeSchema <- c(schema = "...", prefix = writePrefix)
achillesSchema <- NULL

minCellCount <- 6
runPhenotypeDiagnostics <- TRUE
runBaselineCharacteristics <- TRUE
runLargeScaleCharacteristics <- FALSE
phenotypeSample <- 20000

source(file.path(studyPath, "RunStudy.R"))

cli::cli_alert_success("Study finished")
