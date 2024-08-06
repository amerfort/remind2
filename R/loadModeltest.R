#' @importFrom utils globalVariables
globalVariables(".") # nolint


getProjectPath <- function(project = "remind") {
  possibleProjectLocations <- file.path(c("//clusterfs.pik-potsdam.de", "/p/projects"), project) # nolint
  sel <- which(file.exists(possibleProjectLocations))[1]
  if (is.na(sel)) stop("Cannot determine a path to projects on the cluster.")
  possibleProjectLocations[sel]
}


#' @importFrom dplyr bind_cols
#' @importFrom rlang .env
getNewsestModeltests <- function(namePattern, requireMif) {

  modeltestOutPath <- file.path(getProjectPath(), "modeltests", "remind", "output")
  entries <- dir(modeltestOutPath)
  allRunNames <-
    entries %>%
    grep(
      x = .,
      pattern = "^[a-zA-Z0-9-]+_\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}$",
      value = TRUE
    )
  allRuns <-
    allRunNames %>%
    strsplit("_", fixed = TRUE) %>%
    unlist() %>%
    matrix(nrow = 3, dimnames = list(c("name", "date", "time"), NULL)) %>%
    t() %>%
    as_tibble() %>%
    bind_cols(runName = allRunNames) %>%
    mutate(date = as.Date(.data$date)) %>%
    mutate(path = file.path(.env$modeltestOutPath, .data$runName)) %>%
    filter(date >= max(.data$date) - 180) %>% # limit to half a year
    filter(file.exists(file.path(.data$path, "config.Rdata"))) %>%
    mutate(mifScen = getMifScenPath(.data$path))
  selectedRuns <-
    allRuns %>%
    filter(grepl(x = .data$name, pattern = .env$namePattern)) %>%
    filter(!.env$requireMif | file.exists(.data$mifScen)) %>%
    filter(date >= max(.data$date) - 3) # day of newest run until 3 days before

  return(selectedRuns)
}


#' Load compareScenarios Data
#'
#' Load data from mif files into R-objects as used in \link[=compareScenarios]{compareScenarios()}.
#'
#' @param cfgScen,cfgDefault See section "YAML Parameters" in \link[=compareScenarios]{compareScenarios()}.
#' @param envir \code{environment}. The environment where the loaded data is put into.
#' @inheritParams piamPlotComparison::compareScenarios
#' @examples
#' \dontrun{
#' loadCs2Data(
#'   c("path/to/Base.mif", "path/to/NDC.mif"),
#'   "path/to/historical.mif")
#' }
#' @export
loadCs2Data <- function(
  mifScen,
  mifHist,
  cfgScen = NULL,
  cfgDefault = NULL,
  envir = globalenv()
) {

  folder <- tempdir()
  outputFile <- format(Sys.time(), "cs2_%Y%m%d-%H%M%S")
  piamPlotComparison::compareScenarios(
    projectLibrary = "remind2",
    mifScen = mifScen,
    mifHist = mifHist,
    cfgScen = cfgScen,
    outputDir = folder,
    outputFormat = "pdf",
    outputFile = outputFile,
    sections = NULL,
    envir = envir,
    quiet = TRUE
  )
  file.remove(file.path(folder, paste0(outputFile, ".pdf")))
  return(invisible(NULL))
}


cs2InputPaths <- function(outputDirs) {
  path <- list(
    run = outputDirs,
    mifScen = getMifScenPath(outputDirs),
    mifHist = getMifHistPath(outputDirs[1]),
    cfgScen = getCfgScenPath(outputDirs),
    cfgDefault = getCfgDefaultPath(file.path(outputDirs[1], "../.."))
  )
  lapply(path, normalizePath)
}


#' Load Modeltest Results
#'
#' The newest model tests are collected from the cluster and copied into a
#' temporary folder (by default). Then the \link[=compareScenarios]{compareScenarios()}
#' data loading procedure is used to load this data into the users environment.
#'
#' @param namePattern \code{character(1)}. A regular expression to filter the
#'   modeltest run names.
#' @param folder \code{character(1)}. A folder to copy the modeltest data to.
#' @inheritParams loadCs2Data
#' @examples
#' \dontrun{
#' loadModeltest()
#'
#' ssp1 <- new.env()
#' ssp2eu <- new.env()
#' loadModeltest(ssp1, "^SSP1-AMT-")
#' loadModeltest(ssp2eu, "^SSP2-.*-AMT$")
#' ssp1$data
#' ssp2eu$data
#' }
#' @export
loadModeltest <- function(
  envir = globalenv(),
  namePattern = "^SSP2-.*-AMT$",
  folder = tempdir()
) {

  stopifnot(is.environment(envir))
  stopifnot(is.character(namePattern) && length(namePattern) == 1)
  stopifnot(is.character(folder) && length(folder) == 1)

  cat("Obtaining modeltest paths.\n")

  modeltests <- getNewsestModeltests(namePattern, requireMif = TRUE)
  if (NROW(modeltests) == 0) stop("Did not find model tests.")
  path <- cs2InputPaths(modeltests$path)

  if (!dir.exists(folder)) {
    cat(folder, "does not exist -> creating it.\n")
    dir.create(folder, recursive = TRUE)
  }
  folder <- normalizePath(folder, mustWork = TRUE)

  tmpPath <- list(
    mifScen = file.path(folder, paste0(modeltests$name, ".mif")),
    mifHist = file.path(folder, "historical.mif"),
    cfgScen = file.path(folder, paste0(modeltests$name, ".Rdata")),
    cfgDefault = file.path(folder, "default.cfg")
  )

  copyFile <- function(from, to) {
    cat("Copying\n  ", paste(from, collapse = "\n  "),
        "\nto\n  ", paste(to, collapse = "\n  "),
        "\n",
        sep = "")
    success <- file.copy(from, to, overwrite = TRUE)
    if (!all(success))
      warning(
        "failed copying following files\n",
        paste(from[!success], collapse = "\n"),
        immediate. = TRUE
      )
  }

  copyFile(path$mifScen, tmpPath$mifScen)
  copyFile(path$mifHist, tmpPath$mifHist)
  copyFile(path$cfgScen, tmpPath$cfgScen)
  copyFile(path$cfgDefault, tmpPath$cfgDefault)

  cat(
    "Loading cs2 data into ",
    if (environmentName(envir) == "")
      "user specified environment"
    else
      environmentName(envir),
    ".\n", sep = ""
  )

  loadCs2Data(
    mifScen = tmpPath$mifScen,
    mifHist = tmpPath$mifHist,
    cfgScen = tmpPath$cfgScen,
    cfgDefault = tmpPath$cfgDefault,
    envir = envir
  )

  cat("Done.\n")

  return(invisible(NULL))
}
