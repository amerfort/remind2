#' take scenario-config*.csv files and produce colorful xlsx file with values
#' from main.gms in first row, identical values in this column in turquoise,
#' and unknown column names in red
#'
#' @param fileList vector containing csv file paths
#' @param remindPath path to REMIND directory containing the main.gms
#' @param expanddata fill all the data based on copyConfigFrom and main.gms
#' @author Oliver Richters
#' @examples
#'
#'  \dontrun{
#'     colorScenConf(fileList = c("scenario_config.csv"), remindPath = ".")
#'   }
#'
#' @return nothing. For each file in fileList a _colorful.xlsx file is written.
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
#' @importFrom utils read.csv2
#' @importFrom withr defer local_connection
#' @importFrom gms getLine
#' @export
colorScenConf <- function(fileList = "", remindPath = ".", expanddata = FALSE) {
  if (! file.exists(file.path(remindPath, "main.gms"))) {
    remindPath <- normalizePath(file.path(remindPath, ".."))
  }
  cfg <- gms::readDefaultConfig(remindPath)
  readCheckScenarioConfig <- function(csvFile, ...) {
    return(read.csv2(csvFile, stringsAsFactors = FALSE, row.names = 1,
                            comment.char = "", na.strings = "", dec = "."))
  }
  if (expanddata) {
    message("Loading R helper functions from remindmodel.") # overwrite readCheckScenarioConfig
    remindRscripts <- list.files(file.path(remindPath, "scripts", "start"), pattern = "\\.R$", full.names = TRUE)
    invisible(sapply(remindRscripts, source, local = TRUE))
  }
  # enable script to match default data not in gms
  try(cfg$gms[["output"]] <- paste0(cfg$output, collapse = ","))
  try(cfg$gms[["model"]] <- cfg$model)
  try(cfg$gms[["regionmapping"]] <- cfg$regionmapping)
  try(cfg$gms[["inputRevision"]] <- cfg$inputRevision)

  if (fileList == "" || length(fileList) == 0) {
    cat("Specify folder with .csv files. The script also searches in subdirectories.\n")
    cat(paste0("Press enter to use current: ", getwd(), ".\n"))
    folder <- getLine()
    if (folder == "") folder <- getwd()
    fileList <- c("all",
                Sys.glob(file.path(paste0(folder, "/scenario_config*.csv"))),
                Sys.glob(file.path(paste0(folder, "/*/scenario_config*.csv"))),
                Sys.glob(file.path(paste0(folder, "/config/*/scenario_config*.csv")))
      )
    cat("\nPlease choose files:\n")
    cat(paste(seq_along(fileList), fileList, sep = ": "), sep = "\n")
    identifier <- as.numeric(strsplit(getLine(), ",")[[1]])
    if (any(fileList[identifier] == "all")) identifier <- 2:length(fileList)
    fileList <- fileList[identifier]
  }

  for (csvFile in fileList) {
    xlsxFile <- sub(".csv", "_colorful.xlsx", csvFile, fixed = TRUE)
    cat(paste0("\nStart converting '", csvFile, "' to '..._colorful.xlsx'.\n"))
    settings <- readCheckScenarioConfig(csvFile, remindPath = remindPath, fillWithDefault = TRUE, testmode = TRUE)
    settings <- rbind(rep("unknown", length(names(settings))), settings)
    for (switchname in intersect(names(cfg$gms), names(settings))) {
      settings[1, switchname] <- cfg$gms[[switchname]]
    }
    colnamesNeverInDefault <- c("copyConfigFrom", "path_gdx", "path_gdx_ref", "path_gdx_bau",
      "path_gdx_carbonprice", "path_gdx_refpolicycost", "start", "slurmConfig", "description")
    settings[1, intersect(names(settings), colnamesNeverInDefault)] <- ""
    row.names(settings)[1] <- paste0("# main.gms on ", Sys.time())

    wb <- createWorkbook() # create a workbook
    addWorksheet(wb, "Sheet", gridLines = TRUE) #add a worksheet to the workbook

    writeData(wb, "Sheet", settings, rowNames = TRUE)
    writeData(wb, "Sheet", "title")

    blueStyle <- createStyle(fontColour = "#000000", bgFill = "#FFFF00", fgFill = "#00FFFF")
    redStyle <- createStyle(fontColour = "#000000", bgFill = "#FF0000", fgFill = "#FF0000")

    # in default row, color all cells red
    redcols <- seq(2, length(names(settings)) + 1)[!names(settings) %in% colnamesNeverInDefault]
    addStyle(wb = wb, sheet = "Sheet", style = redStyle, rows = 2, cols = redcols)

    # color all cells that are equal to default.cfg, also replacing the red ones where parameters were found
    for (switchname in intersect(names(cfg$gms), names(settings))) {
      colnumber <- which(switchname == names(settings)) + 1
      rownumber <- which(cfg$gms[[switchname]] == settings[, switchname]) + 1
      addStyle(wb = wb, sheet = "Sheet", style = blueStyle, rows = rownumber, cols = colnumber)
    }

    saveWorkbook(wb, xlsxFile, overwrite = TRUE)
  }  # end loop through fileList
  cat(paste0("Completed comparisons with main.gms: ", length(fileList), ".\n\n"))
  cat("- row 2: default values of main.gms, parameters not found are marked red.\n")
  cat("- from row 3: cells identical to default marked turquoise.\n")
  cat("  They may be deleted if you always want to use the default in the future.\n\n")
  cat("Note: opening the xlsx, excel may complain that numbers are saved as text.\n")
  cat("After saving as csv this should disappear and work, but please check.\n\n")
}
