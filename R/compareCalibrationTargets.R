#' Render compareCalibrationTargets
#'
#' @param gdxPaths \code{character(n)}, optionally named. Paths to GDX objects.
#'   If the vector has names, those are used to refer to the scenarios in the
#'   output file.
#' @param outputDir The directory where the output document
#'   and intermediary files are created.
#' @param outputFile File name (without extension) of the
#'   output document to be created.
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#' @author Falk Benke
#' @examples
#' \dontrun{
#' # Simple use.
#' checkVsCalibData(
#'   gdxPaths = c("path/to/fulldata.gdx", "another path/to/fulldata.gdx"),
#'   outputDir = "path/to/output/directory",
#'   outputFile = "myComparison.html"
#' )
#' }
#' @export
compareCalibrationTargets <- function(gdxPaths, outputDir = getwd(),
                                      outputFile = "compareCalibrationTargets.html") {
  yamlParams <- list(
    gdxPaths = normalizePath(gdxPaths, mustWork = TRUE),
    gdxPathNames = names(gdxPaths)
  )
  rmarkdown::render(
    system.file("markdown/compareCalibrationTargets.Rmd", package = "remind2"),
    output_dir = outputDir,
    output_file = outputFile,
    output_format = "html_document",
    params = yamlParams
  )
}
