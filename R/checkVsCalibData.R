#' Render checkVsCalibData
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @param outputFile \code{character(1)}. File name (without extension) of the
#'   output document to be created.
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#' @author Falk Benke
#' @examples
#' \dontrun{
#' # Simple use. Creates PDF:
#' checkVsCalibData(
#'   gdx = "path/to/data.gdx",
#'   outputDir = "path/to/output/directory",
#'   outputFile = "Check_vs_CalibData_Example.pdf"
#' )
#' }
#' @export
checkVsCalibData <- function(gdx, outputDir = getwd(), outputFile = "Check_vs_CalibData.pdf") {

  yamlParams <- list(gdx = normalizePath(gdx, mustWork = TRUE))

  rmarkdown::render(
    system.file("markdown", "checkVsCalibrationData.Rmd", package = "remind2"),
    output_dir = outputDir,
    output_file = outputFile,
    output_format = "pdf_document",
    params = yamlParams
  )
}
