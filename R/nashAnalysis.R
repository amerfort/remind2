#' @title Nash Analysis
#' @description Create plots visualizing nash convergence of a given REMIND run
#'
#' @author Falk Benke
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @param outputFile \code{character(1)}. File name (without extension) of the
#'   output document to be created.
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#'
#' @importFrom rmarkdown render
#'
#' @export
nashAnalysis <- function(gdx = "fulldata.gdx", outputDir = getwd(), outputFile = "Nash Analysis.html") {


  yamlParams <- list(gdx = normalizePath(gdx, mustWork = TRUE))

  rmarkdown::render(
    system.file("markdown", "nashAnalysis.Rmd", package = "remind2"),
    output_dir = outputDir,
    output_file = outputFile,
    output_format = "html_document",
    params = yamlParams
  )
}
