#' Render CompareScenarios2
#'
#' A deprecated function. Please refer to piamPlotComparison::compareScenarios
#'
#' @param ... parameters to be passed on to piamPlotComparison::compareScenarios
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#' @export
compareScenarios2 <- function(...) {
  newArgs <- list(...)
  newArgs[["projectLibrary"]] <- "remind2"

  # cfgDefault can be removed, as it did not have any effect anyways due to a bug
  newArgs$cfgDefault <- NULL

  do.call(piamPlotComparison::compareScenarios, newArgs)
}
