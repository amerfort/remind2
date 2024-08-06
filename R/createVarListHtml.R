node2Html <- function(name, item) {
  if (name == "INFO") {
    html <- info2Html(item)
  } else if (is.list(item)) {
    html <- innerNode2Html(name, item)
  } else {
    html <- paste0(
      "<div><b>", name, "</b>: ",
      paste0(as.character(item), collapse = ", "), "</div>")
  }
  return(html)
}

innerNode2Html <- function(name, item) {
  content <- namedList2Html(item)
  if ("INFO" %in% names(item)) name <- paste0("<b>", name, "</b>")
  html <- c(
    "<details>",
    paste0("  <summary>", name, "</summary>"),
    paste0("  ", content),
    "</details>")
  return(html)
}

info2Html <- function(item) {
  name <- "<i>INFO</i>"
  content <- paste0(
    "<b>", names(item), "</b>: ",
    vapply(item, paste0, character(1), collapse = ", "))
  html <- c(
    "<details>",
    paste0("  <summary>", name, "</summary>"),
    "  <ul>",
    paste0("    <li>", content, "</li>"),
    "  </ul>",
    "</details>")
  return(html)
}

namedList2Html <- function(v) {
  lines <- lapply(seq_along(v), function(i) node2Html(names(v)[i], v[[i]]))
  html <- unlist(lines)
  return(html)
}

renderVarListAsHtml <- function(varList, outFileName, title, htmlBefore) {
  htmlLines <- namedList2Html(varList)
  htmlTemplateFile <- system.file("extdata/variablesAsListTemplate.html", package = "remind2")
  htmlTemplate <- readLines(htmlTemplateFile)
  html <-
    htmlTemplate %>%
    sub("[:TITLE:]", title, ., fixed = TRUE) %>%
    sub("[:BEFORE:]", htmlBefore, ., fixed = TRUE) %>%
    sub("[:CONTENT:]", paste0(htmlLines, collapse = "\n"), ., fixed = TRUE)
  write(html, outFileName)
  return(invisible(NULL))
}

#' Create an HTML Document of a Hierarchical List of Variables
#'
#' Creates a hierarchical list from mif data using \code{\link{variablesAsList}}
#' and writes it as an HTML document that displays the hierarchy via the
#' \code{<details>} HTML5-tag.
#'
#' @seealso \code{\link{variablesAsList}}
#' @param outFileName A single string. The path of the output file, preferably
#'   ending in \code{.html}
#' @param title The title displayed at the top of the created HTML.
#' @param htmlBefore \code{character(1)}. HTML to be put between title and list of variables.
#' @inheritParams variablesAsList
#' @examples
#' \dontrun{
#' loadModeltest()
#' createVarListHtml(data, "variables.html")
#' detailsAR6 <-
#'   readr::read_delim(
#'     paste0(
#'       "https://raw.githubusercontent.com/pik-piam/",
#'       "piamInterfaces/master/inst/mappings/mapping_AR6.csv"),
#'     delim = ";",
#'     col_select = c(r21m42, Definition)
#'   ) %>%
#'   rename(name = r21m42)
#' createVarListHtml(
#'   data,
#'   "variablesWithDescription.html",
#'   title = "Reported REMIND Variables with AR6 Description",
#'   usePlus = TRUE,
#'   details = detailsAR6)
#' }
#' @export
createVarListHtml <- function(
    x,
    outFileName,
    title = "List of Variables",
    htmlBefore = "",
    usePlus = FALSE,
    details = NULL
  ) {
  message("Creating the hierarchical list structure...")
  varList <- variablesAsList(x, entry = "INFO", usePlus = usePlus, details = details)
  outFileName <- normalizePath(outFileName, mustWork = FALSE)
  message("Creating HTML and writing it to ", outFileName, "...")
  renderVarListAsHtml(varList, outFileName, title, htmlBefore)
  message("Done.")
  return(invisible(NULL))
}
