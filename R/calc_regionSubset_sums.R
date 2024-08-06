#' Calculate Sums for Region Subsets
#'
#' Sum up values in `data` for all sets of regions in `regionSubsetList`.
#
#' @param data A [`MAgPIE`][magclass::magclass] object.
#' @param regionSubsetList A list of region subsets to calculate of the form
#'   `list(subset_name = c(region_A, region_B, ...)`
#' @md
#' @return A [`MAgPIE`][magclass::magclass] object.
#' @author Michaja Pehl
#'
#' @importFrom magclass mbind getItems<-
#'
#'
#' @examples
#' calc_regionSubset_sums(population_magpie, list(xAM = c('LAM', 'NAM')))

#' @export
calc_regionSubset_sums <- function(data, regionSubsetList) { #nolint
  if (any(is.null(regionSubsetList), is.null(data)))
    return(NULL)

  mbind(
    lapply(
      names(regionSubsetList),
      function(subsetName) {
        x <- dimSums(data[regionSubsetList[[subsetName]], , ], dim = 1)
        getItems(x, dim = 1) <- subsetName
        return(x)
      }
    )
  )
}
