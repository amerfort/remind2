#' Read in GDX and write *.mif reporting
#'
#' Read in all information from GDX file and create
#' the *.mif reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param gdx_ref reference-gdx for < cm_startyear, used for fixing the prices to this scenario
#' @param file name of the mif file which will be written, if no name is
#' provided a magpie object containing all the reporting information is
#' returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @param gdx_refpolicycost reference-gdx for policy costs, a GDX as created by readGDX, or the file name of a gdx
#' @param testthat boolean whether called by tests, turns some messages into warnings
#' @author Lavinia Baumstark
#' @examples
#'
#' \dontrun{convGDX2MIF(gdx,gdx_refpolicycost,file="REMIND_generic_default.csv",scenario="default")}
#'
#' @export
#' @importFrom dplyr %>% bind_rows filter
#' @importFrom gdx readGDX
#' @importFrom magclass mbind write.report
#' @importFrom piamInterfaces checkSummations checkVarNames
#' @importFrom piamutils deletePlus
#' @importFrom utils write.csv capture.output

convGDX2MIF <- function(gdx, gdx_ref = NULL, file = NULL, scenario = "default",
                        t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                        gdx_refpolicycost = gdx_ref, testthat = FALSE) {

  # Define region subsets ----
  regionSubsetList <- toolRegionSubsets(gdx)
  # ADD EU-27 region aggregation if possible
  if("EUR" %in% names(regionSubsetList)){
    regionSubsetList <- c(regionSubsetList,list(
      "EU27"=c("ENC","EWN","ECS","ESC","ECE","FRA","DEU","ESW")
    ))
  }

  # main reporting ----
  output <- NULL

  message("running reportMacroEconomy...")
  output <- mbind(output,reportMacroEconomy(gdx,regionSubsetList,t)[,t,])
  message("running reportTrade...")
  output <- mbind(output,reportTrade(gdx,regionSubsetList,t)[,t,])
  message("running reportPE...")
  output <- mbind(output,reportPE(gdx,regionSubsetList,t)[,t,])
  message("running reportSE...")
  output <- mbind(output,reportSE(gdx,regionSubsetList,t)[,t,])
  message("running reportFE...")
  output <- mbind(output,reportFE(gdx,regionSubsetList,t))
  message("running reportExtraction...")
  output <- mbind(output,reportExtraction(gdx,regionSubsetList,t)[,t,])
  message("running reportCapacity...")
  output <- mbind(output,reportCapacity(gdx,regionSubsetList,t)[,t,])
  #output <- mbind(output,reportLCOE(gdx)[,t,])     now moved to additional LCOE.mif file because many variables
  message("running reportCapitalStock...")
  output <- mbind(output,reportCapitalStock(gdx,regionSubsetList,t)[,t,])
  message("running reportEnergyInvestment...")
  output <- mbind(output,reportEnergyInvestment(gdx,regionSubsetList,t)[,t,])
  message("running reportEmiAirPol...")
  tmp <- try(reportEmiAirPol(gdx,regionSubsetList,t))  # test whether reportEmiAirPol works
  if (!inherits(tmp, "try-error")) {
    if (!is.null(tmp)) output <- mbind(output, tmp[, t, ])
  } else {
    message("function reportEmiAirPol does not work and is skipped")
  }

  # reporting of variables that need variables from different other report functions
  message("running reportEmi...")
  output <- mbind(output,reportEmi(gdx,output,regionSubsetList,t)[,t,])    # needs output from reportFE
  message("running reportTechnology...")
  output <- mbind(output,reportTechnology(gdx,output,regionSubsetList,t)[,t,])    # needs output from reportSE
  message("running reportPrices...")
  output <- mbind(output,reportPrices(gdx,output,regionSubsetList,t,gdx_ref = gdx_ref)[,t,]) # needs output from reportSE, reportFE, reportEmi, reportExtraction, reportMacroEconomy
  message("running reportCosts...")
  output <- mbind(output,reportCosts(gdx,output,regionSubsetList,t)[,t,])  # needs output from reportEnergyInvestment, reportPrices, reportEnergyInvestments
  message("running reportTax...")
  output <- mbind(output,reportTax(gdx,output,regionSubsetList,t)[,t,])

  # cross variables ----
  # needs variables from different other report* functions
  message("running reportCrossVariables...")
  output <- mbind(output,reportCrossVariables(gdx,output,regionSubsetList,t)[,t,])

  # policy costs, if possible and sensible ----
  if (is.null(gdx_refpolicycost)) {
    gdx_refpolicycost <- gdx
  }
  if (file.exists(gdx_refpolicycost)) {
    gdp_scen <- try(readGDX(gdx, "cm_GDPscen", react = "error"), silent = TRUE)
    gdp_scen_ref <- try(readGDX(gdx_refpolicycost, "cm_GDPscen", react = "error"), silent = TRUE)
    if (! inherits(gdp_scen, "try-error") && ! inherits(gdp_scen_ref, "try-error")) {
      if (gdp_scen[1] == gdp_scen_ref[1]) {
        if (gdx == gdx_refpolicycost) {
          msg_refpc <- "reporting 0 everywhere"
        } else {
          msg_refpc <- paste0("comparing to ", basename(dirname(gdx_refpolicycost)), "/", basename(gdx_refpolicycost), "...")
        }
        message("running reportPolicyCosts, ", msg_refpc)
        output <- mbind(output, reportPolicyCosts(gdx, gdx_refpolicycost, regionSubsetList, t)[,t,])
      } else {
        warning("The GDP scenario differs from that of the reference run. Did not execute 'reportPolicyCosts'! ",
                "If a policy costs reporting is desired, please use the 'policyCosts' output.R script.")
      }
    } else {
      warning("A comparison of the GDP scenarios between this run and its reference run wasn't possible (old remind version). ",
              "Therefore to avoid reporting unsensible policy costs, 'reportPolicyCosts' was not executed. ",
              "If a policy costs reporting is required, please use the 'policyCosts' output.R script.")
    }
  } else {
    warning(paste0("File ", gdx_refpolicycost, " not found. Did not execute 'reportPolicyCosts'! ",
            "If a policy costs reporting is desired, please use the 'policyCosts' output.R script."))
  }

  # SDP variables ----
  message("running reportSDPVariables...")
  tmp <- try(reportSDPVariables(gdx,output,t))  # test whether reportSDPVariables works
  if (!inherits(tmp, "try-error")) {
    if(!is.null(tmp)) output <- tmp
  } else {
    message("function reportSDPVariables does not work and is skipped")
  }

  # climate assessment variables ----
  message("running reportClimate...")
  output <- mbind(output, reportClimate(gdx, output))

  # clean and test output ----
  # Add dimension names "scenario.model.variable"
  getSets(output)[3] <- "variable"
  output <- add_dimension(output,dim=3.1,add = "model",nm = "REMIND")
  output <- add_dimension(output,dim=3.1,add = "scenario",nm = scenario)

  ## check variable names ----
  checkVarNames(getNames(output, dim = 3))

  ## summation checks ----
  .reportSummationErrors <- function(msg, testthat) {
    if (!any(grepl('All summation checks were fine', msg))) {
      msgtext <- paste(msg, collapse = '\n')
      if (isTRUE(testthat)) warning("### Analyzing ", basename(gdx), ":\n", msgtext) else message(msgtext)
    }
  }

  capture.output(
    sumChecks <- checkSummations(
      mifFile = output, dataDumpFile = NULL, outputDirectory = NULL,
      summationsFile = "extractVariableGroups",
      absDiff = 0.01, relDiff = 0.02, roundDiff = TRUE
    ),
    type = 'message') %>%
    .reportSummationErrors(testthat = testthat)

  capture.output(sumChecks <- checkSummations(
    mifFile = output, dataDumpFile = NULL, outputDirectory = NULL,
    summationsFile = system.file('extdata/additional_summation_checks.csv',
                                 package = 'remind2'),
    absDiff = 0.01, relDiff = 0.02, roundDiff = TRUE) %>%
      bind_rows(sumChecks),
    type = 'message'
  ) %>%
    .reportSummationErrors(testthat = testthat)

  ## range checks ----
  test_ranges(
        data = output,
        tests = list(
          list(
            "^Emi\\|CO2\\|Energy\\|Demand\\|Industry\\|.*Fossil \\(Mt CO2/yr\\)$",
            low = 0),
          list("Share.*\\((%|Percent)\\)$", low = 0, up = 100)),
        reaction = 'warning')

  # write or return output ----
  if (!is.null(file)) {
    write.report(output, file = file, ndigit = 7)
    # write same reporting without "+" or "++" in variable names
    deletePlus(file, writemif = TRUE)

    # write additional file on summation errors if needed
    if (nrow(sumChecks) > 0) {
      summation_errors_file <- sub('(\\.[^.]+)$', '_summation_errors.csv', file)
      warning("Summation checks have revealed some gaps! See file ",
              summation_errors_file)
      write.csv(sumChecks, summation_errors_file, quote = FALSE, row.names = FALSE)
    }
  }
  else {
    # return summation errors as attribute
    if (nrow(sumChecks) > 0) {
      warning("Summation checks have revealed some gaps! ",
              "See `summation_errors` attribute on output for details.")
      attr(output, 'summation_errors') <- sumChecks
    }
    return(output)
  }
}
