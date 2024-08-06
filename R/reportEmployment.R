#' Computes the employment values (jobs) across different sectors
#' @param gdx A gdx file output from a REMIND run
#' @param improvements Either "None", "CEEW", "Dias", "Rutovitz_aus",
#' "Solar_found" or "All". Use "All" for all improvements.
#' @param subtype Subtype for how shares of solar rooftop, wind offshore,
#' and small hydro are assumed in the future. Options "current", "irena", and
#' "expert". See calcDspvShare for more information.
#' @param shareManf Either "current" or "local". Current implies current shares
#' of world manufacture remain same until 2050, current means that in 2050 all
#' countries manufacture required components locally.
#' @param multiplier controls how the regional multiplier for non-oecd countries
#' changes with time.
#' @param decline How should the employment factors change over time? "capcosts"
#' means according to capital costs. "static" means it doesn't change
#' @description This function returns a magpie object containing the reporting
#' the jobs for different technologies
#' @return A magpie object
#' @author Aman Malik
#' @examples
#' \dontrun{
#' reportEmployment(gdx, improvements = "All", multiplier = "own",
#' subtype = "expert", shareManf = "local", decline = "capcosts")
#' }
#' @importFrom madrat calcOutput
#' @importFrom magclass getNames add_columns getItems
#' @export

reportEmployment <- function(gdx, improvements, multiplier, subtype, shareManf, decline) {

  # Employment factors from mrremind
  x <- madrat::calcOutput("Employmentfactors", improvements = improvements,
                          multiplier = multiplier)
  x <- x[, , "HP", pmatch = TRUE, invert = TRUE] # excluding (at the moment) jobs in
  #combined heat and power plants
  getNames(x) <- gsub("Wind onshore", "Wind|Onshore", getNames(x))
  getNames(x) <- gsub("Wind offshore", "Wind|Offshore", getNames(x))

  ## DECLINE FACTORS------------------------
  if (decline == "capcosts") {
    # capital costs and OM fixed costs evolution over time for different techs, used to calculated the decline factor
    reportTech <- reportTechnology(gdx)
    reportTech <- collapseNames(reportTech)
    var <- c(
     "Tech|Electricity|Coal|Pulverised Coal w/o CC|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Gas|Combined Cycle w/o CC|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Hydro|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Nuclear|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Solar|PV|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Solar|CSP|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Wind|Onshore|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Wind|Offshore|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Biomass|Combined Heat and Power w/o CC|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Storage|Battery|For PV|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Oil|DOT|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Geothermal|Capital Costs (US$2005/kW)",
     "Tech|Electricity|Coal|Pulverised Coal w/o CC|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Gas|Combined Cycle w/o CC|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Hydro|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Nuclear|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Solar|PV|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Solar|CSP|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Wind|Onshore|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Wind|Offshore|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Biomass|Combined Heat and Power w/o CC|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Oil|DOT|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Geothermal|OM Cost|fixed (US$2005/kW/yr)",
     "Tech|Electricity|Storage|Battery|For PV|OM Cost|fixed (US$2005/kW/yr)",
     NULL
    )
    # only capital costs
    reportTech <- reportTech[, , var]
    capCosts <- reportTech[getItems(x, dim = 1), seq(2015, 2050, 5), "Capital Costs", pmatch = TRUE]
    capCosts <- capCosts[, , ] / setYears(capCosts[, "y2020", ], NULL) # costs relative to 2015

    # the job intensity decreases with capital costs for techs, relative to 2015
    varInX <- getItems(x, dim = 3.1) # variables from employment factor magpie object
    varInRem <- c("Coal", "Gas", "Nuclear", "Biomass", "Hydro", "Hydro", "Wind|Onshore",        "Wind|Offshore", "Solar|PV",
     "Geothermal", "Solar|CSP", "Oil", "Solar|PV", "Storage") # variables from remind (order matters)
    varComb <- paste(varInX, varInRem, sep = ".") # concatenating the two for looping to work

    for (i in varComb) {
      x[getItems(x, dim = 1), , sub("\\..*", "", i)][, , c("CI", "Manf")] <- x[getItems(x, dim = 1), , sub("\\..*", "", i)][, , c("CI", "Manf")] * setNames(capCosts[getItems(x, dim = 1), getYears(x), sub(".*\\.", "", i), pmatch = TRUE], NULL)
    }
    # OM fixed costs used to calculate  decline factors for O&M employment factors
    fixedCosts <- reportTech[getItems(x, dim = 1), seq(2015, 2050, 5), "fixed", pmatch = TRUE] # selecting years until 2050 and removing "World" region
    fixedCosts <- fixedCosts[, , ] / setYears(fixedCosts[, "y2020", ], NULL) # costs relative to 2020
    for (i in varComb) {
  x[getItems(x, dim = 1), , sub("\\..*", "", i)][, , "OM"] <- x[getItems(x, dim = 1), , sub("\\..*", "", i)][, , "OM"] * setNames(fixedCosts[getItems(x, dim = 1), getYears(x), sub(".*\\.", "", i), pmatch = TRUE], NULL)
    }
  # decline factors for coal fuel supply until 2050 are projected depending on
  # historical patterns and convergence between some countries.
  # See calcLabourProductivity for more information.
  coalEf <- calcOutput("CoalLabourProductivity", subtype = "Employment_factor")
  coalEf <- coalEf / setYears(coalEf[, 2020, ], NULL)
  coalEf[which(coalEf == "NaN")] <- 0
  x[, getYears(x), "Coal.Fuel_supply"] <- x[, getYears(x), "Coal.Fuel_supply"] * setNames(coalEf[, getYears(x), ], NULL)
  }

  ## --------------------------------------------------------

  ## Other external parameters--------------------------
  # share of rooftop in total spv, wind offshore in total wind, and hydro small in total hydro
  share <- calcOutput("DspvShare")

  ## New and Absolute capacity from REMIND---------------------------------
  # filtering required capacity variables i.e., only new and existing coal capacities
  remindOutput <- reportCapacity(gdx)
  remFilter <- remindOutput[, , c("Cumulative", "Idle", "Total Cap", "CC", "Hydrogen", "Liquids",
  "Estimated", "Non-Biomass", "For Wind", "For PV", "GT"), pmatch = TRUE, invert = TRUE] # removing cumulative and idle variables in capacity
  remFilter <- remFilter[, , c("Cap|Electricity (GW)", # total sum of all techs and not needed
        "New Cap|Electricity (GW)",
        "Cap|Electricity|Solar (GW)", # is sum of pv and csp and not needed
        "New Cap|Electricity|Solar (GW)"), invert = TRUE]
  # selecting variables only related to power or electricity
  remFilter <- remFilter[, , ][, , "Electricity", pmatch = TRUE]

  # filtering required regions
  regions <- setdiff(getItems(remindOutput, dim = 1), "GLO") # all regions except world
  remFilter <- remFilter[regions, seq(2015, 2050, 5), ]

  ## disaggregating certain variables/adding new variables
  colsToAdd <- c("Solar|PV-utility", "Solar|PV-rooftop", "Hydro-large", "Hydro-small")
  for (j in colsToAdd) {
    remFilter <- add_columns(remFilter, addnm = paste0("New Cap|Electricity|", j, " (GW/yr)"), dim = 3.1)
    remFilter <- add_columns(remFilter, addnm = paste0("Cap|Electricity|", j, " (GW)"), dim = 3.1)
  }

  # adding values to the new variables
remFilter[, , "New Cap|Electricity|Solar|PV-rooftop (GW/yr)"] <-  remFilter[, , "New Cap|Electricity|Solar|PV (GW/yr)"] * share[, , "spv"]
remFilter[, , "New Cap|Electricity|Solar|PV-utility (GW/yr)"] <-  remFilter[, , "New Cap|Electricity|Solar (GW/yr)"] * (1 - share[, , "spv"])

remFilter[, , "Cap|Electricity|Solar|PV-rooftop (GW)"] <- remFilter[, , "Cap|Electricity|Solar|PV (GW)"] * share[, , "spv"]
remFilter[, , "Cap|Electricity|Solar|PV-utility (GW)"] <- remFilter[, , "Cap|Electricity|Solar|PV (GW)"] * (1 - share[, , "spv"])

remFilter[, , "New Cap|Electricity|Hydro-small (GW/yr)"] <- remFilter[, , "New Cap|Electricity|Hydro (GW/yr)"] * share[, , "hydro"]
remFilter[, , "New Cap|Electricity|Hydro-large (GW/yr)"] <- remFilter[, , "New Cap|Electricity|Hydro (GW/yr)"] * (1 - share[, , "hydro"])

remFilter[, , "Cap|Electricity|Hydro-small (GW)"] <- remFilter[, , "Cap|Electricity|Hydro (GW)"] * share[, , "hydro"]
remFilter[, , "Cap|Electricity|Hydro-large (GW)"] <- remFilter[, , "Cap|Electricity|Hydro (GW)"] * (1 - share[, , "hydro"])

remFilter <- remFilter[, , c("New Cap|Electricity|Hydro", "Cap|Electricity|Hydro",
                             "New Cap|Electricity|Solar|PV", "Cap|Electricity|Solar|PV"), invert = TRUE]

  # added/installed capacity from REMIND, for years > 2010
  addedCap <- remFilter[regions, getYears(remFilter) > 2010, "New Cap", pmatch = TRUE] # only new capacities

  addedCapSum <- dimSums(addedCap, dim = 1) # world added capacity for each tech

  # existing/operating capacity from REMIND
  capTot <- remFilter[, , "New Cap", invert = TRUE, pmatch = TRUE] # only existing capacities

  #  share of exports to world installed capacity
  prodShare <- calcOutput("ProdShares")
  prodShare <- prodShare[, "y2019", ] # use 2019 as 2018 values have NAs for important countries
  colsToAdd <- c("Solar|PV-utility", "Solar|PV-rooftop", "Wind|Offshore", "Wind|Onshore")
  prodShare  <- add_columns(prodShare, addnm = colsToAdd, dim = 3.1)
  prodShare[, , c("Solar|PV-utility", "Solar|PV-rooftop")] <- prodShare[, , "spv"]
  prodShare[, , c("Wind|Offshore", "Wind|Onshore")] <- prodShare[, , "wind"]
  prodShare <- prodShare[, , c("spv", "wind"), invert = TRUE]
  ## -----------------------------------------------------

  # share of world capacity addition by region
  shrGLOAdd <- addedCap[, , colsToAdd, pmatch = TRUE] / addedCapSum[, , colsToAdd, pmatch = TRUE]
  prodShareTmp <- new.magpie(regions, c(2015, 2020, 2050), names = c("Solar|PV-utility", "Solar|PV-rooftop", "Wind|Offshore", "Wind|Onshore"))

  if (shareManf == "local" || shareManf == "current") {
    ## prod shares calculate local manufacture of wind and solar components in 2050. Change is linear.
    # assigning 2050 regional share values as prod shares
    prodShareTmp[, "y2050", ] <- as.numeric(shrGLOAdd[, "y2050", getNames(prodShareTmp), pmatch = TRUE])
    # 2015 and 2020 values same as 2019 values
    prodShareTmp[, c(2015, 2020), ] <- prodShare[, , getNames(prodShareTmp)]
    # from 2020 to 2050, linearly interpolate
    prodShare <- time_interpolate(prodShareTmp, seq(2025, 2045, 5), integrate_interpolated_years = TRUE)
    if (shareManf == "current") {
      prodShare[, , ] <- prodShare[, 2020, ] # shares remain same throughout = 2020
    }
  }
  # Obtaining fuel supply variables
  remindProd <- reportExtraction(gdx)["GLO", , , invert = TRUE] # remove GLO
  vars <- c("PE|Production|Biomass (EJ/yr)",
            "PE|Production|Gross|Coal (EJ/yr)",
            "PE|Production|Gross|Oil (EJ/yr)",
            "PE|Production|Gross|Gas (EJ/yr)",
            "PE|Production|Gross|Uranium [Energy] (EJ/yr)"
  )
  remindProd <- remindProd[regions, getYears(x), vars] # period >2010 and only select variables

  pe <- reportPE(gdx)[regions, getYears(x), "PE|Biomass|Traditional (EJ/yr)"]

  # removing the traditional biomass component, where biomass isn't sold, thus no employment
  # in conventional sense
  remindProd[, , "PE|Production|Biomass (EJ/yr)"] <-
  remindProd[, , "PE|Production|Biomass (EJ/yr)"] - setNames(pe[, , ], NULL)

  getNames(remindProd) <- gsub(pattern = "  \\(EJ\\/yr\\)", replacement = " \\(EJ\\/yr\\)", x = getNames(remindProd)) # removing space
  getNames(remindProd) <- gsub(pattern = "Uranium", replacement = "Nuclear", x = getNames(remindProd)) # removing space

  ## Calculating jobs--------------------------------
  techs <- c("Solar|PV-utility", "Solar|PV-rooftop", "Hydro-large", "Hydro-small",
             "Wind|Offshore", "Wind|Onshore", "Coal", "Biomass", "Gas", "Storage", "Oil", "Nuclear", "Geothermal", "Solar|CSP")
  techsExp <-  c("Solar|PV-utility", "Solar|PV-rooftop", "Wind|Offshore", "Wind|Onshore") # technologies with export component

  # 1. Manufacturing jobs for techs with export component.

  # 1a) Total manf jobs (per region) for these techs are equal to total world addition X share of
  # world exports (for that region)

  jobsManf <- new.magpie(getItems(x, dim = 1), getYears(x), techs, fill = 0)
  for (i in techsExp) {
    jobsManf[, , i] <- addedCapSum[, getYears(x), i, pmatch = TRUE] * 1000
    jobsManf[, getYears(x), i] <-  (jobsManf[, getYears(x), i] * setNames(x[, , i][, , "Manf"], NULL) * prodShare[, , i])
  }

  for (i in setdiff(techs, techsExp)) {
      jobsManf[, , i] <- addedCap[, getYears(x), i, pmatch = TRUE] * 1000
      jobsManf[, getYears(x), i] <- (jobsManf[, getYears(x), i] *
            setNames(x[, getYears(x), i, pmatch = TRUE][, , "Manf"],
                     NULL))
      }

  jobsManf <- add_dimension(jobsManf, dim = 3.2, add = "type", nm = "Manf")
  getSets(jobsManf) <- c("region", "year", "variable", "type")

  # 2. Construction and Installation jobs
  jobsCi <- new.magpie(regions, getYears(x), techs, fill = 0)


  for (i in techs) {
      jobsCi[, , i] <- addedCap[, getYears(x), i, pmatch = TRUE] * 1000
      jobsCi[, , i] <-  jobsCi[, getYears(x), i] *
        setNames(x[, , i, pmatch = TRUE][, , "CI"], nm = NULL)
    }

  jobsCi <- add_dimension(jobsCi, dim = 3.2, add = "type", nm = "CI")
  getSets(jobsCi) <- c("region", "year", "variable", "type")


  ## 3. Jobs in O & M
  # jobs = existing capacity per tech * emp factor per tech
  jobsOm <- new.magpie(regions, getYears(x), techs)
  for (i in techs) {
    jobsOm[, , i] <- capTot[, getYears(x), i, pmatch = TRUE] * 1000
    jobsOm[, , i] <-  jobsOm[, , i] *  setNames(x[, , i, pmatch = TRUE][, , "OM"], NULL)
  }
  jobsOm <- add_dimension(jobsOm, dim = 3.2, add = "type", nm = "OM")
  getSets(jobsOm) <- c("region", "year", "variable", "type")

  ## 4. Jobs in Fuel supply
  # jobs = total production (in EJ/yr) per fuel * employment factor per fuel (Jobs/PJ)
  fuels <- c("Coal", "Gas", "Biomass", "Oil")
  jobsProd <- new.magpie(regions, getYears(x), techs, fill = 0)
  for (i in fuels) {
    jobsProd[, , i] <- remindProd[, getYears(x), i, pmatch = TRUE] * 1000
    jobsProd[, , i] <-  jobsProd[, , i] * setNames(x[, , i][, , "Fuel_supply"], NULL)
  }
  jobsProd <- add_dimension(jobsProd, dim = 3.2, add = "type", nm = "Fuel_supply")
  getSets(jobsProd) <- c("region", "year", "variable", "type")

  ## Jobs in Fuel supply, nuclear which are given in terms of SE
  remSeNuc <- reportSE(gdx)[regions, getYears(x), "Nuclear", pmatch = TRUE] * 277777.778 # EJ to GWh
  jobsProd[, , "Nuclear.Fuel_supply"] <- setNames(remSeNuc[, , ], NULL) * setNames(x[, , "Nuclear.Fuel_supply"], NULL)

  ####### put all together #############

  jobs <- mbind(jobsCi, jobsOm, jobsProd, jobsManf)

  getSets(jobs)[3] <- "variable"
  return(jobs)
}
