#' Read in GDX and calculate capacities, used in convGDX2MIF.R for the reporting
#'
#' Read in capacity information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @return MAgPIE object - contains the capacity variables
#' @author Lavinia Baumstark, Christoph Bertram
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{
#' reportCapacity(gdx)
#' }
#' @importFrom quitte calcCumulatedDiscount
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames getSets getSets<- as.magpie
#' @importFrom dplyr %>% filter mutate

reportCapacity <- function(gdx, regionSubsetList = NULL, t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {

  # read sets
  teall2rlf   <- readGDX(gdx, name = c("te2rlf", "teall2rlf"), format = "first_found")
  possibleRefineries <- c("refped", "refdip", "refliq")
  refineries <- intersect(teall2rlf[, 1], possibleRefineries)
  ttot        <- readGDX(gdx, name = "ttot")

  # read parameters
  pm_eta_conv <- readGDX(gdx, "pm_eta_conv", field = "l", restore_zeros = F)
  pm_prodCouple <- readGDX(gdx, c("pm_prodCouple", "p_prodCouple", "p_dataoc"), restore_zeros = FALSE, format = "first_found")

  # read variables
  vm_cap      <- readGDX(gdx, name = c("vm_cap"), field = "l", format = "first_found") * 1000
  vm_deltaCap <- readGDX(gdx, name = c("vm_deltaCap"), field = "l", format = "first_found") * 1000
  v_earlyreti <- readGDX(gdx, name = c("vm_capEarlyReti", "v_capEarlyReti", "v_earlyreti"), field = "l", format = "first_found")

  # read scalars
  sm_c_2_co2 <- as.vector(readGDX(gdx, "sm_c_2_co2"))


  # data preparation
  ttot <- as.numeric(as.vector(ttot))
  vm_cap      <- vm_cap[teall2rlf]
  vm_cap      <- vm_cap[, ttot, ]
  vm_deltaCap <- vm_deltaCap[teall2rlf]
  vm_deltaCap <- vm_deltaCap[, ttot, ]
  v_earlyreti <-   v_earlyreti[, ttot, ]
  t2005 <- ttot[ttot > 2004]

  ####### fix negative values of dataoc to 0 - using the lines from reportSE.R ##################
  #### adjust regional dimension of dataoc
  dataoc <- new.magpie(getRegions(vm_cap), getYears(pm_prodCouple), magclass::getNames(pm_prodCouple), fill = 0)
  dataoc[getRegions(pm_prodCouple), , ] <- pm_prodCouple
  getSets(dataoc) <- getSets(pm_prodCouple)

  dataoc[dataoc < 0] <- 0


  # build reporting
  tmp1 <- NULL
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , c("tnrs", "fnrs")], dim = 3),                                 "Cap|Electricity|Nuclear (GW)"))
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , c("spv", "csp")], dim = 3),                                   "Cap|Electricity|Solar (GW)"))
  if ("windoff" %in% magclass::getNames(vm_cap, dim = 1)) {
    tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , c("wind", "windoff")], dim = 3),                            "Cap|Electricity|Wind (GW)"))
  } else {
    tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , "wind"], dim = 3),                                          "Cap|Electricity|Wind (GW)"))
  }
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , "hydro"], dim = 3),                                           "Cap|Electricity|Hydro (GW)"))
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , "dot"], dim = 3),                                             "Cap|Electricity|Oil (GW)"))
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , c("igcc", "pc", "coalchp", "igccc")], dim = 3),               "Cap|Electricity|Coal (GW)"))
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , c("ngcc", "ngt", "gaschp", "ngccc")], dim = 3),               "Cap|Electricity|Gas (GW)"))
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , c("bioigccc", "biochp", "bioigcc")], dim = 3),                "Cap|Electricity|Biomass (GW)"))
  tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , "geohdr"], dim = 3),                                          "Cap|Electricity|Geothermal (GW)"))
  if (all(c("h2turbVRE", "h2turb") %in% magclass::getNames(vm_cap, dim = 1))) {
    tmp1 <- mbind(tmp1, setNames(dimSums(vm_cap[, , c("h2turb", "h2turbVRE")], dim = 3),                        "Cap|Electricity|Hydrogen (GW)"))
  }
  tmp1 <- mbind(tmp1, setNames(dimSums(tmp1, dim = 3),                                                          "Cap|Electricity (GW)"))

  tmp7 <- NULL
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("gash2c")], dim = 3),         "Cap|Hydrogen|Gas|w/ CC (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("gash2")], dim = 3),         "Cap|Hydrogen|Gas|w/o CC (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("gash2", "gash2c")], dim = 3),         "Cap|Hydrogen|Gas (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("coalh2c")], dim = 3),         "Cap|Hydrogen|Coal|w/ CC (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("coalh2")], dim = 3),         "Cap|Hydrogen|Coal|w/o CC (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("coalh2", "coalh2c")], dim = 3),         "Cap|Hydrogen|Coal (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("bioh2c")], dim = 3),         "Cap|Hydrogen|Biomass|w/ CC (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("bioh2")], dim = 3),         "Cap|Hydrogen|Biomass|w/o CC (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("bioh2", "bioh2c")], dim = 3),         "Cap|Hydrogen|Biomass (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("elh2", "elh2VRE")], dim = 3),         "Cap|Hydrogen|Electricity (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(vm_cap[, , c("elh2", "elh2VRE")], dim = 3) / pm_eta_conv[, , "elh2"],         "Cap (GWel)|Hydrogen|Electricity (GW)"))
  tmp7 <- mbind(tmp7, setNames(dimSums(tmp7, dim = 3),                      "Cap|Hydrogen (GW)"))

  tmp <- NULL
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "igccc"], dim = 3),                        "Cap|Electricity|Coal|IGCC|w/ CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "igcc"], dim = 3),                         "Cap|Electricity|Coal|IGCC|w/o CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "igccc"], dim = 3),                        "Cap|Electricity|Coal|w/ CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "coalchp"], dim = 3),                      "Cap|Electricity|Coal|CHP (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "ngccc"], dim = 3),                        "Cap|Electricity|Gas|CC|w/ CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "ngcc"], dim = 3),                         "Cap|Electricity|Gas|CC|w/o CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "ngccc"], dim = 3),                        "Cap|Electricity|Gas|w/ CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "ngt"], dim = 3),                          "Cap|Electricity|Gas|GT (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "gaschp"], dim = 3),                       "Cap|Electricity|Gas|CHP (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("igcc", "pc", "coalchp")], dim = 3),     "Cap|Electricity|Coal|w/o CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("ngcc", "ngt", "gaschp")], dim = 3),     "Cap|Electricity|Gas|w/o CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("bioigccc")], dim = 3),                  "Cap|Electricity|Biomass|w/ CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("biochp", "bioigcc")], dim = 3),         "Cap|Electricity|Biomass|w/o CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "biochp"], dim = 3),                       "Cap|Electricity|Biomass|CHP (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "dot"], dim = 3),                          "Cap|Electricity|Oil|w/o CC (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "spv"], dim = 3),                          "Cap|Electricity|Solar|PV (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "csp"], dim = 3),                          "Cap|Electricity|Solar|CSP (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("biochp", "gaschp", "coalchp")], dim = 3), "Cap|Electricity|CHP (GW)"))

  if ("windoff" %in% magclass::getNames(vm_cap, dim = 1)) {
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "wind"], dim = 3),                "Cap|Electricity|Wind|Onshore (GW)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "windoff"], dim = 3),             "Cap|Electricity|Wind|Offshore (GW)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "storwindoff"], dim = 3) * 1.2,   "Cap|Electricity|Storage|Battery|For Wind Offshore (GW)"))
  }
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "storspv"], dim = 3) * 4,         "Cap|Electricity|Storage|Battery|For PV (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "storwind"], dim = 3) * 1.2,      "Cap|Electricity|Storage|Battery|For Wind (GW)"))

  # heat
  tmp_chp <- NULL
  tmp_chp <- mbind(tmp_chp, setNames(dimSums(vm_cap[, , c("solhe")], dim = 3),                                                          "Cap|Heat|Solar (GW)"))
  tmp_chp <- mbind(tmp_chp, setNames(dimSums(vm_cap[, , c("geohe")], dim = 3),                                                          "Cap|Heat|Electricity|Heat Pump (GW)"))
  tmp_chp <- mbind(tmp_chp, setNames(dimSums(vm_cap[, , c("coalhp")], dim = 3)
                                     + dimSums(vm_cap[, , c("coalchp")] * dataoc[,,"pecoal.seel.coalchp.sehe"], dim = 3, na.rm = TRUE), "Cap|Heat|Coal (GW)"))
  tmp_chp <- mbind(tmp_chp, setNames(dimSums(vm_cap[, , c("biohp")], dim = 3)
                                     + dimSums(vm_cap[, , c("biochp")] * dataoc[,,"pebiolc.seel.biochp.sehe"], dim = 3, na.rm = TRUE),  "Cap|Heat|Biomass (GW)"))
  tmp_chp <- mbind(tmp_chp, setNames(dimSums(vm_cap[, , c("gashp")], dim = 3)
                                     + dimSums(vm_cap[, , c("gaschp")] * dataoc[,,"pegas.seel.gaschp.sehe"], dim = 3, na.rm = TRUE),    "Cap|Heat|Gas (GW)"))
  tmp_chp <- mbind(tmp_chp, setNames(dimSums(tmp_chp, dim = 3),                                                                         "Cap|Heat (GW)"))
  tmp <- mbind(tmp, tmp_chp)

  # gases
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("gastr", "coalgas", "biogas", "h22ch4")], dim = 3), "Cap|Gases (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("gastr")], dim = 3), "Cap|Gases|Natural Gas (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("coalgas")], dim = 3), "Cap|Gases|Coal (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("biogas")], dim = 3), "Cap|Gases|Biomass (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("h22ch4")], dim = 3), "Cap|Gases|Hydrogen (GW)"))

  # liquids
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("refliq", "bioftrec", "bioftcrec", "coalftrec", "coalftcrec", "MeOH","biodiesel", "bioeths", "bioethl")], dim = 3), "Cap|Liquids (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("refliq")], dim = 3), "Cap|Liquids|Oil (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("bioftrec", "bioftcrec","biodiesel", "bioeths", "bioethl")], dim = 3), "Cap|Liquids|Biomass (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("coalftrec", "coalftcrec")], dim = 3), "Cap|Liquids|Coal (GW)"))
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("MeOH")], dim = 3), "Cap|Liquids|Hydrogen (GW)"))

  # carbon management
  if ("dac" %in% magclass::getNames(vm_cap, dim = 1)) {
  tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("dac")], dim = 3)*sm_c_2_co2, "Cap|Carbon Management|DAC (Mt CO2/yr)"))
  }

  # Newly built capacities electricity (Should all go into tmp2, so that this can be used for calculating cumulated values in tmp5 below)
  tmp2 <- NULL
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("ngcc", "ngt", "gaschp", "ngccc")], dim = 3),            "New Cap|Electricity|Gas (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("igccc", "igcc", "pc", "coalchp")], dim = 3),            "New Cap|Electricity|Coal (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("bioigccc", "biochp", "bioigcc")], dim = 3),             "New Cap|Electricity|Biomass (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("geohdr", "hydro", "spv", "csp", "wind")], dim = 3),       "New Cap|Electricity|Non-Biomass Renewables (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("tnrs", "fnrs")], dim = 3),  "New Cap|Electricity|Nuclear (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "dot"], dim = 3),                      "New Cap|Electricity|Oil (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(tmp2, dim = 3),              "New Cap|Electricity (GW/yr)"))

  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "geohdr"], dim = 3),         "New Cap|Electricity|Geothermal (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "igccc"], dim = 3),          "New Cap|Electricity|Coal|IGCC|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "igcc"], dim = 3),           "New Cap|Electricity|Coal|IGCC|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "ngccc"], dim = 3),          "New Cap|Electricity|Gas|CC|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "ngcc"], dim = 3),           "New Cap|Electricity|Gas|CC|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "spv"], dim = 3),            "New Cap|Electricity|Solar|PV (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "csp"], dim = 3),            "New Cap|Electricity|Solar|CSP (GW/yr)"))

  if ("windoff" %in% magclass::getNames(vm_deltaCap, dim = 1)) {
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "wind"], dim = 3),           "New Cap|Electricity|Wind|Onshore (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "windoff"], dim = 3),        "New Cap|Electricity|Wind|Offshore (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("wind", "windoff")], dim = 3),    "New Cap|Electricity|Wind (GW/yr)"))
  } else {
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "wind"], dim = 3),           "New Cap|Electricity|Wind (GW/yr)"))
  }
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "hydro"], dim = 3),         "New Cap|Electricity|Hydro (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "ngccc"], dim = 3),          "New Cap|Electricity|Gas|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("ngcc", "ngt", "gaschp")], dim = 3), "New Cap|Electricity|Gas|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "ngt"], dim = 3),            "New Cap|Electricity|Gas|GT (GW/yr)"))
  if (all(c("h2turbVRE", "h2turb") %in% magclass::getNames(vm_cap, dim = 1))) {
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("h2turb", "h2turbVRE")], dim = 3),            "New Cap|Electricity|Hydrogen (GW/yr)"))
  }
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "igccc"], dim = 3),                    "New Cap|Electricity|Coal|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("igcc", "pc", "coalchp")], dim = 3), "New Cap|Electricity|Coal|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "bioigccc"], dim = 3),                 "New Cap|Electricity|Biomass|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("biochp", "bioigcc")], dim = 3),     "New Cap|Electricity|Biomass|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("spv", "csp")], dim = 3),           "New Cap|Electricity|Solar (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "storspv"], dim = 3) * 4,            "New Cap|Electricity|Storage|Battery|For PV (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "storwind"], dim = 3) * 1.2,         "New Cap|Electricity|Storage|Battery|For Wind (GW/yr)"))
  # Newly built capacities hydrogen
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("bioh2c", "bioh2")], dim = 3),   "New Cap|Hydrogen|Biomass (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("elh2", "elh2VRE")], dim = 3),                "New Cap|Hydrogen|Electricity (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("elh2", "elh2VRE")], dim = 3) / pm_eta_conv[, , "elh2"], "New Cap (GWel)|Hydrogen|Electricity (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gash2c", "coalh2c", "gash2", "coalh2")], dim = 3), "New Cap|Hydrogen|Fossil (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("bioh2c", "bioh2", "elh2", "elh2VRE", "gash2c", "coalh2c", "gash2", "coalh2")], dim = 3), "New Cap|Hydrogen (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "bioh2c"], dim = 3),              "New Cap|Hydrogen|Biomass|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "bioh2"], dim = 3),               "New Cap|Hydrogen|Biomass|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gash2c", "coalh2c")], dim = 3), "New Cap|Hydrogen|Fossil|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gash2", "coalh2")], dim = 3),   "New Cap|Hydrogen|Fossil|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gash2")], dim = 3),   "New Cap|Hydrogen|Gas|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gash2c")], dim = 3),   "New Cap|Hydrogen|Gas|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("coalh2")], dim = 3),   "New Cap|Hydrogen|Coal|w/o CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("coalh2c")], dim = 3),   "New Cap|Hydrogen|Coal|w/ CC (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gash2c", "gash2")], dim = 3),   "New Cap|Hydrogen|Gas (GW/yr)"))
  tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("coalh2c", "coalh2")], dim = 3),   "New Cap|Hydrogen|Coal (GW/yr)"))

  # heat capacity additions
  tmp_chpAdd <- NULL
  tmp_chpAdd <- mbind(tmp_chpAdd, setNames(dimSums(vm_deltaCap[, , c("solhe")], dim = 3),                                                       "New Cap|Heat|Solar (GW/yr)"))
  tmp_chpAdd <- mbind(tmp_chpAdd, setNames(dimSums(vm_deltaCap[, , c("geohe")], dim = 3),                                                       "New Cap|Heat|Electricity|Heat Pump (GW/yr)"))
  tmp_chpAdd <- mbind(tmp_chpAdd, setNames(dimSums(vm_deltaCap[, , c("coalhp")], dim = 3)
                                           + dimSums(vm_deltaCap[, , c("coalchp")] * dataoc[,,"pecoal.seel.coalchp.sehe"], dim = 3, na.rm = T), "New Cap|Heat|Coal (GW/yr)"))
  tmp_chpAdd <- mbind(tmp_chpAdd, setNames(dimSums(vm_deltaCap[, , c("biohp")], dim = 3)
                                           + dimSums(vm_deltaCap[, , c("biochp")] * dataoc[,,"pebiolc.seel.biochp.sehe"], dim = 3, na.rm = T),  "New Cap|Heat|Biomass (GW/yr)"))
  tmp_chpAdd <- mbind(tmp_chpAdd, setNames(dimSums(vm_deltaCap[, , c("gashp")], dim = 3)
                                           + dimSums(vm_deltaCap[, , c("gaschp")] * dataoc[,,"pegas.seel.gaschp.sehe"], dim = 3, na.rm = T),    "New Cap|Heat|Gas (GW/yr)"))
  tmp_chpAdd <- mbind(tmp_chpAdd, setNames(dimSums(tmp_chpAdd, dim = 3),                                                                        "New Cap|Heat (GW/yr)"))
  tmp2 <- mbind(tmp2, tmp_chpAdd)

    # Newly built capacities liquids
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c(refineries, "coalftrec", "coalftcrec", "bioftrec", "bioftcrec", "biodiesel", "bioeths", "bioethl", "MeOH")], dim = 3),
        "New Cap|Liquids (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c(refineries, "coalftrec", "coalftcrec")], dim = 3),
        "New Cap|Liquids|Fossil (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("coalftrec", "coalftcrec")], dim = 3),
                                 "New Cap|Liquids|Coal (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , refineries], dim = 3),
                                 "New Cap|Liquids|Oil (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("bioftrec", "bioftcrec", "biodiesel", "bioeths", "bioethl")], dim = 3),
        "New Cap|Liquids|Biomass (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("MeOH")], dim = 3),
        "New Cap|Liquids|Hydrogen (GW/yr)"))


    # Newly built capacities gases
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gastr", "coalgas", "biogas", "h22ch4")], dim = 3),
        "New Cap|Gases (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("gastr", "coalgas")], dim = 3),
        "New Cap|Gases|Fossil (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "gastr"], dim = 3),
                                 "New Cap|Gases|Gas (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "coalgas"], dim = 3),
                                 "New Cap|Gases|Coal (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("biogas")], dim = 3),
        "New Cap|Gases|Biomass (GW/yr)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("h22ch4")], dim = 3),
        "New Cap|Gases|Hydrogen (GW/yr)"))

    # carbon management
    if ("dac" %in% magclass::getNames(vm_cap, dim = 1)) {
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("dac")], dim = 3)*sm_c_2_co2, "New Cap|Carbon Management|DAC (Mt CO2/yr/yr)"))
    }


  # add terms calculated from previously calculated capacity values
  tmp_aux <- NULL
  tmp_aux <- mbind(tmp_aux, setNames(dimSums(tmp[, , c("Cap|Electricity|Storage|Battery|For PV (GW)", "Cap|Electricity|Storage|Battery|For Wind (GW)")], dim = 3),   "Cap|Electricity|Storage|Battery (GW)"))
  tmp <- mbind(tmp, tmp_aux)

  tmp_aux <- NULL
  names_capacities <- c("Cap|Electricity|Gas (GW)",
                        "Cap|Electricity|Nuclear (GW)",
                        "Cap|Electricity|Coal (GW)",
                        "Cap|Electricity|Biomass (GW)",
                        "Cap|Electricity|Hydrogen (GW)",
                        "Cap|Electricity|Geothermal (GW)",
                        "Cap|Electricity|Oil (GW)")
  names_capacities <- intersect(names_capacities, getNames(tmp1))

  tmp_aux <- mbind(tmp_aux, setNames(dimSums(tmp1[, , names_capacities], dim = 3) + 0.6 * tmp1[, , "Cap|Electricity|Hydro (GW)"] + tmp[, , "Cap|Electricity|Storage|Battery (GW)"],
                        "Cap|Electricity|Estimated firm capacity counting hydro at 0p6 (GW)"))
  tmp1 <- mbind(tmp1, tmp_aux)


  tmp_aux <- NULL
  tmp_aux <- mbind(tmp_aux, setNames(dimSums(tmp2[, , c("New Cap|Electricity|Storage|Battery|For PV (GW/yr)", "New Cap|Electricity|Storage|Battery|For Wind (GW/yr)")], dim = 3),   "New Cap|Electricity|Storage|Battery (GW/yr)"))
  tmp2 <- mbind(tmp2, tmp_aux)

  # Idle capacities and Total (sum of operating and idle)
  tmp4 <- NULL
  tmp4 <- mbind(tmp4, setNames(dimSums(vm_cap[, , "igcc"], dim = 3) * v_earlyreti[, , "igcc"] / (1 - v_earlyreti[, , "igcc"]) +
    dimSums(vm_cap[, , "coalchp"], dim = 3) * v_earlyreti[, , "coalchp"] / (1 - v_earlyreti[, , "coalchp"]) +
    dimSums(vm_cap[, , "pc"], dim = 3) * v_earlyreti[, , "pc"] / (1 - v_earlyreti[, , "pc"]),
  "Idle Cap|Electricity|Coal|w/o CC (GW)"))
  tmp4 <- mbind(tmp4, setNames(dimSums(vm_cap[, , "ngcc"], dim = 3) * v_earlyreti[, , "ngcc"] / (1 - v_earlyreti[, , "ngcc"]) +
    dimSums(vm_cap[, , "gaschp"], dim = 3) * v_earlyreti[, , "gaschp"] / (1 - v_earlyreti[, , "gaschp"]) +
    dimSums(vm_cap[, , "ngt"], dim = 3) * v_earlyreti[, , "ngt"] / (1 - v_earlyreti[, , "ngt"]),
  "Idle Cap|Electricity|Gas|w/o CC (GW)"))
  tmp4 <- mbind(tmp4, setNames(dimSums(vm_cap[, , "dot"], dim = 3) * v_earlyreti[, , "dot"] / (1 - v_earlyreti[, , "dot"]),
    "Idle Cap|Electricity|Oil|w/o CC (GW)"))
  tmp4 <- mbind(tmp4, setNames(tmp4[, , "Idle Cap|Electricity|Coal|w/o CC (GW)"] + tmp[, , "Cap|Electricity|Coal|w/o CC (GW)"],
    "Total Cap|Electricity|Coal|w/o CC (GW)"))
  tmp4 <- mbind(tmp4, setNames(tmp4[, , "Idle Cap|Electricity|Gas|w/o CC (GW)"] + tmp[, , "Cap|Electricity|Gas|w/o CC (GW)"],
    "Total Cap|Electricity|Gas|w/o CC (GW)"))
  # Cumulate things on extensive time set
  tmp <- mbind(tmp, tmp7, tmp1, tmp2, tmp4)

  # Cumulative capacities = cumulating new capacities, starting with 0 in 2005
  tmp6 <- tmp2[, t2005, ]
  getSets(tmp6)[3] <- "variable"
  tmp6 <- quitte::as.quitte(tmp6)
  mylist <- lapply(levels(tmp6$variable), function(x) {
    calcCumulatedDiscount(data = tmp6 %>%
      filter(.data$variable == x),
    nameVar = x,
    discount = 0.0) %>%
      mutate(variable = gsub("New", replacement = "Cumulative", x))
  })


  tmp6 <- do.call("rbind", mylist)
  tmp6 <- as.magpie(quitte::as.quitte(tmp6))
  magclass::getNames(tmp6) <- paste0(magclass::getNames(tmp6), " (GW)")



  magclass::getNames(tmp6)[grep("Cumulative Cap\\|Carbon Management", getNames(tmp6))] <- gsub("GW","Mt CO2/yr",
                                                                                               magclass::getNames(tmp6)[grep("Cumulative Cap\\|Carbon Management", getNames(tmp6))])


  tmp <- mbind(tmp[, t2005, ], tmp6)
  # add global values
  tmp <- mbind(tmp, dimSums(tmp, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    tmp <- mbind(tmp, calc_regionSubset_sums(tmp, regionSubsetList))

  getSets(tmp)[3] <- "variable"
  return(tmp)
}
