#' Read in GDX and calculate secondary energy, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in secondary energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Gunnar Luderer, Lavinia Baumstark, Felix Schreyer, Falk Benke
#' @examples
#' \dontrun{
#' reportSE(gdx)
#' }
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mselect getSets getSets<- getYears getNames<- mbind
#' @importFrom abind abind
#' @importFrom rlang sym

reportSE <- function(gdx, regionSubsetList = NULL, t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {

  ####### get realisations #########
  module2realisation <- readGDX(gdx, "module2realisation")
  rownames(module2realisation) <- module2realisation$modules

  ####### get power realisations #########
  realisation <- readGDX(gdx, "module2realisation")
  power_realisation <- if ("power" %in% realisation[, 1]) realisation[which(realisation[, 1] == "power"), 2]

  ####### conversion factors ##########
  pm_conv_TWa_EJ <- 31.536
  ####### read in needed data #########
  ## sets
  pe2se    <- readGDX(gdx, "pe2se")
  se2se    <- readGDX(gdx, "se2se")
  all_te   <- readGDX(gdx, "all_te")
  te       <- readGDX(gdx, "te")
  tefosccs <- readGDX(gdx, c("teFosCCS", "tefosccs"), format = "first_found")
  teccs    <- readGDX(gdx, c("teCCS", "teccs"), format = "first_found")
  tenoccs  <- readGDX(gdx, c("teNoCCS", "tenoccs"), format = "first_found")
  techp    <- readGDX(gdx, c("teChp", "techp"), format = "first_found")
  terenew_nobio <- readGDX(gdx, c("teReNoBio", "terenew_nobio"), format = "first_found")
  pebio    <- readGDX(gdx, c("peBio", "pebio"), format = "first_found")
  entySe   <- readGDX(gdx, c("entySe", "sety"), format = "first_found")
  entyPe   <- readGDX(gdx, c("entyPe", "pety"), format = "first_found")
  oc2te    <- readGDX(gdx, c("pc2te", "oc2te"), format = "first_found")

  se_Liq    <- intersect(c("seliqfos", "seliqbio", "seliqsyn"), entySe)
  se_Gas    <- intersect(c("segafos", "segabio", "segasyn"), entySe)
  se_Solids <- intersect(c("sesofos", "sesobio"), entySe)

  # Gases and Liquids can also be made from H2 via CCU
  input_gas <- c(entyPe, "seh2")
  input_liquids <- c(entyPe, "seh2")

  ## parameter
  pm_prodCouple <- readGDX(gdx, c("pm_prodCouple", "p_prodCouple", "p_dataoc"), restore_zeros = FALSE, format = "first_found")
  pm_prodCoupleEmi <- readGDX(gdx, "pm_prodCoupleEmi", restore_zeros = FALSE, react = "silent")
  pm_prodCouple <- mbind(pm_prodCouple, pm_prodCoupleEmi)
  pm_prodCouple[is.na(pm_prodCouple)] <- 0

  p_macBase <- readGDX(gdx, c("p_macBaseMagpie", "pm_macBaseMagpie","p_macBase"), format = "first_found")
  #  p_macEmi  <- readGDX(gdx,"p_macEmi")
  ## variables
  vm_prodSe <- readGDX(gdx, name = c("vm_prodSe", "v_seprod"), field = "l", restore_zeros = FALSE, format = "first_found") * pm_conv_TWa_EJ
  vm_prodSe <- mselect(vm_prodSe, all_enty1 = entySe)

  #  storloss only exist for versions previous to the power module creation and for the IntC and DTcoup power module realisation
  if (power_realisation %in% c("IntC", "DTcoup")) {
    storLoss <- readGDX(gdx, name = c("v32_storloss", "v_storloss"), field = "l", restore_zeros = TRUE, format = "first_found") * pm_conv_TWa_EJ
    # calculate minimal temporal resolution #####
    y <- Reduce(intersect, list(getYears(vm_prodSe), getYears(storLoss)))
  } else { # RLDC power module
    storLoss <- NULL
    y <- getYears(vm_prodSe)
  }

  vm_macBase <- readGDX(gdx, name = c("vm_macBase"), field = "l", restore_zeros = FALSE, format = "first_found") * pm_conv_TWa_EJ
  vm_macBase <- vm_macBase[, y, ]
  vm_emiMacSector <- readGDX(gdx, name = c("vm_emiMacSector"), field = "l", restore_zeros = FALSE, format = "first_found") * pm_conv_TWa_EJ
  vm_emiMacSector <-   vm_emiMacSector[, y, ]
  ####### set temporal resolution #####
  vm_prodSe    <- vm_prodSe[, y, ]
  storLoss  <- storLoss[, y, ]
  p_macBase <- p_macBase[, y, ]
  ####### fix negative values to 0 ##################
  #### adjust regional dimension of dataoc
  dataoc <- new.magpie(getRegions(vm_prodSe), getYears(pm_prodCouple), magclass::getNames(pm_prodCouple), fill = 0)
  dataoc[getRegions(pm_prodCouple), , ] <- pm_prodCouple
  getSets(dataoc) <- getSets(pm_prodCouple)

  dataoc[dataoc < 0] <- 0
  ###### include se2se technologies in summation
  te_pese2se <- c(pe2se$all_te, se2se$all_te)

  ####### internal function for reporting ###########

  se.prod <- function(vm_prodSe, dataoc, oc2te, entySe, enty.input, se.output, te = te_pese2se,
                      name = NULL, storageLoss = storLoss, all_pety = entyPe, storageLossOnly = F) {

    if (storageLossOnly && is.null(storageLoss)) {
      return(NULL)
    }

    # test if inputs make sense
    if (length(setdiff(enty.input, abind::abind(all_pety, entySe))) > 0) {
      warning(paste("Input energy enty.input ", setdiff(enty.input, abind::abind(all_pety, entySe)), " is not element of entyPe or entySe"))
    }

    if (length(setdiff(se.output, abind::abind(all_pety, entySe))) > 0) {
      warning(paste("se.output ", setdiff(se.output, abind::abind(all_pety, entySe)), " is not element of entyPe or entySe"))
    }

    if (storageLossOnly == F) {
      ## secondary energy production with secarrier as a main product
      x1 <- dimSums(mselect(vm_prodSe, all_enty = enty.input, all_enty1 = se.output, all_te = te), dim = 3, na.rm = T)
      ## secondary energy production with secarrier as a couple product
      ## identify all oc techs with secarrier as a couple product
      sub_oc2te <- oc2te[(oc2te$all_enty %in% enty.input) & (oc2te$all_enty1 %in% entySe) & (oc2te$all_enty2 %in% se.output) & (oc2te$all_te %in% te), ]
      x2 <- dimSums(vm_prodSe[sub_oc2te] * dataoc[sub_oc2te], dim = 3, na.rm = T)
    }

    ## storage losses
    input.pe2se <- pe2se[(pe2se$all_enty %in% enty.input) & (pe2se$all_enty1 %in% se.output) & (pe2se$all_te %in% te), ]
    if ((nrow(input.pe2se) == 0) || (is.null(storageLoss))) {
      x3 <- 0
    } else {
      x3 <- dimSums(storageLoss[input.pe2se], dim = 3, na.rm = T)
    }

    if (storageLossOnly) {
      out <- x3
    } else {
      out <- (x1 + x2 - x3)
    }

    if (!is.null(name)) magclass::getNames(out) <- name
    return(out)
  }

  se.prodLoss <- function(vm_prodSe, dataoc, oc2te, entySe, enty.input, se.output, te = te_pese2se,
                          name = NULL, storageLoss = storLoss, all_pety = entyPe) {
    return(se.prod(vm_prodSe, dataoc, oc2te, entySe, enty.input, se.output, te, name, storageLoss, all_pety, storageLossOnly = T))
  }

  ## reporting should adhere to the following logic:
  ## if a category has more than one subcategory, the subcategories should be reported *explicitly*.

  tmp1 <- se.prod(vm_prodSe, dataoc, oc2te, entySe, abind(entyPe, entySe), entySe, name = "SE (EJ/yr)")

  ## Biomass
  tmp1 <- mbind(tmp1,
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, entySe, name = "SE|Biomass (EJ/yr)")
  )

  ## Electricity
  tmp1 <- mbind(tmp1,
    se.prod(vm_prodSe, dataoc, oc2te, entySe, append(entyPe, "seh2"), "seel",   name = "SE|Electricity (EJ/yr)"), # seh2 to account for se2se production once we add h2 to elec technology
    se.prod(vm_prodSe, dataoc, oc2te, entySe, entyPe, "seel", te = techp,       name = "SE|Electricity|Combined Heat and Power w/o CC (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seel",                    name = "SE|Electricity|+|Biomass (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seel", te = teccs,        name = "SE|Electricity|Biomass|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seel", te = tenoccs,      name = "SE|Electricity|Biomass|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seel", te = "bioigccc",   name = "SE|Electricity|Biomass|++|Gasification Combined Cycle w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seel", te = "bioigcc",    name = "SE|Electricity|Biomass|++|Gasification Combined Cycle w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seel", te = "biochp",     name = "SE|Electricity|Biomass|++|Combined Heat and Power w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seel", te = setdiff(pe2se$all_te, c("bioigccc", "bioigcc", "biochp")),
                                                                                name = "SE|Electricity|Biomass|++|Other (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel",                 name = "SE|Electricity|+|Coal (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel", te = teccs,     name = "SE|Electricity|Coal|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel", te = tenoccs,   name = "SE|Electricity|Coal|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel", te = "igcc",    name = "SE|Electricity|Coal|++|Gasification Combined Cycle w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel", te = "igccc",   name = "SE|Electricity|Coal|++|Gasification Combined Cycle w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel", te = "pc",      name = "SE|Electricity|Coal|++|Pulverised Coal w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel", te = "coalchp", name = "SE|Electricity|Coal|++|Combined Heat and Power w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seel", te = setdiff(pe2se$all_te, c("igcc", "igccc", "pc", "coalchp")),
                                                                                name = "SE|Electricity|Coal|++|Other (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seel",                  name = "SE|Electricity|+|Gas (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seel", te = teccs,      name = "SE|Electricity|Gas|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seel", te = tenoccs,    name = "SE|Electricity|Gas|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seel", te = "ngcc",     name = "SE|Electricity|Gas|++|Combined Cycle w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seel", te = "ngccc",    name = "SE|Electricity|Gas|++|Combined Cycle w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seel", te = "ngt",      name = "SE|Electricity|Gas|++|Gas Turbine (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seel", te = "gaschp",   name = "SE|Electricity|Gas|++|Combined Heat and Power w/o CC (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "seh2", "seel",                   name = "SE|Electricity|+|Hydrogen (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "peoil", "seel",                  name = "SE|Electricity|+|Oil (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "peoil", "seel", te = tenoccs,    name = "SE|Electricity|Oil|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "peoil", "seel", te = "dot",      name = "SE|Electricity|Oil|DOT (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, entyPe, "seel", te = terenew_nobio,
                                                                                name = "SE|Electricity|Non-Biomass Renewables (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "peur", "seel",                   name = "SE|Electricity|+|Nuclear (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegeo", "seel",                  name = "SE|Electricity|+|Geothermal (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pehyd", "seel",                  name = "SE|Electricity|+|Hydro (EJ/yr)"),

    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pesol", "seel",                  name = "SE|Electricity|+|Solar (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pesol", "seel", te = "csp",      name = "SE|Electricity|Solar|+|CSP (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pesol", "seel", te = "spv",      name = "SE|Electricity|Solar|+|PV (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pewin", "pesol"), "seel",      name = "SE|Electricity|WindSolar (EJ/yr)"),

    se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, c("pewin", "pesol"), "seel",  name = "SE|Electricity|Curtailment (EJ/yr)"),
    se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, "pesol", "seel",              name = "SE|Electricity|Curtailment|+|Solar (EJ/yr)"),
    se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, "pesol", "seel", te = "csp",  name = "SE|Electricity|Curtailment|Solar|+|CSP (EJ/yr)"),
    se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, "pesol", "seel", te = "spv",  name = "SE|Electricity|Curtailment|Solar|+|PV (EJ/yr)")
  )

  if ("windoff" %in% te) {
    tmp1 <- mbind(tmp1,
      se.prod(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = "wind",       name = "SE|Electricity|Wind|+|Onshore (EJ/yr)"),
      se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = "wind",   name = "SE|Electricity|Curtailment|Wind|+|Onshore (EJ/yr)"),
      se.prod(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = "windoff",    name = "SE|Electricity|Wind|+|Offshore (EJ/yr)"),
      se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = "windoff",
                                                                                    name = "SE|Electricity|Curtailment|Wind|+|Offshore (EJ/yr)"),
      se.prod(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = c("wind","windoff"),
                                                                                    name = "SE|Electricity|+|Wind (EJ/yr)"),
      se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = c("wind","windoff"),
                                                                                    name = "SE|Electricity|Curtailment|+|Wind (EJ/yr)")
    )
  } else {
    tmp1 <- mbind(tmp1,
      se.prod(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = "wind",       name = "SE|Electricity|+|Wind (EJ/yr)"),
      se.prod(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = "wind",       name = "SE|Electricity|Wind|+|Onshore (EJ/yr)"),
      se.prodLoss(vm_prodSe, dataoc, oc2te, entySe, "pewin", "seel", te = "wind",   name = "SE|Electricity|Curtailment|+|Wind (EJ/yr)")
    )
  }

  ## Gases
  if (!(is.null(vm_macBase) & is.null(vm_emiMacSector))) {
    ## exogenous variable for representing reused gas from waste landfills (accounted in the model as segabio)
    MtCH4_2_TWa <- readGDX(gdx, "sm_MtCH4_2_TWa", react = "silent")
    if (is.null(MtCH4_2_TWa)) {
      MtCH4_2_TWa <- 0.001638
    }
    tmp1 <- mbind(tmp1,
      setNames(MtCH4_2_TWa * (vm_macBase[, , "ch4wstl"] - vm_emiMacSector[, , "ch4wstl"]), "SE|Gases|Biomass|Waste (EJ/yr)")
    )
  } else {
    tmp1 <- mbind(tmp1,
      setNames(new.magpie(cells_and_regions = getRegions(dataoc), years = y, fill = 0), "SE|Gases|Biomass|Waste (EJ/yr)")
    )
  }

  tmp1 <- mbind(tmp1,
    se.prod(vm_prodSe, dataoc, oc2te, entySe, input_gas, se_Gas,                name = "SE|Gases (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Gas,                    name = "SE|Gases|+|Biomass (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Gas, te = teccs,        name = "SE|Gases|Biomass|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Gas, te = tenoccs,      name = "SE|Gases|Biomass|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "seh2", se_Gas,                   name = "SE|Gases|+|Hydrogen (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal"), se_Gas,     name = "SE|Gases|+|Fossil (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", se_Gas,                  name = "SE|Gases|Fossil|+|Natural Gas (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", se_Gas,                 name = "SE|Gases|Fossil|+|Coal (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", se_Gas, te = teccs,     name = "SE|Gases|Fossil|Coal|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", se_Gas, te = tenoccs,   name = "SE|Gases|Fossil|Coal|+|w/o CC (EJ/yr)")
  )

  ## Heat
  tmp1 <- mbind(tmp1,
    se.prod(vm_prodSe, dataoc, oc2te, entySe, entyPe, "sehe",                   name = "SE|Heat (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe,  pebio, "sehe",                   name = "SE|Heat|+|Biomass (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "sehe",                 name = "SE|Heat|+|Coal (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "sehe",                  name = "SE|Heat|+|Gas (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegeo", "sehe",                  name = "SE|Heat|+|Geothermal (EJ/yr)"), # same as SE|Heat|Electricity|Heat Pump
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegeo", "sehe",                  name = "SE|Heat|Electricity|Heat Pump (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pesol", "sehe",                  name = "SE|Heat|+|Solar (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe,  entyPe, "sehe", te = techp,      name = "SE|Heat|Combined Heat and Power (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "sehe", te = "coalchp", name = "SE|Heat|Coal|Combined Heat and Power (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "sehe", te = "gaschp",   name = "SE|Heat|Gas|Combined Heat and Power (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe,  pebio, "sehe", te = "biochp",    name = "SE|Heat|Biomass|Combined Heat and Power (EJ/yr)")
  )

  ## Hydrogen
  tmp1 <- mbind(tmp1,
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c(entyPe, entySe), "seh2",        name = "SE|Hydrogen (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seh2",                    name = "SE|Hydrogen|+|Biomass (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seh2", te = teccs,        name = "SE|Hydrogen|Biomass|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, "seh2", te = tenoccs,      name = "SE|Hydrogen|Biomass|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seh2",                 name = "SE|Hydrogen|+|Coal (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seh2", te = teccs,     name = "SE|Hydrogen|Coal|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", "seh2", te = tenoccs,   name = "SE|Hydrogen|Coal|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seh2",                  name = "SE|Hydrogen|+|Gas (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seh2", te = teccs,      name = "SE|Hydrogen|Gas|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", "seh2", te = tenoccs,    name = "SE|Hydrogen|Gas|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "seel", "seh2",                   name = "SE|Hydrogen|+|Electricity (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "seel", "seh2", te = "elh2",      name = "SE|Hydrogen|Electricity|+|Standard Electrolysis (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "seel", "seh2", te = "elh2VRE",   name = "SE|Hydrogen|Electricity|+|VRE Storage Electrolysis (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal", "peoil"), "seh2",
                                                                                name = "SE|Hydrogen|Fossil (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal", "peoil"), "seh2", te = teccs,
                                                                                name = "SE|Hydrogen|Fossil|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal", "peoil"), "seh2", te = tenoccs,
                                                                                name = "SE|Hydrogen|Fossil|+|w/o CC (EJ/yr)")
  )

  ## Liquids
  tmp1 <- mbind(tmp1,
    se.prod(vm_prodSe, dataoc, oc2te, entySe, input_liquids, se_Liq,            name = "SE|Liquids (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Liq,                    name = "SE|Liquids|+|Biomass (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Liq, te = teccs,        name = "SE|Liquids|Biomass|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Liq, te = tenoccs,      name = "SE|Liquids|Biomass|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pebiolc", se_Liq,                name = "SE|Liquids|Biomass|++|Cellulosic (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pebiolc", se_Liq, teccs,         name = "SE|Liquids|Biomass|Cellulosic|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pebiolc", se_Liq, tenoccs,       name = "SE|Liquids|Biomass|Cellulosic|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pebioil", "pebios"), se_Liq,   name = "SE|Liquids|Biomass|++|Non-Cellulosic (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pebios", se_Liq,                 name = "SE|Liquids|Biomass|Conventional Ethanol (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Liq, "bioftcrec",       name = "SE|Liquids|Biomass|BioFTR|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Liq, "bioftrec",        name = "SE|Liquids|Biomass|BioFTR|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Liq, "biodiesel",       name = "SE|Liquids|Biomass|Biodiesel|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "seh2", se_Liq,                   name = "SE|Liquids|+|Hydrogen (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal", "peoil"), se_Liq,
                                                                                name = "SE|Liquids|+|Fossil (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", se_Liq,                 name = "SE|Liquids|Fossil|+|Coal (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", se_Liq, te = teccs,     name = "SE|Liquids|Fossil|Coal|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", se_Liq, te = tenoccs,   name = "SE|Liquids|Fossil|Coal|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", se_Liq,                  name = "SE|Liquids|Fossil|+|Gas (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", se_Liq, te = teccs,      name = "SE|Liquids|Fossil|Gas|+|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pegas", se_Liq, te = tenoccs,    name = "SE|Liquids|Fossil|Gas|+|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "peoil", se_Liq,                  name = "SE|Liquids|Fossil|+|Oil (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal", "peoil"), se_Liq, te = teccs,
                                                                                name = "SE|Liquids|Fossil|++|w/ CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal", "peoil"), se_Liq, te = tenoccs,
                                                                                name = "SE|Liquids|Fossil|++|w/o CC (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, c("pegas", "pecoal", "peoil"), se_Liq, te = tenoccs,
                                                                                name = "SE|Liquids|Fossil|w/ oil|w/o CC (EJ/yr)")
  )

  ## Solids
  tmp1 <- mbind(tmp1,
    se.prod(vm_prodSe, dataoc, oc2te, entySe, entyPe, se_Solids,                name = "SE|Solids (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, "pecoal", se_Solids,              name = "SE|Solids|+|Coal (EJ/yr)"),
    # SE|Solids|Biomass is supposed to exclude traditional biomass
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Solids, te = setdiff(pe2se$all_te, "biotr"),
                                                                                name = "SE|Solids|+|Biomass (EJ/yr)"),
    se.prod(vm_prodSe, dataoc, oc2te, entySe, pebio, se_Solids, te = "biotr",   name = "SE|Solids|+|Traditional Biomass (EJ/yr)")
  )

  ## Trade
  if (module2realisation["trade", 2] == "se_trade") {

    vm_Mport <- readGDX(gdx, "vm_Mport", field = "l", restore_zeros = F)[, t, ]
    vm_Xport <- readGDX(gdx, "vm_Xport", field = "l", restore_zeros = F)[, t, ]

    tmp1 <- mbind(tmp1,
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seh2") * pm_conv_TWa_EJ,
        "SE|Hydrogen|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seel") * pm_conv_TWa_EJ,
        "SE|Electricity|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seliqsyn") * pm_conv_TWa_EJ,
        "SE|Liquids|Hydrogen|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "segasyn") * pm_conv_TWa_EJ,
        "SE|Gases|Hydrogen|Net Imports (EJ/yr)"))
  }

  # SE Demand Flows ----
  # SE|Input|X|Y variables denote the demand of energy carrier X
  # flowing into sector/production of Y.

  vm_demFeSector <- readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = F)[, y, ] * pm_conv_TWa_EJ
  vm_demFeSector[is.na(vm_demFeSector)] <- 0
  # SE demand
  vm_demSe <- readGDX(gdx, "vm_demSe", field = "l", restore_zeros = F)[, y, ] * pm_conv_TWa_EJ
  # SE demand of specific energy system technologies
  v_demSeOth <- readGDX(gdx, c("v_demSeOth","vm_demSeOth"), field = "l", restore_zeros = F)[, y, ] * pm_conv_TWa_EJ
  # conversion efficiency
  pm_eta_conv <- readGDX(gdx, "pm_eta_conv", field = "l", restore_zeros = F)[, y, ]

  # hydrogen used for electricity production via H2 turbines
  tmp1 <- mbind(tmp1,
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel"), dim = 3), "SE|Input|Hydrogen|Electricity (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel", all_te = "h2turb"), dim = 3), "SE|Input|Hydrogen|Electricity|+|Normal Turbines (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel", all_te = "h2turbVRE"), dim = 3), "SE|Input|Hydrogen|Electricity|+|Forced VRE Turbines (EJ/yr)")
  )

  # hydrogen used for synthetic fuels
  tmp1 <- mbind(tmp1,
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("seliqsyn", "segasyn"), all_te = c("MeOH", "h22ch4")), dim = 3), "SE|Input|Hydrogen|Synthetic Fuels (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seliqsyn", all_te = "MeOH"), dim = 3), "SE|Input|Hydrogen|Synthetic Fuels|+|Liquids (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "segasyn", all_te = "h22ch4"), dim = 3), "SE|Input|Hydrogen|Synthetic Fuels|+|Gases (EJ/yr)")
  )
  # hydrogen used for other energy system technologies subsumed in v_demSeOth
  # e.g. co-firing of h2 in csp
      tmp1 <- mbind(tmp1,
                    setNames(dimSums(mselect(v_demSeOth, all_enty = "seh2"), dim = 3),
                    "SE|Input|Hydrogen|Other Energy System Consumption (EJ/yr)"))


  # SE electricity use

  ### calculation of electricity use for own consumption of energy system
  vm_prodFe <- readGDX(gdx, "vm_prodFe", field = "l", restore_zeros = F)
  vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = F)

  # filter for coupled production coefficents which consume seel
  # (have all_enty2=seel and are negative)
  teprodCoupleSeel <- getNames(mselect(pm_prodCouple, all_enty2 = "seel"), dim = 3)
  CoeffOwnConsSeel <- pm_prodCouple[, , teprodCoupleSeel]
  CoeffOwnConsSeel[CoeffOwnConsSeel > 0] <- 0
  CoeffOwnConsSeel_woCCS <- CoeffOwnConsSeel[, , "ccsinje", invert = T]

  # FE and SE production that has own consumption of electricity
  # calculate vm_prodSe back to TWa (was in EJ before), but prod couple coefficient is defined in TWa(input)/Twa(output)
  prodOwnCons <- mbind(vm_prodFe, vm_prodSe / pm_conv_TWa_EJ)[, , getNames(CoeffOwnConsSeel_woCCS, dim = 3)]

  tmp1 <- mbind(tmp1, setNames(
    -pm_conv_TWa_EJ *
      (dimSums(CoeffOwnConsSeel_woCCS * prodOwnCons[, , getNames(CoeffOwnConsSeel_woCCS, dim = 3)], dim = 3, na.rm = T) +
        dimSums(CoeffOwnConsSeel[, , "ccsinje"] * vm_co2CCS[, , "ccsinje"], dim = 3,  na.rm = T)),
    "SE|Input|Electricity|Self Consumption Energy System (EJ/yr)"))

  # electricity for central ground heat pumps
  tmp1 <- mbind(tmp1, setNames(
    -pm_conv_TWa_EJ *
      (dimSums(CoeffOwnConsSeel_woCCS[, , "geohe"] * prodOwnCons[, , "geohe"], dim = 3)),
    "SE|Input|Electricity|Self Consumption Energy System|Central Ground Heat Pump (EJ/yr)"))

  # electricity for fuel extraction, e.g. electricity used for oil and gas extraction

  # read in with restore_zero = F first, to get non-zero third dimension
  pm_fuExtrOwnCons_reduced <- readGDX(gdx, "pm_fuExtrOwnCons", restore_zeros = F)
  # read in again with restore_zero = T to get all regions in case the parameter is zero for some regions
  pm_fuExtrOwnCons <- readGDX(gdx, "pm_fuExtrOwnCons", restore_zeros = T)[,,getNames(pm_fuExtrOwnCons_reduced)]
  vm_fuExtr <- readGDX(gdx, "vm_fuExtr", field = "l", restore_zeros = F)[,y,]
  pe2rlf <- readGDX(gdx, "pe2rlf")
  pe2rlfemi <- pe2rlf %>% filter(!!sym("all_enty") %in% getNames(pm_fuExtrOwnCons, dim=2))


  # calculate electricity for fuel extraction as in q32_balSe
  # by multiplying fuel consumption of extraction with extraction quantities
  tmp1 <- mbind(tmp1,
                setNames(
                  # sum over all PE carriers and extraction grades
                  dimSums(
                    # sum over pm_fuExtrOwnCons to reduce all_enty dimensions
                    dimSums(mselect( pm_fuExtrOwnCons, all_enty = "seel"), dim = 3.1)
                    * vm_fuExtr[,,getNames(pm_fuExtrOwnCons, dim=2)], dim=3)
                    * pm_conv_TWa_EJ,
                  "SE|Input|Electricity|PE Production (EJ/yr)"))

  # set to zero in 2005 as the fuel production electricity demand is not included in the SE balance equation in this year
  # due to incompatibilities with the InitialCap module
  tmp1[,"y2005","SE|Input|Electricity|PE Production (EJ/yr)"] <- 0

  # share of electrolysis H2 in total H2
  p_shareElec_H2 <- collapseNames(tmp1[, , "SE|Hydrogen|+|Electricity (EJ/yr)"] / tmp1[, , "SE|Hydrogen (EJ/yr)"])
  p_shareElec_H2[is.na(p_shareElec_H2)] <- 0


  # share of domestically produced H2 (only not 1 if se trade module on and hydrogen can be imported/exported)
  if (module2realisation["trade", 2] == "se_trade") {
    p_share_H2DomProd <-  collapseNames(tmp1[, , "SE|Hydrogen (EJ/yr)"] / (tmp1[, , "SE|Hydrogen|Net Imports (EJ/yr)"] + tmp1[, , "SE|Hydrogen (EJ/yr)"]))
  } else {
    p_share_H2DomProd <- tmp1[, , "SE|Hydrogen (EJ/yr)"]
    p_share_H2DomProd[] <- 1
  }

  tmp1 <- mbind(tmp1,
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "build"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Input|Electricity|Buildings (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "indst"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Input|Electricity|Industry (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feelt", emi_sectors = "trans"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdelt"),
    "SE|Input|Electricity|Transport (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "CDR"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Input|Electricity|CDR (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2"), dim = 3),
      "SE|Input|Electricity|Hydrogen (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2", all_te = "elh2"), dim = 3),
      "SE|Input|Electricity|Hydrogen|+|Standard Electrolysis (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2", all_te = "elh2VRE"), dim = 3),
      "SE|Input|Electricity|Hydrogen|+|VRE Storage (EJ/yr)")
  )

  # electricity for specific H2 usages
  tmp1 <- mbind(
    tmp1,
    # calculate electricity going into domestic (!) H2 production for direct H2 use (FE Hydrogen)
    setNames(
      dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("feh2s", "feh2t")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|direct FE H2 (EJ/yr)"
    ),
    # calculate electricity used for storage of electricity
    setNames(
      dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel"), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Electricity Storage (EJ/yr)"
    )
  )

  # electricity used for domestic (!) synfuel production
  tmp1 <- mbind(tmp1,
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("seliqsyn", "segasyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Synthetic Fuels (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("seliqsyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Liquids (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("segasyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Gases (EJ/yr)"))


  # transmission losses from se2fe conversion of electricity
  tmp1 <- mbind(tmp1,
    setNames(dimSums(vm_demSe[, , "tdels"] * (1 - pm_eta_conv[, , "tdels"]), dim = 3),
      "SE|Electricity|Transmission Losses (EJ/yr)"))

  # add global values
  out <- mbind(tmp1, dimSums(tmp1, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))

  getSets(out)[3] <- "variable"
  return(out)
}
