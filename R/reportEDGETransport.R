#' Reporting for the coupled EDGE-T Transport Sector Model (REMIND Module edge_esm)
#'
#' Data is loaded from the EDGE-T subfolder in the output folder.
#' The input files can be (re-) generated calling
#' `Rscript EDGETransport.R --reporting`
#' from the output folder.
#'
#' *Warning* The function modifies the "REMIND_generic_<scenario>.mif" file by appending the
#' additional reporting variables and replaces the "_withoutPlus" version.
#'
#' Region subsets are obtained from fulldata.gdx
#'
#' @param output_folder path to the output folder, default is current folder.
#' @param remind_root path to the REMIND root directory, defaults to two levels up from output_folder.
#' @param append try to find the REMIND output file in the output folder and append to it. If FALSE, the function returns a quitte object with the reporting variables.
#' @author Alois Dirnaichner Marianna Rottoli
#'
#' @importFrom rmndt approx_dt readMIF writeMIF
#' @importFrom gdxdt readgdx
#' @importFrom data.table fread fwrite rbindlist copy CJ setnames := data.table as.data.table
#' @importFrom dplyr %>%
#' @importFrom quitte as.quitte
#' @importFrom utils capture.output
#' @export

reportEDGETransport <- function(output_folder=".",
                                remind_root=NULL,
                                append=TRUE) {

  if(is.null(remind_root)){
    remind_root <- file.path(output_folder, "../..")
  }
  gdx <- file.path(output_folder, "fulldata.gdx")

  ## check the regional aggregation
  regionSubsetList <- toolRegionSubsets(gdx)

  # ADD EU-27 region aggregation if possible
  if("EUR" %in% names(regionSubsetList)){
    regionSubsetList <- c(regionSubsetList,list(
      "EU27"=c("ENC","EWN","ECS","ESC","ECE","FRA","DEU","ESW")
    ))
  }

  sub_folder = "EDGE-T/"

  ## NULL Definitons for codeCheck compliance
  RegionCode <- CountryCode <- cfg <- `.` <- sector <- subsector_L3 <- region <- year <- NULL
  subsector_L2 <- subsector_L1 <- aggr_mode <- vehicle_type <- det_veh <- aggr_nonmot <- NULL
  demand_F <- demand_EJ <- remind_rep <- V25 <- aggr_veh <- technology <- NULL
  ttot <- se_share <- fe_demand <- variable <- value <- demand_VKM <- loadFactor <- NULL
  all_enty <- ef <- variable_agg <- model <- scenario <- period <- NULL
  Region <- Variable <- co2 <- co2val <- elh2 <- fe  <- NULL
  int <- se <- sec  <- sharesec <- te  <- tech <-  val <- share <- NULL
  eff <- sharebio <- sharesyn <- totseliq <- type <- ven <- NULL
  unit <- NULL

  load(file.path(output_folder, "config.Rdata"))

  datapath <- function(fname){
    file.path(output_folder, sub_folder, fname)
  }

  ## load input data from last EDGE run
  demand_km <- readRDS(datapath(fname = "demandF_plot_pkm.RDS"))[
    , demand_F := demand_F * 1e-3] ## million -> billion pkm
  load_factor <- readRDS(datapath(fname = "loadFactor.RDS"))
  annual_mileage <- readRDS(datapath(fname = "annual_mileage.RDS"))
  demand_vkm <- merge(demand_km, load_factor, by=c("year", "region", "vehicle_type"))
  demand_vkm[, demand_VKM := demand_F/loadFactor] ## billion vkm

  demand_ej <- readRDS(datapath(fname = "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ

  ## ES and FE Demand

  reportingESandFE <- function(datatable, mode){

    datatable[, sector := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "Pass", "Freight")]

    ## attribute aggregated mode and vehicle names for plotting purposes, and aggregate
    ## Large Categories (as used by REMIND)
    datatable[subsector_L2 == "trn_pass_road_LDV",
              aggr_mode := "Pass|Road|LDV"]
    datatable[subsector_L2 != "trn_pass_road_LDV" & sector == "Pass",
              aggr_mode := "Pass|non-LDV"]

    ## A little more detail: Vehicle Aggregates
    datatable[grepl("^Truck", vehicle_type), aggr_veh := "Freight|Road"]
    datatable["Freight Rail_tmp_vehicletype" == vehicle_type, aggr_veh := "Freight|Rail"]
    ## there seem to be no passenger ships in EDGE-T!
    datatable[subsector_L3 == "International Ship", aggr_veh := "Freight|International Shipping"]
    datatable[subsector_L3 == "Domestic Ship", aggr_veh := "Freight|Navigation"]

    datatable[grepl("Bus", vehicle_type), aggr_veh := "Pass|Road|Bus"]


    if(mode == "ES")
      datatable[grepl("Cycle|Walk", subsector_L3), aggr_nonmot := "Pass|Road|Non-Motorized"]

    ## Rail & Aviation
    datatable[grepl("Passenger Rail|HSR", vehicle_type), aggr_veh := "Pass|Rail"]
    datatable[subsector_L3 == "Domestic Aviation", aggr_veh := "Pass|Aviation|Domestic"]
    datatable[subsector_L3 == "International Aviation", aggr_veh := "Pass|Aviation|International"]

    ## Freight
    datatable[grepl("^Truck", vehicle_type), det_veh := sprintf("Freight|Road|%s", vehicle_type)]

    ## High Detail: Ecoinvent-Compatible Output
    datatable[grepl("Subcompact", vehicle_type),
              det_veh := "Pass|Road|LDV|Small"]
    datatable[grepl("Mini", vehicle_type),
              det_veh := "Pass|Road|LDV|Mini"]
    datatable[vehicle_type == "Compact Car", det_veh := "Pass|Road|LDV|Medium"]
    datatable[grepl("Large Car|Midsize Car", vehicle_type), det_veh := "Pass|Road|LDV|Large"]
    datatable[grepl("SUV", vehicle_type),
              det_veh := "Pass|Road|LDV|SUV"]
    datatable[grepl("Van|Multipurpose", vehicle_type),
              det_veh := "Pass|Road|LDV|Van"]

    datatable[grepl("Motorcycle|Scooter|Moped", vehicle_type),
              det_veh := "Pass|Road|LDV|Two-Wheelers"]

    prepare4MIF <- function(dt, remind_unit, valcol, varcol){
      ## REMIND years, loading from MIF File would take too long
      yrs <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
      remind_scenario <- cfg$title

      prefix <- switch(mode,
                       "FE" = "FE|Transport|",
                       "ES" = "ES|Transport|",
                       "VKM" = "ES|Transport|VKM|")

      ## we only care for non-NA variables (NA is basically *all others*)
      toadd = dt[!is.na(get(varcol)), .(model=cfg$model_name, scenario=remind_scenario, region,
                                        variable=paste0(prefix, get(varcol)),
                                        unit=remind_unit, period=year,
                                        value=get(valcol))]

      toadd <- approx_dt(toadd, yrs, xcol="period", ycol = "value",
                         idxcols=colnames(toadd)[1:5],
                         extrapolate=T)
      return(toadd)
    }

    if(mode == "ES"){
      report <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "bn pkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn tkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn pkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(aggr_nonmot), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_nonmot")],
          "bn pkm/yr", "V1", "aggr_nonmot"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "det_veh")], "bn pkm/yr", "V1", "det_veh")))
    }else if(mode == "VKM"){
      report <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "bn vkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "det_veh")], "bn vkm/yr", "V1", "det_veh")))
    }else{
      report <- rbindlist(list(
        prepare4MIF(
          datatable[!is.na(aggr_mode), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "EJ/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[!is.na(aggr_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "EJ/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "det_veh")],
          "EJ/yr", "V1", "det_veh")))
    }

    ## with energy carrier

    ## remove cycling and walking placeholder techs for ESs
    techmap <- data.table(technology=unique(datatable$technology),
                          key="technology")[!c("Cycle_tmp_technology", "Walk_tmp_technology")]

    if(mode == "ES"){
      ## for energy services, it is better to refer to the actual technologies
      ## and not the fuel types (-> LCA)
      techmap[, remind_rep := technology]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
                    "bn pkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                   "bn tkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                    "bn pkm/yr", "V1", "aggr_veh"),
        prepare4MIF(datatable[!is.na(det_veh), sum(demand_F, na.rm=T),
                              by = c("region", "year", "det_veh", "remind_rep")
                              ][, det_veh := paste0(det_veh, "|", remind_rep)],
                              "bn pkm/yr", "V1", "det_veh")))
    }else if(mode == "VKM"){
      ## for energy services, it is better to refer to the actual technologies
      ## and not the fuel types (-> LCA)
      techmap[, remind_rep := technology]
      techmap["LA-BEV", remind_rep := "BEV"]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
                    "bn vkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                    "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(datatable[!is.na(det_veh), sum(demand_VKM, na.rm=T),
                              by = c("region", "year", "det_veh", "remind_rep")
                              ][, det_veh := paste0(det_veh, "|", remind_rep)],
                              "bn vkm/yr", "V1", "det_veh")))
    }else{
      techmap["BEV", remind_rep := "Electricity"]
      techmap["Electric", remind_rep := "Electricity"]
      techmap["FCEV", remind_rep := "Hydrogen"]
      techmap["Hydrogen", remind_rep := "Hydrogen"]
      techmap["Liquids", remind_rep := "Liquids"]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[!is.na(aggr_mode), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
                    "EJ/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[!is.na(aggr_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                    "EJ/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "det_veh", "remind_rep")
                    ][, det_veh := paste0(det_veh, "|", remind_rep)],
                    "EJ/yr", "V1", "det_veh")))

      df <- rbind(
        datatable[!is.na(aggr_mode), sum(demand_EJ, na.rm = T),
                  by = c("region", "year", "aggr_mode", "remind_rep")
        ][, variable := paste0(aggr_mode, "|", remind_rep)][,c("region", "year", "variable", "V1")],
        datatable[!is.na(aggr_veh), sum(demand_EJ, na.rm = T),
                  by = c("region", "year", "aggr_veh", "remind_rep")
        ][, variable := paste0(aggr_veh, "|", remind_rep)][,c("region", "year", "variable", "V1")]
      )

      # splits Liquids variables into Biomass, Fossil and Hydrogen according to FE demand shares
      .split_liquids <- function(df) {
        demFeSector <- readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = F)

        # biomass share in biomass+hydrogen liquids in total transport sector
        bioShareTrans <- dimSums(mselect(demFeSector, all_enty = "seliqbio", emi_sectors = "trans"), dim = 3, na.rm = T) /
          dimSums(mselect(demFeSector, all_enty = c("seliqbio", "seliqsyn"), emi_sectors = "trans"), dim = 3, na.rm = T)

        # hydrogen share in biomass+hydrogen liquids in total transport sector
        synShareTrans <- dimSums(mselect(demFeSector, all_enty = "seliqsyn", emi_sectors = "trans"), dim = 3, na.rm = T) /
          dimSums(mselect(demFeSector, all_enty = c("seliqbio", "seliqsyn"), emi_sectors = "trans"), dim = 3, na.rm = T)

        # calculate LDV share ----

        # liquids for LDVs
        demFeSectorLdv <- mselect(demFeSector,
                                  all_enty = c("seliqfos", "seliqbio", "seliqsyn"),
                                  all_enty1 = "fepet", emi_sectors = "trans"
        )

        feShareLdvLiqFos <- dimSums(demFeSectorLdv[, , "seliqfos.fepet"], dim = 3, na.rm = T) / dimSums(demFeSectorLdv, dim = 3, na.rm = T)

        # for non-fossil liquids we apply the share of the transport sector to the subsector
        feShareLdvLiqBio <- dimSums(mselect(demFeSectorLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * bioShareTrans / dimSums(demFeSectorLdv, dim = 3, na.rm = T)
        feShareLdvLiqSyn <- dimSums(mselect(demFeSectorLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * synShareTrans / dimSums(demFeSectorLdv, dim = 3, na.rm = T)

        # calculate share for Non-LDV (Trucks, Domestic Aviation etc.) ----

        # liquids for Non-LDVs
        demFeSectorNonLdv <- mselect(demFeSector,
                                     all_enty = c("seliqfos", "seliqbio", "seliqsyn"),
                                     all_enty1 = "fedie", emi_sectors = "trans", all_emiMkt = "ES"
        )

        feShareNonLdvLiqFos <- demFeSectorNonLdv[, , "seliqfos.fedie"] / dimSums(demFeSectorNonLdv, dim = 3, na.rm = T)
        feShareNonLdvLiqBio <- dimSums(mselect(demFeSectorNonLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * bioShareTrans / dimSums(demFeSectorNonLdv, dim = 3, na.rm = T)
        feShareNonLdvLiqSyn <- dimSums(mselect(demFeSectorNonLdv, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * synShareTrans / dimSums(demFeSectorNonLdv, dim = 3, na.rm = T)

        # calculate share for Bunkers ----

        # liquids for bunkers
        demFeSectorBunkers <- mselect(demFeSector,
                                      all_enty = c("seliqfos", "seliqbio", "seliqsyn"),
                                      all_enty1 = "fedie", emi_sectors = "trans", all_emiMkt = "other"
        )

        feShareBunkersLiqFos <- demFeSectorBunkers[, , "seliqfos.fedie"] / dimSums(demFeSectorBunkers, dim = 3, na.rm = T)
        feShareBunkersLiqBio <- dimSums(mselect(demFeSectorBunkers, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * bioShareTrans / dimSums(demFeSectorBunkers, dim = 3, na.rm = T)
        feShareBunkersLiqSyn <- dimSums(mselect(demFeSectorBunkers, all_enty = c("seliqbio", "seliqsyn")), dim = 3, na.rm = T) * synShareTrans / dimSums(demFeSectorBunkers, dim = 3, na.rm = T)

        m <- as.magpie(df)
        y <- intersect(getItems(m, dim = 2), getItems(demFeSector, dim = 2))
        tmp <- mbind(
          setNames(m[, y, "Pass|Road|LDV|Liquids"] * feShareLdvLiqFos[, y, ], "Pass|Road|LDV|Liquids|Fossil"),
          setNames(m[, y, "Pass|Road|LDV|Liquids"] * feShareLdvLiqBio[, y, ], "Pass|Road|LDV|Liquids|Biomass"),
          setNames(m[, y, "Pass|Road|LDV|Liquids"] * feShareLdvLiqSyn[, y, ], "Pass|Road|LDV|Liquids|Hydrogen"),
          setNames(m[, y, "Freight|Road|Liquids"] * feShareNonLdvLiqFos[, y, ], "Freight|Road|Liquids|Fossil"),
          setNames(m[, y, "Freight|Road|Liquids"] * feShareNonLdvLiqBio[, y, ], "Freight|Road|Liquids|Biomass"),
          setNames(m[, y, "Freight|Road|Liquids"] * feShareNonLdvLiqSyn[, y, ], "Freight|Road|Liquids|Hydrogen"),
          setNames(m[, y, "Pass|Road|Bus|Liquids"] * feShareNonLdvLiqFos[, y, ], "Pass|Road|Bus|Liquids|Fossil"),
          setNames(m[, y, "Pass|Road|Bus|Liquids"] * feShareNonLdvLiqBio[, y, ], "Pass|Road|Bus|Liquids|Biomass"),
          setNames(m[, y, "Pass|Road|Bus|Liquids"] * feShareNonLdvLiqSyn[, y, ], "Pass|Road|Bus|Liquids|Hydrogen"),
          setNames(m[, y, "Freight|Rail|Liquids"] * feShareNonLdvLiqFos[, y, ], "Freight|Rail|Liquids|Fossil"),
          setNames(m[, y, "Freight|Rail|Liquids"] * feShareNonLdvLiqBio[, y, ], "Freight|Rail|Liquids|Biomass"),
          setNames(m[, y, "Freight|Rail|Liquids"] * feShareNonLdvLiqSyn[, y, ], "Freight|Rail|Liquids|Hydrogen"),
          setNames(m[, y, "Pass|Rail|Liquids"] * feShareNonLdvLiqFos[, y, ], "Pass|Rail|Liquids|Fossil"),
          setNames(m[, y, "Pass|Rail|Liquids"] * feShareNonLdvLiqBio[, y, ], "Pass|Rail|Liquids|Biomass"),
          setNames(m[, y, "Pass|Rail|Liquids"] * feShareNonLdvLiqSyn[, y, ], "Pass|Rail|Liquids|Hydrogen"),
          setNames(m[, y, "Pass|Aviation|Domestic|Liquids"] * feShareNonLdvLiqFos[, y, ], "Pass|Aviation|Domestic|Liquids|Fossil"),
          setNames(m[, y, "Pass|Aviation|Domestic|Liquids"] * feShareNonLdvLiqBio[, y, ], "Pass|Aviation|Domestic|Liquids|Biomass"),
          setNames(m[, y, "Pass|Aviation|Domestic|Liquids"] * feShareNonLdvLiqSyn[, y, ], "Pass|Aviation|Domestic|Liquids|Hydrogen"),
          setNames(m[, y, "Freight|Navigation|Liquids"] * feShareNonLdvLiqFos[, y, ], "Freight|Navigation|Liquids|Fossil"),
          setNames(m[, y, "Freight|Navigation|Liquids"] * feShareNonLdvLiqBio[, y, ], "Freight|Navigation|Liquids|Biomass"),
          setNames(m[, y, "Freight|Navigation|Liquids"] * feShareNonLdvLiqSyn[, y, ], "Freight|Navigation|Liquids|Hydrogen"),
          setNames(m[, y, "Pass|Aviation|International|Liquids"] * feShareBunkersLiqFos[, y, ], "Pass|Aviation|International|Liquids|Fossil"),
          setNames(m[, y, "Pass|Aviation|International|Liquids"] * feShareBunkersLiqBio[, y, ], "Pass|Aviation|International|Liquids|Biomass"),
          setNames(m[, y, "Pass|Aviation|International|Liquids"] * feShareBunkersLiqSyn[, y, ], "Pass|Aviation|International|Liquids|Hydrogen"),
          setNames(m[, y, "Freight|International Shipping|Liquids"] * feShareBunkersLiqFos[, y, ], "Freight|International Shipping|Liquids|Fossil"),
          setNames(m[, y, "Freight|International Shipping|Liquids"] * feShareBunkersLiqBio[, y, ], "Freight|International Shipping|Liquids|Biomass"),
          setNames(m[, y, "Freight|International Shipping|Liquids"] * feShareBunkersLiqSyn[, y, ], "Freight|International Shipping|Liquids|Hydrogen")
        )

        as.data.frame(tmp, rev = 2) %>%
          as.data.table() %>%
          prepare4MIF("EJ/yr", ".value", "variable") %>%
          return()
      }

      report_liquids_split <- .split_liquids(df)

      report_liquids_split_w <- report_liquids_split[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
      report_liquids_split <- rbind(report_liquids_split, report_liquids_split_w)

    }

    ## add World
    report_w = report[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    report = rbind(report, report_w)

    report_tech_w = report_tech[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    report_tech = rbind(report_tech, report_tech_w)

    if (mode == "FE") {
      report_complete <- rbindlist(list(report, report_tech, report_liquids_split))
    } else {
      report_complete <- rbindlist(list(report, report_tech))
    }


    ## in case the aggregation features EU sub-regions, include also the aggregated versions
    return(report_complete)
  }


  ## Demand emissions
  reportingEmi <- function(repFE, gdx){

    ## load emission factors for fossil fuels
    p_ef_dem <- readgdx(gdx, "p_ef_dem")[all_enty %in% c("fepet", "fedie", "fegas")]  ## MtCO2/EJ
    p_ef_dem[all_enty == "fegas", all_enty := "fegat"]
    setnames(p_ef_dem, old = c("value", "all_regi"), new = c("ef", "region"))
    ## attribute explicitly fuel used to the FE values
    emidem = repFE[grepl("Liquids|Gases|Hydrogen|Electricity", variable) & region != "World"]   ## EJ
    emidem[, all_enty := ifelse(grepl("Liquids", variable), "fedie", NA)]
    emidem[, all_enty := ifelse(grepl("LDV.+Liquids", variable), "fepet", all_enty)]
    emidem[, all_enty := ifelse(grepl("Gases", variable), "fegat", all_enty)]
    emidem[, all_enty := ifelse(grepl("Electricity", variable), "feelt", all_enty)]
    emidem[, all_enty := ifelse(grepl("Hydrogen", variable), "feh2t", all_enty)]
    ## merge with emission factors
    emidem = emidem[p_ef_dem, on = c("all_enty","region")]
    ## calculate emissions and attribute variable and unit names
    emidem[, value := value*ef][, c("variable", "unit") := list(gsub("FE", "Emi\\|CO2", variable), "Mt CO2/yr")]

    emi = rbind(copy(emidem)[, c("type", "variable") := list("tailpipe", paste0(variable, "|Tailpipe"))],
                   copy(emidem)[, c("type", "variable") := list("demand", paste0(variable, "|Demand"))])

    prodFe <- readgdx(gdx, "vm_prodFE")[, ttot := as.numeric(ttot)]
    setnames(prodFe,
             c("period", "region", "se", "all_enty", "te", "fe_demand"))
    prodFe[, se_share := fe_demand/sum(fe_demand), by=c("period", "region", "all_enty")]
    prodFe <- prodFe[all_enty %in% c("fedie", "fepet", "fegat") & se %in% c("segafos", "seliqfos")][, c("se", "te", "fe_demand") := NULL]

    emi <- prodFe[emi, on=c("period", "region", "all_enty")]
    ## in case no fossil fuels are used (e.g. 100% biodiesel), the value in se_share results NA. set the NA value to 0
    emi[is.na(se_share), se_share := 0]
    emi <- emi[all_enty %in% c("fedie", "fepet", "fegat") & type == "demand", value := value*se_share]

    emi[, c("se_share", "type", "ef", "all_enty") := NULL]

    ## aggregate removing the fuel dependency
    emi[, variable_agg := gsub("\\|Liquids|\\|Electricity|\\|Hydrogen|\\|Gases", "", variable)]
    emi = emi[, .(value = sum(value)), by = c("model", "scenario", "region", "unit", "period", "variable_agg")]
    setnames(emi, old = "variable_agg", new = "variable")
    emi = emi[, .(model, scenario, region, variable, unit, period, value)]

    ## add World
    emi_w = emi[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    emi = rbind(emi, emi_w)

    return(emi)
  }

  reportingVehNum <- function(demand_vkm, annual_mileage){
    venum <- copy(demand_vkm)
    ## merge annual mileage
    anmil <- copy(annual_mileage)
    anmil[grepl("Subcompact", vehicle_type),
          variable := "Pass|Road|LDV|Small"]
    anmil[grepl("Mini", vehicle_type),
          variable := "Pass|Road|LDV|Mini"]
    anmil[vehicle_type == "Compact Car", variable := "Pass|Road|LDV|Medium"]
    anmil[grepl("Large Car|Midsize Car", vehicle_type), variable := "Pass|Road|LDV|Large"]
    anmil[grepl("SUV", vehicle_type),
          variable := "Pass|Road|LDV|SUV"]
    anmil[grepl("Van|Multipurpose", vehicle_type),
          variable := "Pass|Road|LDV|Van"]
    anmil[grepl("Motorcycle|Scooter|Moped", vehicle_type),
          variable := "Pass|Road|LDV|Two-Wheelers"]
    anmil[grepl("^Truck", vehicle_type),
          variable := sprintf("Freight|Road|%s", vehicle_type)]
    anmil[grepl("Bus", vehicle_type),
          variable := "Pass|Road|Bus"]

    anmil <- anmil[,.(region, period = year, variable, annual_mileage)]

    anmil <- approx_dt(anmil, unique(demand_vkm$period), xcol = "period", ycol = "annual_mileage", idxcols = c("region", "variable"), extrapolate = T)
    anmil<- unique(anmil[,c("period", "region", "variable", "annual_mileage")])
    anmil <- anmil[, variable := paste0("ES|Transport|VKM|", variable)]
    venum <- merge(demand_vkm, anmil, by = c("variable", "region", "period"))
    venum[, ven := value/annual_mileage] # billion vehicle-km -> thousand vehicles

    venum <- venum[!is.na(ven)]
    venum[, variable := gsub("|VKM", "|VNUM", variable, fixed=TRUE)][, c("value", "annual_mileage") := NULL]
    venum[, unit := "tsd veh"]
    setnames(venum, "ven", "value")
    venum = venum[,.(model, scenario, region, variable, unit, period, value)]
    return(venum)
  }

  reportStockAndSales <- function(annual_mileage){
    vintages_file <- file.path(output_folder, "vintcomp.csv")
    if(!file.exists(vintages_file)){
      print("EDGE-T Reporting: No vintages file found.")
      return(NULL)
    }

    year_c <- construction_year <- Stock <- Sales <- vintage_demand_vkm <- fct <- category <- NULL
    vintgs <- fread(vintages_file)

    ## backward compat. fix
    fct <- 1.
    if("variable" %in% colnames(vintgs)){
      fct <- 1e-6
      setnames(vintgs, "variable", "construction_year")
    }

    vintgs[, year_c := as.numeric(gsub("C_", "", construction_year))]

    ## stock is the full stock up to the end of the current year
    ## sales are the sales of the current year

    setnames(vintgs, "full_demand_vkm", "Stock")
    vintgs[, Stock := Stock * fct]
    vintgs[, Sales := Stock - sum(vintage_demand_vkm), by=.(year, region, vehicle_type, technology)]
    vintgs[, c("construction_year", "vintage_demand_vkm", "year_c") := NULL]
    vintgs <- unique(vintgs)

    vintgs <- data.table::melt(vintgs, measure.vars = c("Stock", "Sales"), variable.name = "category")
    ## vkm -> v-num
    vintgs = merge(vintgs, annual_mileage, by = c("year", "region", "vehicle_type"))
    vintgs[, value := value / annual_mileage]
    vintgs[, variable := ifelse(
               vehicle_type == "Bus_tmp_vehicletype",
               sprintf("%s|Transport|Bus|%s", category, technology),
               sprintf("%s|Transport|LDV|%s|%s", category, vehicle_type, technology))]

    ## totals
    vintgs <- rbindlist(list(
      vintgs,
      vintgs[, .(value=sum(value), variable=gsub("(.+)\\|.+$", "\\1", variable)),
             by=c("category", "year", "region", "vehicle_type")],
      vintgs[grepl("|LDV|", variable, fixed=TRUE),
             .(value=sum(value), variable=sprintf("%s|Transport|LDV", category)),
             by=c("category", "year", "region")]), fill=TRUE)

    vintgs[, c("vehicle_type", "technology", "annual_mileage", "category") := NULL]
    vintgs <- unique(vintgs[!is.na(value)])

    setnames(vintgs, "year", "period")

    vintgs = approx_dt(vintgs, c(2005, 2010, unique(vintgs$period), 2110, 2130, 2150),
                       xcol = "period", ycol = "value", idxcols = c("region", "variable"), extrapolate = T)
    vintgs[period <= 2010|period > 2100, value := 0]

    ## remove the variable (e.g. vehicle_types) that are not present for this specific region
    vintgs[, `:=`(model=cfg$model_name, scenario=cfg$title, unit="Million vehicles")]

    return(vintgs)

  }

  repFE <- reportingESandFE(
    demand_ej,
    mode ="FE")

  repVKM <- reportingESandFE(
    datatable=demand_vkm,
    mode="VKM")

  toMIF <- rbindlist(list(
    repFE,
    repVKM,
    reportingESandFE(
      datatable=demand_km,
      mode="ES"),
    reportingVehNum(repVKM,
                    annual_mileage),
    reportingEmi(repFE = repFE,
                 gdx = gdx)
  ))


  ## add Road Totals
  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("ES\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="ES|Transport|Pass|Road",
            unit="bn pkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Pass|Road|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Pass|Road|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("ES\\|Transport\\|VKM\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="ES|Transport|VKM|Pass|Road",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("FE\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="FE|Transport|Pass|Road",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]), use.names = TRUE)

  ## VKM totals, Road and Rail
  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("ES\\|Transport\\|VKM\\|(Pass|Freight)\\|Road$", variable),
          .(variable="ES|Transport|VKM|Road",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("ES\\|Transport\\|VKM\\|(Pass|Freight)\\|Rail$", variable),
          .(variable="ES|Transport|VKM|Rail",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]
    ), use.names = TRUE)

  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("FE\\|Transport\\|(Pass|Freight)\\|Road$", variable),
          .(variable="FE|Transport|Road",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Road\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Road|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Rail\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Rail|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Road\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Road|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Rail\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Rail|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("FE\\|Transport\\|(Pass|Freight)\\|Rail$", variable),
          .(variable="FE|Transport|Rail",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]), use.names = TRUE)

  toMIF <- rbindlist(list(toMIF, reportStockAndSales(annual_mileage)), use.names=TRUE)


  if (!is.null(regionSubsetList)){
    toMIF <- rbindlist(list(
      toMIF,
      toMIF[region %in% regionSubsetList[["EUR"]],.(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
      toMIF[region %in% regionSubsetList[["NEU"]],.(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)],
      toMIF[region %in% regionSubsetList[["EU27"]],.(value = sum(value), region = "EU27"), by = .(model, scenario, variable, unit, period)]
    ), use.names=TRUE)
  }


  ## Make sure there are no duplicates!
  idx <- anyDuplicated(toMIF, by = c("region", "variable", "period"))
  if(idx){
    warning(paste0("Duplicates found in EDGE-T reporting output:",
                   capture.output(toMIF[idx]), collapse="\n"))
  }

  toMIF <- data.table::dcast(toMIF, ... ~ period, value.var="value")
  setnames(toMIF, colnames(toMIF)[1:5], c("Model", "Scenario", "Region", "Variable", "Unit"))

  if(append){
    name_mif <- list.files(output_folder, pattern = "REMIND_generic", full.names = F) %>%
      .[!grepl("withoutPlu|adjustedPolicy", .)]
    stopifnot(!is.na(name_mif) && length(name_mif) == 1)

    name_mif <- file.path(output_folder, name_mif)


    writeMIF(toMIF, name_mif, append=T)
    deletePlus(name_mif, writemif=T)
  }else{
    return(as.quitte(toMIF))
  }
}
