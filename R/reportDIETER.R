#' Reporting for the coupled DIETER Model
#'
#'
#' The DIETER data is appended to a REMIND_generic_.mif file and the new mif is saved in different
#' location, i.e sub directory "DIETER".
#'
#' @param dieterDatafile full path with name of dieter gdx file.
#' @param outputDir path to the output folder, default is current folder.
#' @author Chen Gong, Pratik Agrawal
#'

#' @importFrom data.table fread fwrite setDT
#' @importFrom quitte read.quitte read.gdx revalue.levels
#' @importFrom readr write_rds
#' @importFrom rlang .data
#' @importFrom dplyr %>% left_join filter select rename mutate arrange ungroup across mutate_all coalesce group_by if_else
#' @importFrom tidyr complete
#' @importFrom utils write.table
#' @export
reportDIETER <- function(dieterDatafile = "report_DIETER.gdx", outputDir = ".") {
  datapath <- function(fname) {
    file.path(outputDir, fname)
  }

  subFolder <- "DIETER"


  gdxToQuitteHourly <- function(gdxfile) {
    file <- datapath(fname = gdxfile)
    outHourly <- NULL
    repHrs <- read.gdx(gdxName = file, requestList.name =  "report_hours", squeeze = FALSE)

    names(repHrs) <- c("gdxfile", "model", "year", "country", "variable", "hour", "value")

    outH <- repHrs %>%
      dplyr::select(.data$model, .data$year, .data$variable, .data$country, .data$hour, .data$value) %>%
      dplyr::mutate(hour = as.numeric(sub("([0-9]+).*$", "\\1", .data$hour))) %>%
      dplyr::group_by(.data$model, .data$variable, .data$year, .data$country) %>%
      complete(hour = (1:8760)) %>%
      dplyr::mutate_all(coalesce, 0) %>%
      dplyr::ungroup(c("model", "variable", "year", "country")) %>%
      dplyr::mutate(Model = .data$model, Scenario = "baseline", Region = .data$country,
                    Hour = .data$hour, Tech =  "all Tech") %>%
      dplyr::mutate(Variable = .data$variable, Period = .data$year,
                    Year = .data$year,
                    Value = round(.data$value, digits = 4)) %>%
      dplyr::arrange(.data$Period) %>%
      dplyr::select(.data$Model, .data$Scenario, .data$Region, .data$Variable,
                    .data$Year, .data$Period, .data$Tech, .data$Value, .data$Hour)

    ###################################################################
    repTechHrs <- read.gdx(gdxName = file, requestList.name = "report_tech_hours", squeeze = FALSE)

    names(repTechHrs) <- c("gdxfile", "model", "year", "country", "variable", "tech", "hour", "value")

    outTH <- repTechHrs %>%
      dplyr::select(.data$model, .data$year, .data$tech, .data$variable, .data$country, .data$hour, .data$value) %>%
      dplyr::mutate(hour = as.numeric(sub("([0-9]+).*$", "\\1", .data$hour))) %>%
      dplyr::mutate(tech = as.character(.data$tech)) %>%
      dplyr::group_by(.data$model, .data$tech, .data$hour, .data$variable, .data$country) %>%
      dplyr::mutate(year = as.numeric(.data$year)) %>%
      complete(year = c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100)) %>%
      dplyr::ungroup(c("model", "tech", "hour", "variable", "country")) %>%
      dplyr::group_by(.data$model, .data$year, .data$variable, .data$country, .data$tech) %>%
      complete(hour = (1:8760)) %>%
      dplyr::mutate_all(coalesce, 0) %>%
      dplyr::ungroup("model", "tech", "hour", "variable", "country", "year") %>%
      dplyr::mutate(Model = .data$model, Scenario = "baseline",  Region = .data$country,
                    Hour = .data$hour, Year = .data$year, Tech = .data$tech) %>%
      dplyr::mutate(Variable = .data$variable, Period = .data$year,
                    Value = round(.data$value, digits = 4)) %>%

      dplyr::arrange(.data$Year) %>%
      dplyr::select(.data$Model, .data$Scenario, .data$Region, .data$Variable,
                    .data$Year, .data$Period, .data$Tech, .data$Value, .data$Hour)
    #################################################################

    outHourly <- rbind(outHourly, outH)
    outHourly <- rbind(outHourly, outTH)

    # Unit column

    outHourly$Unit <- substring(outHourly$Variable, regexpr("\\(", outHourly$Variable))
    outHourly <-  outHourly %>% dplyr::mutate(Unit = dplyr::if_else(grepl("\\(", .data$Unit), .data$Unit, "NA"))
    outHourly$Variable <- mapply(gsub, "\\(.*", "", outHourly$Variable)
    outHourly$Unit <- mapply(gsub, "\\(", "",  outHourly$Unit)
    outHourly$Unit <- mapply(gsub, "\\)", "",  outHourly$Unit)


    return(outHourly)

  }

  gdxToQuitteAnnual <- function(gdxfile) {

    file1 <- datapath(fname = gdxfile)
    outAnnual <- NULL
    ###############################################################################################
    reportAnnual <- read.gdx(gdxName = file1, requestList.name = "report", squeeze = FALSE)

    names(reportAnnual) <- c("gdxfile", "model", "year", "country", "variable", "value")
    out <- reportAnnual %>%
      dplyr::mutate(Model = .data$model, Scenario = "baseline",
                    Region = .data$country, Year = .data$year, Value = round(.data$value, digits = 4),
                    Tech = "all Tech",
                    Variable = .data$variable,
                    Period = "annual") %>%
      dplyr::arrange(.data$Year) %>%
      dplyr::select(.data$Model, .data$Scenario, .data$Region, .data$Variable,
                    .data$Year, .data$Period, .data$Tech, .data$Value)

    #################################################################
    repTech <- read.gdx(gdxName = file1, requestList.name = "report_tech", squeeze = FALSE)

    names(repTech) <- c("gdxfile", "model", "year", "country", "variable", "tech", "value")

    outT <- repTech %>%
      dplyr::select(.data$model, .data$year, .data$tech, .data$variable, .data$country, .data$value) %>%
      dplyr::group_by(.data$model, .data$tech, .data$variable, .data$country) %>%
      dplyr::mutate(year = as.numeric(.data$year)) %>%
      complete(year = c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2070, 2080, 2090, 2100)) %>%
      dplyr::mutate_all(coalesce, 0) %>%
      dplyr::ungroup(c("model", "tech", "variable", "country", "year")) %>%
      dplyr::mutate(Model = .data$model, Scenario = "baseline",
                    Region = .data$country, Year = .data$year, Value = round(.data$value, digits = 4),
                    Tech = .data$tech,
                    Variable = .data$variable,
                    Period = "annual"
      ) %>%
      dplyr::arrange(.data$Year) %>%
      dplyr::select(.data$Model, .data$Scenario, .data$Region, .data$Variable,
                    .data$Year, .data$Period, .data$Tech, .data$Value)

    #################################################################
    outAnnual <- rbind(outAnnual, out)
    outAnnual <- rbind(outAnnual, outT)

    # Get Unit column from Variable column

    outAnnual$Unit <- substring(outAnnual$Variable, regexpr("\\(", outAnnual$Variable))
    outAnnual <-  outAnnual %>% dplyr::mutate(Unit = dplyr::if_else(grepl("\\(", .data$Unit), .data$Unit, "NA"))

    outAnnual$Variable <- mapply(gsub, "\\(.*", "", outAnnual$Variable)
    outAnnual$Unit <- mapply(gsub, "\\(", "",  outAnnual$Unit)
    outAnnual$Unit <- mapply(gsub, "\\)", "",  outAnnual$Unit)


    return(outAnnual)

  }


  outAnnual <- gdxToQuitteAnnual(dieterDatafile)
  annualNew <- data.table::dcast(outAnnual, ... ~ Year, value.var = "Value")


  # check if these years are already present, if not then add
  if (!"2005" %in% colnames(annualNew)) {
    annualNew$`2005` <- NA
  }

  if (!"2110" %in% colnames(annualNew)) {
    annualNew$`2110` <- NA
  }
  if (!"2130" %in% colnames(annualNew)) {
    annualNew$`2130` <- NA
  }
  if (!"2150" %in% colnames(annualNew)) {
    annualNew$`2150` <- NA
  }


  # technology mapping
  dieterTechMapping <- c(CCGT = "CCGT",
                         Solar = "Solar",
                         Wind_on = "Wind",
                         bio = "Biomass",
                         OCGT_eff = "OCGT",
                         ror = "Hydro",
                         nuc = "Nuclear"
  )



  dieterTechMappingStandalone <- c(dieterTechMapping,
                                   lig = "Lignite",
                                   hc = "Hard coal",
                                   coal = "Coal (Lig + HC)"
  )

  annualNew$Tech <- as.factor(annualNew$Tech)
  annualRemind <-  annualNew
  annualNew <- annualNew %>% quitte::revalue.levels(Tech = dieterTechMappingStandalone)


  annualNew$Variable <- paste0(gsub("^\\W+|\\W+$", "", annualNew$Variable), "|", annualNew$Tech)
  annualNew  <- annualNew %>% dplyr::select(-.data$Period, -.data$Tech, -.data$`2005`)


  data.table::setDT(annualNew)
  EOL <- if (.Platform$OS.type == "windows") ";\r\n" else ";\n"

  dir.create(file.path(outputDir, subFolder), showWarnings = FALSE)
  # save Dieter Data as a seperate mif & rds
  fwrite(annualNew, file.path(outputDir, subFolder, "Dieter_Annual.mif"), append = F, sep = ";", eol = EOL)
  read.quitte(file.path(outputDir, subFolder, "Dieter_Annual.mif")) %>%
    readr::write_rds(paste0(gsub(".mif", "", file.path(outputDir, subFolder, "Dieter_Annual.mif")), ".rds"),
                     compress = "xz")


  # append model = 'DIETER' to REMIND-EU mif file
  # then for the switch that appends to the main mif, use coal = "Coal (Lig + HC)",

  dieterTechMappingRemind <- c(dieterTechMapping,
                               coal = "Coal (Lig + HC)"
  )

  annualRemind <- annualRemind %>% dplyr::filter(.data$Model == "DIETER", .data$Tech != "lig", .data$Tech != "hc")
  annualRemind <- annualRemind %>% quitte::revalue.levels(Tech = dieterTechMappingRemind)


  annualRemind$Variable <- paste0(gsub("^\\W+|\\W+$", "", annualRemind$Variable), "|", annualRemind$Tech)
  annualRemind  <- annualRemind %>% dplyr::select(-.data$Period, -.data$Tech)
  annualRemind <- annualRemind[, c(1:5, 24, 6:23)]


  data.table::setDT(annualRemind)

  # append to Main REMIND file
  # load main mif files
  remindMifFiles <- list.files(outputDir, pattern = "REMIND_generic", full.names = F)

  for (mifFile in remindMifFiles) {

    nameMif <- file.path(outputDir, mifFile)
    stopifnot(is.character(nameMif))

    file.copy(from = nameMif, to = file.path(outputDir, subFolder, mifFile), overwrite = TRUE, recursive = FALSE)
    fwrite(annualRemind, file.path(outputDir, subFolder, mifFile), append = T, sep = ";", eol = EOL)


    read.quitte(file.path(outputDir, subFolder, mifFile)) %>%
      readr::write_rds(paste0(gsub(".mif", "", file.path(outputDir, subFolder, mifFile)), ".rds"), compress = "xz")

  }
  ## Hourly Data
  outHourly <- gdxToQuitteHourly(dieterDatafile)
  outHourly  <- outHourly %>% dplyr::select(-.data$Period)

  hourlyNew <- data.table::dcast(outHourly, ... ~ Year, value.var = "Value")
  hourlyNew$Tech <- as.factor(hourlyNew$Tech)
  hourlyNew <- hourlyNew %>% quitte::revalue.levels(Tech = dieterTechMappingStandalone)

  hourlyNew$Variable <- paste0(gsub("^\\W+|\\W+$", "", hourlyNew$Variable), "|", hourlyNew$Tech)
  hourlyNew  <- hourlyNew %>% dplyr::select(-.data$Tech)
  hourlyNew <- hourlyNew[, c(1:4, 6, 7:24, 5)]

  annualNew$Hour <- NA

  # combine annual & hourly data

  hourlyAndAnnual <- rbind(hourlyNew, annualNew)
  hourlyAndAnnual <- hourlyAndAnnual[, c(1:5, 24, 6:23)]

  write.table(hourlyAndAnnual, file.path(outputDir, subFolder, "Dieter_Annualhourlyreport.csv"),
              sep = ";", row.names = F)

  # convert to  datatable
  # create mif and RDS
  data.table::setDT(hourlyAndAnnual)
  EOL <- if (.Platform$OS.type == "windows") ";\r\n" else ";\n"
  fwrite(hourlyAndAnnual, file.path(outputDir, subFolder, "Dieter_Annualhourly.mif"),
         append = F, sep = ";", eol = EOL)


  read.quitte(file.path(outputDir, subFolder, "Dieter_Annualhourly.mif")) %>%
    readr::write_rds(file.path(outputDir, subFolder, "Dieter_Annualhourly.rds"), compress = "xz")
}
