context("REMIND reporting")

library(gdx)
library(data.table)

## Check REMIND output. dt is a data.table in *wide* format,
## i.e., variables are columns. `eqs` is a list of equations of the form
## list(LHS = "RHS", ...). The scope determines if the equations
## should be checked for regions ("regional"), only globally ("world") or
## both ("all"). Sensitivity determines the allowed offset when comparing
## LHS to RHS
check_eqs <- function(dt, eqs, scope="all", sens=1e-10){
  if(scope == "regional"){
    dt <- dt[all_regi != "World"]
  }else if(scope == "world"){
    dt <- dt[all_regi == "World"]
  }

  for(LHS in names(eqs)){
    exp <- parse(text=eqs[[LHS]])
    dt[, total := eval(exp), by=.(all_regi, ttot, scenario, model)]

    dt[, diff := total - get(LHS)]
    if(nrow(dt[abs(diff) > sens]) > 0){
      fail(
        sprintf("Check on data integrity failed for %s", LHS))
    }
    ## expect_equal(dt[["total"]], dt[[LHS]], tolerance = sens)
  }

}

test_that("Test if REMIND reporting is produced as it should and check data integrity", {

  ## uncomment to skip test
  ## success("Skip GDX test")

  testgdx_folder <- "../testgdxs/"

  ## add GDXs for comparison here:
  my_gdxs <- list.files("../testgdxs/", "*.gdx", full.names = TRUE)
  if(length(my_gdxs) == 0){
    dir.create(testgdx_folder, showWarnings = FALSE)
    download.file("https://rse.pik-potsdam.de/data/example/fulldata_REMIND21.gdx",
                  file.path(testgdx_folder, "fulldata.gdx"), mode="wb")
    if(md5sum(file.path(testgdx_folder, "fulldata.gdx")) != "8ce7127ec26cb8bb36cecee3f4fa97f1"){
      fail("MD5 hash not correct. Downloading GDX failed.")
    }
  }
  my_gdxs <- list.files("../testgdxs/", "*.gdx", full.names = TRUE)

  ## please add variable tests below
  check_integrity <- function(out){
    dt <- rmndt::magpie2dt(out)
    stopifnot(!(c("total", "diff") %in% unique(dt[["variable"]])))
    dt_wide <- data.table::dcast(dt, ... ~ variable)

    check_eqs(
      dt_wide,
      list(
        `FE|Transport|+|Liquids (EJ/yr)` = "`FE|Transport|Liquids|+|Biomass (EJ/yr)` + `FE|Transport|Liquids|+|Fossil (EJ/yr)`"
      ))

  }

  runParallelTests <- function(gdxs=NULL,cores=0,par=FALSE){

    new_gdxs <- NULL
    if (is.null(gdxs) & file.exists("/p/projects/")) {
      gdxs <- system2("find","/p/projects/remind/runs/r* -name fulldata.gdx",stdout=T,stderr = FALSE)
      gdxs <- grep("magpie",gdxs,v=T,invert=T,ignore.case = T)
      gd <- length(gdxs)
      gdxs <- gdxs[c(gd)] # floor(gd/2),floor(3*gd/4),
      gdxs <- c(gdxs,"/p/projects/remind/runs/0AA_DONTDELETEME_MO/fulldata.gdx")
      brnam <- unlist(strsplit(gdxs,split="/"))
      new_gdxs <- paste0(brnam[grep("fulldata.gdx",brnam)-1],".gdx")
      file.copy(gdxs,new_gdxs,overwrite = TRUE)
      gdxs <- new_gdxs
    }
    i <- NULL
    # if (Sys.info()["sysname"]=="Linux") {
    #   par <- TRUE
    #   library(foreach)
    #   doMC::registerDoMC(cores = 2)
    # }
    if (par) {
      foreach (i = gdxs) %dopar% {
        cat(paste0(i,"\n"))
        a <- convGDX2MIF(i)
        check_integrity(a)
        cat("\n")
      }
    } else {
      for (i in gdxs) {
        cat(paste0(i,"\n"))
        a <- convGDX2MIF(i)
        check_integrity(a)
        cat("\n")
      }
    }
    unlink(new_gdxs)
  }

  runParallelTests(my_gdxs)

})
