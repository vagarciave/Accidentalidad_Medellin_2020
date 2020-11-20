setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pkg_test_and_load <- function(pkg)
{
  if(pkg %in% rownames(installed.packages()))
  {
    library(pkg, character.only = TRUE)
  }
  else
  {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

init_pkgs <- function()
{
  pkgs <- c("rgdal", "leaflet","PKI", "packrat", "raster", "tidyverse","plotly",
            "reader","shiny","DT","lubridate", "dashboardthemes")
  lapply(pkgs, pkg_test_and_load)
}

init_pkgs()

