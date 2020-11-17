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
  pkgs <- c("mvtnorm", "ggplot2", "MBESS", "Matrix", "ks", 
            "caret", "rgdal", "leaflet", "knitr","rmarkdown", 
            "PKI", "packrat", "raster", "tidyverse")
  lapply(pkgs, pkg_test_and_load)
}

init_pkgs()