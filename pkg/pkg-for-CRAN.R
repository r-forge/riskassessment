
# =========================================
# CREATION DE PACKAGE

#library(rstudioapi)
# library(roxygen2)
library(devtools)

if(length(list.files("~/MLboulot/programmes/qRa/fitdistrpack/travail_fitdistrplus")) > 0)
{
  setwd("~/MLboulot/programmes/qRa/fitdistrpack/travail_fitdistrplus")
}

if(length(list.files("~/Documents/recherche-enseignement/code/R/riskassessment/riskassessment/pkg/")) > 0)
{
    setwd("~/Documents/recherche-enseignement/code/R/riskassessment/riskassessment/pkg/")
}


rm(list=ls(all=TRUE))
#
check("fitdistrplus")
# 
# load_all()
#
build("fitdistrplus")
# 
# install("fitdistrplus")
#
ls()
#
load_all()
#

