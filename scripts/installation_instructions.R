##########################################
#install evonet as a regular package 

if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)}
install_github("EvoNetHIV/EvoNet",subdir="pkg")
devtools::install_github( "statnet/tergmLite",force=T)
devtools::install_github( "statnet/EpiModel", ref ="fast_edgelist")
library(evonet)

##########################################
#install as RStudio project

# 1)install necessary dependencies
install.packages("devtools")
install.packages("EpiModel")
install.packages("data.table")
install.packages("plotrix")
install.packages("testthat")
devtools::install_github( "statnet/tergmLite",force=T)
devtools::install_github( "statnet/EpiModel", ref ="fast_edgelist")

#2) In Rstudio, choose "new project", then "version control", then "git"
#3) url: https://github.com/EvoNetHIV/EvoNetHIV/
#3) Tools -> Project options -> Build options
#    make sure Project Build Tools box has "package"
#    and package directory box has "pkg" as root directory
#4) Build tab -> Build and Reload

#############################################
#When working with evonet as RStudio project, may need to 
#install the following packages when using new computer or 
#if R on CSDE server was updated.# 
#If evonet or EpiModel is loaded, close RStudio and start fresh

install.packages("devtools")
install.packages("EpiModel")
install.packages("data.table")
install.packages("plotrix")
install.packages("testthat")
devtools::install_github( "statnet/tergmLite",force=T)
devtools::install_github( "statnet/EpiModel", ref ="fast_edgelist")

##########################################
#install absdiffby branch as package

install_github("EvoNetHIV/EvoNet",subdir="pkg",ref="absdiffby",force=TRUE)