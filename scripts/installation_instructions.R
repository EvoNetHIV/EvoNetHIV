##########################################
#install evonet as a regular package 
# first close all open R or RStudio instances and open new one
if(!require("devtools")){
    install.packages("devtools")
}

install_github("EvoNetHIV/EvoNet",subdir="pkg", dependencies=T)
#note: if prompted probably best to choose to update all packages
#      but not necessary to build from source (if prompted)
#note: On CSDE servers, various errors may arise when installing/updating various packages,
#      easiest to shut down all open RStudio instances and repeat these steps until all packages
#      updated and installed
library(evonet)

##########################################
#install as RStudio project

# 1)install necessary dependencies and packages
# first close all open R or RStudio instances and open new one

if(!require("devtools")){
    install.packages("devtools")
}

#note: if prompted probably best to choose to update all packages
#      but not necessary to build from source (if prompted)
#note: On CSDE servers, various errors may arise when installing/updating various packages,
#      easiest to shut down all open RStudio instances and repeat these steps until all packages
#      updated and installed
install_github("EvoNetHIV/EvoNet",subdir="pkg", dependencies=T)


#2) In Rstudio, choose "new project", then "version control", then "git"
#3) url: https://github.com/EvoNetHIV/EvoNetHIV/
#3) Tools -> Project options -> Build options
#    make sure Project Build Tools box has "package"
#    and package directory box has "pkg" as root directory
#4) Build tab -> Build and Reload

#############################################
#install older evonet (Release 1, uses EpiModel versions <= 1.8) 

#go to:
# https://github.com/EvoNetHIV/EvoNetHIV/releases/tag/v1.0
#download appropriate version (e.g., zip for windows)
#use install.packages("evonet",repos= PATH/TO/FILE)
#see ?install.packages for  details

#then, for epimodel version that works with evonet "release 1"
#1
install.packages("remotes")
 #2 
library(remotes)
 #3 
install_version("EpiModel",version = "1.8.0")

##########################################
#install absdiffby branch as package
library(devtools)
install_github("EvoNetHIV/EvoNet",subdir="pkg",ref="absdiffby")
#or
#install_github("EvoNetHIV/EvoNet",subdir="pkg",ref="absdiffby_Aug_2018",force=TRUE)
devtools::install_github( "statnet/tergmLite",force=T)
