#When working with evonet as RStudio project, may need to 
#install the following packages when using new computer or 
#if R on CSDE server was updated.


install.packages("devtools")
install.packages("EpiModel")
install.packages("data.table")
install.packages("plotrix")
install.packages("testthat")
devtools::install_github( "statnet/tergmLite",force=T)
devtools::install_github( "statnet/EpiModel", ref ="fast_edgelist")
