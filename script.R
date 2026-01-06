# load packages ###

for(
  pkg in c(
    "ggplot2", # read csv
    "readxl", # read xlsx
    "dplyr", # basics
    "tidyr", # basics
    "ggplot2", # plots
    "data.table", # data.table
    "ggstats", # likert plot
    "survey", # weights
    "patchwork", # combining graphs
    "MASS"
  )
){
  if(!require(pkg, quietly = TRUE, character.only = TRUE)){
    install.packages(pkg)
  }
}

# if trouble loaded ggstats, try:
# remove.packages(c("ggplot2"))
# install.packages('ggplot2', dependencies = TRUE)
# library(ggstats)

# https://cls.ucl.ac.uk/cls-studies/millennium-cohort-study/
# https://ukdataservice.ac.uk/

