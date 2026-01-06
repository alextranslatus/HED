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

load("data/mcs_hed.RData")
dict_mcs <- read_excel("data/mcs_catalog.xlsx")


ggplot(mcs_hed, aes(APCHUN00_m)) +
  geom_bar()

mcs_hed <- as.data.table(mcs_hed)

mcs_hed[APCHUN00_m > 0, childoutofmarriage:= APCHUN00_m]

ggplot(mcs_hed, aes(childoutofmarriage)) +
  geom_bar()

str(mcs_hed$childoutofmarriage)

mcs_hed$childoutofmarriage <- as.factor(mcs_hed$childoutofmarriage)

str(mcs_hed$childoutofmarriage)

levels(mcs_hed$childoutofmarriage) <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree", "Can t say")

table(mcs_hed$childoutofmarriage)

ggplot(mcs_hed, aes(childoutofmarriage)) +
  geom_bar()

ggplot(mcs_hed, aes(childoutofmarriage)) +
  geom_bar() +
  coord_flip()

mcs_hed <- mcs_hed %>%
  drop_na(childoutofmarriage)

ggplot(mcs_hed, aes(childoutofmarriage)) +
  geom_bar() +
  coord_flip()

ggplot(mcs_hed, aes(childoutofmarriage)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  labs(title = "All right to have children without being married",
       x = "",
       y = "Counts")

ggplot(mcs_hed, aes(childoutofmarriage)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  labs(title = "All right to have children without being married",
       x = "",
       y = "Counts")

ggplot(mcs_hed, aes(x = 1, fill = childoutofmarriage)) +
  geom_bar(position = "likert") +
  coord_flip() +
  theme_minimal() +
  labs(title = "All right to have children without being married",
       x = "",
       y = "Proportion",
       fill = "") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))




