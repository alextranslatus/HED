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

mcs_hed[APCHUN00_m > 0 & APCHUN00_m != 6, childoutofmarriage:= 6 - APCHUN00_m]

ggplot(mcs_hed, aes(childoutofmarriage)) +
  geom_bar()

str(mcs_hed$childoutofmarriage)

mcs_hed$childoutofmarriage <- as.factor(mcs_hed$childoutofmarriage)

str(mcs_hed$childoutofmarriage)

levels(mcs_hed$childoutofmarriage) <- rev(c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))

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
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_likert()

table(mcs_hed$educ3)

# stratification par niveau d'éducation

mcs_hed <- mcs_hed %>%
  drop_na(childoutofmarriage) %>%
  drop_na(educ3)

mcs_hed$educ3 <- as.factor(mcs_hed$educ3)
levels(mcs_hed$educ3) <- c("Low", "Medium", "High")

ggplot(mcs_hed, aes(x = educ3, fill = childoutofmarriage)) +
  geom_bar(position = "likert") +
  coord_flip() +
  theme_minimal() +
  labs(title = "All right to have children without being married",
       x = "",
       y = "Proportion",
       fill = "") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_likert()

# pondérations

mcs5 <- mcs_hed %>% 
  filter(inwave5y == 1)
wmcs5 <- svydesign(ids = ~ 1, data = mcs5, weights = ~ mcs5$wgt5y)

ggsurvey(wmcs5, aes(x = educ3, fill = childoutofmarriage)) +
  geom_bar(position = "likert") +
  coord_flip() +
  theme_minimal() +
  labs(title = "All right to have children without being married",
       x = "",
       y = "Proportion",
       fill = "") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_likert()


# comparing mothers and fathers

mcs_hed$childoutofmarriage_m <- mcs_hed$childoutofmarriage
mcs_hed[APCHUN00_p > 0 & APCHUN00_p != 6, childoutofmarriage_p:= 6 - APCHUN00_p]
mcs_hed$childoutofmarriage_p <- as.factor(mcs_hed$childoutofmarriage_p)
levels(mcs_hed$childoutofmarriage_p) <- rev(c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))
mcs_hed <- mcs_hed %>%
  drop_na(childoutofmarriage_m) %>%
  drop_na(childoutofmarriage_p)

m <- ggplot(mcs_hed, aes(x = 1, fill = childoutofmarriage_m)) +
  geom_bar(position = "likert") +
  coord_flip() +
  theme_minimal() +
  labs(title = "",
       x = "Mothers",
       y = "",
       fill = "") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_likert()

p <- ggplot(mcs_hed, aes(x = 1, fill = childoutofmarriage_p)) +
  geom_bar(position = "likert") +
  coord_flip() +
  theme_minimal() +
  labs(title = "",
       x = "Fathers",
       y = "Proportion",
       fill = "") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_likert()

m / p + plot_annotation(title = "All right to have children without being married") +
  plot_layout(axes = "collect")




mcs_hed <- mcs_hed %>% 
  pivot_longer(cols = c('childoutofmarriage_m', 'childoutofmarriage_p'),
               names_to = 'parentsex',
               values_to = 'values')
  




