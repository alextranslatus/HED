

# vars_to_keep <- c(
#   "inwave5y", "wgt5y", "educ3", "sex", "mborn", "fborn", "born", "MCSID",
#   "APNGFE00_m","APCHSU00_m","APFASU00_m","APWKHA00_m","APCHUN00_m","APCHFA00_m",
#   "APSIPA00_m","APRASC00_m","APRESC00_m","APRANG00_m","APRENG00_m","APRAMA00_m",
#   "APREMA00_m","APLEBC00_m","APNOSE00_m","APCOTR00_m","APEDBP00_m",
#   "CPRASC00_m","CPRESC00_m","CPSCHB00_m","CPBRIT00_m","CPETHI00_m","CPCBRI00_m",
#   "CPCETH00_m",
#   "APNGFE00_p","APCHSU00_p","APFASU00_p","APWKHA00_p","APCHUN00_p","APCHFA00_p",
#   "APSIPA00_p","APRASC00_p","APRESC00_p","APRANG00_p","APRENG00_p","APRAMA00_p",
#   "APREMA00_p","APLEBC00_p","APNOSE00_p","APCOTR00_p","APEDBP00_p",
#   "CPRASC00_p","CPRESC00_p","CPSCHB00_p","CPBRIT00_p","CPETHI00_p","CPCBRI00_p",
#   "CPCETH00_p"
# )
# 
# mcs_hed <- mcs[, ..vars_to_keep]
# save(mcs_hed, file = "data/mcs_hed.Rdata")

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

# load data and dictionnary ####

# https://cls.ucl.ac.uk/cls-studies/millennium-cohort-study/
# https://ukdataservice.ac.uk/

load("data/mcs_hed.Rdata")
mcs <- as.data.table(mcs_hed)
dict_mcs <- read_excel("data/mcs_catalog.xlsx")

# let's look at one variable about social attitudes ####

# Sons should be encouraged more than daughters

table(mcs$CPSCHB00_m)

ggplot(mcs, aes(x = CPSCHB00_m)) + 
  geom_bar(position="identity")

# let's look at the dictionary to get the levels: ####

# (-9.0) Refusal (-8.0) Don't Know (-1.0) Not applicable (1.0) Strongly agree (2.0) Agree (3.0) Neither agree nor disagree (4.0) Disagree (5.0) Strongly disagree (6.0) Can t say

# what should we do next? ####

# clear up the levels: drop levels other than 1-5
# stratify: by education, by gender, by income, by ethnicity, etc.
# turn the y-axis to percentage
# stack the levels, to add to 100: allows us to compare different variables next + it's easier to read
# put the bars horizontally
# arrange the labels
# add weights
# make it pretty, while making sure to make the colours effective
# ...

# keep only relevant levels and reverse the coding so that disagree is on the left ####

mcs[CPSCHB00_m > 0 & CPSCHB00_m != 6, encour_sons_m:= 6 - CPSCHB00_m]

table(mcs$CPSCHB00_m)
table(mcs$encour_sons_m)

ggplot(mcs, aes(x = encour_sons_m)) + 
  geom_bar(position="identity")

# clean up labels and flip it ####

mcs$encour_sons_m <- as.factor(mcs$encour_sons_m)
levels(mcs$encour_sons_m) <- rev(c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))

table(mcs$encour_sons_m)

ggplot(mcs, aes(x = encour_sons_m)) + 
  geom_bar(position="identity") +
  coord_flip() +
  labs(title = "Sons should be encouraged more than daughters",
       x = "",
       y = "Counts")

# drop missing values ####
# careful, you should think quite a bit about what you do about missing values, here we just drop them to make the graph clearer

mcs_nan <- mcs %>% 
  drop_na(encour_sons_m)

ggplot(mcs_nan, aes(x = encour_sons_m)) + 
  geom_bar(position="identity") +
  coord_flip() +
  labs(title = "Sons should be encouraged more than daughters",
       x = "",
       y = "Counts")

# stacking the levels ####

ggplot(mcs_nan, aes(x = 1, fill = encour_sons_m)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Sons should be encouraged more than daughters",
       x = "",
       y = "Proportion",
       fill = "") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# ggstats likert plot ####

ggplot(mcs_nan) +
  aes(y = 1, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# making it prettier, adjusting the colours ####

ggplot(mcs_nan) +
  aes(y = 1, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() + # the order matters here: if you put theme_minimal after theme, it cancels out theme
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_likert()

# legend at the bottom, on one row, or get rid of it? ####

ggplot(mcs_nan) +
  aes(y = 1, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

ggplot(mcs_nan) +
  aes(y = 1, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +
  scale_fill_likert()

# what about weights? ####
# you need to create a weighted object:

mcs5 <- mcs_nan %>%
  filter(inwave5y == 1)
wmcs5 <- svydesign(ids = ~1, data = mcs5, weights = ~ mcs5$wgt5y)

# then:
ggsurvey(wmcs5) + # change this only
  aes(y = 1, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

# let's stratify! ####

ggsurvey(wmcs5) +
  aes(y = educ3, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

# clean up labels again ####

table(mcs_nan$educ3, useNA = "always")

# need to change educ3 in the non weighted data
mcs_nan$educ3 <- as.factor(mcs_nan$educ3)
levels(mcs_nan$educ3) <- c("Low", "Medium", "High")

table(mcs_nan$educ3, useNA = "always")

# then weigh it again
mcs5 <- mcs_nan %>%
  filter(inwave5y == 1)
wmcs5 <- svydesign(ids = ~1, data = mcs5, weights = ~ mcs5$wgt5y)

ggsurvey(wmcs5) +
  aes(y = educ3, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

# get rid of the missings, and weigh again
mcs5 <- mcs_nan %>%
  filter(inwave5y == 1) %>%
  drop_na(educ3)
wmcs5 <- svydesign(ids = ~1, data = mcs5, weights = ~ mcs5$wgt5y)

ggsurvey(wmcs5) +
  aes(y = educ3, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

# from then on, I'm using unweighted data, to make it simpler, but in your work, 
# you should code all of your variables the way you want, then weigh the data 
# and use that weighted object for your graphs ####

# we could compare mothers' and fathers' answers ####

# coding fathers' answers ####

table(mcs$CPSCHB00_p)

mcs[CPSCHB00_p > 0 & CPSCHB00_p != 6, encour_sons_f:= 6 - CPSCHB00_p]

table(mcs$CPSCHB00_p)
table(mcs$encour_sons_f)

mcs$encour_sons_f <- as.factor(mcs$encour_sons_f)
levels(mcs$encour_sons_f) <- rev(c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))

table(mcs$encour_sons_f)

mcs_nan <- mcs %>% 
  drop_na(encour_sons_m) %>% 
  drop_na(encour_sons_f)

table(mcs_nan$encour_sons_f)
table(mcs_nan$encour_sons_m)

# plot both ####

p1 <- ggplot(mcs_nan) +
  aes(y = 1, fill = encour_sons_m) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Mothers",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

p2 <- ggplot(mcs_nan) +
  aes(y = 1, fill = encour_sons_f) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Fathers",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

p1 + p2 + plot_layout(ncol = 1, heights = c(1, 1))

p1 / p2 + plot_layout(guides = "collect", axis_titles = "collect", axes = "collect") +
  plot_annotation(title = "Sons should be encouraged more than daughters") &
  theme(legend.position = 'bottom')

# not quite right
# let's do something different:
# cleaner: let's try to stratify by parent's sex ####

# we have two variables right now: for each child, we have encour_sons_m and encour_sons_f
# instead, for each child, we want encour_sons for mothers, and encour_sons for fathers

mcs_longer <- mcs_nan %>% 
  pivot_longer(cols=c('encour_sons_m', 'encour_sons_f'),
               names_to='parentsex',
               values_to='encour_sons')

head(mcs_longer$encour_sons)
head(mcs_longer$parentsex)

mcs_longer$parentsex[mcs_longer$parentsex == "encour_sons_m"] <- "Mothers"
mcs_longer$parentsex[mcs_longer$parentsex == "encour_sons_f"] <- "Fathers"

ggplot(mcs_longer) +
  aes(y = parentsex, fill = encour_sons) +
  geom_bar(position = "likert") +
  scale_x_continuous(label = label_percent_abs()) +
  labs(title = "Sons should be encouraged more than daughters",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_likert() +
  guides(fill = guide_legend(nrow = 1))

# can we test that these are really different? if so, how? ####

# ordinal logistic regression
model <- polr(encour_sons ~ parentsex, data = mcs_longer)
summary(model)
ctable <- coef(summary(model))
pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
cbind(ctable, "p value" = pvals)
exp(coef(model)) # mothers have about 20% lower odds of giving a more pro-agreement response

# or chi-square test

tab <- table(mcs_longer$parentsex, mcs_longer$encour_sons)
tab
chisq.test(tab)

# or you could collapse the levels to agree versus disagree, and run a logistic regression, etc.
# and then maybe you could add confidence intervals to your bars













