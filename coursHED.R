

library(ggplot2)

load("data/analysisdata/mcs.Rdata")

table(mcs$APNGFE00_m)
table(mcs$APNGFE00_p)

# "Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree", "Can't say"

mcshed <- mcs %>% 
  as.data.table()

elfe[, wgt2m:= M02E_PONDREFC2]











table(elfehed$educ3, elfehed$matleave2m)

prop.table(table(elfehed$educ3, elfehed$matleave2m))

round(prop.table(table(elfehed$educ3, elfehed$matleave2m)), 2)


ggplot(elfehed, aes(x = educ3, fill = APNGFE00_m)) + 
  geom_bar(position="fill")

elfehed <- elfehed %>%
  mutate(across(-c(wgt2m, wgt1y, wgt2y, wgt3y, weights = wgt5y), as.factor))

for(i in c("nappies2m3", "tuckin2m3", "bath2m3", "walk2m3", "night2m3", "doctor2m3", "dishes2m3", "groceries2m3", "cook2m3", "laundry2m3", "clean2m3", "diy2m3")) {
  levels(elfehed[, i]) <- c("Mother mostly", "Balanced", "Father mostly")
}

for(i in c("nappies2m3", "tuckin2m3", "bath2m3", "walk2m3", "night2m3", "doctor2m3", "dishes2m3", "groceries2m3", "cook2m3", "laundry2m3", "clean2m3", "diy2m3")) {
  levels(elfehed[, i]) <- c("Mother mostly", "Balanced", "Father mostly")
}

for(i in c("leave")) {
  levels(elfehed[, i]) <- c("Both parents", "Father only", "Mother only", "Neither")
}

for(i in c("educ3")) {
  levels(elfehed[, i]) <- c("Low", "Medium", "High")
}

ggplot(elfehed, aes(x = educ3, fill = leave)) + 
  geom_bar(position="fill")

elfehed <- elfehed %>%
  drop_na(leave)


ggplot(elfehed, aes(x = educ3, fill = leave)) + 
  geom_bar(position="fill") +
  scale_y_continuous(name = "percentage", 
                     labels = scales::label_percent()) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Who takes parental leave?",
       x = "Highest level of education in the couple",
       fill = "Leave") +
  theme(axis.title.x=element_blank())


weighted_elfe <- elfehed %>% 
  drop_na(wgt2m)

weighted_elfe <- svydesign(ids = ~1, data = weighted_elfe, weights = ~ weighted_elfe$wgt2m)


ggplot(weighted_elfe, aes(x = educ3, fill = leave)) + 
  geom_bar(position="fill") +
  scale_y_continuous(name = "percentage", 
                     labels = scales::label_percent()) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Who takes parental leave?",
       x = "Highest level of education in the couple",
       fill = "Leave") +
  theme(axis.title.x=element_blank())






