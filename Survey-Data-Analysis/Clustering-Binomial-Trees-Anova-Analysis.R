setwd('')

install.packages("GGRidge")
install.packages("rpart")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(fastDummies)
library(rpart)
library(rattle)
library(GGRidge)
library(pls)
library(tidyverse)


Survey <- read.csv("Survey.csv", header=TRUE, sep=",",
                   na.strings="")
Survey <- Survey[-c(1:4,6:17,31,33,38,42,49:63,67)]
Survey <- Survey[-c(1,2),]
Survey %>% 
  filter(!Progress==100) %>% 
  summary(n())

Survey <- Survey %>% 
  rename("age"="Q1",
         "sex"="Q2",
         "nationality"="Q40",
         "education"="Q3",
         "relationship_status"="Q4",
         "household_size"="Q5",
         "income"="Q6",
         "Riga"="Q7",
         "Riga_80km"="Q8",
         "internet"="Q9")


Survey <- Survey %>% 
  filter(Progress>89|
           Progress==100,
         Riga_80km=="Yes"|
           Riga=="Yes")

Survey <- Survey %>% 
  mutate(Q10_has_used=ifelse(Q10=="No, I haven't",0,1))
Survey <- Survey %>% 
  mutate(Q10_regular=ifelse(Q10=="Yes, I do it often / regularly",1,0))

Survey <- Survey %>% 
  mutate(Q10.1_website=ifelse(grepl("website", Q10.1), 1, 0),
         Q10.1_time=ifelse(grepl("Time-consuming", Q10.1), 1, 0),
         Q10.1_deliverytime=ifelse(grepl("due", Q10.1), 1, 0),
         Q10.1_assistance=ifelse(grepl("assistance", Q10.1), 1, 0),
         Q10.1_people=ifelse(grepl("people", Q10.1), 1, 0),
         Q10.1_expensive=ifelse(grepl("Expensive", Q10.1), 1, 0),
         Q10.1_assortment=ifelse(grepl("assortment", Q10.1), 1, 0),
         Q10.1_deals=ifelse(grepl("deals", Q10.1), 1, 0),
         Q10.1_feel=ifelse(grepl("feel", Q10.1), 1, 0),
         Q10.1_easier_regular=ifelse(grepl("easier", Q10.1), 1, 0))


Survey <- Survey %>% 
  mutate(Q10.2_delivery_time=ifelse(grepl("Delivery time", Q10.2), 1, 0),
         Q10.2_quality=ifelse(grepl("quality", Q10.2), 1, 0),
         Q10.2_preorder=ifelse(grepl("pre-order", Q10.2), 1, 0),
         Q10.2_delivery_price=ifelse(grepl("price", Q10.2), 1, 0),
         Q10.2_range=ifelse(grepl("range", Q10.2), 1, 0),
         Q10.2_deal=ifelse(grepl("deal", Q10.2), 1, 0),
         Q10.2_price=ifelse(grepl("Lower", Q10.2), 1, 0),
         Q10.2_exclusive=ifelse(grepl("Exclusive", Q10.2), 1, 0),
         Q10.2_novelty=ifelse(grepl("Novelty", Q10.2), 1, 0),
         Q10.2_loyalty=ifelse(grepl("Loyalty", Q10.2), 1, 0),
         Q10.2_fresh=ifelse(grepl("Freshest", Q10.2), 1, 0),
         Q10.2_website=ifelse(grepl("web", Q10.2), 1, 0))

Survey <- Survey %>% 
  mutate(Q10.4_biweekly=ifelse(grepl("week",Q10.4),1,0))

Survey <- Survey %>% 
  mutate(Q10.5_rimi=ifelse(grepl("Rimi",Q10.5),1,0),
         Q10.5_barbora=ifelse(grepl("Barbora",Q10.5),1,0),
         Q10.5_nuko=ifelse(grepl("Nuko",Q10.5),1,0),
         Q10.5_pienaveikals=ifelse(grepl("Piena Veikals",Q10.5),1,0),
         Q10.5_augluserviss=ifelse(grepl("Augluserviss.lv",Q10.5),1,0),
         Q10.5_wolt=ifelse(grepl("Wolt",Q10.5),1,0),
         Q10.5_boltmarket=ifelse(grepl("Bolt",Q10.5),1,0),
         Q10.5_top=ifelse(grepl("Top",Q10.5),1,0),
         Q10.5_lats=ifelse(grepl("LaTS",Q10.5),1,0),
         Q10.5_livin=ifelse(grepl("Livin",Q10.5),1,0),
         Q10.5_delive=ifelse(grepl("Delive",Q10.5),1,0),)

Survey <- Survey %>%
  mutate(Q10.7_no=ifelse(grepl("Hasn't", Q10.7), 1, 0),
         Q10.7_more=ifelse(grepl("more", Q10.7), 1, 0),
         Q10.7_less=ifelse(grepl("less", Q10.7), 1, 0))

Survey <- Survey %>% 
  mutate(Q10.8_delivery_time=ifelse(grepl("Delivery time", Q10.8), 1, 0),
         Q10.8_quality=ifelse(grepl("quality", Q10.8), 1, 0),
         Q10.8_preorder=ifelse(grepl("pre-order", Q10.8), 1, 0),
         Q10.8_delivery_price=ifelse(grepl("price", Q10.8), 1, 0),
         Q10.8_range=ifelse(grepl("range", Q10.8), 1, 0),
         Q10.8_deal=ifelse(grepl("deal", Q10.8), 1, 0),
         Q10.8_price=ifelse(grepl("Lower", Q10.8), 1, 0),
         Q10.8_exclusive=ifelse(grepl("Exclusive", Q10.8), 1, 0),
         Q10.8_novelty=ifelse(grepl("Novelty", Q10.8), 1, 0),
         Q10.8_loyalty=ifelse(grepl("Loyalty", Q10.8), 1, 0),
         Q10.8_fresh=ifelse(grepl("Freshest", Q10.8), 1, 0),
         Q10.8_website=ifelse(grepl("web", Q10.8), 1, 0))

Survey <- Survey %>% 
  mutate(Q10.9_yes=ifelse(grepl("Yes",Q10.9),1,0),
         Q10.9_no=ifelse(grepl("No",Q10.9),1,0))

Survey <- Survey %>%   
  mutate(Q10.10_more=ifelse(grepl("More",Q10.10),1,0),
       Q10.10_less=ifelse(grepl("Less",Q10.10),1,0),
       Q10.10_no_change=ifelse(grepl("shops",Q10.10),1,0))

Survey <- Survey %>% 
  mutate(Q11_online=ifelse(grepl("Order",Q11),1,0),
         Q11_regular=ifelse(grepl("Go",Q11),1,0),
         Q11_both=ifelse(grepl("prefer",Q11),1,0),
         Q11_haventonline=ifelse(grepl("shopped",Q11),1,0))

Survey <- Survey %>% 
  mutate(Q15_yes=ifelse(grepl("Yes",Q15),1,0))

Survey <- Survey %>% 
  mutate(Q31_anywhere=ifelse(grepl("anywhere",Q31),1,0),
         Q31_time=ifelse(grepl("Saves",Q31),1,0),
         Q31_choose=ifelse(grepl("Opportunities",Q31),1,0),
         Q31_interactions=ifelse(grepl("Fewer",Q31),1,0),
         Q31_discounts=ifelse(grepl("discounts",Q31),1,0),
         Q31_impulse=ifelse(grepl("impulse",Q31),1,0),
         Q31_history=ifelse(grepl("history",Q31),1,0),
         Q31_pressurefree=ifelse(grepl("hurry",Q31),1,0),
         Q31_cart=ifelse(grepl("cart",Q31),1,0),
         Q31_environment=ifelse(grepl("environment",Q31),1,0),
         Q31_safer=ifelse(grepl("Safer",Q31),1,0),
         Q31_compare=ifelse(grepl("compare",Q31),1,0),)

Survey <- Survey %>% 
  mutate(Q32_deliverytime=ifelse(grepl("Delivery",Q32),1,0),
         Q32_quality=ifelse(grepl("Quality",Q32),1,0),
         Q32_assortment=ifelse(grepl("assortment",Q32),1,0),
         Q32_discounts=ifelse(grepl("carried",Q32),1,0),
         Q32_cravings=ifelse(grepl("cravings",Q32),1,0),
         Q32_damaged=ifelse(grepl("Damaged",Q32),1,0),
         Q32_wrong=ifelse(grepl("Wrong",Q32),1,0),
         Q32_conditions=ifelse(grepl("Bad",Q32),1,0),
         Q32_deliverycost=ifelse(grepl("Cost",Q32),1,0))

Survey <- Survey %>% 
  mutate(Q35_meat=ifelse(grepl("Meat",Q35),1,0),
         Q35_fish=ifelse(grepl("Fish",Q35),1,0),
         Q35_seafood=ifelse(grepl("Seafood",Q35),1,0),
         Q35_frozen=ifelse(grepl("Frozen",Q35),1,0),
         Q35_fruits=ifelse(grepl("Fruits",Q35),1,0),
         Q35_vegetables=ifelse(grepl("Vegetables",Q35),1,0),
         Q35_dairy_eggs=ifelse(grepl("Dairy",Q35),1,0),
         Q35_bread=ifelse(grepl("Bread",Q35),1,0),
         Q35_canned=ifelse(grepl("Canned",Q35),1,0),
         Q35_grains=ifelse(grepl("Grains",Q35),1,0),
         Q35_snacks=ifelse(grepl("Snacks",Q35),1,0),
         Q35_nota=ifelse(grepl("None",Q35),1,0))

summary(Survey)

Survey %>% 
  ggplot(aes(age))+geom_bar()

Survey %>% 
  ggplot(aes(sex))+geom_bar()

Survey %>% 
  ggplot(aes(income))+geom_bar()

Survey %>% 
  ggplot(aes(nationality))+geom_bar()

Survey %>% 
  ggplot(aes(education))+geom_bar()

Survey %>% 
  ggplot(aes(relationship_status))+geom_bar()

Survey %>% 
  ggplot(aes(household_size))+geom_bar()

SurveyCOVID <- Survey %>% 
  filter(Q10.7=="Yes, I now order more online")

summary(SurveyCOVID)


****
Survey %>% 
  ggplot(aes(Q10))+geom_bar()

Survey %>% 
  ggplot(aes(Q15))+geom_bar()

Survey %>% 
  ggplot(aes(Q11))+geom_bar()


ANOVA_has_used <- Survey %>% 
  aov(Q10_has_used~age+sex+nationality+education+relationship_status+income+
        Riga,data=.)
summary(ANOVA_has_used)

Survey %>% 
  glm(Q10_has_used~age+sex+income+Riga,data=.,family = binomial(link="logit")) %>% 
  summary

ANOVA_regular <- Survey %>% 
  aov(Q10_regular~age+sex+nationality+education+relationship_status+income+
        Riga,data=.)
summary(ANOVA_regular)




ANOVA_awareness <- Survey %>% 
  aov(Q15_yes~age+sex+nationality+education+relationship_status+income+
        Riga,data=.)
summary(ANOVA_awareness)

SurveyBin <- Survey %>%
  filter(sex == "Male" | sex == "Female")

tree.fit_has_used <- SurveyBin %>%
  rpart(Q10_has_used ~ sex+income+age+Riga, data=., 
        method="class")

fancyRpartPlot(tree.fit_has_used, caption = NULL)


ANOVA_Q11 <- Survey %>% 
  aov(Q11_online~age+sex+income+Riga+education+
        relationship_status+nationality+household_size,data=.) 
summary(ANOVA_Q11)

Survey <- Survey %>% 
  mutate(big_household=ifelse(household_size=="1 (Individual household)"|
                                household_size=="2",0,1))

Survey %>% 
  glm(Q11_online~big_household,data=.,family = binomial(link="logit")) %>% 
  summary


#### this is regularity - ANOVA shows no meaningful relationships. This could be due to small sample size because this question is only answered by those who shop regularly. 

ANOVA_regularity <- Survey %>% 
  filter(Q10=="Yes, I do it often / regularly") %>% 
  aov(Q10.4_biweekly~age+sex+income+Riga+education+
        relationship_status+nationality+household_size,data=.) 
summary(ANOVA_regularity)

####has covid made you purchase groceries online more (only for ppl that have purchased online)

Survey %>% 
  filter(!Q10=="No, I haven't") %>% 
  ggplot(aes(Q10.7))+geom_bar()

ANOVA_covid <- Survey %>% 
  filter(!Q10=="No, I haven't") %>%
  aov(Q10.7_more~age+sex+nationality+education+relationship_status+income+
        Riga,data=.)
summary(ANOVA_covid)

Survey %>% 
  filter(!Q10=="No, I haven't") %>%
  lm(Q10.7_more~age+Riga,data=.) %>% 
  summary

tree.fit_covid <- SurveyBin %>%
  filter(!Q10=="No, I haven't") %>%
  rpart(Q10.7_more ~ age+Riga, data=., 
        method="class")

fancyRpartPlot(tree.fit_covid, caption = NULL)

############################# Q10.1 un Q10.2 Cluster ANALYSIS


########REAL Q10.1

SampleClean22 <- Survey %>%
  filter(!Q10=="Yes, I do it often / regularly") %>% 
  select(Q10.1_website, Q10.1_assistance, Q10.1_people, Q10.1_assortment,Q10.1_deals, Q10.1_deliverytime, Q10.1_easier_regular, Q10.1_expensive, Q10.1_feel, Q10.1_people, 
         rich, big_household, single) %>%
  filter_all(all_vars(!is.na(.)))

clust.complete22 <- SampleClean22 %>%
  select(-rich, -big_household, -single) %>% 
  dist() %>%
  hclust(method="complete")


SampleClean22 <- SampleClean22 %>%
  mutate(cluster = cutree(clust.complete22, k=2))


test <- SampleClean22 %>%
  group_by(cluster) %>%
  summarise_all(mean)


#################### REAL Q10.2

SampleClean33 <- Survey %>%
  filter(!Q10=="Yes, I do it often / regularly") %>% 
  select(Q10.2_quality, Q10.2_preorder, Q10.2_delivery_price, Q10.2_range, Q10.2_deal, Q10.2_delivery_time, Q10.2_price, Q10.2_exclusive, Q10.2_novelty, Q10.2_loyalty, Q10.2_fresh, Q10.2_website,
         rich, big_household, single) %>%
  filter_all(all_vars(!is.na(.)))

clust.complete33 <- SampleClean33 %>%
  select(-rich, -big_household, -single) %>% 
  dist() %>%
  hclust(method="complete")


SampleClean33 <- SampleClean33 %>%
  mutate(cluster = cutree(clust.complete33, k=2))


test33 <- SampleClean33 %>%
  group_by(cluster) %>%
  summarise_all(mean)


