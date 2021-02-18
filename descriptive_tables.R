#Table 1, Table S3, Table S4

library(lubridate)
library(tidyverse)
library(scales)
library(ggpubr)
library(cowplot)

setwd("C:/Users/Tess/Box/Shared/Documents/COVID-19/SCCOSMO/Risk_Factors_Paper")
data <- read.csv("table1_intermediate.csv", stringsAsFactors=F)
data_outcomes <- read.csv("table1_inter_outcomes.csv", stringsAsFactors=F)
#data_outcomes2 <- read.csv("table1_inter_outcomesv2.csv", stringsAsFactors=F)
data_all <- read.csv("table1_pt2_intermediate.csv", stringsAsFactors=F)
data_div <- read.csv("tables3_intermediate.csv", stringsAsFactors=F)
data_outbreak <- read.csv("tables4_intermediate.csv", stringsAsFactors=F)

#also need to import activities and covid outcomes data separately
data_activities <- read.csv("age_activities.csv", stringsAsFactors=F)

#set dates as dates
data <- data %>% mutate(date=dmy(date))
data_all <- data_all %>% mutate(date=dmy(date))
data_div <- data_div %>% mutate(date=dmy(date))
data_outbreak <- data_outbreak %>% mutate(date=dmy(date))
data_activities <- data_activities %>% mutate(date=dmy(date))

#wrangle data_activities and data_outcomes for merging with data
data_activities <- data_activities %>% group_by(instid, date) %>% 
  summarise(work_2wk=sum(cum_sum_work_2wk), other_2wk=sum(cum_sum_other_2wk), all_2wk=sum(cum_sum_all_2wk))
data_activities <- data_activities %>% filter(date=="2020-03-01"|date=="2020-10-10")

data_outcomes <- data_outcomes %>% rename(instid=instid_const) %>% select(-hosp_ever) %>% select(-icu_ever)
data_outcomes <- data_outcomes %>% mutate(date=ymd("2020-10-10")) #no covid outcomes for March 1
data_outcomes_all <- data_outcomes %>% group_by(date, instid) %>% select(-age_const) %>% select(-covid_risk_const) %>% summarise_all(sum)
data_outcomes_age <- data_outcomes %>% mutate(agecat=case_when(age_const<=3~"18-49",
                                                               age_const>3 & age_const<=5~"50-69",
                                                               age_const>5~"70+"))
data_outcomes_age <- data_outcomes_age %>% group_by(date, instid, agecat) %>% summarise_all(sum)
data_outcomes_risk <- data_outcomes %>% mutate(covidriskcat=case_when(covid_risk_const<3~"<3",
                                                                      covid_risk_const>=3~"3+"))
data_outcomes_risk <- data_outcomes_risk %>% group_by(date, instid, covidriskcat) %>% summarise_all(sum)

#merge data together
data <- left_join(data, data_activities, by=c("instid", "date"))
data <- left_join(data, data_outcomes_all, by=c("instid", "date"))

#TABLE 1
data_all_counts <- data %>% subset(select=-c(instid, outbreak, division)) %>% 
  group_by(date) %>% summarise_all(sum)
data_all_props <- data_all_counts %>% group_by(date) %>%
  mutate_at(vars(-date, -numinmates, -all_2wk, -work_2wk, -other_2wk, -test_ever, -covid_ever, -covid_resolved,
                 -hosp_resolved, -icu_resolved, -covid_died), ~ ./ numinmates)
data_all_props <- data_all_props %>% 
  mutate(work_2wk=work_2wk/all_2wk, other_2wk=other_2wk/all_2wk,
    covid_died=covid_died/covid_resolved, icu_resolved=icu_resolved/covid_resolved, hosp_resolved=hosp_resolved/covid_resolved, 
    covid_resolved=covid_resolved/test_ever, covid_ever=covid_ever/test_ever, test_ever=NA)
data_all_counts <- left_join(data_all_counts, data_all %>% select(date, covidrisk_mean, roomocc_mean, bmi_mean, age_mean), by="date")
data_all_props <- left_join(data_all_props, data_all %>% select(date, covidrisk_sd, roomocc_sd, bmi_sd, age_sd), by="date")
data_all_counts <- data_all_counts %>% mutate(type="counts_means") %>% rename(covidrisk=covidrisk_mean, roomocc=roomocc_mean, bmi=bmi_mean, age=age_mean)
data_all_props <- data_all_props %>% mutate(type="props_sds") %>% rename(covidrisk=covidrisk_sd, roomocc=roomocc_sd, bmi=bmi_sd, age=age_sd)
data_all_out <- bind_rows(data_all_counts, data_all_props)
data_all_out <- data.frame(as.array(t(data_all_out)))
write.csv(data_all_out, "table1_final.csv", row.names=T)

#TABLE 1 OUTCOMES BY AGE
data_all_counts_age <- data_outcomes_age %>% group_by(agecat, date) %>% select(-instid) %>% summarise_all(sum) %>% mutate(type="counts")
data_all_props_age <- data_all_counts_age %>% group_by(agecat, date) %>% 
  mutate(covid_died=covid_died/covid_resolved, icu_resolved=icu_resolved/covid_resolved, hosp_resolved=hosp_resolved/covid_resolved, 
         covid_resolved=covid_resolved/test_ever, covid_ever=covid_ever/test_ever, test_ever=NA, type="props")
data_all_age_out <- bind_rows(data_all_counts_age, data_all_props_age)
write.csv(data_all_age_out, "table1_outcomes_age.csv", row.names=F)

#TABLE 1 OUTCOMES BY COVID RISK SCORE
data_all_counts_risk <- data_outcomes_risk %>% group_by(covidriskcat, date) %>% select(-instid) %>% summarise_all(sum) %>% mutate(type="counts")
data_all_props_risk <- data_all_counts_risk %>% group_by(covidriskcat, date) %>% 
  mutate(covid_died=covid_died/covid_resolved, icu_resolved=icu_resolved/covid_resolved, hosp_resolved=hosp_resolved/covid_resolved, 
         covid_resolved=covid_resolved/test_ever, covid_ever=covid_ever/test_ever, test_ever=NA, type="props")
data_all_risk_out <- bind_rows(data_all_counts_risk, data_all_props_risk)
write.csv(data_all_risk_out, "table1_outcomes_risk.csv", row.names=F)

#TABLE S3
data_div_counts <- data %>% subset(select=-c(instid, outbreak)) %>% 
  group_by(date, division) %>% summarise_all(sum)
data_div_props <- data_div_counts %>% group_by(date, division) %>%
  mutate_at(vars(-date, -division, -numinmates, -all_2wk, -work_2wk, -other_2wk, -test_ever, -covid_ever, -covid_resolved,
                 -hosp_resolved, -icu_resolved, -covid_died), ~ ./ numinmates)
data_div_props <- data_div_props %>% 
  mutate(work_2wk=work_2wk/all_2wk, other_2wk=other_2wk/all_2wk,
         covid_died=covid_died/covid_resolved, icu_resolved=icu_resolved/covid_resolved, hosp_resolved=hosp_resolved/covid_resolved, 
         covid_resolved=covid_resolved/test_ever, covid_ever=covid_ever/test_ever, test_ever=NA)
data_div_counts <- left_join(data_div_counts, data_div %>% select(date, division, covidrisk_mean, roomocc_mean, bmi_mean, age_mean), by=c("date", "division"))
data_div_props <- left_join(data_div_props, data_div %>% select(date, division, covidrisk_sd, roomocc_sd, bmi_sd, age_sd), by=c("date", "division"))
data_div_counts <- data_div_counts %>% mutate(type="counts_means")  %>% rename(covidrisk=covidrisk_mean, roomocc=roomocc_mean, bmi=bmi_mean, age=age_mean)
data_div_props <- data_div_props %>% mutate(type="props_sds")  %>% rename(covidrisk=covidrisk_sd, roomocc=roomocc_sd, bmi=bmi_sd, age=age_sd)
data_div_out <- bind_rows(data_div_counts, data_div_props) 
data_div_out <- data.frame(as.array(t(data_div_out)))
write.csv(data_div_out, "tables3_final.csv", row.names=T)

#TABLE 1 OUTBREAK VS. NON-OUTBREAK
data_outbreak_counts <- data %>% subset(select=-c(instid, division)) %>%
  group_by(date, outbreak) %>% summarise_all(sum)
data_outbreak_props <- data_outbreak_counts %>% group_by(date, outbreak) %>%
  mutate_at(vars(-date, -outbreak, -numinmates,-all_2wk, -work_2wk, -other_2wk, -test_ever, -covid_ever, -covid_resolved,
                 -hosp_resolved, -icu_resolved, -covid_died), ~ ./ numinmates)
data_outbreak_props <- data_outbreak_props %>% 
  mutate(work_2wk=work_2wk/all_2wk, other_2wk=other_2wk/all_2wk,
         covid_died=covid_died/covid_resolved, icu_resolved=icu_resolved/covid_resolved, hosp_resolved=hosp_resolved/covid_resolved, 
         covid_resolved=covid_resolved/test_ever, covid_ever=covid_ever/test_ever, test_ever=NA)
data_outbreak_counts <- left_join(data_outbreak_counts, data_outbreak %>% select(date, outbreak, covidrisk_mean, roomocc_mean, bmi_mean, age_mean), by=c("date", "outbreak"))
data_outbreak_props <- left_join(data_outbreak_props, data_outbreak %>% select(date, outbreak, covidrisk_sd, roomocc_sd, bmi_sd, age_sd), by=c("date", "outbreak"))
data_outbreak_counts <- data_outbreak_counts %>% mutate(type="counts_means")  %>% rename(covidrisk=covidrisk_mean, roomocc=roomocc_mean, bmi=bmi_mean, age=age_mean)
data_outbreak_props <- data_outbreak_props %>% mutate(type="props_sds") %>% rename(covidrisk=covidrisk_sd, roomocc=roomocc_sd, bmi=bmi_sd, age=age_sd)
data_outbreak_out <- bind_rows(data_outbreak_counts, data_outbreak_props)
data_outbreak_out <- data.frame(as.array(t(data_outbreak_out)))
write.csv(data_outbreak_out, "tables4_final.csv", row.names=T)