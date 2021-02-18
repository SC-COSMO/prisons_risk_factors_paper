library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
source("process_followup.R")

data_all <- read_dta("~/risk_factors/risk_factors_surv_all_res_2020-12-26.dta") %>% filter(incustody > 0)
print(paste0("Total in custody: ", nrow(data_all)))

inst_names <- read_csv("~/risk_factors/inst_names.txt") %>% mutate(instid = as.character(instid))
data <- read_dta("~/risk_factors/risk_factors_surv_admission_2020-12-26.dta") %>% 
                filter(outbreak_day_first == 0) %>%
                mutate(ntest_day_last = ntest_last,
                       housing = ifelse(roomcensus > 2, "Dorm","Cell"), 
                       labor = ifelse(room_labor > 0, "Room labor", "No room labor"),
                      #seclvl = as.factor(seclvl),
                      risk6 = pmin(6, covidriskcdcr),
                      race = relevel(factor(case_when(
                          racex == 1 ~ "Black",
                          racex == 2 ~ "Hispanic",
                          racex == 3 ~ "Hispanic", #mexican
                          racex == 4 ~ "White",
                          TRUE ~ "Other/Unknown"
                        ), levels = c("Black","Hispanic","White","Other/Unknown")), ref = "White"),
                      cvd = pmax(cvd, cvdhigh)) %>% filter(pos_before == 0)

dataN <- process_followup(data, "last")

print(paste0("Housed in prison with outbreak: ", nrow(data_all %>% filter(instid %in% unique(dataN$instid)))))
print(paste0("Housed in prison at start of outbreak: ", nrow(dataN)))
print(paste0("Housed in prison without mass introduction of cases: ", nrow(dataN %>% filter(!(name %in% c("CCC","COR","SQ"))))))

data90 <- process_followup(data, 90) %>% filter(!(name %in% c("CCC","COR","SQ")))
print(paste0("Housed in prisons with outbreaks beginning on or before July 12, 2020: ", nrow(data90)))
print(paste0("Persons with at least one test during outbreak: ", nrow(data90 %>% filter(!is.na(test_day)))))

save(data90, "data/data90.rda")