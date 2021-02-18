*process data on covid outcomes for table 1, table s3, table s4

clear all
set more off

*load all days before Oct 10 to track cumulative covid outcomes
cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Analytic_dset"
use date cdcr locstatus_num instid numinmates residentid agecat covidriskcdcr newtest new_pos covidloc location flagfirst positive died if (date>=21975 & date<=22198) using analyticdset_2020-12-26.dta, clear
keep if cdcr==1

*only include those in custody >= 1 day since March 01
bysort residentid: egen num_days=sum(locstatus_num==2)
drop if num_days<1

sort residentid date

*generate person-level outcomes
gen hospital = ((covidloc == "Hospitalized" | covidloc == "ICU") & location == "OutToMedical")
replace hospital = 1 if died == 1 & hospital[_n-1] ==1 & residentid == residentid[_n-1]

gen icu = (covidloc == "ICU" & location == "OutToMedical")
replace icu = 1 if died == 1 & icu[_n-1] ==1 & residentid == residentid[_n-1]

gen died_cov_cvpos = (died == 1 & positive == 1)

*excluding one-day hospital stays
gen hosp_excl1day = hospital
replace hosp_excl1day  = 0 if (hospital == 1 & hospital[_n+1] == 0 & hospital[_n-1] == 0 & residentid == residentid[_n+1] & residentid == residentid[_n-1])
replace  hosp_excl1day = 0 if (hospital == 1 & hospital[_n+1] == 0 & residentid == residentid[_n+1] & flagfirst == 1)

*resolved only
gen rs = 1 if covidloc == "Resolved" | covidloc == "Died"
bys resid: egen rsx = sum(rs)
foreach var in died_cov_cvpos hosp_excl1day icu new_pos {
	gen 		`var'resolved = `var'
	replace 	`var'resolved = 0 if rsx == 0
}


xtset residentid date

*generate person-level variables
bysort residentid (date): gen test_ever=sum(newtest)
replace test_ever=1 if test_ever>1 & test_ever!=.
bysort residentid (date): gen covid_ever=sum(new_pos)
replace covid_ever=1 if covid_ever>1 & covid_ever!=.
bysort residentid (date): gen covid_resolved=sum(new_posresolved)
replace covid_ever=1 if covid_resolved>1 & covid_resolved!=.
bysort residentid (date): gen hosp_ever=sum(hosp_excl1day)
replace hosp_ever=1 if hosp_ever>1 & hosp_ever!=.
bysort residentid (date): gen hosp_resolved=sum(hosp_excl1dayresolved)
replace hosp_resolved=1 if hosp_resolved>1 & hosp_resolved!=.
bysort residentid (date): gen icu_ever=sum(icu)
replace icu_ever=1 if icu_ever>1 & icu_ever!=.
bysort residentid (date): gen icu_resolved=sum(icuresolved)
replace icu_resolved=1 if icu_resolved>1 & icu_resolved!=.
bysort residentid (date): gen covid_died=sum(died_cov_cvpos)
replace covid_died=1 if covid_died>1 & covid_died!=.

*variable for instid on last date of observation 
bysort residentid: egen last_date=max(date) //last date each person was in the dataset
bysort residentid: gen inst_const_tmp=instid if date==last_date 
bysort residentid: egen instid_const=mode(inst_const_tmp)

*use age and covid risk score at end
bysort residentid: gen age_const_tmp=agecat if date==last_date
bysort residentid: egen age_const=mean(age_const_tmp)
bysort residentid: gen covid_risk_const_tmp=covidriskcdcr if date==last_date
bysort residentid: egen covid_risk_const=mean(covid_risk_const_tmp)

*collapse all dates and take the max of cumulative outcomes (can't just take last day because some ppl have been released or died)
collapse (max) test_ever covid_ever covid_resolved hosp_ever hosp_resolved icu_ever icu_resolved covid_died, by(residentid instid_const age_const covid_risk_const)

preserve
collapse (sum) test_ever covid_ever covid_resolved hosp_ever hosp_resolved icu_ever icu_resolved covid_died, by(instid_const age_const covid_risk_const)
cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Tess"
export delimited using "table1_inter_outcomes.csv", replace
restore


exit, clear

