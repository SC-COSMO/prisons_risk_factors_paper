*processes data on activities for Table 1, Table S3, Table S4, Figure 2, Figure S8, Figure S9. Figure S13

clear all
set more off

cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Analytic_dset"
use date month instid cdcr locstatus_num agecat covidriskcdcr residentid workpersoncontacts schoolpersoncontacts otherpersoncontacts workminutes schoolminutes otherminutes workpersoncontactminutes schoolpersoncontactminutes otherpersoncontactminutes numinmates if inrange(date, 21960, 22198) using analyticdset_2020-12-26.dta, clear

keep if cdcr==1
drop cdcr

*define somebody as participating in an activity if they had personcontacts > 0 and minutes > 0
gen all=1*(locstatus_num==2) //don't count in numerator or denominator if not in custody
gen work_any=all*(workpersoncontacts>0 & workpersoncontacts!=. & workminutes>0 & workminutes!=.)
gen other_any=1*(otherpersoncontacts>0 & otherpersoncontacts!=. & otherminutes>0 & otherminutes!=.)

xtset residentid date 
bysort residentid (date): gen cum_sum_all=sum(all)
bysort residentid (date): gen cum_sum_work=sum(work_any)
bysort residentid (date): gen cum_sum_other=sum(other_any)

gen cum_sum_all_2wk=cum_sum_all-L14.cum_sum_all
gen cum_sum_work_2wk=cum_sum_work-L14.cum_sum_work
gen cum_sum_other_2wk=cum_sum_other-L14.cum_sum_other

*only count those who have been in custody for the past 2 weeks
replace cum_sum_all_2wk=0 if cum_sum_all_2wk!=14
replace cum_sum_all_2wk=1 if cum_sum_all_2wk==14

*of those who have been in custody for past 2 weeks, flag whether they worked any days
replace cum_sum_work_2wk=1 if cum_sum_work_2wk>=1 
replace cum_sum_work_2wk=0 if cum_sum_all_2wk==0
replace cum_sum_other_2wk=1 if cum_sum_other_2wk>=1 
replace cum_sum_other_2wk=0 if cum_sum_all_2wk==0

cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Tess"
preserve
collapse (sum) cum_sum_all_2wk cum_sum_work_2wk cum_sum_other_2wk, by(date month agecat instid)
export delimited using "age_activities.csv", replace
restore

preserve
collapse (sum) cum_sum_all_2wk cum_sum_work_2wk cum_sum_other_2wk, by(date month covidriskcdcr instid)
export delimited using "covidrisk_activities.csv", replace
restore

exit, clear

