//////////////////////////
//This file selects dataset for survival analysis in Risk Factors paper
//Author:  Elizabeth Chin
//Feb 5, 2021
//directories
global d_piroot         /share/pi/jeremygf/CDCR
global d_root           /share/pi/jeremygf/CDCR/LEA_ROOT
global d_log            `"$d_root/Log_files"'
global d_TL          `"$d_piroot/TESS_LIZ"'

///////////////////////
//SET UP
/////////////////////

global filedate = "2020-12-26_16_00"
global filename = "2020-12-26"
global version = "v1"

//adust locals to reflect current filedate

local logname   RISKFACTORSSURV

//start logfile
cap log close
log using `"$d_log/`logname'"', replace

//grab the daily dataset
use if date < date("20201011","YMD") & date >= date("20200301","YMD") using `"$d_TL/Analytic_dset/analyticdset_$filename`'"',clear
drop *_000*

sort resid date
bysort resid (date) : gen cum_pos = sum(new_pos)
bysort resid (date) : gen cum_tests = sum(newtest)
gen after_pos = cum_pos > 0
replace after_pos = 0 if new_pos > 0 & cum_pos == 1
gen tests_until_pos = newtest & !after_pos

*drop all not in custody
gen incustody = locationstatus == "InCustody"
keep if incustody == 1

*Only include CDCR prisons
merge m:1 institutionid using `"$d_TL/Recodes/Classification`'"'
keep if _merge == 3
drop _merge

* get first date
sort resid date
by resid: egen date_admitted = min(date)

* if an individual has worked in the prior 2 weeks
gen work = 0
replace work = 1 if !missing(workpersoncontactminutes) & workpersoncontactminutes > 0
rangestat (sum) work, interval(date -13 0) by(resid) 
gen labor = work_sum > 0

* fill in missing time varying parameters (security level)
replace seclvl = seclvl[_n-1] if resid[_n-1]==resid & missing(seclvl)

sort instid date
by instid date: egen inst_pos = sum(new_pos)
by instid date: egen inst_tests = count(tests_until_pos)

sort resid date
bysort resid (date) : gen cum_outbreaks = sum(inst_pos >= 10)

gen date_outbreak = .
replace date_outbreak = date - 14 if cum_outbreaks == 1 & inst_pos >= 10
replace date_outbreak = date_outbreak[_n-1] if resid[_n-1]==resid & missing(date_outbreak)

gen instid_outbreak = ""
replace instid_outbreak = instid if cum_outbreaks == 1 & inst_pos >= 10
replace instid_outbreak = instid_outbreak[_n-1] if resid[_n-1]==resid & missing(instid_outbreak)

sort resid seq
replace seclvl = seclvl[_n-1] if resid[_n-1]==resid & missing(seclvl)
replace date_outbreak = date_outbreak[_n-1] if resid[_n-1]==resid & missing(date_outbreak)
replace instid_outbreak = instid_outbreak[_n-1] if resid[_n-1]==resid & missing(instid_outbreak)

gen outbreak_day = date - date_outbreak
gen test_day = .
replace test_day = outbreak_day if tests_until_pos > 0
gen test_day60 = test_day
gen test_day90 = test_day
gen test_day120 = test_day
gen test_day150 = test_day
replace test_day60 = . if test_day > 59
replace test_day90 = . if test_day > 89
replace test_day120 = . if test_day > 119
replace test_day150 = . if test_day > 149

gen ntest_day60 = tests_until_pos
gen ntest_day90 = tests_until_pos
gen ntest_day120 = tests_until_pos
gen ntest_day150 = tests_until_pos
replace ntest_day60 = . if test_day > 59
replace ntest_day90 = . if test_day > 89
replace ntest_day120 = . if test_day > 119
replace ntest_day150 = . if test_day > 149

gen pos_day = .
replace pos_day = outbreak_day if new_pos > 0
gen pos_day60 = new_pos
gen pos_day90 = new_pos
gen pos_day120 = new_pos
gen pos_day150 = new_pos
replace pos_day60 = . if outbreak_day > 59
replace pos_day90 = . if outbreak_day > 89
replace pos_day120 = . if outbreak_day > 119
replace pos_day150 = . if outbreak_day > 149

* drop if individual isn't at their first outbreak institutiton
* drop if prior to first day of outbreak
* censor at death
keep if instid == instid_outbreak
drop if outbreak_day < 0
drop if died + diedcustody > 0

format date_* %td

sort instid facid bldgid rmid date
by instid facid bldgid rmid date: egen room_labor = sum(labor)

sort resid date

save `"$d_TL/LIZ/risk_factors_surv_full_$filename`'"'

gen room = instid + "_" + facid + "_" + bldgid + "_" + rmid

collapse (firstnm) date_admitted test_day_first=test_day outbreak_day_pos=pos_day room pos_before=after_pos date_first=date date_outbreak outbreak_day_first=outbreak_day instid roomcensus labor room_labor covidriskcdcr seclvl ageinyears racex sex immunocompromised diabetes* ddp cvd* htn* mhi copd* asthma cancer* pregnancy lungdisease dialysis advancedliverdisease hiv* bmi (lastnm) date_last=date outbreak_day_last=outbreak_day (max) test_day_last=test_day test_day60 test_day90 test_day120 test_day150 pos_day* pos_day_last=new_pos (sum) ntest_last=tests_until_pos incustody ntest*, by(resid)

save `"$d_TL/LIZ/risk_factors_surv_admitted_$filename`'"'



