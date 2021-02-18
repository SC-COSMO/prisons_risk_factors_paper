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

local logname   RISKFACTORSSURV_ALL

//start logfile
cap log close
log using `"$d_log/`logname'"', replace

//grab the daily dataset
use if date < date("20201011","YMD") & date >= date("20200301","YMD") using `"$d_TL/Analytic_dset/analyticdset_$filename`'"',clear

sort resid date
bysort resid (date) : gen cum_tests = sum(newtest)

*drop all not in custody
gen incustody = locationstatus == "InCustody"
keep if incustody == 1

*Only include CDCR prisons
merge m:1 institutionid using `"$d_TL/Recodes/Classification`'"'
keep if _merge == 3
drop _merge

collapse (firstnm) date_first=date (lastnm) date_last=date instid (max) cum_tests (sum) incustody, by(resid)

save `"$d_TL/LIZ/risk_factors_surv_all_res_$filename`'"'

