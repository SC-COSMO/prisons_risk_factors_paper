*used to calculate numbers in figure S2 (panel D is in another script)

clear all
set more off

*load all days before Oct 10 to track cumulative covid outcomes
cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Analytic_dset"
use date cdcr locstatus_num instid facid bldgid roomid numinmates residentid if (date>=21915 & date<=22198) using analyticdset_2020-12-26.dta, clear
keep if cdcr==1

gen count_total=1*(date>=21975) //flag for in custody on or after March 1

*top of flow diagram
preserve
keep if locstatus_num==2
collapse (max) count_total, by(residentid)
tab count_total //124,634
restore

tab numinmates if locstatus_num==2 & date==21975 //starting pop in table 1, figure 1 left panel
tab numinmates if locstatus_num==2 & date==22198 //ending pop in table 1, figure 1 left panel

tab numinmates if locstatus_num==2 & date==21975 & roomid!=. //table 1 room occupancy info, figure 1 right panel, figure 2
tab numinmates if locstatus_num==2 & date==22198 & roomid!=. //table 1 room occupancy info, figure 1 right panel

gen march_flag=1*(date==21975)
bysort residentid: egen in_custody_march=max(march_flag)
tab in_custody_march if locstatus_num==2 & date==22198 & roomid!=.

*for activities 
xtset residentid date 
gen in_custody=1*(locstatus_num==2)
bysort residentid (date): gen cum_sum_all=sum(in_custody)
gen cum_sum_all_2wk=cum_sum_all-L14.cum_sum_all
replace cum_sum_all_2wk=0 if cum_sum_all_2wk!=14
replace cum_sum_all_2wk=1 if cum_sum_all_2wk==14

preserve
drop if date<21975
drop if locstatus_num!=2
collapse (max) cum_sum_all_2wk, by(residentid)
tab cum_sum_all_2wk //activities
restore

