*used to process data for figure 1, figure S5, figure s7, figure s10, figure s11, figure s12

clear all
set more off

cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Analytic_dset"
use cdcr locstatus_num date month instid facid bldgid roomid residentid agecat covidriskcdcr seclvl numinmates if (date==21975|date==22006|date==22036|date==22067|date==22097|date==22128|date==22159|date==22189|date==22198) using analyticdset_2020-12-26.dta, clear
*keep if date==21975|date==22006|date==22036|date==22067|date==22097|date==22128|date==22159|date==22189|date==22198 //first of the month plus Oct 10
keep if cdcr==1

gen pop=1
replace pop=0 if locstatus_num!=2 // fig 1 left panel only includes those in custody, includes those with missing room info
gen room_occ=1
replace room_occ=. if roomid==.|locstatus_num!=2 // don't count missing room info in occupancy info (right panel, other figures)
bysort date month instid facid bldgid roomid: egen room_occ_total=total(room_occ), missing

cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Tess"

*FIGURE 1, FIGURE S5*
preserve
*calculate average room size over all prisons
bysort date: egen room_occ_mean_all=mean(room_occ_total)
*calculate total pop and average room size by inst over time
collapse (sum) numinmates pop (mean) room_occ_mean=room_occ_total (max) room_occ_max=room_occ_total (p2) room_occ2=room_occ_total (p3) room_occ3=room_occ_total (p97) room_occ97=room_occ_total (p98) room_occ98=room_occ_total, by(date month instid room_occ_mean_all)
export delimited using fig1.csv, replace
restore

*FIGURE S7, S10, S11, S12
drop if locstatus_num!=2|roomid==.
*create room type variable
gen room_type="NA"
replace room_type="Cell" if room_occ_total==1|room_occ_total==2
replace room_type="Dorm" if room_occ_total>2 & room_occ_total!=.
*tab room_occ room_type, missing 
*drop ppl who weren't in the prison on March 1 2020
gen march_tag=1*(date==21975)
bysort residentid: egen in_custody_march=sum(march_tag)
*tab in_custody_march march_tag 
drop if in_custody_march==0 
*drop if room_type=="NA"
*for those in custody March 1, track status over time by room type and by age, covid risk score, security level, institution id
*include in each month's calculations if they're in custody and in a non-missing room ID during that month

preserve
collapse (sum) room_occ, by(date month instid room_type covidriskcdcr)
export delimited using "release_rehouse_risk.csv", replace
restore

preserve
collapse (sum) room_occ, by(date month instid room_type agecat)
export delimited using "release_rehouse_age.csv", replace
restore

preserve
collapse (sum) room_occ, by(date month instid room_type seclvl)
export delimited using "release_rehouse_sec.csv", replace 
restore

*stats for text
gen risk_high=1*(covidriskcdcr>=3)
tab room_type risk_high if date==22198, missing

exit, clear
