*used to process data for figure S6 (exclude covid cases from room occupancy counts)

clear all
set more off

cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Analytic_dset"
use cdcr new_pos locstatus_num date month instid facid bldgid roomid residentid if (date<=22198) using analyticdset_2020-12-26.dta, clear
keep if cdcr==1

gen room_occ=1
replace room_occ=. if roomid==.|locstatus_num!=2 // don't count missing room info in occupancy info
bysort date month instid facid bldgid roomid: egen room_occ_total=total(room_occ), missing

*tag those who have ever had covid, by March 1, 2020
xtset residentid date
bysort residentid (date): gen covid_flag=sum(new_pos)
gen covid_march_tmp=covid_flag*(date==21975) //these are all 0's - no one has gotten confirmed covid by March 1
bysort residentid: egen covid_march=total(covid_march_tmp)
*tag those who have ever had covid, by October 10, 2020 (entire period loaded)
bysort residentid: egen covid_ever=total(new_pos)

keep if date==22198|date==21975
drop if covid_march==1 //covid cases from march are excluded from both panels (but we keep them in calcs of total room size)
drop if covid_ever==1 & date==22198 //covid cases between march and october are excluded from bottom panel (but we keep them in calcs of total room size)

cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Tess"

*FIGURE S4*
*calculate average room size over all prisons
bysort date: egen room_occ_mean_all=mean(room_occ_total)
*calculate total pop and average room size by inst over time
collapse (mean) room_occ_mean=room_occ_total (p2) room_occ2=room_occ_total (p3) room_occ3=room_occ_total (p97) room_occ97=room_occ_total (p98) room_occ98=room_occ_total, by(date month instid room_occ_mean_all)
export delimited using occ_no_covid.csv, replace
