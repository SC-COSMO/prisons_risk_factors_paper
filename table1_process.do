*process data for table 1, table s3, table s4 (except covid outcomes and activities, which are separate)

clear all
set more off

*import crosswalk and save as dta file
cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Tess"
import delimited using inst_crosswalk_v3.csv, clear
tostring instid, replace
save crosswalk.dta, replace

cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Analytic_dset"
use date cdcr locstatus_num instid facid bldgid roomid numinmates male racex racex_old agecat ageinyears seclvl covidriskcdcr bmi advancedliverdisease asthma cancer copd lungdisease cvd cvdhigh diabetes diabeteshigh hiv hivhigh immunocompromised htn dialysis pregnancy comorbidities if (date==21975|date==22198) using analyticdset_2020-12-26.dta, clear

keep if cdcr==1
drop cdcr
keep if locstatus_num==2
drop locstatus_num

*convert dummies from categorical variables
replace cvd=1 if cvdhigh==1
gen comorbid_any=advancedliverdisease + asthma + cancer + copd + lungdisease + cvd + cvdhigh + diabetes + diabeteshigh + hiv + hivhigh + immunocompromised + htn + dialysis + pregnancy
replace comorbid_any=1*(comorbid_any>0)
tab seclvl, gen(seclvl_) //category left out is "missing" security level
tab agecat, gen(age_) 
replace age_7=age_7+age_8 //combine 80-89 and 90+
drop age_8
gen covid_risk_3plus=1*(covidriskcdcr>=3)
gen black=1*(racex_old==1)
gen hispanic=1*(racex_old==2|racex_old==3)
gen white=1*(racex_old==4)
gen other_race=1*(racex_old==5)
gen black_new=1*(racex==1)
gen hispanic_new=1*(racex==2|racex==3)
gen white_new=1*(racex==4)
gen other_race_new=1*(racex==5)
gen obese=1*(bmi>=40)
replace obese=. if bmi==.

*calculate room type info
gen room_occ=1
replace room_occ=. if roomid==. // don't count missing room info in occupancy info
bysort date instid facid bldgid roomid: egen room_occ_total=total(room_occ), missing
gen cell=1*inrange(room_occ_total, 1, 2)
gen dorm=1*(room_occ_total>2)
replace cell=0 if room_occ_total==.
replace dorm=0 if room_occ_total==.
gen roomocc_10=1*(room_occ_total>=10)
replace roomocc_10=. if room_occ_total==.


cd  "/share/pi/jeremygf/CDCR/TESS_LIZ/Tess"
merge m:1 instid using crosswalk.dta 

*TABLE 1: main counts/stats*
preserve
collapse (sum) numinmates male age_* seclvl_* covid_risk_3plus black* hispanic* white* other_race* cell dorm roomocc_10 obese comorbid_any advancedliverdisease asthma cancer copd cvd diabetes hiv htn immunocompromised, by(date instid division outbreak)
export delimited using "table1_intermediate.csv", replace
restore

*TABLE 1 pt 2: averages over all institutions*
preserve
collapse (mean) covidrisk_mean=covidriskcdcr roomocc_mean=room_occ_total bmi_mean=bmi age_mean=ageinyears (sd) covidrisk_sd=covidriskcdcr roomocc_sd=room_occ_total bmi_sd=bmi age_sd=ageinyears, by(date)
export delimited using "table1_pt2_intermediate.csv", replace
restore

*TABLE S3 (mostly use Table 1 file, but export avgs by division): 
preserve 
collapse (mean) covidrisk_mean=covidriskcdcr roomocc_mean=room_occ_total bmi_mean=bmi age_mean=ageinyears (sd) covidrisk_sd=covidriskcdcr roomocc_sd=room_occ_total bmi_sd=bmi age_sd=ageinyears, by(date division)
export delimited using "tables3_intermediate.csv", replace
restore

*TABLE S4 (Table 1 but only outbreak institutions): 
preserve 
collapse (mean) covidrisk_mean=covidriskcdcr roomocc_mean=room_occ_total bmi_mean=bmi age_mean=ageinyears (sd) covidrisk_sd=covidriskcdcr roomocc_sd=room_occ_total bmi_sd=bmi age_sd=ageinyears, by(date outbreak)
export delimited using "tables4_intermediate.csv", replace
restore

