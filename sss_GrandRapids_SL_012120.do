*Choose state (change name below to full proper case name)
global state "Michigan"

*Define paths
global path "V:\Performance\Project files\Self-Sufficiency Analysis\\$state"
global raw "$path\Raw Data"
global stata "$path\Intermediate Datasets"
global output "$path\Output"
global lookups "$path\Lookups"

clear all
set more off

cd "$stata"	

***** Intermediate datasets *************
* Detroit_CPI.dta
* Fedpov_ss_thresh17.dta
* Detroit_CPIwide.dta
* AllMI_17_byage_county.dta
* AllMI_17_bykids_county.dta

***** adjust ACS microdata **************
cd "$stata"
	use "IPUMS_forLivWage.dta", clear

	*Subset for each metro
foreach loc in `"Grand Rapids-Kentwood, MI"'  /*Add propcase metro name here*/ {

		capture noisily {
		
		local exploc = subinstr("`loc'"," ","",.)
		local exploc = subinstr("`exploc'",",","",.)
		set more off
		
		*Load ACS data
		cd "$stata"
		use "IPUMS_forLivWage.dta", clear

		*Trim data and drop group quarters
		keep if strpos(cbsa_name,"`loc'") > 0 & !inlist(gq,3,4)
		disp "1"
		
		*Split households into families
		gen serial_str = string(serial,"%09.0f")
		gen sfn_str = string(famunit,"%02.0f")
		gen serial_fam = serial_str + sfn_str

		*Create demographic dummy variables
		gen obs = 1
		
		disp "2"
			
		*Working-age adults, and children
		gen workingage_adult = ((age >= 26 & age < 65) | ///
			(age >= 18 & age <= 25 & school == 1))
		gen adult = workingage_adult == 1 | age >= 65
		gen emp_adult = adult == 1 & empstat == 1
		gen unemp_adult = adult == 1 & empstat == 2
		gen wrkage_emp = empstat == 1 & workingage_adult == 1
		gen child = workingage_adult == 0 & age <= 25
		gen senior = age >= 65
		gen indkid = child == 1 & age >= 14
		gen depen = (child == 1 & age <= 12) | (dis == 1)
		gen col_student = school == 2 & age <= 25 & age >= 18
		gen emp = empstat == 1
		gen under18 = age < 18
		gen hhage = 1 if relate == 1 & age >= 65
			replace hhage = 2 if relate == 1 & age < 65
			replace hhage = 0 if relate != 1
			label define hhage_lbl 0 "Not Householder" 1 "Householder 65+" 2 "Householder <65"
			label values hhage hhage_lbl
		
		gen infant = child == 1 & age >= 0 & age <= 2
		gen preschooler = child == 1 & age >= 3 & age <= 5
		gen schoolage = child == 1 & age >= 6 & age <= 12
		gen teenager = child == 1 & age >= 13
		
		*Self-employed
		gen selfemp = inlist(classwkrd,13,14) & emp_adult == 1
		
		*Armed forces
		gen armfrc = (inlist(empstatd,13,14,15) | inlist(classwkrd,26)) & emp_adult == 1
			
		*Age categories
		gen agecat = 0
			replace agecat = 1 if child == 1
			replace agecat = 2 if workingage_adult == 1
			replace agecat = 3 if senior == 1
		label define agecat_lbl 0 `"Check"' 1 `"Child"' 2 `"Working-Age Adult"' 3 `"Senior"'
		label values agecat agecat_lbl
		
		*Specific age categories
		gen ageg = 1 if agecat == 2 & age >= 18 & age <= 24
			replace ageg = 2 if age >= 25 & age <= 34
			replace ageg = 3 if age >= 35 & age <= 44
			replace ageg = 4 if age >= 45 & age <= 54
			replace ageg = 5 if age >= 55
			replace ageg = 0 if agecat != 2
		label define ageg_lbl 0 `"Not working-age adult"' 1 `"18 - 24"' 2 `"25 - 34"' 3 `"35 - 44"' 4 `"45 - 54"' 5 `"55+"'
		label values ageg ageg_lbl
		
		*Home ownership
		gen owner = 1 if ownershpd == 12
		replace owner = 2 if ownershpd == 13
		replace owner = 0 if inlist(ownershpd,21,22)
		
		label define owner_lbl 0 `"Renting"' 1 `"Home owned outright"' 2`"Home owned with mortgage"'
		label values owner owner_lbl
			
		*Label
		replace eduatt = 4 if (child == 1 | educ == 0)
		label define eduatt_lbl 0 `"Less than High School"' 1 `"High School"' 2 `"Some College or AA"' ///
		3 "BA or Above" 4 `"N/A"'
		label values eduatt eduatt_lbl
		
		label define racecomp_lbl 1 `"White"'
		label define racecomp_lbl 2 `"Black"', add
		label define racecomp_lbl 3 `"Hispanic"', add
		label define racecomp_lbl 4 `"Asian"', add
		label define racecomp_lbl 5 `"Other"', add
		label values racecomp racecomp_lbl
		
		replace empstat = 0 if child == 1
		label define empstat_lbl 0 `"N/A"' 1 `"Employed"' 2 `"Unemployed"' 3 `"Not in Labor Force"', modify
		label values empstat empstat_lbl
		
		disp "3"
		
		*Adjust topcoded incomes
		foreach incvar in incinvst incwage incother incwelfr incretir incbus00 incsupp incss incearn inctot hhincome {
							/*intp incwage  oip      pap       retp     semp     ssip    ssp   pernp inctot hhincome {*/
			if inlist("`incvar'","incinvst","incretir","incbus00","incearn") {
				gen p_`incvar' = 0 if inlist(`incvar',999999) | `incvar' < 0
			}
			
			if inlist("`incvar'","incwage") {
				gen p_`incvar' = 0 if inlist(`incvar',999999,999998) | `incvar' < 0
			}
			
			if inlist("`incvar'","incother","incwelfr","incsupp","incss") {
				gen p_`incvar' = 0 if inlist(`incvar',99999) | `incvar' < 0
			}
			
			if inlist("`incvar'","inctot","hhincome") {
				gen p_`incvar' = 0 if inlist(`incvar',9999999) | `incvar' < 0
			}
			replace p_`incvar' = `incvar' if p_`incvar' == .
			
			*Not applying adjustment factor - IPUMS recommends avoiding since it's the same across all months
			*gen p_`incvar' = m_`incvar' /** adjust*/
			
		}
		disp "4"
		
// 		*Add in county definitions
// 		drop countyfips
// 		gen puma_year  = 2000 if year < 2012
// 			replace puma_year = 2012 if year >= 2012
//			
// 		cd "$lookups"
// 		merge m:1 puma_year puma statefip using "`exploc'_puma2county_allyrs.dta", keepusing(county) gen(locmerge)
// 		drop if locmerge == 2
// 		drop locmerge
		
		
		*Clean replicate weights
		forvalues i = 1/80 {
			rename repwtp`i' pwgtp`i'
			replace pwgtp`i' = 0 if pwgtp`i' < 0
			replace pwgtp`i' = perwt if pwgtp`i'==.
		}
		
		forvalues i = 1/80 {
			rename repwt`i' wgtp`i'
			replace wgtp`i' = 0 if wgtp`i' < 0
			replace wgtp`i' = perwt if wgtp`i'==.
		}

		*Save trimmed dataset
		cd "$stata"
		save "famlivwage_`exploc'_ipums.dta", replace
		disp "5"
		
		}

}
*output: famlivwage_ipums.dta


***** housing cost share ********
foreach loc in `"Grand Rapids-Kentwood, MI"'  /*Add propcase metro name here*/ {

	set more off
	local exploc = subinstr("`loc'"," ","",.)
	local exploc = subinstr("`exploc'",",","",.)
	disp "`loc'"
	
	capture noisily {
		
	*Load
		cd "$stata"
		use "famlivwage_`exploc'_ipums.dta", clear
		
	*Test where home values are populated - owners, or renters too?
		gen valueh_pop = valueh != . & valueh != 0 & valueh != 9999999
		
// 		tab2 ownershp valueh_pop
// 		tab2 ownershpd valueh_pop
// 		tab2 year valueh_pop
		
		gen rentgrs_pop = rentgrs != . & rentgrs != 0 & rentgrs != 9999999
		
// 		tab2 ownershpd rentgrs_pop
// 		tab2 year rentgrs_pop
		
		gen bedrooms_pop = bedrooms != .
		
// 		tab2 year bedrooms_pop
// 		tab2 year bedrooms
		
		*Only homeowners report values

	*Replace home value reports for people who do not own homes;
		gen valueh_own = valueh
			replace valueh_own = . if inlist(ownershpd,21,22) | valueh == 0 | valueh== .
		gen rentgrs_rent = rentgrs 
			replace rentgrs_rent = . if ownershpd != 22 | rentgrs == 0 
		
	*Collapse to find gross rent ratios (county:MSA)
		bys year serial_fam: gen famvals = _n == 1
		
	
		preserve
			keep if ownershpd == 22 & famvals == 1
			collapse (median) rentgrs [fweight = hhwt], by(year countyfips county_name)
			rename rentgrs rentgrs_cty
			cd "$stata"
			save "rentgrs_ctymed_`exploc'.dta", replace
		restore
		
		preserve
			keep if ownershpd == 22 & famvals == 1
			collapse (median) rentgrs if famvals == 1 [fweight = hhwt], by(year)
			rename rentgrs rentgrs_msa
			cd "$stata"
			save "rentgrs_msamed_`exploc'.dta", replace
		restore
		
		preserve
			cd "$stata"
			use "rentgrs_ctymed_`exploc'.dta", clear
			merge m:1 year using "rentgrs_msamed_`exploc'.dta", keepusing(rentgrs_msa)
			drop if _merge == 2
			drop _merge
			
			gen rentratio = rentgrs_cty / rentgrs_msa
			
			cd "$stata"
			save "rentgrs_ctymsaratio_`exploc'.dta", replace
		restore
	
	*Collapse to find median home prices by bedroom number 
		gen bedrooms_tc = 1 if bedrooms == 2
			replace bedrooms_tc = 2 if bedrooms == 3
			replace bedrooms_tc = 3 if bedrooms == 4
			replace bedrooms_tc = 4 if bedrooms >= 5
			 
		collapse (rawsum) valueh_pop rentgrs_pop (median) valueh_own rentgrs_rent [fweight = hhwt] ///
		if famvals == 1, by(year countyfips county_name bedrooms_tc)
		
		drop if bedrooms_tc == .
		
	*Correct housing values and rent amounts by making sure:
		* (1) for rent, that larger # of bedrooms has at least same value as lower # of bedrooms
		* (2) for housing, that lesser # of bedrooms does not exceed value of greater # of bedrooms
		
		* (1) rent correction - lag (since 4-bedroom rentals tend to have rent less than 3-bedroom)
		bys year countyfips: gen rent_lag = rentgrs_rent[_n - 1]
		replace rentgrs_rent = rent_lag if rentgrs_rent < rent_lag & rent_lag != .
		
		* (2) housing correction - lead (since 1-bedroom houses tend to have values that exceed 2-bedroom)
		gsort year countyfips -bedrooms_tc
		by year countyfips: gen hhval_lead = valueh_own[_n - 1]
		replace valueh_own = hhval_lead if valueh_own > hhval_lead & hhval_lead != .
		
// 		*Check the number of records by county and year combination
// 		bys year: tab2 county14 valueh_pop
// 		tab2 county14 rentgrs_pop
		
// 		*Check the median number of records per bedroom number for all counties and years
// 		tabstat rentgrs_pop, s(median) by(bedrooms_tc)
		
	*Calculate the monthly amount needed for property tax and homeowner's insurance
	*for people who own their home outright
	
		gen proptax_ins = ((0.007 * valueh_own) + 800) / 12
		
	*Calculate the amount needed for a down payment and closing costs

		gen down_close_cost = 0.08 * valueh_own
		
	*Calculate the amount needed to save this over ten years, assuming a 1.5% 
	*savings rate and accumulating over 10 years

		gen mosavings_winterest = down_close_cost * [(0.015/12) / (((1 + (0.015/12))^120)-1)]
		gen monthcheck = mosavings_winterest * (((1 + (0.015/12))^120 - 1) / (0.015/12))
		gen mosavings_wointerest = down_close_cost / 120
		
		drop monthcheck

		cd "$stata"
		save "median_home_savings_`exploc'.dta", replace
		
	}
	
}
* output: median_home_savings.dta


foreach loc in `"Grand Rapids-Kentwood, MI"'  {

	local exploc = subinstr("`loc'"," ","",.)
	local exploc = subinstr("`exploc'",",","",.)
	disp "`loc'"
	set more off
	
	capture noisily {
		
	cd "$stata"
	use "famlivwage_`exploc'_ipums.dta", clear
	drop incinvst incwage incother incwelfr incretir incbus00 incsupp incss incearn inctot

	*Find family income by various measures

	*Individual income combinations
	*Wages only - incwage
	gen incwage = p_incwage
	*Wages and cash transfers
	gen wagecasht = p_incwage + p_incwelfr
	*Wages, cash transfers, SSI, Social Security
	gen wagetrans = p_incwage + p_incwelfr + p_incsupp + p_incss
	*Wages, cash transfers, SSI, Social Security, and retirement
	gen wagetransret = p_incwage + p_incwelfr + p_incsupp + p_incss + p_incretir
	*Wages, retirement, other, and rental/interest (all non-transfer income), except include SS for seniors
	gen nontrans = p_incwage + p_incretir + p_incother + p_incinvst + p_incbus00
		*Adjusted above for seniors and people with disabilities
		gen nontransadj = nontrans
		replace nontransadj = nontrans + p_incss if senior == 1 | empstat == 36
		replace nontransadj = nontrans + p_incss + p_incsupp if dis == 1
	*Transfers only - cash transfers, SSI, Social Security
	gen trans = p_incwelfr + p_incsupp + p_incss
	*Wages, cash transfers, SSI, Social Security, retirement, other, and rental/interest (should = inctot)
	gen sumall = p_incwage + p_incwelfr + p_incsupp + p_incss + p_incretir + p_incother + p_incinvst + p_incbus00
	*Total income - inctot
	gen inctot = p_inctot
	*Gross income for benefits
	gen bengross = p_incwage + p_incbus00 + p_incretir + p_incother + p_incinvst + p_incss + p_incsupp
	*Net income for benefits
	gen bennet = bengross  /*- (90 * emp_adult_rec) - ((rentgrs + mortamt1 + mortamt2)*12)*/ /*Currently, we do not have information
	on how much individuals in the microdata spend on eligible deductions like housing, child care, or medical expenses,
	or much clarity on what expenses are eligible for deduction. This will result in more families being identified as
	SNAP-eligible families than there actually are*/
	
	*Flag if individual is receiving TANF
	gen welfrep = p_incwelfr > 0 & p_incwelfr != .
	*Flag if individual is receiving SNAP
	gen foodstmprep = foodstmp == 2 
	*Flag if individual is receiving SSI
	gen ssirep = p_incsupp > 0 & p_incsupp != .
	
	*Family income combinations
	foreach var in incwage wagecasht wagetrans wagetransret nontrans nontransadj trans sumall inctot bengross bennet {
		bys year serial_fam puma statefip: egen fam_`var' = total(`var')
	}

	*Determine family composition
	gen nilf_adult = adult == 1 & empstat == 3 & dis != 1
	gen nilf = empstat == 3 & dis != 1
	gen wrkage_lf = inlist(empstat,1,2) & workingage_adult == 1
	
	foreach mem in emp_adult unemp_adult child adult senior workingage_adult wrkage_emp wrkage_lf indkid ///
		infant preschooler schoolage teenager col_student nilf_adult nilf welfrep depen ///
		foodstmprep ssirep under18 hhage armfrc selfemp {
		bys year serial_fam puma statefip: egen `mem'_rec = total(`mem')
	}
	bys serial_fam year puma statefip: egen tot_members = total(obs)
		*For families where no one is designated householder, assign to oldest
		bys serial_fam year puma statefip: egen chkhead = total(cond(relate==1,1,0,.))
		bys year serial_fam puma statefip: egen max_age = max(age)
		replace hhage_rec = 1 if (chkhead == 0 | hhage_rec == 0) & max_age >= 65 & inlist(tot_members,1,2)
		replace hhage_rec = 2 if (chkhead == 0 | hhage_rec == 0) & max_age < 65 & inlist(tot_members,1,2)
		drop max_age chkhead
		*For households where all are under 18, clean to make all members adults
		replace under18_rec = 0 if under18_rec == tot_members
		
	*Correct transfer receipt numbers 
	replace welfrep_rec = 0 if poverty > 140 | under18_rec == 0 | under18_rec == .
	replace welfrep = 0 if poverty > 140 | under18_rec == 0 | under18_rec == .
	
	*Disability status - individual
	gen dis_status = dis == 1
	*School status - individual
	gen inschool = school == 2
	*Caretaker status
	gen caretaker = (nilf_adult == 1 & dis != 1 & depen_rec > 0 & emp_adult_rec > 0)
	
	*Drop families where all the employed adults are either self-employed, a likely college student,
	*or in the armed forces
	drop if armfrc_rec != 0 & armfrc_rec != . & armfrc_rec == emp_adult_rec
	drop if selfemp_rec != 0 & selfemp_rec != . & selfemp_rec == emp_adult_rec
	drop if col_student_rec != 0 & col_student_rec != . & ///
		col_student_rec == tot_members & col_student_rec == nilf_rec
	
	*Generate family-wide flags
	gen notworking_flag = adult_rec > emp_adult_rec
	gen indkids_only = indkid_rec == child_rec & child_rec != 0
	gen support_kids = child_rec > 0 & child_rec != . /*& workingage_adult_rec > 0 & workingage_adult_rec != .*/
	
	*Top-code certain family compositions for federal poverty threshold
	gen tot_members_cat  = tot_members
		replace tot_members_cat = 9 if tot_members > 9
	gen under18_rec_raw = under18_rec
	replace under18_rec = 8 if under18_rec > 8
	
	*For families greater than two people, age of householder is irrelevant
	replace hhage_rec = 0 if tot_members > 2
	
// 	*Merge fed poverty data onto microdata by total family members num
// 	cd "$stata"
// 	merge m:1 year tot_members_cat under18_rec hhage_rec using "Fedpov_ss_thresh16.dta", gen(fed_merge)
// 	drop if fed_merge == 2
//	
// 	drop fed_merge
//	
// 	*Create alternative poverty variable (unsure how IPUMS calculates theirs)
// 	rename poverty poverty_ipums
//	
// 	gen poverty = fam_bengross / (costs_fedpov_adj / 2) * 100
// 	replace poverty = 500 if poverty > 500
	

	*Find number of bedrooms required by each family
	gen adult_bedrq = ceil(adult_rec / 2)
	gen child_bedrq = ceil(child_rec / 2)
	gen bedrooms_tc = adult_bedrq + child_bedrq
		replace bedrooms_tc = 4 if bedrooms_tc > 4
		
	*Drop extra variables
	drop rent rentgrs famunit subfam ///
	schltype trantime serial_str sfn_str emp_adult unemp_adult nilf_adult ///
	indkid emp infant preschooler schoolage teenager 
	/*p_* incwage wagecasht wagetrans wagetransret nontrans nontransadj trans sumall inctot*/
	
	cd "$stata"
	save "famlivwage_`exploc'2_ipums.dta", replace
	
//  	cd "$stata"
//  	use "famlivwage_`exploc'2_ipums.dta", clear
	
 	*Merge fed poverty data onto microdata by total family members num
 	cd "$stata"
 	merge m:1 year tot_members_cat under18_rec hhage_rec using "Fedpov_ss_thresh17.dta", gen(fed_merge)
 	drop if fed_merge == 2
	
 	drop fed_merge
		
	*Merge on self-sufficiency threshold by family composition - 
	*merge on one cost measure by puma, and one by county
	
	local _county
	local _puma puma 
	
	foreach geocost in _county /*_puma*/ {
	
		capture noisily {
		
		*First, merge by age-specific child categories;
		cd "$stata"
		merge m:1 year countyfips ``geocost'' adult_rec infant_rec preschooler_rec schoolage_rec ///
			teenager_rec using "AllMI_17_byage`geocost'12.dta", gen(ss1)
		drop if ss1 == 2
		
		*Then, merge by aggregate child numbers
		merge m:1 year countyfips ``geocost'' adult_rec child_rec using "AllMI_17_bykids`geocost'12.dta", gen(ss2)
		drop if ss2 == 2
		
		drop ss1 ss2
		
// 		*Replace extrapolated housing from self-sufficiency standard with FMR updates
// 		cd "$stata"
// 		merge m:1 year county bedrooms_tc using "fmr_2000_2016_`exploc'.dta", keepusing(fmr_estimate)
// 		drop if _merge == 2
// 		drop _merge
// 		replace Housing = fmr_estimate_adj if Housing != .
// 		replace Housing2 = fmr_estimate_adj if Housing2 != .
			
		*Add in savings for home down payment for renters
		cd "$stata"
		merge m:1 year countyfips bedrooms_tc using "median_home_savings_`exploc'.dta", keepusing(mosavings_wointerest proptax_ins)
		drop if _merge == 2
		drop _merge
		
		*Make adjustments for outright home ownership
// 		set varabbrev off
		replace Housing = proptax_ins if ownershpd == 12 & Housing != .
		replace Housing2 = proptax_ins if ownershpd == 12 & Housing2 != .
		
		gen MortgageSavings = mosavings_wointerest if inlist(ownershpd,21,22) 
			replace MortgageSavings = 0 if !inlist(ownershpd,21,22)
			drop mosavings_wointerest
		
		*Make adjustments for family compositions and childcare status
		replace ChildCare = 0 if caretaker == 1 & ChildCare != .
		replace ChildCareTaxCredit = 0 if caretaker == 1 & ChildCareTaxCredit != .
		replace ChildCare2 = 0 if caretaker == 1 & ChildCare2 != .
		replace ChildCareTaxCredit2 = 0 if caretaker == 1 & ChildCareTaxCredit2 != .  
		
		*Remove EITC if household above 245% of poverty threshold or above maximum limit
		replace EITC = 0 if poverty > 245 & EITC != .
		replace EITC2 = 0 if poverty > 245 & EITC2 != .
		replace EITC = -6431 if EITC < -6431 & EITC != . 
		replace EITC2 = -6431 if EITC2 < -6431 & EITC2 != .
		replace taxrate`geocost' = 0.15 if taxrate`geocost' < 0 & taxrate`geocost' != .
		replace taxrate`geocost'2 = 0.15 if taxrate`geocost'2 < 0 & taxrate`geocost'2 != .
		
		*Add in savings for emergency fund (custom, not UW)
		egen monthly_excltax = rowtotal(Housing ChildCare Food Transportation HealthCare ///
		Miscellaneous EITC ChildCareTaxCredit ChildTaxCredit)
		gen EmergencySavings = ((monthly_excltax * 3) / 108) / emp_adult_rec
			replace EmergencySavings = ((monthly_excltax * 3) / 108) if inlist(emp_adult_rec,0,.)
		
		gen monthly_incltax = (monthly_excltax + MortgageSavings + EmergencySavings) / (1 - taxrate`geocost') if taxrate`geocost' != .
		gen annualcosts`geocost' = monthly_incltax * 12
		
		egen monthly_excltax2 = rowtotal(Housing2 ChildCare2 Food2 Transportation2 HealthCare2 ///
		Miscellaneous2 EITC2 ChildCareTaxCredit2 ChildTaxCredit2)
		
		gen EmergencySavings2 = ((monthly_excltax2 * 3) / 108) / emp_adult_rec
			replace EmergencySavings2 = ((monthly_excltax2 * 3) / 108) if inlist(emp_adult_rec,0,.)
		
		gen monthly_incltax2 = (monthly_excltax2 + MortgageSavings + EmergencySavings2) / (1 - taxrate`geocost'2) if taxrate`geocost'2 != .
		gen annualcosts2`geocost' = monthly_incltax2 * 12
		
		if "`geocost'" == "_county" {
			
			capture noisily {
			
			preserve
			
				drop Taxes
				gen Taxes = monthly_incltax - (monthly_excltax + MortgageSavings + EmergencySavings)
				
				contract year countyfips emp_adult_rec nilf_adult_rec adult_rec infant_rec preschooler_rec schoolage_rec ///
				teenager_rec child_rec /*caretaker*/ owner ///
				Housing ChildCare Food Transportation HealthCare ///
				Miscellaneous EITC ChildCareTaxCredit ChildTaxCredit /// 
				EmergencySavings MortgageSavings Taxes
				
				*Create name for family compositions;
				gen adultc = string(adult_rec) + " adult" if adult_rec >= 1
					replace adultc = subinstr(adultc,"adult","adults",1) if adult_rec > 1
					replace adultc = adultc + " (" + string(nilf_adult) + " not working)" if nilf_adult_rec > 0
					
				gen childc = ", " + string(infant_rec) + " infant" if infant_rec >= 1			
					replace childc = subinstr(childc,"infant","infants",1) if infant_rec > 1
					
					replace childc = childc + ", " + string(preschooler_rec) + " preschooler" if preschooler_rec >= 1
					replace childc = subinstr(childc,"preschooler","preschoolers",1) if preschooler_rec > 1
					
					replace childc = childc + ", " + string(schoolage_rec) + " schoolage child" if schoolage_rec >= 1
					replace childc = subinstr(childc,"schoolage child","schoolage children",1) if schoolage_rec > 1
					
					replace childc = childc + ", " + string(teenager_rec) + " teenager" if teenager_rec >= 1
					replace childc = subinstr(childc,"teenager","teenagers",1) if teenager_rec > 1
					
				decode owner, gen(ownerc)
				
				gen famcomp = adultc + childc + " (" + ownerc + ")"
				
				order countyfips famcomp Housing ChildCare Food Transportation HealthCare ///
				Miscellaneous EITC ChildCareTaxCredit ChildTaxCredit /// 
				EmergencySavings MortgageSavings Taxes adultc childc ownerc
				
// 				drop *_rec
				keep if emp_adult_rec + nilf_adult_rec == adult_rec
				keep if year == 2017
				
				cd "$output"
				export excel using "`loc' Family Expense Categories (IS $S_DATE).xlsx", ///
					firstrow(variables) sheet("county_exp") sheetreplace
				cd "$stata"
				
			restore
			
			}
			
			}
		}
		
		/*
		*Drop extra variables
		drop Housing* ChildCare* Food* Transportation* HealthCare* ///
		Miscellaneous* EarnedIncome* ChildCareTaxCredit* ChildTaxCredit* /// 
		EmergencySavings* MortgageSavings Taxes* taxrate* monthly_excltax* monthly_incltax*
		*/
		
		*Populate living wage with twice fed poverty, if living wage blank
		gen living_hhinc`geocost' = annualcosts`geocost'
			replace living_hhinc`geocost' = annualcosts2`geocost' if annualcosts`geocost' == .
			disp "1"
			replace living_hhinc`geocost' = costs_fedpov_adj if ///
			living_hhinc`geocost' == . | (living_hhinc`geocost' < 0 & living_hhinc`geocost' != .)
			disp "2"
			
		*Display the number of records where twice fed poverty was used
		gen fedchk`geocost' = (living_hhinc`geocost' == costs_fedpov_adj & ///
			annualcosts`geocost' == . & annualcosts2`geocost' == .)
		tab year fedchk`geocost', row
		tab year fedchk`geocost' [fweight = perwt], row

		*Identify families below the living wage by different income measures
		foreach var in incwage inctot wagecasht wagetrans nontrans nontransadj sumall {
			gen below_`var'`geocost' = fam_`var' < (living_hhinc`geocost')
		}
		gen below_ftotinc`geocost' = ftotinc < living_hhinc`geocost'
		gen below_hhincome`geocost' = hhincome < living_hhinc`geocost'
		
		*Identify families lifted above threshold by transfers (cash, SSI, Social Security)
		gen translift`geocost' = below_nontrans`geocost' == 1 & below_sumall`geocost' == 0
			replace translift`geocost'  = 2 if translift`geocost' == 0 & below_nontrans`geocost' == 0
			replace translift`geocost' = 0 if below_sumall`geocost' == 1
			
		label define translift`geocost'_lbl 0 `"Below Including Transfers"' 1 `"Above Due to Transfers"' 2 `"Above Due to Wages"'
		label values translift`geocost' translift`geocost'_lbl
			
	}
	
	label variable puma `"PUMA"'
	
	*Sanity check for struggling share by nontransadj county
	tab2 year below_nontransadj_county [fweight = perwt], row
	
	cd "$stata"
	save "famlivwage_`exploc'3_ipums_t.dta", replace
	
	}
	
}
*End for struggling family ID 

/*********************
Aggregate and Export
**********************/

foreach loc in `"Grand Rapids-Kentwood, MI"' {

	local exploc = subinstr("`loc'"," ","",.)
	local exploc = subinstr("`exploc'",",","",.)
	disp "`loc'"
	set more off
	
	capture noisily {

	*Collapse using basic crosstabs - empstat, gender, education, race, age

	*Identify subset
	foreach subpop in all {

		*Identify desired geography
		
		local msa
		local puma puma
		local desert desert
		local county county
		
		foreach geo in msa /*puma desert county*/ {
		
			*Identify desired income variable

			foreach incvar in /*below_ftotinc below_hhincome below_inctot*/ below_nontransadj_county /*below_nontransadj_puma below_incwage translift below_ftotinc*/ {
				set more off
				
				cd "$stata"
				use "famlivwage_`exploc'3_ipums_t.dta", clear
				
				bys year puma serial_fam: gen famvals = _n == 1
				
				*If the geography is the MSA, produce all crosstabs
				if "`geo'" == "msa" {
					
					*Collapse using primary person weight
					disp "`incvar' `geo' `subpop' 1"
					preserve
						collapse (rawsum) records=obs (sum) weight_pop=obs [fweight = perwt], ///
							by(year ``geo'' `incvar' agecat ageg dis_status sex eduatt racecomp empstat support_kids selfemp)
						gen rectype = "Person"
						cd "$stata"
						save "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", replace
					restore
					
					*Repeat, for families
					disp "`incvar' `geo' `subpop' 1 hh"
					preserve
						collapse (rawsum) records=obs (sum) weight_pop=obs [fweight = hhwt] if famvals == 1, ///
							by(year ``geo'' `incvar')
						gen rectype = "Family"
						cd "$stata"
						save "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta", replace
					restore
						
					
					*Collapse using replicate weights
					disp "`incvar' `geo' `subpop' 2"
					forvalues i = 1/80 {
						disp "`incvar' `geo' `subpop' 2_`i'"
						set more off
						preserve
							collapse (sum) weight_pop`i'=obs [fweight = pwgtp`i'], ///
								by(year ``geo'' `incvar' agecat ageg dis_status sex eduatt racecomp empstat support_kids selfemp)
							cd "$stata"
							merge 1:1 year ``geo'' `incvar' agecat ageg dis_status sex eduatt racecomp empstat support_kids selfemp using "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta"
							drop _merge
							save "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", replace
						restore
					}
					
					*Repeat, for families
					disp "`incvar' `geo' `subpop' 2 hh"
					forvalues i = 1/80 {
						disp "`incvar' `geo' `subpop' 2_`i' hh"
						set more off
						preserve
							collapse (sum) weight_pop`i'=obs [fweight = wgtp`i'] if famvals == 1, ///
								by(year ``geo'' `incvar')
							cd "$stata"
							merge 1:1 year ``geo'' `incvar' using "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta"
							drop _merge
							save "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta", replace
						restore
					}
					
					*Add aggregate rows
					cd "$stata"
					use "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", clear
					
					*Clean up blank values
					foreach var in agecat ageg dis_status sex eduatt racecomp empstat support_kids selfemp {
						replace `var' = -1 if `var' == . 
						label define `var'_lbl -1 `"Blank"', add
						label values `var' `var'_lbl
					}
					
					*save "`incvar'_demog2.dta", replace
					/*ssc install tuples*/
					tuples agecat dis_status sex eduatt racecomp empstat support_kids, asis max(6) display
					disp "`incvar' `geo' `subpop' 3"
					
					forvalues i = 0/`ntuples' {
						disp "`incvar' `geo' `subpop' 3_`i'"
						disp "`tuple`i''"
						preserve
							if strpos("`tuple`i''","agecat") > 0 & strpos("`tuple`i''","empstat") > 0 {
								collapse (sum) records weight_pop*, by(year ``geo'' `incvar' `tuple`i'' ageg selfemp rectype)
							}
							else if strpos("`tuple`i''","agecat") > 0 & strpos("`tuple`i''","empstat") == 0 {
								collapse (sum) records weight_pop*, by(year ``geo'' `incvar' `tuple`i'' ageg rectype)
							}
							else if strpos("`tuple`i''","agecat") == 0 & strpos("`tuple`i''","empstat") > 0 {
								collapse (sum) records weight_pop*, by(year ``geo'' `incvar' `tuple`i'' selfemp rectype)
							}
							else {
								collapse (sum) records weight_pop*, by(year ``geo'' `incvar' `tuple`i'' rectype)
							}
							gen agg_row = 1
							append using "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta"
							save "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", replace
						restore
						}	
					
					cd "$stata"
					use "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", clear
					append using "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta"
					
					disp "`incvar' `geo' `subpop' 4"
					foreach var in agecat ageg dis_status sex eduatt racecomp empstat support_kids selfemp {
						replace `var' = -2 if `var' == . & (agg_row == 1 | rectype == "Family")
						label define `var'_lbl -2 `"Total"', add
						label values `var' `var'_lbl
					}
					replace agg_row = 0 if agg_row == .
			}
			
			*If geography is more granular than the MSA, then produce only aggregate estimates
			else {
					*Collapse using primary person weight
					disp "`incvar' `geo' `subpop' 1"
					preserve
						collapse (rawsum) records=obs (sum) weight_pop=obs [fweight = perwt], ///
							by(year ``geo'' `incvar')
						gen rectype = "Person"
						cd "$stata"
						save "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", replace
					restore
					
					*Repeat, for families
					disp "`incvar' `geo' `subpop' 1 hh"
					preserve
						collapse (rawsum) records=obs (sum) weight_pop=obs [fweight = hhwt] if famvals == 1, ///
							by(year ``geo'' `incvar')
						gen rectype = "Family"
						cd "$stata"
						save "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta", replace
					restore
						
					
					*Collapse using replicate weights
					disp "`incvar' `geo' `subpop' 2"
					forvalues i = 1/80 {
						disp "`incvar' `geo' `subpop' 2_`i'"
						set more off
						preserve
							collapse (sum) weight_pop`i'=obs [fweight = pwgtp`i'], ///
								by(year ``geo'' `incvar')
							cd "$stata"
							merge 1:1 year ``geo'' `incvar' using "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta"
							drop _merge
							save "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", replace
						restore
					}
					
					*Repeat, for families
					disp "`incvar' `geo' `subpop' 2 hh"
					forvalues i = 1/80 {
						disp "`incvar' `geo' `subpop' 2_`i' hh"
						set more off
						preserve
							collapse (sum) weight_pop`i'=obs [fweight = wgtp`i'] if famvals == 1, ///
								by(year ``geo'' `incvar')
							cd "$stata"
							merge 1:1 year ``geo'' `incvar' using "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta"
							drop _merge
							save "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta", replace
						restore
					}
					cd "$stata"
					use "`exploc'_`incvar'_`geo'_`subpop'_demog_ipums.dta", clear
					append using "`exploc'_`incvar'_`geo'_`subpop'_fam_ipums.dta"
					
					disp "`incvar' `geo' `subpop' 4"
					foreach var in agecat ageg dis_status sex eduatt racecomp empstat support_kids selfemp inschool {
						gen `var' = -2
						label define `var'_lbl -2 `"Total"', add
						label values `var' `var'_lbl
					}
					gen agg_row = 1
			}
			*End geography distinction
			
			*Calculate standard error
				forvalues i = 1/80 {
					disp "`incvar' `geo' `subpop' 5_`i'"
					gen diff`i' = (weight_pop`i' - weight_pop)^2
				}
				egen diffsum = rowtotal(diff1-diff80)
				gen se = sqrt((4/80)*diffsum)
				
				drop diff1-diff80 weight_pop80-weight_pop1
				drop diffsum
				
				order year rectype `incvar' agecat ageg dis_status sex racecomp eduatt empstat support_kids selfemp records weight_pop se agg_row ``geo''
				
				label variable year `"Year"'
				label variable rectype `"Record Type"'
				label variable `incvar' `"Wage Category - `incvar'"'
				label variable agecat `"Age Category"'
				label variable ageg `"Age Group"'
				label variable dis_status `"Disability Status"'
				label variable sex `"Sex"'
				label variable eduatt `"Educational Attainment"'
				label variable racecomp `"Race"'
				label variable support_kids `"Supporting Kids"'
				label variable selfemp `"Self-Employed"'
				label variable empstat `"Employment Status"'
				label variable records `"Raw Record Count"'
				label variable weight_pop `"Weighted Count"'
				label variable se `"Standard Error"'
				label variable agg_row `"Aggregation Flag"'
				
			*Save, export
				compress
 				cd "$stata"
 				save "`exploc'_`incvar'_`geo'_`subpop'_demog_se_ipums.dta", replace
			
 				local expinc = subinstr("`incvar'","below_","",.)
 				local expgeo = substr("`geo'",1,1)

				cd "$stata"
				use "`exploc'_`incvar'_`geo'_`subpop'_demog_se_ipums.dta", clear
				
				cd "$output"
// 				export excel using "`loc' Struggling Families Demographics IPUMS SS (IS 2017.10.27).xlsx", sheet("`expinc'_`expgeo'") sheetreplace firstrow(varlabels)
				outsheet * using "`loc' Struggling Families Demographics `geo' (IS $S_DATE).csv", comma replace
			
			}
		}
	}
	
	}
}


*End aggregation loop



****** SUMMARIES ***********************
cd "$stata"
use "famlivwage_GrandRapids-KentwoodMI3_ipums_t.dta", replace
keep if year == 2017

gen empstat_dt = empstat if agecat == 2
	replace empstat_dt = 4 if empstat == 3 & dis == 1 & agecat == 2
	replace empstat_dt = 5 if empstat == 3 & dis == 0 & caretaker == 1 & agecat == 2
	
	label define empstat_dt_lbl 1 "Employed" 2 "Unemployed" 3 "Not in Labor Force" 4 "NILF - Disabled" 5 "NILF - Likely Caretakers"
	label values empstat_dt empstat_dt_lbl

gen oow = (empstat_dt == 3 & dis == 0 & caretaker == 0 & agecat == 2 & ageg >= 2 & ageg <= 4 & inschool == 0)

*Summaries to produce:

*Overall share of struggling population
	preserve
		collapse (sum) pop = perwt, by(below_nontransadj_county)
		cd "$output"
		export excel "Grand Rapids Struggling Families Summary Stats (SL $S_DATE).xlsx", ///
		sheetreplace sheet("overall") firstrow(variables)
	restore

*Composition of struggling population by:
	*1 - Age group
	*2 - Employment status
	*3 - Education status
	*4 - Gender
	*5 - Race
	*6 - Parent status
	local age agecat
	local emp empstat caretaker dis
	local edu eduatt
	local gender sex
	local race racecomp
	local parent support_kids

	foreach cat in age emp edu gender race parent {
		preserve
			if "`cat'" == "emp" | "`cat'" == "parent" | "`cat'" == "edu" {
				keep if agecat == 2
			}
			collapse (sum) pop = perwt, by(below_nontransadj_county ``cat'')
			bys below_nontransadj_county: egen totstat = sum(pop)
			gen shareofstat = pop / totstat
			cd "$output"
			export excel "Grand Rapids Struggling Families Summary Stats (SL $S_DATE).xlsx", ///
			sheetreplace sheet("`cat'_shareofstrug") firstrow(variables)
		restore
	}


*Share of each group that is struggling and a working-age adult:
	*1 - Race
	*2 - Gender
	*3 - Education

	local edu eduatt
	local sex sex
	local race racecomp

	local lfpart "& inlist(empstat, 1, 2)"
	local lfpartoow "& (inlist(empstat, 1, 2) | oow == 1)"
	local wrkage

	foreach cat in edu sex race {
		foreach uni in lfpart lfpartoow wrkage {
			preserve
				keep if agecat == 2 ``uni''
				collapse (sum) pop = perwt, by(below_nontransadj_county ``cat'')
				bys ``cat'': egen totcat = sum(pop)
				gen shareinstat = pop / totcat
				cd "$output"
				export excel "Grand Rapids Struggling Families Summary Stats (SL $S_DATE).xlsx", ///
				sheetreplace sheet("`cat'_`uni'_shrwhostrg") firstrow(variables)
			restore
		}
	}
	
*Share of each age category struggling
	preserve
		collapse (sum) pop = perwt, by(below_nontransadj_county agecat)
		bys agecat: egen totcat = sum(pop)
		gen shareinstat = pop / totcat
		cd "$output"
		export excel "Grand Rapids Struggling Families Summary Stats (SL $S_DATE).xlsx", ///
		sheetreplace sheet("agecat_shrwhostrg") firstrow(variables)
	restore

*Find share of NILF people who are not disabled, not likely caretakers, 
* who might be discouraged workers

preserve
	keep if below_nontransadj_county == 1 & empstat == 3 & agecat == 2 & inschool == 0
	collapse (sum) pop = perwt, by(empstat_dt oow ageg eduatt sex racecomp support_kids)
	cd "$output"
	export excel "Grand Rapids Struggling Families Summary Stats (SL $S_DATE).xlsx", ///
	sheetreplace sheet("oow_summ") firstrow(variables)
restore

*Share of employed and struggling who are a part of each education category
tab empstat if agecat == 2 & inschool == 0 & below_nontransadj_county == 1 [fweight = perwt]
tab empstat if agecat == 2 & inschool == 0 & below_nontransadj_county == 1 [fweight = perwt], nolab
tab eduatt if empstat == 1 & agecat == 2 & inschool == 0 & below_nontransadj_county == 1 [fweight = perwt]

*Share of employed and struggling who are of each race and education category
tab racecomp if empstat == 1 & ///
agecat == 2 & inschool == 0 & ///
below_nontransadj_county == 1 [fweight = perwt]

*Share of discouraged workers who are of each race and education category
tab racecomp if empstat == 3 & oow == 1 & ///
agecat == 2 & inschool == 0 & ///
below_nontransadj_county == 1 [fweight = perwt]

*Cross tab
tab2 empstat_dt racecomp if agecat == 2 & inschool == 0 & ///
below_nontransadj_county == 1 & year == 2017 [fweight = perwt], row

tab2 empstat_dt racecomp if agecat == 2 & inschool == 0 & ///
below_nontransadj_county == 1 & year == 2017 [fweight = perwt], column

tab2 empstat_dt racecomp if agecat == 2 & inschool == 0 & ///
below_nontransadj_county == 0 & year == 2017 [fweight = perwt], row

tab2 empstat_dt racecomp if agecat == 2 & inschool == 0 & ///
year == 2017 [fweight = perwt], row


// tab empstat [fweight = perwt] if below_nontransadj_county == 1 & year == 2017 & agecat == 2
// tab oow [fweight = perwt] if empstat == 3 & agecat == 2 & below_nontransadj_county == 1
