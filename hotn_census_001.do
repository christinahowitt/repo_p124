
**  GENERAL DO-FILE COMMENTS
**  program:      hotn_census_001.do
**  project:      Comparing HotN and Census 
**  author:       HAMBLETON \ 11-OCT-2014
**  task:          
 


** General algorithm set-up
version 15
clear all
macro drop _all
set more 1
set linesize 80

** Set working directories: this is for DATASET and LOGFILE import and export
** DATASETS to encrypted SharePoint folder
local datapath "X:\The University of the West Indies\DataGroup - repo_data\data_p124"
** LOGFILES to unencrypted OneDrive folder
local logpath X:\OneDrive - The University of the West Indies\repo_datagroup\data_p124

** Close any open log fileand open a new log file
capture log close
cap log using "`logpath'\hotn_census", replace

******************************************************************************
** COMPARE THE FOLLOWING
** 1. AGE and SEX (census table 01.02)
** 2. Level of education (census table 05.06)
** 3. Occupation types (census table 06.01) - Completed Training group. Must collaspse census occupation groups
******************************************************************************


** HOTN WEIGHTS file
use "`datapath'\version01\1-input\hotn_weights_v4.dta", clear
tempfile weights41
save `weights41', replace

** Use FINAL HOTN DATA version 4.1 and MERGE with full weights file (v4)
use "`datapath'\version01\1-input\hotn_v41.dta", clear
tempfile hotn41
** drop weights variables from main file prior to merging with main weights file
drop wue wbase_un wbase_ad wrr_un wrr_ad whotn_un whotn_ad wps_b2010 wps_b2000 wps_idb wps_wpp 
drop wfinal1_un wfinal2_un wfinal3_un wfinal4_un wfinal1_ad wfinal2_ad wfinal3_ad wfinal4_ad
drop _merge
merge 1:1 pid using `weights41'
drop _merge
save `hotn41', replace


** LOAD CENSUS AGE and SEX and generate TOTAL populations
use "`datapath'\version01\1-input\bb_pop_2010.dta", clear
sort sex age18
merge 1:m age18 sex using `hotn41'
drop if age18<=5 & pid==.
drop _merge

** Barbados census unadjusted for undercount
gen pop2010u = .
** females
replace pop2010u = 7992 if sex==1 & age18==6
replace pop2010u = 7902 if sex==1 & age18==7
replace pop2010u = 8718 if sex==1 & age18==8
replace pop2010u = 8573 if sex==1 & age18==9
replace pop2010u = 9253 if sex==1 & age18==10
replace pop2010u = 8698 if sex==1 & age18==11
replace pop2010u = 7166 if sex==1 & age18==12
replace pop2010u = 5824 if sex==1 & age18==13
replace pop2010u = 4582 if sex==1 & age18==14
replace pop2010u = 3912 if sex==1 & age18==15
replace pop2010u = 3262 if sex==1 & age18==16
replace pop2010u = 2486 if sex==1 & age18==17
replace pop2010u = 2474 if sex==1 & age18==18
** males
replace pop2010u = 7515 if sex==2 & age18==6
replace pop2010u = 7374 if sex==2 & age18==7
replace pop2010u = 8023 if sex==2 & age18==8
replace pop2010u = 7871 if sex==2 & age18==9
replace pop2010u = 8163 if sex==2 & age18==10
replace pop2010u = 7657 if sex==2 & age18==11
replace pop2010u = 6420 if sex==2 & age18==12
replace pop2010u = 5162 if sex==2 & age18==13
replace pop2010u = 3674 if sex==2 & age18==14
replace pop2010u = 3069 if sex==2 & age18==15
replace pop2010u = 2290 if sex==2 & age18==16
replace pop2010u = 1572 if sex==2 & age18==17
replace pop2010u = 1282 if sex==2 & age18==18


** HOTN pop by age and sex and total HOTN population
bysort sex age18: gen pophotn = _N
gen tpophotn = _N
** Total Census 2010 population aged 25 and over 
egen tag = tag(sex age18)
egen tpop2010_t = sum(pop2010) if tag==1
*egen tpop2010 = min(tpop2010_t)
drop tpop2010_t
egen tpop2010u_t = sum(pop2010u) if tag==1
egen tpop2010u = min(tpop2010u_t)
drop tpop2010u_t
order pid sex age18 pophotn tpophotn pop2010 tpop2010 pop2010u tpop2010u

keep pid sex age18 pophotn tpophotn pop2010 tpop2010 pop2010u tpop2010u wfinal1_ad wfinal3_ad wps_b2010

/** Population Pyramids to compare populations
** Note that these age/sex population proportions sum to 1 (across all ages and both sexes)
** So a bivariate distribution across age/sex
gen pop1 = (pophotn/tpophotn)*1234
gen pop3 = (pop2010/tpop2010)*1234
gen pop30 = (pop2010u/tpop2010u)*1234


** Sum all populations to size of HOTN survey (n=1,234)
collapse (sum) wfinal1_ad wps_b2010, by(age18 sex pophotn pop2010 pop1 pop3 pop30)

** pop1 = HOTN
** pop3 = CENSUS 2010 (adjusted for undercount)
** pop30 = CENSUS 2010 (unadjusted for undercount)
** pop6 = FINAL WEIGHT 1
egen sp1 = sum(pop1)
egen sp3 = sum(pop3)
egen sp30 = sum(pop30)
egen sp6 = sum(wfinal1_ad)
egen sp8 = sum(wps_b2010)
rename wfinal1_ad pop6 
rename wps_b2010 pop8

** Reshape to draw women and men separately
** Suffix --> 2=male    1=female
drop pophotn pop2010
reshape wide pop1 pop3 pop30 pop6 pop8 , i(age18) j(sex)
** MALES on LHS of graphic
replace pop12 = -pop12
replace pop32 = -pop32
replace pop302 = -pop302
replace pop62 = -pop62
replace pop82 = -pop82
gen zero = 0

** Comparing unweighted HOTN to 2010 BARBADOS CENSUS (raw and adjusted for undercount)
** NB: un-adjusted census has EXACTLY the same age/sex distribution as adjusted

** COLOR PALETTE
** MEN: 		0 145 201	(NHS: Light Blue)
** WOMEN:	0 158 73 	(NHS: Green)
** MEN 	(Blue) 		240,80,0,0
** WOMEN (Green)	160,0,160,0
#delimit ;
	twoway 
	/// HOTN - women (pop11 - unadjusted, pop61 - adjusted)
	(bar pop11 age18, horizontal lc(gs0) fc("160 0 160 0") )
	/// HOTN  men (pop12 - unadjusted, pop62 - adjusted)
	(bar pop12 age18, horizontal lc(gs0) fc("240 80 0 0") )
	/// BSS 2010 (adjusted)
	(connect age18 pop31, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	(connect age18 pop32, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	/// BSS 2010 (un-adjusted)
	///(connect age18 pop301, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	///(connect age18 pop302, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	
	(sc age18 zero, mlabel(age18) mlabcolor(black) msymbol(i))
	, 
	plotregion(c(gs16) ic(gs16) ilw(thin) lw(thin)) 		
	graphregion(color(gs16) ic(gs16) ilw(thin) lw(thin)) ysize(3)

	xtitle("") ytitle("")
	plotregion(style(none))
	ysca(noline) ylabel(none)
	xsca(noline titlegap(-3.5))
	xlabel(, tlength(0) nogrid gmin gmax)
	legend(size(medsmall) position(11) bm(t=1 b=0 l=0 r=0) colf cols(1)
	region(fcolor(gs16) lw(vthin) margin(l=2 r=2 t=2 b=2)) order(1 2 3 - "")
	lab(2 "HotN males") 
	lab(1 "HotN females")
	lab(3 "Barbados census 2010")
	);
#delimit cr
*graph export graph\pyramid_unadjusted.eps, as(eps) replace logo(off) cmyk(on) preview(on) 
 
 
 
** COLOR PALETTE
** MEN: 		0 145 201	(NHS: Light Blue)
** WOMEN:	0 158 73 	(NHS: Green)
** MEN 	(Blue) 		240,80,0,0
** WOMEN (Green)	160,0,160,0
#delimit ;
	twoway 
	/// HOTN - women (pop11 - unadjusted, pop61 - adjusted)
	(bar pop61 age18, horizontal lc(gs0) fc("160 0 160 0") )
	/// HOTN  men (pop12 - unadjusted, pop62 - adjusted)
	(bar pop62 age18, horizontal lc(gs0) fc("240 80 0 0") )
	/// BSS 2010 (adjusted)
	(connect age18 pop31, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	(connect age18 pop32, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	/// BSS 2010 (un-adjusted)
	///(connect age18 pop301, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	///(connect age18 pop302, symbol(T) mc(gs0*0.75) lc(gs0*0.75))
	
	(sc age18 zero, mlabel(age18) mlabcolor(black) msymbol(i))
	, 
	plotregion(c(gs16) ic(gs16) ilw(thin) lw(thin)) 		
	graphregion(color(gs16) ic(gs16) ilw(thin) lw(thin)) ysize(3)

	xtitle("") ytitle("")
	plotregion(style(none))
	ysca(noline) ylabel(none)
	xsca(noline titlegap(-3.5))
	xlabel(, tlength(0) nogrid gmin gmax)
	legend(size(medsmall) position(11) bm(t=1 b=0 l=0 r=0) colf cols(1)
	region(fcolor(gs16) lw(vthin) margin(l=2 r=2 t=2 b=2)) order(1 2 3 - "")
	lab(2 "HotN males") 
	lab(1 "HotN females")
	lab(3 "Barbados census 2010")
	);
#delimit cr
*graph export graph\pyramid_adjusted.eps, as(eps) replace logo(off) cmyk(on) preview(on) 


** Percentage under or overcount of HOTN compared to 2010 census by sex and age
* women
gen pcount1 = (pop11/pop31)*100
* men
gen pcount2 = (pop12/pop32)*100
format pcount1 %9.1f
format pcount2 %9.1f
list age18 pcount2 pcount1, noobs clean








/** LOAD CENSUS EDUCATION
import excel using "data\20101011_census_data.xlsx", sheet("education") first clear

** DERIVED EDUCATION GROUPS
**	1	primary school or less
**	2	secondary school completed
**	3	technical/secretarial after primary/secondary
**	4	tertiary
gen educ4 = .
replace educ4 = 1 if educ_group=="preprimary" | educ_group=="primary" | educ_group=="none"
replace educ4 = 2 if educ_group=="secondary" 
replace educ4 = 3 if educ_group=="post-secondary" | educ_group=="senior-composite"
replace educ4 = 4 if educ_group=="tertiary"
replace educ4 = 5 if educ_group=="other"
label define educ4 1 "primary or less" 2 "secondary" 3 "technical" 4 "tertiary" 5 "other"
label values educ4 educ4
label define sex 1 "female" 2 "male" 3 "both"
label values sex_group sex
keep if educ4<.
sort sex educ4
tab educ_group sex [weight=ed_count], col nofreq
tab educ4 sex [weight=ed_count], col nofreq



** Use FINAL DATA version 4.1
use C:\statistics\analysis\a001\versions\version09\m01-2014-06-04\data\all\version41\hotn_v41.dta, clear
tab educ sex
tab educ sex, nol
gen educ4 = .
replace educ4 = 1 if educ==1|educ==2|educ==3
replace educ4 = 2 if educ==4
replace educ4 = 3 if educ==5|educ==6
replace educ4 = 4 if educ==7|educ==8|educ==9
label define educ4 1 "primary or less" 2 "secondary" 3 "technical" 4 "tertiary" 5 "other"
label values educ4 educ4
tab educ4 sex , col nofreq

svyset ed [pweight=wfinal1_ad], strata(region) 
svy: tab educ4 sex, col percent




** LOAD CENSUS OCCUPATION
import excel using "data\20101011_census_data.xlsx", sheet("occupation") first clear
drop F
label define sex 1 "women" 2 "men" 3 "both"
label values sex sex
drop sex_text
sort sex occg
drop if occg==98
label define occg 1 "legislators" 2 "professional" 3 "assoc.prof" 4 "clerk" 5 "service work" 6 "agricult" 7 "craft"  8 "plant op" 9 "elemtary" 99 "armed foces"
label values occg occg
tab occg sex [weight=occ_count], col nofreq
keep occ_count occg sex
collapse (sum) occ_count, by(occg sex)
drop if occg==99
drop if sex==3
bysort sex: egen ctot = sum(occ_count)
bysort sex: gen cperc = (occ_count/ctot)*100
sort occg sex
tempfile census
save `census'

** Use FINAL DATA version 4.1
use C:\statistics\analysis\a001\versions\version09\m01-2014-06-04\data\all\version41\hotn_v41.dta, clear
tab occg sex
tab occg sex, nol
tab occg sex , col nofreq
svyset ed [pweight=wfinal1_ad], strata(region) 
svy: tab occg sex, col percent
gen k = 1
collapse (sum) k , by(sex occg)
keep k occg sex
merge 1:1 sex occg using `census'
keep if _merge==3
drop _merge
bysort sex: egen htot = sum(k)
bysort sex: gen hperc = (k/htot)*100

label define occg_new 1 "Managers" 2 "Professionals" 3 "Associate Professionals" 4 "Clerks" 5 "Service Workers" 6 "Agriculture/Fishery" 7 "Crafts"  8 "Plant Operators" 9 "Elementary Workers"
label values occg occg_new

** MEN 	(Blue) 		240,80,0,0  /  120, 40, 0 , 0
** WOMEN (Green)	160,0,160,0  /  80, 0, 80, 0

** WOMEN
#delimit ;
graph   hbar (sum) hperc cperc if sex==1, bargap(10) xalt

		plotregion(c(gs16) ic(gs16) ilw(thin) lw(thin)) 		
		graphregion(color(gs16) ic(gs16) ilw(thin) lw(thin)) 
		ysize(8)

		over(occg, gap(100) axis(off fill)) 
		
		blabel(none, format(%9.0f) pos(outside) size(medsmall))
		
		bar(1, fc("160 0 160 0") lc(gs0) blw(vthin))
		bar(2, fc("80 0 80 0") lc(gs0) blw(vthin))
	
	   	ylab(0(10)40, nogrid glc(gs0)) yscale(noline range(0(5)45))
	    ytitle("Percentage", margin(t=3) size(medium)) 

		legend(size(medium) position(12) bm(t=0 b=5 l=0 r=0) colf cols(1)
		region(fcolor(gs16) lw(vthin) margin(l=1 r=1 t=1 b=1)) 
		lab(1 "HotN") 
		lab(2 "2010 Census")
		);
#delimit cr
graph export graph\occ_women.eps, as(eps) replace logo(off) cmyk(on) preview(on)


** MEN
gen hperc_m = hperc*-1
gen cperc_m = cperc*-1

#delimit ;
graph   hbar (sum) hperc_m cperc_m if sex==2, bargap(10)

		plotregion(c(gs16) ic(gs16) ilw(thin) lw(thin)) 		
		graphregion(color(gs16) ic(gs16) ilw(thin) lw(thin)) 
		ysize(8)

		over(occg, gap(100)) 
		
		blabel(none, format(%9.0f) pos(outside) size(medsmall))

		bar(1, fc("240 80 0 0") lc(gs0) blw(vthin))
		bar(2, fc("120 40 0 0") lc(gs0) blw(vthin))
	
	   	ylab(-40 "40" -30 "30" -20 "20" -10 "10" 0, nogrid glc(gs0)) yscale(noline fill range(-45(5)0))
	    ytitle("Percentage", margin(t=3) size(medium)) 
		
		legend(size(medium) position(12) bm(t=0 b=5 l=0 r=0) colf cols(1)
		region(fcolor(gs16) lw(vthin) margin(l=1 r=1 t=1 b=1)) 
		lab(1 "HotN") 
		lab(2 "2010 Census")
		);
#delimit cr	
graph export graph\occ_men.eps, as(eps) replace logo(off) cmyk(on) preview(on) 


	
