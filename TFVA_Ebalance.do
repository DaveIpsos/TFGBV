
****
cd "C:\Users\Daves\OneDrive - Florida State University\Research Collaboration with Dr Sesan"
set maxvar 8000
clear all
use "NGIR8AFL.dta", clear

**Target population
fre v044 v012 v501
gen pop=0
replace pop=1 if (v044==1)
fre pop

***Married or partnered women
fre v501 if pop==1
recode v501(0=0 "Never in union")(1=1 "Married")(2=2 "Cohabiting")(3/5=3 "Formerly married (widowed/divorced/separated)"), gen(marry)
ta marry if pop==1

recode v501(0=0 "Never in union")(1/5=1 "Ever married/cohabiting"), gen(marry2)
ta marry2 if pop==1

**Main outcome
fre sd21a sd21b sd21c sd21d if pop==1
foreach var in sd21a sd21b sd21c sd21d {
	recode `var' (0=0 "Unexposed")(1/2=1 "Exposed")(else=.), gen(new_`var')
}

fre new_sd21a new_sd21b new_sd21c new_sd21d if pop==1 

egen intense = rowtotal(new_sd21a new_sd21b new_sd21c new_sd21d) if pop==1
clonevar intensity = intense
replace intensity=. if (intense==0) & (new_sd21a==. & new_sd21b==. & new_sd21c==. & new_sd21d==.)
fre intensity if pop==1
recode intensity (0=0 "Unexposed")(1/4=1 "Exposed"), gen(tfva)
fre tfva if pop==1

**Main predictors variables: TFVA
****Involuntary performance of sexual acts (main independent variable)
fre d125 if pop==1
recode d125(0 6=0 "No reported experience")(1=1 "Experience"), gen(sex_vio)
ta sex_vio if pop==1

****Parental violence
***Father beat mother
fre d121
clonevar father_vio = d121
recode father_vio (8=0)
fre father_vio if pop==1
***Mother hurt respondent
fre d115b if pop==1
clonevar mother_hurt = d115b
replace mother_hurt=. if mother_hurt== .a
fre mother_hurt if pop==1
***Father hurt respondent
fre d115c
clonevar father_hurt = d115c
replace father_hurt=. if father_hurt== .a
fre father_hurt if pop==1
***generate parental violence
gen parental_vio =.
replace parental_vio=1 if (father_vio==1 | mother_hurt==1 | father_hurt==1)
replace parental_vio=0 if (father_vio==0 & mother_hurt==0 & father_hurt==0)
la def parental_vio 0 "No experience" 1 "Experience"
la val parental_vio parental_vio
fre parental_vio if pop==1

**Number of sexual partners
fre v836
clonevar multiple_sex = v836
replace multiple_sex=0 if v536==0
replace multiple_sex=. if multiple_sex==98
fre multiple_sex 

recode multiple_sex (0=0 "None")(1=1 "Single")(2/95=2 "Multiple")(else=.), gen(multi_sex)
fre multi_sex if pop==1

**Justification of violence
fre v744a v744b v744c v744d v744e v822
gen justification = 0
replace justification = 1 if (v744a==1 | v744b==1 | v744c==1 | v744d==1 | v744e==1 | v822==1)
la def justification 0 No 1 Yes
la val justification justification
fre justification

***Own phone
fre v169a
clonevar phone = v169a
fre phone

***Use of internet
fre v171a
recode v171a (0=0 "No Internet use")(1/2=1 "Internet use"), gen(internet)
fre internet

***Age
fre v012
clonevar wmage = v012
fre wmage

***Education
fre v133
clonevar education = v133
fre education 

***Wealth
fre v190 if pop==1
clonevar wealth = v190
fre wealth
recode wealth (1/2=0 "Poor")(3/5=1 "Non-poor"), gen(wealth2)
fre wealth2

**Ethnicity
*numlabel, add
ta v131
recode v131 (109 130 179=1 "Hausa/Fulani/Kanuri")(138=2 "Igbo")(298=3 "Yoruba")(else=4 "Others"), gen(ethnic)
ta ethnic 

**Religion
fre v130 if pop==1
recode v130 (1/2=1 "Christianity")(3=0 "Islam")(else=2 "Others"), gen(religion)
fre religion if pop==1

***Residence
ta v025
clonevar residence = v025
fre residence
recode residence (1=1)(2=0)
la def residence 0 Rural 1 Urban
la val residence residence
fre residence

****Region
fre v024 if pop==1
recode v024 (1/3=0 "North")(4/6=1 "South"), gen(region)
fre region

***Checking missing data
egen nmiss=rowmiss(tfva sex_vio parental_vio justification multiple_sex wmage marry phone internet education v191 ethnic religion residence region)
ta nmiss if (pop==1) // missing is 0.49% (137)

missings report tfva parental_vio justification nmiss multiple_sex wmage marry region phone internet education v191 ethnic religion residence region if (pop==1), percent

****Generating weight*****
gen strata=v023
gen psu=v021
gen wt=d005/1000000

svyset psu [pweight=wt], strata(strata) singleunit(centered)

***Effect of sexual violence experience on TFVA exposure
**Examining linearity in multiple sex 
svy,subpop(if pop==1 & nmiss==0): logistic tfva multiple_sex
predict xb_mi
lowess xb_mi multiple_sex if pop==1 & nmiss==0, logit yline(0)  // nonlinear trend

drop xb_mi
svy,subpop(if pop==1 & nmiss==0): logistic sex_vio multiple_sex
predict xb_mi
lowess xb_mi multiple_sex if pop==1 & nmiss==0, logit yline(0)  // nonlinear trend

gen multiple_sexs = multiple_sex * multiple_sex // generate quadratic term


**Examining linearity in years of education (sum non-linearity, deserving interaction)
drop xb_mi
svy,subpop(if pop==1 & nmiss==0): logistic tfva education
predict xb_mi
lowess xb_mi education if pop==1 & nmiss==0, logit yline(0)

drop xb_mi
svy,subpop(if pop==1 & nmiss==0): logistic sex_vio education
predict xb_mi
lowess xb_mi education if pop==1 & nmiss==0, logit yline(0)

**Examining linearity in age 
drop xb_mi
svy,subpop(if pop==1 & nmiss==0): logistic tfva wmage
predict xb_mi
lowess xb_mi wmage if pop==1 & nmiss==0, logit yline(0)

drop xb_mi
svy,subpop(if pop==1 & nmiss==0.): logistic sex_vio wmage
predict xb_mi
lowess xb_mi wmage if pop==1 & nmiss==0, logit yline(0)

**Examining linearity in wealth index 
drop xb_mi
svy,subpop(if pop==1 & nmiss==0): logistic tfva v191
predict xb_mi
lowess xb_mi v191 if pop==1 & nmiss==0, logit yline(0)

drop xb_mi
svy,subpop(if pop==1 & nmiss==0): logistic sex_vio v191
predict xb_mi
lowess xb_mi v191 if pop==1 & nmiss==0, logit yline(0)


*------------------------------------------------------------------*
* TABLE 2 — Weighted crosstab: row %, row totals, chi-square
* p-values computed explicitly via svy:tab and injected
*------------------------------------------------------------------*
collect clear

local colvar  tfva
local vlist   sex_vio parental_vio justification wealth marry ///
              phone internet television ethnic religion residence region
local cond    (pop==1 & nmiss==0)

* --- svyset once (CHANGED: needed for the tests) ---
* use your actual design variables if available, e.g.:
* svyset v021 [pw=wt], strata(v022) singleunit(centered)
svyset [pw=wt]

qui levelsof `colvar' if `cond', local(collevels)

foreach var of local vlist {

    * CHANGED: dropped ", test" — we no longer use dtable's internal test
    dtable i.`colvar' [pw=wt] if `cond', by(`var', nototal) name(`var')

    * move each row's weighted N into the Total (.m) column
    collect addtags `colvar'[.m], fortags(var[_N]#result[frequency])

    * pair each row total with 100%
    qui levelsof `var' if `cond', local(levels)
    foreach level of local levels {
        collect get percent = 100, tags(`colvar'[.m] `var'[`level'])
    }
}
* CHANGED: removed both test-related remap lines

* combine all variable blocks
collect combine c = `vlist', replace

*--- NEW: compute Rao–Scott chi-square p-values and inject them -----
foreach var of local vlist {
    qui svy, subpop(if `cond'): tabulate `var' `colvar'
    local pval = e(p_Pear)

    * attach to the first level of `var' (first row of each block)
    qui summ `var' if `cond'
    local min = r(min)

    if `pval' < 0.001 {
        collect get p = "<0.001", tags(`var'[`min'] `colvar'[test])
    }
    else {
        collect get p = strofreal(`pval', "%6.3f"), ///
            tags(`var'[`min'] `colvar'[test])
    }
}

*--- inject the overall weighted N header row (unchanged) ------------
qui summ wt if `cond'
local Ntot = r(sum)
foreach l of local collevels {
    qui summ wt if `cond' & `colvar'==`l'
    local n = r(sum)
    local pc = 100*r(sum)/`Ntot'
    collect get frequency = `n',  tags(N[_N] `colvar'[`l'])
    collect get percent   = `pc', tags(N[_N] `colvar'[`l'])
}
collect get frequency = `Ntot', tags(N[_N] `colvar'[.m])
collect get percent   = 100,    tags(N[_N] `colvar'[.m])

*--- labels, headers, ordering ---------------------------------------
collect style header `colvar', title(label)
collect label levels `colvar' .m "Total" test "p-value", modify
collect style header result N, level(hide)

collect style autolevels `colvar' `collevels' .m test, clear
* CHANGED: result autolevels now includes the injected "p"
collect style autolevels result _dtable_stats p, clear

collect style cell result[frequency fvfrequency], nformat(%15.0fc)
collect style cell result[percent fvpercent],    nformat(%6.2f)
collect style cell result[p], halign(center)

*--- final layout and export -----------------------------------------
collect layout (N `vlist') (`colvar'#result)

*collect export "TABLE2_DESCRIPTIVES.xlsx", replace
collect export "TABLE2_DESCRIPTIVES.docx", replace

***Numeric variables
svy,subpop(if pop==1 & nmiss==0): reg tfva v191
svy,subpop(if pop==1 & nmiss==0): reg tfva wmage
svy,subpop(if pop==1 & nmiss==0): reg tfva education
svy,subpop(if pop==1 & nmiss==0): reg tfva multiple_sex


****Analyses
**Balancing algorithm
ebalfit i.parental_vio i.justification multiple_sexs multiple_sex v191 wmage ///
i.marry i.phone i.internet education i.ethnic i.religion i.residence i.region if (pop==1 & nmiss==0) [iw=wt], by(sex_vio) swap baltab

**Generate ebalance weights
predict wbal if (pop==1 & nmiss==0)

**Examine balance across control variables
**Means
tabstat multiple_sexs multiple_sex v191 wmage ///
 education [aw=wbal] if (pop==1 & nmiss==0), by(sex_vio) nototal

***Proportions
ta marry, gen(marriage)
ta ethnic, gen(tribe)
ta religion, gen(relig)
ta parental_vio, gen(parents)
ta justification, gen(justify)
ta phone, gen(phones)
ta internet, gen(internets)
ta residence, gen(residences)
ta region, gen(regions)

tabstat parents1 parents2 justify1 justify2 phones1 phones2 internets1 internets2 residences1 residences2 regions1 regions2 marriage1 marriage2 marriage3 marriage4 tribe1 tribe2 tribe3 tribe4 relig1 relig2 relig3 ///
 [aw=wbal] if (pop==1 & nmiss==0), by(sex_vio) nototal


***Combine survey and ebalance weights
gen weight_combined = wbal * wt

*==============================================================================
* COMPLETE E-VALUE CALCULATION - CORRECTED
*==============================================================================
***Declare combined survey weight
svyset psu [pweight=weight_combined], strata(strata) singleunit(centered)

* Run regression
svy, subpop(if pop==1 & nmiss==0): reg tfva i.sex_vio ///
    i.parental_vio i.justification multiple_sexs multiple_sex v191 wmage ///
	i.marry education i.phone i.internet i.ethnic i.religion ///
	i.residence i.region
	
est store firstresults
outreg2 using "reg.doc" , stats(coef ci) noobs sideway addstat("Prob > F", e(p), "N (subpop)", e(N_subpop)) dec(2) replace 

	
*------------------------------------------------------------------------------
* Extract treatment effect
*------------------------------------------------------------------------------
est restore firstresults

local rd = _b[1.sex_vio]
local se_rd = _se[1.sex_vio]
local ci_lower_rd = `rd' - 1.96*`se_rd'
local ci_upper_rd = `rd' + 1.96*`se_rd'

display ""
display "=== TREATMENT EFFECT ==="
display "Risk Difference: " %7.4f `rd'
display "Standard Error:  " %7.4f `se_rd'  
display "95% CI: [" %7.4f `ci_lower_rd' ", " %7.4f `ci_upper_rd' "]"
display ""

*------------------------------------------------------------------------------
* Get baseline probability
*------------------------------------------------------------------------------

quietly margins if sex_vio==0, subpop(if pop==1 & nmiss==0)
local p0 = r(b)[1,1]

display "Baseline probability (sex_vio=0): " %7.4f `p0'

*------------------------------------------------------------------------------
* Calculate Risk Ratio
*------------------------------------------------------------------------------

local p1 = `p0' + `rd'
local rr = `p1' / `p0'

* RR confidence interval
local p1_lower = `p0' + `ci_lower_rd'
local rr_lower = `p1_lower' / `p0'

local p1_upper = `p0' + `ci_upper_rd'
local rr_upper = `p1_upper' / `p0'

display ""
display "=== RISK RATIO ==="
display "Risk Ratio: " %6.3f `rr'
display "95% CI: [" %6.3f `rr_lower' ", " %6.3f `rr_upper' "]"
display ""

*------------------------------------------------------------------------------
* Calculate E-value (CORRECTED SYNTAX)
*------------------------------------------------------------------------------

display "=== E-VALUE CALCULATION ==="
evalue rr `rr', lcl(`rr_lower') ucl(`rr_upper')

*------------------------------------------------------------------------------
* Summary
*------------------------------------------------------------------------------

display ""
display "=== SUMMARY FOR PAPER ==="
display "Risk difference: " %6.4f `rd' " (95% CI: " %6.4f `ci_lower_rd' ", " %6.4f `ci_upper_rd' ")"
display "Risk ratio: " %6.3f `rr' " (95% CI: " %6.3f `rr_lower' ", " %6.3f `rr_upper' ")"
display "Baseline probability: " %7.4f `p0'
display "Risk ratio: " %7.4f `rr'
display "RR 95% CI: [" %7.4f `rr_lower' ", " %7.4f `rr_upper' "]"

***Marginal effects
est restore firstresults
margins i.sex_vio, subpop(if pop==1 & nmiss==0) 

local blue   "0 114 178"

marginsplot, ///
    title("") ///
    xtitle("Lifetime exposure to sexual violence") ytitle("") ///
	subtitle("(A) Probability of CTFGBV", position(11) ring(0) justification(right)) ///
    recast(scatter) ///
    ciopts(recast(rspike) lwidth(medthick) lcolor("`blue'")) ///
    plotopts(msymbol(O) msize(large) mfcolor("`blue'") mlcolor(white) mlwidth(vthin)) ///
    ylabel(0(0.06)0.18, format(%4.2f) angle(0)) ///
    yscale(range(0 0.18)) ///
    xlabel(0 "No" 1 "Yes", noticks) ///
    yline(0, lpattern(dash) lcolor(gs12)) ///
    legend(off) fxsize(80) ///
    scheme(cleanplots) name(plot_sexualviolence, replace) 		
	

*******************************************************************************************
**********Examing non-recursive association between sexual violence and TFVA***************
*******************************************************************************************

**Main outcome: Ever experienced TFVA
fre sd21a sd21b sd21c sd21d if pop==1
foreach var in sd21a sd21b sd21c sd21d {
	recode `var' (0=0 "Unexposed")(1/3=1 "Exposed")(else=.), gen(new`var')
}

fre newsd21a newsd21b newsd21c newsd21d if pop==1 

egen tfvab = anymatch(newsd21a newsd21b newsd21c newsd21d), values(1)
fre tfvab
la def tfvab 1 Yes 0 No
la val tfvab tfvab
fre tfvab

***Checking missing data
egen nmisss=rowmiss(tfvab parental_vio justification multiple_sexs multiple_sex wmage marry phone internet education v191 ethnic religion residence region)
ta nmisss if (pop==1) // missing is 0.49% (137)

missings report tfvab parental_vio justification nmiss multiple_sexs multiple_sex wmage marry region phone internet education v191 ethnic religion residence region if (pop==1), percent

****Generating Target sample****
gen insamp = (nmisss==0 & pop==1)
fre insamp

*------------------------------------------------------------------*
* TABLE 3 — Weighted crosstab: row %, row totals, chi-square
* p-values computed explicitly via svy:tab and injected
*------------------------------------------------------------------*
collect clear

local colvar  tfvab
local vlist   sex_vio parental_vio justification wealth marry ///
              phone internet ethnic religion residence region
local cond    (insamp==1)

* --- svyset once (CHANGED: needed for the tests) ---
* use your actual design variables if available, e.g.:
* svyset v021 [pw=wt], strata(v022) singleunit(centered)
svyset [pw=wt]

qui levelsof `colvar' if `cond', local(collevels)

foreach var of local vlist {

    * CHANGED: dropped ", test" — we no longer use dtable's internal test
    dtable i.`colvar' [pw=wt] if `cond', by(`var', nototal) name(`var')

    * move each row's weighted N into the Total (.m) column
    collect addtags `colvar'[.m], fortags(var[_N]#result[frequency])

    * pair each row total with 100%
    qui levelsof `var' if `cond', local(levels)
    foreach level of local levels {
        collect get percent = 100, tags(`colvar'[.m] `var'[`level'])
    }
}
* CHANGED: removed both test-related remap lines

* combine all variable blocks
collect combine c = `vlist', replace

*--- NEW: compute Rao–Scott chi-square p-values and inject them -----
foreach var of local vlist {
    qui svy, subpop(if `cond'): tabulate `var' `colvar'
    local pval = e(p_Pear)

    * attach to the first level of `var' (first row of each block)
    qui summ `var' if `cond'
    local min = r(min)

    if `pval' < 0.001 {
        collect get p = "<0.001", tags(`var'[`min'] `colvar'[test])
    }
    else {
        collect get p = strofreal(`pval', "%6.3f"), ///
            tags(`var'[`min'] `colvar'[test])
    }
}

*--- inject the overall weighted N header row (unchanged) ------------
qui summ wt if `cond'
local Ntot = r(sum)
foreach l of local collevels {
    qui summ wt if `cond' & `colvar'==`l'
    local n = r(sum)
    local pc = 100*r(sum)/`Ntot'
    collect get frequency = `n',  tags(N[_N] `colvar'[`l'])
    collect get percent   = `pc', tags(N[_N] `colvar'[`l'])
}
collect get frequency = `Ntot', tags(N[_N] `colvar'[.m])
collect get percent   = 100,    tags(N[_N] `colvar'[.m])

*--- labels, headers, ordering ---------------------------------------
collect style header `colvar', title(label)
collect label levels `colvar' .m "Total" test "p-value", modify
collect style header result N, level(hide)

collect style autolevels `colvar' `collevels' .m test, clear
* CHANGED: result autolevels now includes the injected "p"
collect style autolevels result _dtable_stats p, clear

collect style cell result[frequency fvfrequency], nformat(%15.0fc)
collect style cell result[percent fvpercent],    nformat(%6.2f)
collect style cell result[p], halign(center)

*--- final layout and export -----------------------------------------
collect layout (N `vlist') (`colvar'#result)

*collect export "TABLE2_DESCRIPTIVES.xlsx", replace
collect export "TABLE2_DESCRIPTIVES.docx", replace


*------------------------------------------------------------------*
* TABLE 4 — Weighted crosstab: row %, row totals, chi-square
* p-values computed explicitly via svy:tab and injected
*------------------------------------------------------------------*
collect clear

local colvar  sex_vio
local vlist   tfvab parental_vio justification wealth marry ///
              phone internet ethnic religion residence region
local cond    (insamp==1)

* --- svyset once (CHANGED: needed for the tests) ---
* use your actual design variables if available, e.g.:
* svyset v021 [pw=wt], strata(v022) singleunit(centered)
svyset [pw=wt]

qui levelsof `colvar' if `cond', local(collevels)

foreach var of local vlist {

    * CHANGED: dropped ", test" — we no longer use dtable's internal test
    dtable i.`colvar' [pw=wt] if `cond', by(`var', nototal) name(`var')

    * move each row's weighted N into the Total (.m) column
    collect addtags `colvar'[.m], fortags(var[_N]#result[frequency])

    * pair each row total with 100%
    qui levelsof `var' if `cond', local(levels)
    foreach level of local levels {
        collect get percent = 100, tags(`colvar'[.m] `var'[`level'])
    }
}
* CHANGED: removed both test-related remap lines

* combine all variable blocks
collect combine c = `vlist', replace

*--- NEW: compute Rao–Scott chi-square p-values and inject them -----
foreach var of local vlist {
    qui svy, subpop(if `cond'): tabulate `var' `colvar'
    local pval = e(p_Pear)

    * attach to the first level of `var' (first row of each block)
    qui summ `var' if `cond'
    local min = r(min)

    if `pval' < 0.001 {
        collect get p = "<0.001", tags(`var'[`min'] `colvar'[test])
    }
    else {
        collect get p = strofreal(`pval', "%6.3f"), ///
            tags(`var'[`min'] `colvar'[test])
    }
}

*--- inject the overall weighted N header row (unchanged) ------------
qui summ wt if `cond'
local Ntot = r(sum)
foreach l of local collevels {
    qui summ wt if `cond' & `colvar'==`l'
    local n = r(sum)
    local pc = 100*r(sum)/`Ntot'
    collect get frequency = `n',  tags(N[_N] `colvar'[`l'])
    collect get percent   = `pc', tags(N[_N] `colvar'[`l'])
}
collect get frequency = `Ntot', tags(N[_N] `colvar'[.m])
collect get percent   = 100,    tags(N[_N] `colvar'[.m])

*--- labels, headers, ordering ---------------------------------------
collect style header `colvar', title(label)
collect label levels `colvar' .m "Total" test "p-value", modify
collect style header result N, level(hide)

collect style autolevels `colvar' `collevels' .m test, clear
* CHANGED: result autolevels now includes the injected "p"
collect style autolevels result _dtable_stats p, clear

collect style cell result[frequency fvfrequency], nformat(%15.0fc)
collect style cell result[percent fvpercent],    nformat(%6.2f)
collect style cell result[p], halign(center)

*--- final layout and export -----------------------------------------
collect layout (N `vlist') (`colvar'#result)

*collect export "TABLE2_DESCRIPTIVES.xlsx", replace
collect export "TABLE2_DESCRIPTIVES.docx", replace


**First model for TFVA
svy,subpop(insamp):ivregress 2sls tfvab (sex_vio = i.parental_vio i.justification) ///
    multiple_sex multiple_sexs v191 wmage i.marry ///
    education residence i.region i.phone i.internet i.ethnic i.religion 
est store two_stage_v1

***Marginal effects
est restore two_stage_v1
margins, at(sex_vio=(0 1)) subpop(if insamp==1)

local red   "204 121 167"
marginsplot, ///
    title("") ///
    xtitle("Lifetime exposure to sexual violence") ytitle("") ///
	subtitle("(B) Probability of LTFGBV", position(11) ring(0) justification(right)) ///
    recast(scatter) ///
    ciopts(recast(rspike) lwidth(medthick) lcolor("`red'")) ///
    plotopts(msymbol(O) msize(large) mfcolor("`red'") mlcolor(white) mlwidth(vthin)) ///
    ylabel(-0(0.5)1.5, format(%4.2f) angle(0)) ///
    yscale(range(0 1.5)) ///
    xlabel(0 "No" 1 "Yes", noticks) ///
    yline(0, lpattern(dash) lcolor(gs12)) ///
    legend(off) fxsize(80) ///
    scheme(cleanplots) name(plot_lsv, replace) 		
	

**Second model for sexual violence
svy,subpop(insamp): ivregress 2sls sex_vio (tfvab = i.phone i.internet) ///
    multiple_sex multiple_sexs v191 wmage i.marry ///
    education i.residence i.region i.parental_vio i.justification i.ethnic i.religion 
est store two_stage_v2

outreg2 using "reg.doc" , stats(coef ci) noobs sideway addstat("Prob > F", e(p), "N (subpop)", e(N_subpop)) dec(2) append 

***Marginal effects
est restore two_stage_v2
margins, at(tfvab=(0 1)) subpop(if insamp==1)

local grey "115 115 115"

marginsplot, ///
    title("") ///
    xtitle("Lifetime exposure to TFGBV") ytitle("") ///
	subtitle("(C) Probability of LSV", position(11) ring(0) justification(right)) ///
    recast(scatter) ///
    ciopts(recast(rspike) lwidth(medthick) lcolor("`grey'")) ///
    plotopts(msymbol(O) msize(large) mfcolor("`grey'") mlcolor(white) mlwidth(vthin)) ///
    ylabel(0(0.5)1.5, format(%4.2f) angle(0)) ///
    yscale(range(0 1.5)) ///
    xlabel(0 "No" 1 "Yes", noticks) ///
    yline(0, lpattern(dash) lcolor(gs12)) ///
    legend(off) fxsize(80) ///
    scheme(cleanplots) name(plot_tfva, replace) 	
	

***Combine all graphs
graph combine plot_sexualviolence plot_lsv plot_tfva, ///
    rows(1) cols(3) ///
    graphregion(color(white)) ///
    imargin(tiny) ///
	note("CTFGBV = Current experience of TFGBV; LTFGBV = Lifetime experience of TFGBV; LSV = Lifetime experience of sexual violence", color(gs8) size(vsmall)) ///
    name(combined_graphs, replace)	

graph export "FIGURE1_COMBINED.png", replace width(4000)


***Model diagnotics and tests
**First model
ivregress 2sls tfvab (sex_vio = i.parental_vio i.justification) ///
    multiple_sex multiple_sexs v191 wmage i.marry ///
    education residence i.region i.phone i.internet i.ethnic i.religion ///
     if insamp==1, cluster(psu)
estat endogenous         // test of endogeneity using robust regression F
weakivtest  // test of relevance using Montiel-Pflueger robust weak instrument test

****Test of validity using Hansen's J statistic
ivreg2 tfvab (sex_vio = i.parental_vio i.justification) ///
    multiple_sex multiple_sexs v191 wmage i.marry ///
    education residence i.region i.phone i.internet i.ethnic i.religion ///
     if insamp==1, cluster(psu) first


***Second model
ivregress 2sls sex_vio (tfvab = i.phone i.internet) ///
    multiple_sex multiple_sexs v191 wmage i.marry ///
    education i.residence i.region i.parental_vio i.justification i.ethnic i.religion ///
     if insamp==1, cluster(psu)
estat endogenous      // test of endogeneity using robust regression F
weakivtest            //  test of relevance using Montiel-Pflueger robust weak instrument test 

****Test of validity using Hansen's J statistic
ivreg2 sex_vio (tfvab = i.phone i.internet) ///
    multiple_sex multiple_sexs v191 wmage i.marry ///
    education i.residence i.region i.parental_vio i.justification i.ethnic i.religion ///
     if insamp==1, cluster(psu) first


***Descriptive analysis
dtable i.tfva i.tfvab i.sex_vio i.parental_vio i.justification i.wealth i.marry i.phone ///
i.internet i.ethnic i.religion i.residence i.region wmage multiple_sex education, continuous(v191 wmage multiple_sex education, statistics(mean sd)) ///
    factor(i.tfva i.sex_vio i.parental_vio i.justification i.marry i.phone ///
           i.internet i.ethnic i.religion i.residence i.region) ///
    svy subpop(if insamp==1) nformat(%6.2f percent fvpercent) ///
    export(TABLE_DESCRIPTIVES.docx, replace)

***Univariate regression for numeric variables
svyset psu [pweight=wt], strata(strata) singleunit(centered)

***TFGBV as the outcome
svy,subpop(insamp): reg tfvab v191
svy,subpop(insamp): reg tfvab wmage
svy,subpop(insamp): reg tfvab education
svy,subpop(insamp): reg tfvab multiple_sex

***Sexual violence as the outcome
svy,subpop(insamp): reg sex_vio v191
svy,subpop(insamp): reg sex_vio wmage
svy,subpop(insamp): reg sex_vio education
svy,subpop(insamp): reg sex_vio multiple_sex
