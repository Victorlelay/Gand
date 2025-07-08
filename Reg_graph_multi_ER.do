**************************************************
* Regressions and visualization: multi_ER
* Victor Le Lay

*** Note: ER = Ethnoracial
**************************************************

* Dataset
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace

**************************************************
* 1. Multiple ethnoracial attributions
**************************************************

*******************
* 1.A Variables
*******************

* Multiple ethnoracial
gen multi_ER = 99
replace multi_ER = 0 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 // pas ER
replace multi_ER = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 1 // 1 ER
replace multi_ER = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 1 // 2 ou + ER

*******************
* 1.B Models
*******************

*--- Here, we are doing nested logistic regression (multi_ER)

*******************
* 1.B.i General population
*******************

logit MentalH ib1.multi_ER [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi]
vif // issue with multicolinearity, I do not include d_discri in the control variables

*******************
* 1.B.ii Only the discriminated
*******************

* drop if no discrimination
drop if d_discri == 3

*--- Here, we are doing nested logistic regression (multi_ER)

logit MentalH ib1.multi_ER [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi]
vif // no problem with multicolinearity, I include d_discri in the control variables

*******************
* 1.B.iii Men (all)
*******************
* Data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen multi_ER = 99
replace multi_ER = 0 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 // pas ER
replace multi_ER = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 1 // 1 ER
replace multi_ER = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 1 // 2 ou + ER

* Drop if woman
drop if sexee == 2

*--- Here, we are doing nested logistic regression (multi_ER)

logit MentalH ib1.multi_ER [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi]
vif // no problem with multicolinearity, I include d_discri in the control variables

*******************
* 1.B.iv Men (only discriminated)
*******************

* drop if no discrimination
drop if d_discri == 3

*--- Here, we are doing nested logistic regression (multi_ER)

logit MentalH ib1.multi_ER [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform


*******************
* 1.B.v Women (all)
*******************
* Data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen multi_ER = 99
replace multi_ER = 0 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 // pas ER
replace multi_ER = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 1 // 1 ER
replace multi_ER = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 1 // 2 ou + ER

* drop if man
drop if sexee == 1

*--- Here, we are doing nested logistic regression (multi_ER)

logit MentalH ib1.multi_ER [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.multi_ER ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi]
vif // no problem with multicolinearity, I include d_discri in the control variables

*******************
* 1.B.vi Women (only discriminated)
*******************

* drop if no discrimination
drop if d_discri == 3

*--- Here, we are doing nested logistic regression (multi_ER)

logit MentalH ib1.multi_ER [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform

**************************************************
* 2. Visualisation
**************************************************

*******************
* 2.A Models' implementation
*******************

* --- only among people who reported at least one discrimination (men OR women, not both)

use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace

gen multi_ER = 99
replace multi_ER = 0 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 // pas ER
replace multi_ER = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 1 // 1 ER
replace multi_ER = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 1 // 2 ou + ER

*--- Men
* Data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen multi_ER = 99
replace multi_ER = 0 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 // pas ER
replace multi_ER = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 1 // 1 ER
replace multi_ER = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 1 // 2 ou + ER

* drop if no discrimination / if woman
drop if d_discri == 3
drop if Nombre_attributions == 0
drop if sexee == 2

* Models
logit MentalH ib1.multi_ER [pweight = poidsi], or
eststo M1
estimates store M_hommes_simple
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
eststo M2
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
estimates store M_hommes_ctrl

*--- Women
* Data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen multi_ER = 99
replace multi_ER = 0 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 // pas ER
replace multi_ER = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 1 // 1 ER
replace multi_ER = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 1 // 2 ou + ER

* Drop if no discrimination / if man
drop if d_discri == 3
drop if Nombre_attributions == 0
drop if sexee == 1

* Models
logit MentalH ib1.multi_ER [pweight = poidsi], or
eststo M1
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
estimates store M_femmes_simple
logit MentalH ib1.multi_ER ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
eststo M2
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
estimates store M_femmes_ctrl


*******************
* 2.B Graphs
*******************

*--- Men
coefplot (M_hommes_simple, label("Without Controls")) ///
         (M_hommes_ctrl, label("With Controls")), ///
    keep(*.multi_ER) ///
    title("Multiple ethno-racial bases for discrimination: men", size(small)) ///
    xtitle("Odds ratios", size(small)) ///
    ytitle("", size(medium)) ///
    ylabel(1 "No ethnoracial reason" ///
           2 "1 ethnoracial reason" ///
           3 "2 or more ethnoracial reasons", labsize(small)) ///
    eform xline(1) baselevels ///
    msize(vsmall) ///
    xlabel(0.5(0.5)2, labsize(small)) ///
    level(90) scheme(s1mono) ///
    graphregion(margin(l+20 r+20)) ///
    plotregion(margin(l+10 r+10)) ///
    legend(size(small)) ///
    saving(gr_hommes.gph, replace)

*--- Women
coefplot (M_femmes_simple, label("Without Controls")) ///
         (M_femmes_ctrl, label("With Controls")), ///
    keep(*.multi_ER) ///
    title("Multiple ethno-racial bases for discrimination: women", size(small)) ///
    xtitle("Odds ratios", size(small)) ///
    ytitle("", size(medium)) ///
    ylabel(1 "No ethnoracial reason" ///
           2 "1 ethnoracial reason" ///
           3 "2 or more ethnoracial reasons", labsize(small)) ///
    eform xline(1) baselevels ///
    msize(vsmall) ///
    xlabel(0.5(0.5)2, labsize(small)) ///
    level(90) scheme(s1mono) ///
    graphregion(margin(l+20 r+20)) ///
    plotregion(margin(l+10 r+10)) ///
    legend(size(small)) ///
    saving(gr_femmes.gph, replace)


	
*** Combined graph
graph combine gr_hommes.gph gr_femmes.gph, ///
    col(1) ///
    ycommon ///
    scheme(white_jet) ///
    imargin(medium) ///
    iscale(.9) ///
    xsize(6.5) ysize(8) ///
    note("Note: 95% confidence intervals around the OR — comparison of simple and adjusted models (covariates)", size(vsmall)) ///
    saving(gr_combined_MentalH_both.gph, replace)
	