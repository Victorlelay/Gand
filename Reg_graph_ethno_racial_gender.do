***************************************************************
* Regressions and visualization: intersectional ER and gender
* Victor Le Lay

*** Note: ER = Ethnoracial
***************************************************************


* Dataset
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace


**************************************************
* 1. Ethnoracial and gender attributions
**************************************************

*******************
* 1.A Variables
*******************

gen ethno_racial_gender = 99
replace ethno_racial_gender = 0 if Nombre_attributions == 0 // aucune discri
replace ethno_racial_gender = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre == 0 // ER pas genre
replace ethno_racial_gender = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre > 0 // ER et genre
replace ethno_racial_gender = 3 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 & Discri_genre > 0 // genre pas ER
replace ethno_racial_gender = 4 if Nombre_attributions > 0 & Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart + Discri_genre == 0 // autre motif


*******************
* 1.B Models
*******************

*--- Here, we are doing nested logistic regression (multi_ER)

*******************
* 1.B.i General population
*******************

logit MentalH ib1.ethno_racial_gender [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi]
vif // I do not include d_discri (frequency)

*******************
* 1.B.ii Discriminated only
*******************

* drop if no discri
drop if d_discri == 3
drop if ethno_racial_gender == 0

* Models
logit MentalH ib1.ethno_racial_gender [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib2.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH iB2.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib2.d_discri [pweight = poidsi]
vif // I include d_discri (frequency)


*******************
* 1.B.iii Men (all)
*******************

* data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen ethno_racial_gender = 99
replace ethno_racial_gender = 0 if Nombre_attributions == 0 // aucune discri
replace ethno_racial_gender = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre == 0 // ER pas genre
replace ethno_racial_gender = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre > 0 // ER et genre
replace ethno_racial_gender = 3 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 & Discri_genre > 0 // genre pas ER
replace ethno_racial_gender = 4 if Nombre_attributions > 0 & Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart + Discri_genre == 0 // autre motif

* drop if woman
drop if sexee == 2

* Models
logit MentalH ib1.ethno_racial_gender [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib2.d_discri [pweight = poidsi]
vif // I do not include d_discri (frequency)


*******************
* 1.B.iv Men (discriminated only)
*******************

* drop if no discrimination
drop if d_discri == 3

* Models
logit MentalH ib1.ethno_racial_gender [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib2.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib2.d_discri [pweight = poidsi]
vif // I  include d_discri (frequency)



*******************
* 1.B.v Women (all)
*******************
* data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen ethno_racial_gender = 99
replace ethno_racial_gender = 0 if Nombre_attributions == 0 // aucune discri
replace ethno_racial_gender = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre == 0 // ER pas genre
replace ethno_racial_gender = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre > 0 // ER et genre
replace ethno_racial_gender = 3 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 & Discri_genre > 0 // genre pas ER
replace ethno_racial_gender = 4 if Nombre_attributions > 0 & Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart + Discri_genre == 0 // autre motif

* drop if man
drop if sexee == 1

* Models
logit MentalH ib1.ethno_racial_gender [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok888.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity check
reg MentalH ib1.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib3.d_discri [pweight = poidsi]
vif // I do not include d_discri (frequency)


*******************
* 1.B.vi Women (discriminated only)
*******************

* drop if no discrimination
drop if d_discri == 3
drop if ethno_racial_gender == 0

* models
logit MentalH ib1.ethno_racial_gender [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.ethno_racial_gender ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib2.d_discri[pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform


* Multicolinearity check
reg MentalH ib1.ethno_racial_gender ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib3.d_discri [pweight = poidsi]
vif // I include d_discri




**************************************************
* 2. Visualisation
**************************************************

*******************
* 2.A Models' implementation
*******************
*--- Visualisation - Odds ratio (only discriminated women)

*****************************************************************************
* data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen ethno_racial_gender = 99
replace ethno_racial_gender = 0 if Nombre_attributions == 0 // aucune discri
replace ethno_racial_gender = 1 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre == 0 // ER pas genre
replace ethno_racial_gender = 2 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart > 0 & Discri_genre > 0 // ER et genre
replace ethno_racial_gender = 3 if Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart == 0 & Discri_genre > 0 // genre pas ER
replace ethno_racial_gender = 4 if Nombre_attributions > 0 & Discri_peau + Discri_relig + Discri_orig + Discri_accent + Discri_quart + Discri_genre == 0 // autre motif

* drop if no discrimination
drop if d_discri == 3

* Models
logit MentalH i.ethno_racial_gender [pweight = poidsi], or
eststo M1

logit MentalH i.ethno_racial_gender ib2.dip_rec ib1.sexee agenq ///
      ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ///
      ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
eststo M2


* Graph
coefplot (M1, label("Without Controls")) ///
         (M2, label("With Controls")), ///
    keep(*.ethno_racial_gender) ///
    title("idem but only for discriminated women" size(medsmall)) ///
    xtitle("Odds ratios", size(medium)) ///
    ytitle("", size(medium)) ///
    ylabel(1 "No ethnoracial/gender discrimination" ///
           2 "Ethnoracial discrimination only" ///
           3 "Ethnoracial and gender discrimination" ///
           4 "Gender discrimination") ///
    eform xline(1) baselevels ///
    msize(vsmall) ///
    xlabel(0.5(0.5)2, labsize(small)) ///
    level(90) scheme(s1mono) ///
    graphregion(margin(l+20 r+20)) ///
    plotregion(margin(l+10 r+10))
