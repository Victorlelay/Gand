***************************************************************
* ONLY Visualization: Number of attributions
* Victor Le Lay

***************************************************************

**************************************************
* 1. Everybody (full dataset)
**************************************************
*--- Mutlticolinearity check
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
reg MentalH ib1.Nombre_attributions ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi]
vif // Big issue, we do not include d_discri (frequency) here

* new test without this variable
reg MentalH ib1.Nombre_attributions ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi]
vif // no problem of multicolinearity anymore

*******************
* 1.A Models
*******************
*--- M1: simple model (no control)
logit MentalH ib1.Nombre_attributions [pweight = poidsi], or
eststo M1
outreg2 using "Nbr1.xls", excel replace dec(3) label stats(coef se ci) eform

*--- M2: adjusted model (all the control variables except for frequency)
logit MentalH ib1.Nombre_attributions ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
eststo M2
outreg2 using "Nbr2.xls", excel replace dec(3) label stats(coef se ci) eform


*******************
* 1.B Graph
*******************
coefplot (M1, label("Without Controls")) ///
         (M2, label("With Controls")), ///
    keep(*.Nombre_attributions) ///
    title("Number of reasons of discrimination reported: all", size(small)) ///
    xtitle("Odds ratios", size(small)) ///
    ytitle("", size(small)) ///
    ylabel(1 "No discrimination" ///
		   2 "1 discrimination" ///
           3 "2 or + discrimination", labsize(small)) ///
    eform xline(1) baselevels ///
    msize(vsmall) ///
    xlabel(0.5(0.5)2, labsize(small)) ///
    level(90) scheme(s1mono) ///
    graphregion(margin(l+20 r+20)) ///
	legend(size(small))
    plotregion(margin(l+10 r+10))
	
	
**************************************************
* 2. Only discriminated people
**************************************************
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace

* drop if no discrimination
drop if d_discri == 3 

*--- Mutlticolinearity check
reg MentalH ib1.Nombre_attributions ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi]
vif // no multicolinearity issue here, I can include frquency here

*******************
* 2.A Models
*******************
*-- for once, not only among discriminated individuals
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace

*--- M1: simple model (no control)
logit MentalH ib1.Nombre_attributions [pweight = poidsi], or
eststo M1

*--- M2: adjusted model (all the control variables except for frequency)
logit MentalH ib1.Nombre_attributions ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
eststo M2


*******************
* 2.B Graph
*******************
coefplot (M1, label("Without Controls")) ///
         (M2, label("With Controls")), ///
    keep(*.Nombre_attributions) ///
    title("Number of reasons of discrimination reported: all", size(small)) ///
    xtitle("Odds ratios", size(small)) ///
    ytitle("", size(small)) ///
    ylabel(1 "No discrimination" ///
		   2 "1 discrimination" ///
           3 "2 or + discrimination", labsize(small)) ///
    eform xline(1) baselevels ///
    msize(vsmall) ///
    xlabel(0.5(0.5)2, labsize(small)) ///
    level(90) scheme(s1mono) ///
    graphregion(margin(l+20 r+20)) ///
	legend(size(small))
    plotregion(margin(l+10 r+10))
