**************************************************
* Regressions and visualization: multigenre, multirelig, skin_origin_religion
* Victor Le Lay

*** Note: ER = Ethnoracial
**************************************************


* Import dataset
clear all
set more off
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear // cf file ""
save base_regressions.dta, replace

* we drop the observation if the origine is "other" (G1 or G2)
drop if origine_tous_g2bis == 100|origine_tous_g2bis==111
**************************************************
* 1. Simple attributions
**************************************************

*******************
* 1.A Variables
*******************

* Principal ethnoracial attribution (orig + skin)
gen attri_ethnorac = 0
replace attri_ethnorac = 1 if Discri_peau == 1 | Discri_orig == 1

* Secondary ethnoracial attribution (neighbourhood / accent)
gen attri_ethnorac_other = 0
replace attri_ethnorac_other = 1 if Discri_quart == 1 | Discri_accent == 1

* Religous attribution
gen attri_relig = 0
replace attri_relig = 1 if Discri_relig == 1

* Gender attribution
gen attri_gender = 0
replace attri_gender = 1 if Discri_genre == 1

* Other attribution
gen attri_other = 0
replace attri_other = 1 if Discri_age == 1 | Discri_handi == 1 | Discri_fami == 1 | ///
                          Discri_orientsex == 1 | Discri_habi == 1 | Discri_poids == 1 | Discri_autre == 1

*******************
* 1.B Models
*******************

*** Simple model (M1)
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other [pweight=poidsi], or
outreg2 using "AttriM1.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity test
regress attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other [pweight=poidsi]
vif

*** Adjusted model (with controls) (M2)
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight=poidsi], or
outreg2 using "AttriM2.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity test
regress attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight=poidsi]
vif



**************************************************
* 2. Multiple attributions
**************************************************

*******************
* 2.A Variables
*******************


* Multiple ethnoracial attributions
gen plus_ER = 0
replace plus_ER = 1 if (Discri_peau + Discri_orig + Discri_quart + Discri_accent) >= 2

* Ethnoracial + religion
gen ER_relig = 0
replace ER_relig = 1 if (Discri_peau + Discri_orig + Discri_quart + Discri_accent >= 1) & Discri_relig == 1

* Ethnoracial + gender
gen ER_genre = 0
replace ER_genre = 1 if (Discri_peau + Discri_orig + Discri_quart + Discri_accent >= 1) & Discri_genre == 1

* Ethnoracial + other
gen ER_autre = 0
replace ER_autre = 1 if (Discri_peau + Discri_orig + Discri_quart + Discri_accent >= 1) & (Discri_age + Discri_handi + Discri_fami + Discri_orientsex + Discri_habi + Discri_poids + Discri_autre >= 1)

* Religion + gender
gen relig_genre = 0
replace relig_genre = 1 if Discri_genre == 1 & Discri_relig == 1

* Religion + other
gen relig_autre = 0
replace relig_autre = 1 if d_pqdisc_j == 1 & ///
                          (Discri_age + Discri_handi + Discri_fami + Discri_orientsex + Discri_habi + ///
                           Discri_poids + Discri_autre >= 1)

* ER or religion + gender
gen ER_ou_relig_genre = 0
replace ER_ou_relig_genre = 1 if ((Discri_peau + Discri_orig + Discri_accent + Discri_quart >= 1) | Discri_relig == 1) & Discri_genre == 1

* ER or religion + other
gen ER_ou_relig_autre = 0
replace ER_ou_relig_autre = 1 if ///
    (Discri_peau + Discri_orig + Discri_accent + Discri_quart >= 1 | Discri_relig == 1) & ///
    (Discri_age + Discri_handi + Discri_fami + Discri_orientsex + Discri_habi + ///
                           Discri_poids + Discri_autre >= 1)


*******************
* 2.B Models
*******************
						   
*** Simple model (M1)
logit MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre [pweight=poidsi], or
outreg2 using "AttriMultiM1.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity test
regress MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre [pweight=poidsi]
vif

*** Adjusted model (with controls) (M2)
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight=poidsi], or
outreg2 using "AttriMultiM2.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity test
regress attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight=poidsi]
vif // WARNING: huge multicolinearity issue




**************************************************
* 3. Recoding of robust (to multicolinearity) variables
**************************************************
* --- Aim: variables that enable us to look at the interaction between ER and religion / gender

*******************
* 3.A Variables
*******************

* Multiple religion
gen multi_relig = 0
replace multi_relig = 1 if Discri_peau + Discri_orig + Discri_quart + Discri_accent == 1 & Discri_relig == 0
replace multi_relig = 2 if Discri_peau + Discri_orig + Discri_quart + Discri_accent >= 2 & Discri_relig == 0
replace multi_relig = 3 if Discri_peau + Discri_orig + Discri_quart + Discri_accent == 0 & Discri_relig == 1
replace multi_relig = 4 if (Discri_peau + Discri_orig + Discri_quart + Discri_accent >= 1) & Discri_relig == 1

* Labels for multiple_relig
label define multi_relig_lbl 0 "0" 1 "1_ER" 2 "2+_ER" 3 "relig" 4 "ER+relig"
label values multi_relig multi_relig_lbl

* - Multiple gender
gen multi_genre = 0
replace multi_genre = 1 if Discri_peau + Discri_orig + Discri_quart + Discri_accent == 1 & Discri_genre == 0
replace multi_genre = 2 if Discri_peau + Discri_orig + Discri_quart + Discri_accent >= 2 & Discri_genre == 0
replace multi_genre = 3 if Discri_peau + Discri_orig + Discri_quart + Discri_accent == 0 & Discri_genre == 1
replace multi_genre = 4 if (Discri_peau + Discri_orig + Discri_quart + Discri_accent >= 1) & Discri_genre == 1

* Labels for multiple_genre
label define multi_genre_lbl 0 "0" 1 "1_ER" 2 "2+_ER" 3 "genre" 4 "ER+genre"
label values multi_genre multi_genre_lbl

*******************
* 3.B Models
*******************

* --- Multi_relig					   
*** Simple model (M1)
logit MentalH ib1.multi_relig [pweight = poidsi], or
outreg2 using "MultiRelig1.xls", excel replace dec(3) label stats(coef se ci) eform

*** Adjusted model (with controls) (M2)
logit MentalH ib1.multi_relig ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi], or
outreg2 using "MultiRelig2.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity tests
regress MentalH ib1.multi_relig ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi]
vif // Not perfect but acceptable


* --- Multi_genre
*** Simple model (M1)
logit MentalH ib1.multi_genre [pweight = poidsi], or	
outreg2 using "MultiGenre1.xls", excel replace dec(3) label stats(coef se ci) eform

*** Adjusted model (with controls) (M2)
logit MentalH ib1.multi_genre ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi], or
outreg2 using "MultiGenre2.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity test
regress MentalH ib1.multi_genre ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi]
vif	// Not perfect but acceptable



**************************************************
* 3. Effects of the number of attributions on the Mental Health
**************************************************

*** Simple model (M1)
logit MentalH ib1.Nombre_attributions [pweight = poidsi], or	
outreg2 using "Nbre.xls", excel replace dec(3) label stats(coef se ci) eform

*** Adjusted model (with controls) (M2)
logit MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib3.d_discri [pweight = poidsi], or
outreg2 using "MultiGenre2.xls", excel replace dec(3) label stats(coef se ci) eform

* Multicolinearity test
regress MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib3.d_discri [pweight = poidsi]
vif // WARNING: huge multicolinearity issue


* --- Solution: do not take into account the missing values for the frequency
gen frequence_reduite = .
replace frequence_reduite = 1 if d_discri == 1  // souvent
replace frequence_reduite = 2 if d_discri == 2  // parfois

* Now, test again multicolinearity...
regress MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.frequence_reduite [pweight = poidsi]
vif // colinearity is now acceptable 

*** Adjusted model (with controls) (M2)
logit MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.frequence_reduite[pweight = poidsi], or
outreg2 using "test.xls", excel replace dec(3) label stats(coef se ci) eform


* --- same analysis but only among the individuals who reported at least one analysis

drop if d_discri == 3

*** Simple model (M1)
logit MentalH ib1.Nombre_attributions [pweight = poidsi], or	
outreg2 using "Nbre1.xls", excel replace dec(3) label stats(coef se ci) eform

*** Adjusted model (with controls) (M2)
logit MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.frequence_reduite[pweight = poidsi], or
outreg2 using "test1.xls", excel replace dec(3) label stats(coef se ci) eform





**************************************************
* 4. origin_skin_religion: intersectional analysis (ethnoracial/religious reason)
**************************************************
 

*******************
* 4.A Variable
*******************
gen origin_skin_religion1 = 5
replace origin_skin_religion1 = 0 if Nombre_attributions == 0 // aucun motif
replace origin_skin_religion1 = 1 if Discri_orig > 0 & Discri_relig + Discri_peau == 0  // que origine
replace origin_skin_religion1 = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion1 = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion1 = 4 if Discri_relig == 1 & Nombre_attributions > 1 // religion ET au moins 1 autre attrib



*******************
* 4.B Models
*******************

*--- Here, we are doing nested logistic regression (origin_skin_religion)


*******************
* 4.B.i General population
*******************
logit MentalH ib1.origin_skin_religion [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib1.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform


*******************
* 4.B.ii Men (all)
*******************
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace

gen origin_skin_religion1 = 5
replace origin_skin_religion1 = 0 if Nombre_attributions == 0 // aucun motif
replace origin_skin_religion1 = 1 if Discri_orig > 0 & Discri_relig + Discri_peau == 0  // que origine
replace origin_skin_religion1 = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion1 = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion1 = 4 if Discri_relig == 1 & Nombre_attributions > 1 // religion ET au moins 1 autre attrib

* keep only men
drop if sexee == 2

* Models
logit MentalH ib1.origin_skin_religion1 [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib3.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform



*******************
* 4.B.iii Men (only discriminated)
*******************
* drop if no discrimination
drop if d_discri == 3

gen origin_skin_religion3 = 5
replace origin_skin_religion3 = 1 if Discri_orig > 0 & Discri_relig + Discri_peau == 0  // que origine
replace origin_skin_religion3 = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion3 = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion3 = 4 if Discri_relig == 1 & Nombre_attributions > 1 // religion ET au moins 1 autre attrib

* Models
logit MentalH ib1.origin_skin_religion3 [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform


*******************
* 4.B.iv Women (all)
*******************
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace


gen origin_skin_religion1 = 5
replace origin_skin_religion1 = 0 if Nombre_attributions == 0 // aucun motif
replace origin_skin_religion1 = 1 if Discri_orig > 0 & Discri_relig + Discri_peau == 0  // que origine
replace origin_skin_religion1 = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion1 = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion1 = 4 if Discri_relig == 1 & Nombre_attributions > 1 // religion ET au moins 1 autre attrib

* drop men
drop if sexee == 1

* Models
logit MentalH ib1.origin_skin_religion1 [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion1 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis ib3.d_discri [pweight = poidsi], or
outreg2 using "ok9.xls", excel replace dec(3) label stats(coef se ci) eform


*******************
* 4.B.v Women (only discriminated)
*******************
* drop if no discrimination
drop if d_discri == 3

* Models
logit MentalH ib1.origin_skin_religion3 [pweight = poidsi], or
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec [pweight = poidsi], or
outreg2 using "ok1.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee [pweight = poidsi], or
outreg2 using "ok2.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
outreg2 using "ok3.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis [pweight = poidsi], or
outreg2 using "ok4.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic [pweight = poidsi], or
outreg2 using "ok5.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area [pweight = poidsi], or
outreg2 using "ok6.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit [pweight = poidsi], or
outreg2 using "ok7.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion3 ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform




**************************************************
* 5. Visualisation
**************************************************

*******************
* 4.A Models' implementation
*******************

* --- only among people who reported at least one discrimination

use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace

gen origin_skin_religion = 5
replace origin_skin_religion = 0 if Nombre_attributions == 0  // que origine
replace origin_skin_religion = 1 if Discri_orig > 0 & Discri_relig + Discri_peau == 0  // que origine
replace origin_skin_religion = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion = 4 if Discri_relig == 1 & Nombre_attributions + 1 // religion ET au moins 1 autre attrib

*--- All the discriminated
* Data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen origin_skin_religion = 5
replace origin_skin_religion = 0 if Nombre_attributions == 0  // que origine
replace origin_skin_religion = 1 if Discri_orig > 0 & Discri_relig + Discri_peau == 0  // que origine
replace origin_skin_religion = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion = 4 if Discri_relig == 1 & Nombre_attributions + 1 // religion ET au moins 1 autre attrib

* Drop if no discrimination
drop if d_discri == 3
drop if Nombre_attributions == 0

* Models
logit MentalH ib1.origin_skin_religion [pweight = poidsi], or
eststo M1
estimates store M_total_simple // No control variable
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
eststo M2
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
estimates store M_total_ctrl // All the control variables (except for the frequency)


*--- Men
* Data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen origin_skin_religion = 5
replace origin_skin_religion = 0 if Nombre_attributions == 0  // que origine
replace origin_skin_religion = 1 if Discri_orig > 0 & Discri_relig + Discri_peau == 0  // que origine
replace origin_skin_religion = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion = 4 if Discri_relig == 1 & Nombre_attributions + 1 // religion ET au moins 1 autre attrib

* Drop if no discrimination / if woman
drop if d_discri == 3
drop if Nombre_attributions == 0
drop if sexee == 2

* Models
logit MentalH ib1.origin_skin_religion [pweight = poidsi], or
eststo M1
estimates store M_hommes_simple // No control variable
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
eststo M2
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
estimates store M_hommes_ctrl // All the control variables (except for the frequency)

*--- Women
* Data
use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace
gen origin_skin_religion = 5
replace origin_skin_religion = 1 if Discri_orig > 0 & Discri_relig + Discri_peau + Discri_accent + Discri_quart  == 0  // que origine
replace origin_skin_religion = 2 if Discri_peau > 0 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0  // que peau
replace origin_skin_religion = 3 if Discri_orig + Discri_peau > 1 & Discri_relig == 0  // orig et peau
replace origin_skin_religion = 4 if Discri_relig == 1 & Nombre_attributions + 1 // religion ET au moins 1 autre attrib

* Drop if no discrimination / if man
drop if d_discri == 3
drop if Nombre_attributions == 0
drop if sexee == 1

* Models
logit MentalH ib1.origin_skin_religion [pweight = poidsi], or
eststo M1
outreg2 using "ok0.xls", excel replace dec(3) label stats(coef se ci) eform
estimates store M_femmes_simple // No control variable
logit MentalH ib1.origin_skin_religion ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit ib30.origine_tous_g2bis [pweight = poidsi], or
eststo M2
outreg2 using "ok8.xls", excel replace dec(3) label stats(coef se ci) eform
estimates store M_femmes_ctrl // All the control variables (except for the frequency)


*******************
* 4.B Graphs
*******************

*--- All the discriminated
coefplot (M_total_simple, label("Without Controls")) ///
         (M_total_ctrl, label("With Controls")), ///
    keep(*.origin_skin_religion) ///
    title("Individuals who reported at least one discrimination", size(small)) ///
    xtitle("Odds ratios", size(small)) ///
    ytitle("", size(small)) ///
    ylabel(1 "Origin only" ///
           2 "Skin colour only" ///
           3 "Origin and skin colour" ///
           4 "Religion (alone or combination)" ///
           5 "Other reasons", labsize(small)) ///
    eform xline(1) baselevels ///
    msize(vsmall) ///
    xlabel(0.5(0.5)2, labsize(small)) ///
    level(90) scheme(s1mono) ///
    graphregion(margin(l+20 r+20)) ///
    plotregion(margin(l+10 r+10)) ///
	legend(size(small)) ///
    saving(gr_total.gph, replace)
	
	
*--- Men
coefplot (M_hommes_simple, label("Without Controls")) ///
         (M_hommes_ctrl, label("With Controls")), ///
    keep(*.origin_skin_religion) ///
    title("The combined effects of origin-, skin color-, religion-based discrimination: men", size(small)) ///
    xtitle("Odds ratios", size(medium)) ///
    ytitle("", size(medium)) ///
    ylabel(1 "Origin only" ///
           2 "Skin color only" ///
           3 "Origin and skin color" ///
           4 "Religion (alone or combination)" ///
		   5 "Other reasons", labsize(small)) ///
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
    keep(*.origin_skin_religion) ///
    title("Women who reported at least one discrimination", size(small)) ///
    xtitle("Odds ratios", size(medium)) ///
    ytitle("", size(medium)) ///
    ylabel(1 "Origin only" ///
           2 "Skin colour only" ///
           3 "Origin and skin colour" ///
           4 "Religion (alone or combination)" ///
		   5 "Other reasons", labsize(small)) ///
    eform xline(1) baselevels ///
    msize(vsmall) ///
    xlabel(0.5(0.5)2, labsize(small)) ///
    level(90) scheme(s1mono) ///
    graphregion(margin(l+20 r+20)) ///
    plotregion(margin(l+10 r+10)) ///
	legend(size(small)) ///
	saving(gr_femmes.gph, replace)

	
*--- Combined graph
graph combine gr_total.gph gr_hommes.gph gr_femmes.gph, ///
    col(1) ///
    ycommon ///
    scheme(white_jet) ///
    imargin(medium) ///
    iscale(.9) ///
    xsize(6.5) ysize(8) ///
    note("Note: 95% confidence intervals around the OR — comparison of simple and adjusted models (with covariates)", size(vsmall)) ///
    saving(gr_combined_MentalH_both.gph, replace)
