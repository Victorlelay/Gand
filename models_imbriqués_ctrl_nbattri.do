* --- nbr attributions en contrôle*

use "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", clear
save base_regressions.dta, replace


********* Nombre d'attributions ethnoraciales
* Modèle 1 : pas de contrôles
* logit cat weighted
logit MentalH ib0.multiple_ethnoracial Nombre_attributions [pweight = poidsi]
logit MentalH ib0.multiple_ethnoracial Nombre_attributions [pweight = poidsi], or 
outreg2 using "NBAttribERM1.xls", excel replace dec(3) label stats(coef se ci) eform

reg MentalH ib0.multiple_ethnoracial Nombre_attributions [pweight = poidsi]
vif
* pb de multicolinéarité

	
********* Indicatrices
drop if Nombre_attributions==0




*--------------- Juste les main effects
* M1 : pas de ctrl
logit MentalH ethno_racial skin_colour religion_indica [pweight = poidsi], or 
outreg2 using "MEM1.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica [pweight = poidsi]
vif //pas de pb de multicolinéarité


* Modèle 2
logit MentalH ethno_racial skin_colour religion_indica ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi], or
outreg2 using "MEM2.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ib0.multiple_ethnoracial ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area Nombre_attributions [pweight = poidsi]
vif //pas de pb de multicolinéarité




	
*** Interactions ER * SKIN COLOUR ethno_racial##skin_colour
** M1
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour [pweight = poidsi], or 
outreg2 using "InteERSC1.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour [pweight = poidsi]
vif //pas de pb de multicolinéarité

*** M2 (ctrl)
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi], or
outreg2 using "InteERSC2.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions[pweight = poidsi]
vif //pas de pb de multicolinéarité


*** Interactions ER * religion ethno_racial##religion_indica
** M1
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica [pweight = poidsi], or 
outreg2 using "InteERRel1.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica [pweight = poidsi]
vif //pas de pb de multicolinéarité

*** M2 (ctrl)
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi], or
outreg2 using "InteERRel2.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area ib1.Nombre_attributions [pweight = poidsi]
vif //pas de pb de multicolinéarité
