* --- Importer les données .dta
use "C:\Users\vctrl\Desktop\UGent\Code\Data\test_data_final.dta", clear
save test_data.dta, replace
* --- Créer et recoder la variable origine_tous_g2bis
gen origine_tous_g2bis = .

replace origine_tous_g2bis = 1   if inlist(origine_tous_g2, 0, 10, 11)
replace origine_tous_g2bis = 20  if origine_tous_g2 == 20
replace origine_tous_g2bis = 22  if origine_tous_g2 == 22
replace origine_tous_g2bis = 30  if inlist(origine_tous_g2, 30, 40)
replace origine_tous_g2bis = 33  if inlist(origine_tous_g2, 33, 44)
replace origine_tous_g2bis = 50  if inlist(origine_tous_g2, 50, 60, 70)
replace origine_tous_g2bis = 55  if inlist(origine_tous_g2, 55, 66, 77)
replace origine_tous_g2bis = 60  if origine_tous_g2 == 90
replace origine_tous_g2bis = 66  if origine_tous_g2 == 99
replace origine_tous_g2bis = 70  if inlist(origine_tous_g2, 80, 100, 110)
replace origine_tous_g2bis = 77  if inlist(origine_tous_g2, 88, 111)
replace origine_tous_g2bis = 80  if inlist(origine_tous_g2, 120, 130, 140, 150)
replace origine_tous_g2bis = 88  if inlist(origine_tous_g2, 121, 131, 141, 151)
replace origine_tous_g2bis = 100 if origine_tous_g2 == 160
replace origine_tous_g2bis = 111 if origine_tous_g2 == 161



* --- Supprimer les observations sans ascendance migratoire
drop if origine_tous_g2bis == 1

* --- Création de la variable dépendante MentalH
gen MentalH = 0
replace MentalH = 1 if s_deprim == 1 | s_noplais == 1


egen Total_attributions = rowtotal(d_pqdisc_a d_pqdisc_b d_pqdisc_c d_pqdisc_d d_pqdisc_e ///
                                   d_pqdisc_f d_pqdisc_g d_pqdisc_h d_pqdisc_i d_pqdisc_j ///
                                   d_pqdisc_k d_pqdisc_l d_pqdisc_m)


* --- Créer la variable Nombre_attributions selon le nombre de raisons
gen Nombre_attributions = 0
replace Nombre_attributions = 1 if Total_attributions == 1
replace Nombre_attributions = 2 if Total_attributions > 1

* --- Covid
gen datenq_extr=int(datenq/100)
gen datenq_extr2=int(datenq/10000)
gen datenq_extr3=datenq_extr2*100
gen monthnq=datenq_extr-datenq_extr3
order aenq datenq* monthnq
tab monthnq,m
drop datenq_extr*
gen pandemic=0
replace pandemic=1 if (monthnq>2 & aenq==2020)
replace pandemic=1 if (aenq==2021)
lab var pandemic "Covid - post march 2020"

* --- Diplôme
recode f_dip (1/4=1)(5/6=2)(7/8=3)(.=1), gen(dip_rec)
* NB: missing recoded to low level of education 
label def edu 1 "<BAC" 2 "BAC" 3 ">BAC", modify
label values dip_rec edu

*--- Revenu
*recodification a_estim avec les valeurs de la médiane (pondérée) de la variable a_montan dans chaque tranche de la variabe a_estim pour combler les valeurs manquantes de a_montan
drop a_estim_rec
recode a_estima (1=300)(2=500)(3=700)(4=890)(5=1050)(6=1300)(7=1600)(8=1800)(9=2100)(10=2600)(11=3300)(12=4500)(13=6700)(14=11000)(98/99=2700), g(a_estim_rec)
g a_montan_rec=a_montan
replace a_montan_rec=a_estim_rec if a_montan_drap==-1
replace a_montan_rec=. if a_montan<=495 | a_montan>11000
xtile income3=a_montan_rec, nq(3)

/*suppression valeurs extremes 1% et 99%*/
****/!\ toujours un pb de missing values

*--- Religion

gen religion = .

replace religion = 1 if relego1>=10&relego1<=19&relego1!=.

replace religion = 2 if relego1==20|relego1==21

replace religion = 3 if relego1>=30&relego1!=.

replace religion=4 if r_relsoi==2 |relego1==1

label define religion 1 "Christian" 2 "Muslim" 3 "Other" 4 "No current religion"
label values religion religion

* Recode of Religion including those who stated no religion (Refus or NSP) in the last, 4th, category

gen religion_nomis = .

replace religion_nomis = 1 if relego1>=10&relego1<=19&relego1!=.

replace religion_nomis = 2 if relego1==20|relego1==21

replace religion_nomis = 3 if relego1>=30&relego1!=.

replace religion_nomis=4 if r_relsoi!=1 |relego1==1

label define religion_nomis 1 "Christian" 2 "Muslim" 3 "Other" 4 "No current religion or no religion stated"
label values religion_nomis religion_nomis

*--- FAMILY status 

** Marital

recode matrie (1=1 "Single") (2=2 "Married") (3 4 = 3 "Widowed or Divorced"), into (marital_stat)

** Number of children

recode nbenf (0=0 "No children") (1=1 "1 child") (2=2 "2 Children") (3/max=3 "Three or more children"), gen(number_kids)

** Family status
gen family_sit=.
replace family_sit=1 if marital_stat!=2 & number_kids==0
replace family_sit=2 if marital_stat!=2 & number_kids>0
replace family_sit=3 if marital_stat==2 & number_kids==0
replace family_sit=4 if marital_stat==2 & number_kids>0


label def family_sit 1 "Not married no kids" 2 "Not married with kids" 3 "Married no kids" 4 "Married with kids"
label values family_sit family_sit


bysort family_sit: ta marital_stat number_kids, m

recode family_sit (1 3=1 "Single/Wid/Div no kids") (2 4=2 "Single/Wid/Div with kids") (5=3 "Married no kids")(6 7 8 =4 "Married with kids" ), gen(family_rec)

*--- RURAL / URBAN 
 
recode tuu_rec (0 1 2 3= 1 "Less than 200 000 h.")(4=2 "200 000 - 1 999 999 h.") (5=3 "Paris"), into (size_area)
 

* 1) Renommage des variables de discrimination
rename d_pqdisc_a Discri_age
rename d_pqdisc_b Discri_genre
rename d_pqdisc_c Discri_handi
rename d_pqdisc_d Discri_peau
rename d_pqdisc_e Discri_orig
rename d_pqdisc_f Discri_quart
rename d_pqdisc_g Discri_accent
rename d_pqdisc_h Discri_fami
rename d_pqdisc_i Discri_orientsex
rename d_pqdisc_j Discri_relig
rename d_pqdisc_k Discri_habi
rename d_pqdisc_l Discri_poids
rename d_pqdisc_m Discri_autre
 
 *-- On remplace les missing values par des 0
ds Discri_*
foreach var in `r(varlist)' {
    replace `var' = 0 if missing(`var')
}

*--- multiple_ethnoracial
gen multiple_ethnoracial = .

replace multiple_ethnoracial = 0 if ///
    Discri_age + Discri_genre + Discri_peau + Discri_orig + Discri_quart + ///
    Discri_accent + Discri_fami + Discri_orientsex + Discri_relig + ///
    Discri_habi + Discri_autre + Discri_handi + Discri_poids == 0

replace multiple_ethnoracial = 1 if ///
    Discri_orig == 1 & Discri_relig + Discri_peau == 0

replace multiple_ethnoracial = 2 if ///
    Discri_peau == 1 & Discri_orig + Discri_accent + Discri_quart + Discri_relig == 0

replace multiple_ethnoracial = 3 if ///
    Discri_relig == 1 & Discri_orig + Discri_accent + Discri_quart + Discri_peau == 0

replace multiple_ethnoracial = 4 if ///
    Discri_orig + Discri_peau == 2 & Discri_relig == 0

replace multiple_ethnoracial = 5 if ///
    Discri_orig + Discri_relig == 2 & Discri_peau == 0

replace multiple_ethnoracial = 6 if ///
    Discri_peau + Discri_relig == 2 & Discri_orig + Discri_accent + Discri_quart == 0

replace multiple_ethnoracial = 7 if ///
    (Discri_orig == 1 | Discri_accent == 1 | Discri_quart == 1) & ///
    Discri_relig + Discri_peau == 2

replace multiple_ethnoracial = 8 if missing(multiple_ethnoracial)


*--- number_multiple_ethnoracial
gen number_multiple_ethnoracial = ""

replace number_multiple_ethnoracial = "No discri"      if multiple_ethnoracial == 0
replace number_multiple_ethnoracial = "One discri."    if inrange(multiple_ethnoracial, 1, 3)
replace number_multiple_ethnoracial = "At least 2"     if inrange(multiple_ethnoracial, 4, 7)
replace number_multiple_ethnoracial = "Others"         if multiple_ethnoracial == 8


*--- INDICATRICES
***** ethno_racial
gen ethno_racial = 0
replace ethno_racial = 1 if (Discri_accent + Discri_quart + Discri_orig) > 0

***** skin_colour
gen skin_colour = 0
replace skin_colour = 1 if Discri_peau > 0

***** religion
gen religion_indica = 0
replace religion_indica = 1 if Discri_relig > 0





*** ----- On passe à la modélisation
* On enlève les G1 et G2 autres
drop if origine_tous_g2bis == 100 | origine_tous_g2bis == 111

save "C:\Users\vctrl\Desktop\UGent\Code\Data\base_regressions.dta", replace

********* Nombre d'attributions 
* Modèle 1 : pas de contrôles
* logit cat weighted
logit MentalH ib1.Nombre_attributions [pweight = poidsi]
logit MentalH ib1.Nombre_attributions [pweight = poidsi], or
outreg2 using "NBAttribM1.xls", excel replace dec(3) label stats(coef se ci) eform
est store M1_nb_attri


* Modèle 2
logit MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi], or
outreg2 using "NBAttribM22.xls", excel replace dec(3) label stats(coef se ci) eform
est store M2_nb_attri

** On vérifie si pb de multicolinéarité
regress MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.family_sit tuu_rec [pweight = poidsi]
vif //pas de pb de multicolinéarité

*** Modèles imbriqués
esttab M1_nb_attri M2_nb_attri, ci star label b(%9.3f) ///
    title(Comparaison des modèles M1 et M2 - Logit) ///
    compress alignment(D) ///
    stats(N r2_p, labels("Observations" "Pseudo R²"))


	
	
	
	
	

********* Nombre d'attributions ethnoraciales
* Modèle 1 : pas de contrôles
* logit cat weighted
logit MentalH ib0.multiple_ethnoracial [pweight = poidsi]
logit MentalH ib0.multiple_ethnoracial [pweight = poidsi], or 
outreg2 using "NBAttribERM1.xls", excel replace dec(3) label stats(coef se ci) eform

est store M1_nb_ethnoracial

* Modèle 2
logit MentalH ib0.multiple_ethnoracial ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi], or
outreg2 using "NBAttribERM22.xls", excel replace dec(3) label stats(coef se ci) eform

est store M2_nb_ethnoracial

** On vérifie si pb de multicolinéarité
regress MentalH ib0.multiple_ethnoracial ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib1.family_sit tuu_rec [pweight = poidsi]
vif //pas de pb de multicolinéarité

*** Modèles imbriqués
esttab M1_nb_ethnoracial M2_nb_ethnoracial, ci star label b(%9.3f) ///
    title(Comparaison des modèles M1 et M2 - Logit) ///
    compress alignment(D) ///
    stats(N r2_p, labels("Observations" "Pseudo R²"))

	
********* Indicatrices
drop if Nombre_attributions==0




*--------------- Juste les main effects
* M1 : pas de ctrl
logit MentalH ethno_racial skin_colour religion_indica [pweight = poidsi], or 
outreg2 using "MEM1.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica [pweight = poidsi]
vif //pas de pb de multicolinéarité


* Modèle 2
logit MentalH ethno_racial skin_colour religion_indica ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi], or
outreg2 using "MEM2.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ib0.multiple_ethnoracial ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi]
vif //pas de pb de multicolinéarité


	
*** Interactions ER * SKIN COLOUR ethno_racial##skin_colour
** M1
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour [pweight = poidsi], or 
outreg2 using "InteERSC1.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour [pweight = poidsi]
vif //pas de pb de multicolinéarité

*** M2 (ctrl)
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi], or
outreg2 using "InteERSC2.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##skin_colour ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi]
vif //pas de pb de multicolinéarité


*** Interactions ER * religion ethno_racial##religion_indica
** M1
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica [pweight = poidsi], or 
outreg2 using "InteERRel1.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica [pweight = poidsi]
vif //pas de pb de multicolinéarité

*** M2 (ctrl)
logit MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi], or
outreg2 using "InteERRel2.xls", excel replace dec(3) label stats(coef se ci) eform

regress MentalH ethno_racial skin_colour religion_indica ethno_racial##religion_indica ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq ib4.religion_nomis ib0.pandemic ib4.family_sit ib1.size_area [pweight = poidsi]
vif //pas de pb de multicolinéarité




