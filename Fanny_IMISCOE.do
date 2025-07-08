***************************************************************
* Analysis made for the Fanny (IMISCOE)
* Victor Le Lay

***************************************************************


**************************************************
* 1. Data
*--- For the first part of the analysis (desrciptive statistics), I will use the original Quetelet taset
*--- Then, for the regressions/stats on the LCA made on R, I will use the Quetelt dataset modified in the R file named 'LCA_Fanny_racism_IMISCOE.R'
**************************************************

* Original Quetelet Dataset (n = 27 181)
use "C:\Users\vctrl\Desktop\UGent\Code\Data\indiv.dta", clear
save indiv.dta, replace

* Origin x Generation variable
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

* drop if native , n = 20 251
drop if origine_tous_g2bis == 1 // Dataset 1

* Racism variable (d_racism x d_racpot)
gen racism = 99
replace racism = 1 if d_racism == 1 // Case 1: have already experienced racism 
replace racism = 2 if d_racism == 2 & d_racpot == 1 // Case 2: Did not but think they could in the future (in France)
replace racism = 3 if d_racpot == 2 & racism == 99 // Case 3: Dit not and do not think they ever will


**************************************************
* 2. Descriptive statistics
**************************************************
* Table 1: non weighted n for racism 
tab racism

* Table 2: weighted % for racism 
svyset _n [pweight=poidsi]
svy: tabulate racism, percent


**************************************************
* 3. Multinomial logistic models
**************************************************

***********************************
* 2.A Control variables (creation)
***********************************

*--- Education (code from Sorana)
recode f_dip (1/4=1)(5/6=2)(7/8=3)(.=1), gen(dip_rec)
* NB: missing recoded to low level of education 
label def edu 1 "<BAC" 2 "BAC" 3 ">BAC", modify
label values dip_rec edu

*--- Covid (before vs after the pandemic) (code from Sorana)
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
gen origine = 99
replace origine = 2 if origine_tous_g2bis == 20 | origine_tous_g2bis == 22
replace origine = 3 if origine_tous_g2bis == 33 | origine_tous_g2bis == 30
replace origine = 5 if origine_tous_g2bis == 50 | origine_tous_g2bis == 55
replace origine = 6 if origine_tous_g2bis == 60 | origine_tous_g2bis == 66
replace origine = 7 if origine_tous_g2bis == 70 | origine_tous_g2bis == 77
replace origine = 8 if origine_tous_g2bis == 80 | origine_tous_g2bis == 88
replace origine = 9 if origine_tous_g2bis == 90 | origine_tous_g2bis == 99
replace origine = 10 if origine_tous_g2bis == 100 | origine_tous_g2bis == 111


*--- Decomposition origine_tous_g2bis (origin x generation) into two variables (origin AND generation)
* Generation (simplified)
gen group_num = 0
replace group_num = 1 if group1 ==1 // G1 immigrant
replace group_num = 2 if group1 ==2 // G1 from French overseas territories
replace group_num = 3 if group1 ==3 | group1 == 4 // G2 (immigrants + French overseas territories)


***********************************
* 2.B Models
***********************************
*--- all the population (except for natives)
mlogit racism i.sexee i.dip_rec ib3.origine ib3.group_num pandemic agenq [pweight = poidsi], baseoutcome(3)
estimates store m0
outreg2 [m0] using "modelDataset1_or.xls", excel replace dec(3) ///
label eform stats(coef se ci)

* Multicolinearity check
reg racism i.sexee i.dip_rec ib3.origine ib3.group_num pandemic agenq [pweight = poidsi]
vif // pas de pb de multicolinéarité

**************************************************
* 4. Creation dataset for LCA
**************************************************
*--- No natives + drop if no racism experienced or expected
drop if racism == 3|racism==99


**************************************************
* 5. Analysis after LCA made in R
**************************************************
***********************************
* 5.A Data
***********************************
* importation dataset (after LCA is done) from R
use "C:\Users\vctrl\Desktop\UGent\Code\Data\dataLCA.dta", clear
save dataLCA.dta, replace

* dip_rec needs to be a numerical variable
encode dip_rec, gen(dip_rec_num)


***********************************
* 5.B Models - Relative Risk Ratio (RRR)
***********************************
* RRR are the same for multinomial logistic analysis the OR are for the basic logistic analysis (same interpretation)

*--- M1: no control variables
mlogit Latent_class racism [pweight = poidsi], rrr baseoutcome(2)
estimates store m1
outreg2 [m2] using "model1_or.xls", excel replace dec(3) ///
label eform stats(coef se ci)

*--- M2: M1 + control variables (gender, age, educatio, origin, generation, covid)
mlogit Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic [pweight = poidsi], rrr baseoutcome(2)
estimates store m2
outreg2 [m2] using "mode2_or.xls", excel replace dec(3) ///
label eform stats(coef se ci)

* Multicolinearity check
reg Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic [pweight = poidsi]
vif // no problem here

*--- M3: M2 + context on when/where de discrimination has been experienced
*** /!\ WARNING : this model makes no sense has individuals who belong to racism = 2 have not reported a location for the discrimination (haven't expericend it yet)
mlogit Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic i.racism_location_pub i.racism_location_ws [pweight = poidsi], rrr baseoutcome(2)
estimates store m3
outreg2 [m3] using "mode3_or.xls", excel replace dec(3) ///
label eform stats(coef se ci)

* Multicolinearity check
reg Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic i.racism_location_pub i.racism_location_ws [pweight = poidsi]
vif // no problem here


***********************************
* 5.C Models - log coeffs
***********************************
* RRR are the same for multinomial logistic analysis the OR are for the basic logistic analysis (same interpretation)

*--- M1: no control variables
mlogit Latent_class racism [pweight = poidsi], baseoutcome(2)
estimates store m1
outreg2 [m2] using "model1_or.xls", excel replace dec(3) ///
label eform stats(coef se ci)

*--- M2: M1 + control variables (gender, age, educatio, origin, generation, covid)
mlogit Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic [pweight = poidsi], baseoutcome(2)
estimates store m2
outreg2 [m2] using "mode2_or.xls", excel replace dec(3) ///
label eform stats(coef se ci)

* Multicolinearity check
reg Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic [pweight = poidsi]
vif // no problem here

*--- M3: M2 + context on when/where de discrimination has been experienced
*** /!\ WARNING : this model makes no sense has individuals who belong to racism = 2 have not reported a location for the discrimination (haven't expericend it yet)
mlogit Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic i.racism_location_pub i.racism_location_ws [pweight = poidsi], baseoutcome(2)
estimates store m3
outreg2 [m3] using "mode3_or.xls", excel replace dec(3) ///
label eform stats(coef se ci)

* Multicolinearity check
reg Latent_class racism ib1.sexee agenq ib3.dip_rec_num ib3.origine ib3.group_num pandemic i.racism_location_pub i.racism_location_ws [pweight = poidsi]
vif // no problem here
