**************************************************
* Regressions and visualization: LCA discrimination // WOMEN
* Victor Le Lay

*** Note: ER = Ethnoracial
**************************************************



**************************************************
*1. DATA
**************************************************

**************************************************
*1.A DATASET
**************************************************

* Dataset
use "C:\Users\vctrl\Desktop\UGent\Code\Data\LCA_discri\data_female_4.dta", clear
save all.dta, replace


**************************************************
*1.B COVARIATES
**************************************************

*** Before or after COVID pandemic start 

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


* Migrant génération

* Three categories: Natives, 1st and 2nd (conflating descendants with one or with two parents)

encode group1, gen(group1_num)


gen generation_disc = .
replace generation_disc = 2 if group1_num==1 | group1_num==2
replace generation_disc = 3 if group1_num==3 | group1_num==4

* Origin

gen origin = 999
replace origin = 2 if origine_tous_g2bis == 20 | origine_tous_g2bis == 22
replace origin = 3 if origine_tous_g2bis == 30 | origine_tous_g2bis == 33
replace origin = 5 if origine_tous_g2bis == 50 | origine_tous_g2bis == 55
replace origin = 6 if origine_tous_g2bis == 60 | origine_tous_g2bis == 66
replace origin = 7 if origine_tous_g2bis == 70 | origine_tous_g2bis == 77
replace origin = 8 if origine_tous_g2bis == 80 | origine_tous_g2bis == 88
replace origin = 10 if origine_tous_g2bis == 100 | origine_tous_g2bis == 111

label define origin_lab ///
    2 "French Overseas Territories" ///
    3 "Maghreb" ///
    5 "Subsaharian Africa" ///
    6 "Turkey and Middle-East" ///
    7 "Asia" ///
    8 "Europe" ///
    10 "Other origin"

label values origin origin_lab
**** Education 

** Recodification diplome
encode f_dip, gen(f_dip_num)
gen dip_rec = 1
replace dip_rec = 1 if f_dip_num == 1 | f_dip_num == 4 | f_dip_num == 5 | f_dip_num == 6 
replace dip_rec = 2 if f_dip_num == 2 | f_dip_num == 3
replace dip_rec = 3 if f_dip_num == 7 | f_dip_num == 8

* NB: missing recoded to low level of education 
label def edu 1 "<BAC" 2 "BAC" 3 ">BAC", modify
label values dip_rec edu


******** RELIGION 


*being religious
encode r_relsoi, gen(r_relsoi_num)
gen religious = r_relsoi_num ==3

*religion
encode relego1, gen(relego1_num)
gen religion_nomis = 4

replace religion_nomis = 1 if relego1_num == 2 | relego1_num == 4 | relego1_num == 5 | relego1_num == 13 | relego1_num == 14 

replace religion_nomis = 2 if relego1_num == 9 | relego1_num == 10

replace religion_nomis = 3 if relego1_num == 1 | relego1_num == 3 | relego1_num == 6| relego1_num == 7 | relego1_num == 8

replace religion_nomis=4 if relego1_num == 11 | relego1_num == 12 | relego1_num == 15

label define religion_nomis_lab 1 "Christian" 2 "Muslim" 3 "Other" 4 "No current religion or no religion stated"
label values religion_nomis religion_nomis_lab


**************** FAMILY status 
encode matrie, gen(matrie_num)
gen family_sit = .
replace family_sit = 1 if (matrie_num == 1 | matrie_num == 2 | matrie_num == 4) & nbenf == 0
replace family_sit = 2 if (matrie_num == 1 | matrie_num == 2 | matrie_num == 4) & nbenf > 0
replace family_sit = 3 if matrie_num == 3 & nbenf == 0
replace family_sit = 4 if matrie_num == 3 & nbenf > 0


**************** Size area
encode(tuu_rec), gen(tuu_rec_num)
gen size_area = 99
replace size_area = 1 if tuu_rec_num == 1 | tuu_rec_num == 2 | tuu_rec_num == 4 | tuu_rec_num == 6
replace size_area = 2 if tuu_rec_num == 3
replace size_area = 3 if tuu_rec_num == 5




**************** Latent classes
gen latent_class = 99
replace latent_class = 1 if classe_latente == 2
replace latent_class = 2 if classe_latente == 1
replace latent_class = 3 if classe_latente == 4
replace latent_class = 4 if classe_latente == 3

label define LCA_lab 1 "Natio. + Religion" 2 "Skin + Natio." 3 "Other" 4 "Gender + Ethnoracial"
label values latent_class LCA_lab

**************** Mental Health
gen MentalH = 0
replace MentalH = 1 if s_deprim == "Oui" | s_noplais == "Oui"

label define MH_lab 1 "Mental Health issues reported" 0 "No mental Health issues reported"
label values MentalH MH_lab

**************** Gender
encode(sexee), gen(sexee_num)

**************************************************
*1. Regressions (Models)
**************************************************

**************************************************
*1.A No control (M1)
**************************************************
logit MentalH ib2.latent_class, or
outreg2 using "M1_women4_LCA.xls", excel replace dec(3) label stats(coef se ci) eform

**************************************************
*1.B Controls (M2)
**************************************************

*** M2: M1 + Socio-demographic
logit MentalH ib2.latent_class ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit i.generation_disc ib3.origin, or
outreg2 using "M2_women4_LCA.xls", excel replace dec(3) label stats(coef se ci) eform

* On check multicolinearity
reg MentalH ib2.latent_class ib2.dip_rec ib1.sexee_num agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit i.generation_disc ib3.origin
vif // no issue


*** M3: M2 + Nb_evalutions
encode(d_discri), gen(d_discri_num)

logit MentalH ib2.latent_class ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit i.generation_disc ib3.origin ib1.d_discri_num, or
outreg2 using "M3_women4_LCA.xls", excel replace dec(3) label stats(coef se ci) eform

* On check multicolinearity
reg MentalH ib2.latent_class ib2.dip_rec agenq ib4.religion_nomis ib0.pandemic ib1.size_area ib4.family_sit i.generation_disc ib3.origin ib1.d_discri_num
vif // no issue
