* --- Importer les données .dta
use "C:\Users\vctrl\Desktop\UGent\Code\Data\test data.dta", clear
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


* Attribution ethnoraciale principale
gen attri_ethnorac = 0
replace attri_ethnorac = 1 if d_pqdisc_d == 1 | d_pqdisc_e == 1

* Attribution ethnoraciale secondaire (quartier / accent)
gen attri_ethnorac_other = 0
replace attri_ethnorac_other = 1 if d_pqdisc_f == 1 | d_pqdisc_g == 1

* Attribution religieuse
gen attri_relig = 0
replace attri_relig = 1 if d_pqdisc_j == 1

* Attribution genre
gen attri_gender = 0
replace attri_gender = 1 if d_pqdisc_b == 1

* Autre attribution
gen attri_other = 0
replace attri_other = 1 if d_pqdisc_a == 1 | d_pqdisc_c == 1 | d_pqdisc_h == 1 | ///
                          d_pqdisc_i == 1 | d_pqdisc_k == 1 | d_pqdisc_l == 1 | d_pqdisc_m == 1

* Plusieurs raisons ethnoraciales
gen plus_ER = 0
replace plus_ER = 1 if (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g) >= 2

* Ethnoracial + religion
gen ER_relig = 0
replace ER_relig = 1 if (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) & d_pqdisc_j == 1

* Ethnoracial + genre
gen ER_genre = 0
replace ER_genre = 1 if (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) & d_pqdisc_b == 1

* Ethnoracial + autre
gen ER_autre = 0
replace ER_autre = 1 if (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) & (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k + d_pqdisc_l + d_pqdisc_m >= 1)

* Religion + genre
gen relig_genre = 0
replace relig_genre = 1 if d_pqdisc_b == 1 & d_pqdisc_j == 1

* Religion + autre
gen relig_autre = 0
replace relig_autre = 1 if d_pqdisc_j == 1 & ///
                          (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k + ///
                           d_pqdisc_l + d_pqdisc_m >= 1)

* ER ou religion + genre
gen ER_ou_relig_genre = 0
replace ER_ou_relig_genre = 1 if ((d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) | d_pqdisc_j == 1) & d_pqdisc_b == 1

* ER ou religion + autre
gen ER_ou_relig_autre = 0
replace ER_ou_relig_autre = 1 if ///
    (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1 | d_pqdisc_j == 1) & ///
    (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k + d_pqdisc_l + d_pqdisc_m >= 1)


								  
								  
* --- Bivariate association

* --- Nbr attri		
* j'ai fait un test du chi² surtout pour vérifier que je trouvais les mêmes résultats que sur R						
tabulate MentalH Nombre_attributions, chi2
* logit quanti: on ne le reproduira plus pour les variables suivantes
logit MentalH Nombre_attributions [pweight = poidsi]
label define ncatlbl 0 "zéro" 1 "une" 2 "deux ou +"
label values Nombre_attributions ncatlbl
* logit cat
logit MentalH ib1.Nombre_attributions
* logit cat weighted
logit MentalH ib1.Nombre_attributions [pweight = poidsi]
logit MentalH ib1.Nombre_attributions [pweight = poidsi], or 
* a priori : 1) le logit quanti ne sert pas à grand chose (j'ai en R des critères AIC et Anova quand il y a des var de ctrl) mais ça ne me semble pas nécessaire d'inclure cela ici


* --- Attributions simples (inclut les régressions non pondérées pour comparer)
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other, or
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other [pweight=poidsi]
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other [pweight=poidsi], or
* On test la multicolinéarité
regress attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other
vif
regress attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other [pweight=poidsi]
vif

* --- Attributions multiples (inclut les régressions non pondérées pour comparer)
logit MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre
logit MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre, or
logit MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre [pweight=poidsi]
logit MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre [pweight=poidsi], or
* On teste la multicolinéarité
regress MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre [pweight=poidsi]
vif
regress MentalH plus_ER ER_relig ER_genre ER_autre relig_genre relig_autre ER_ou_relig_genre ER_ou_relig_autre [pweight=poidsi]
vif
* comme on pouvait s'y attendre, pondéré ou non, il y a un gros problème de multicolinéarité, on va donc recréer une variable multi relig et multi genre pour contrer ce phénomène

* --- On recode donc des variables adaptées pour regarder l'interaction entre ER et genre/
* - Religion
gen multi_relig = 0
replace multi_relig = 1 if d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g == 1 & d_pqdisc_j == 0
replace multi_relig = 2 if d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 2 & d_pqdisc_j == 0
replace multi_relig = 3 if d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g ==0 & d_pqdisc_j == 1
replace multi_relig = 4 if (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) & d_pqdisc_j == 1

* Déclarer multi_relig comme une variable factorielle
label define multi_relig_lbl 0 "0" 1 "1_ER" 2 "2+_ER" 3 "relig" 4 "ER+relig"
label values multi_relig multi_relig_lbl

* Avec 0 come référence (quand on met 1 en référence, on n'a plus que le coefficient 0 qui est significatif...)
logit MentalH i.multi_relig [pweight = poidsi]
logit MentalH ib0.multi_relig [pweight = poidsi], or

* - Genre
gen multi_genre = 0
replace multi_genre = 1 if d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g == 1 & d_pqdisc_b == 0
replace multi_genre = 2 if d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 2 & d_pqdisc_b == 0
replace multi_genre = 3 if d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g ==0 & d_pqdisc_b == 1
replace multi_genre = 4 if (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) & d_pqdisc_b == 1

* Déclarer multi_genre comme une variable factorielle
label define multi_genre_lbl 0 "0" 1 "1_ER" 2 "2+_ER" 3 "genre" 4 "ER+genre"
label values multi_genre multi_genre_lbl

* Avec 0 come référence (quand on met 1 en référence, on n'a plus que le coefficient 0 qui est significatif...)
logit MentalH i.multi_genre [pweight = poidsi]
logit MentalH i.multi_genre [pweight = poidsi], or




* --- Control

* Recatégoriser f_dip en dip_rec
gen dip_rec = .
replace dip_rec = 1 if inrange(f_dip, 1, 4)        // <BAC
replace dip_rec = 2 if inrange(f_dip, 5, 6)        // BAC
replace dip_rec = 3 if inrange(f_dip, 7, 8)        // >BAC
replace dip_rec = 1 if missing(dip_rec)            // Par défaut, NA => <BAC


* --- Nbr attributions

logit MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
logit MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
* On vérifie la multicolinéarité : pas de souci
regress MentalH ib1.Nombre_attributions ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
vif

* --- Attributions simples (inclut les régressions non pondérées pour comparer)
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
logit MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
* On vérifie la multicolinéarité : pas de souci
regress MentalH attri_ethnorac attri_ethnorac_other attri_relig attri_gender attri_other ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
vif

* --- Attributions multiples
* --- Religion
logit MentalH i.multi_relig ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
logit MentalH i.multi_relig ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
* On vérifie la multicolinéarité : pas de souci
regress MentalH i.multi_relig ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
vif

* --- Genre
logit MentalH i.multi_genre ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
logit MentalH i.multi_genre ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi], or
* On vérifie la multicolinéarité : pas de souci
regress MentalH i.multi_genre ib30.origine_tous_g2bis ib2.dip_rec ib1.sexee agenq [pweight = poidsi]
vif

save test_data.dta, replace

* --- On va séparer H et F :
* --- Supprimer les observations sans ascendance migratoire

* --- Femmes
use test_data.dta, clear
preserve
keep if sexee == 2
save femmes_data.dta, replace
restore

use femmes_data.dta, clear
logit MentalH i.multi_genre ib30.origine_tous_g2bis ib2.dip_rec agenq [pweight = poidsi]


* --- Hommes
use test_data.dta, clear
preserve
keep if sexee == 1
save hommes_data.dta, replace
restore

use hommes_data.dta, clear
logit MentalH i.multi_genre ib30.origine_tous_g2bis ib2.dip_rec agenq [pweight = poidsi]











