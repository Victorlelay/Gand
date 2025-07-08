library(dplyr)
library(tidyr)
library(car)
library(haven)

# Remark: on utilise le VIF pour tester la multicolinéarité dans les modèles:
# VIF_j = A/(1-R²_j) => VIF_j ~ 1 <=> R²_j ~ 0 et donc X_j n'est pas du tout corrélé avec les autres variables explicatives


########################################################################################
# Importation de la table
test_data <- read_dta("Bases de données/test data.dta")
View(test_data)
# recoding of the variable origine_tous_g2

test_data <- test_data %>% mutate(origine_tous_g2bis = case_when(
  (origine_tous_g2 == 0 | origine_tous_g2 == 10 | origine_tous_g2 == 11) ~ 1, # population with no direct migratory ancestry
  (origine_tous_g2 == 20) ~ 20, # From Dom-tom
  (origine_tous_g2 == 22) ~ 22, # Descendants of immigrants from Dom-Tom
  (origine_tous_g2 == 30 | origine_tous_g2 == 40) ~ 30, # Immigrants from Maghreb
  (origine_tous_g2 == 33 |origine_tous_g2 == 44) ~ 33, # Descendants of immigrants from Maghreb
  (origine_tous_g2 == 50 | origine_tous_g2 == 60 | origine_tous_g2 == 70) ~ 50, # Subsaharian African immigrants
  (origine_tous_g2 == 55 | origine_tous_g2 == 66 | origine_tous_g2 == 77) ~ 55, # Descendants of Subsaharian African immigrants
  (origine_tous_g2 == 90) ~ 60, # Immigrants from Turkey and Middle East
  (origine_tous_g2 == 99) ~ 66, # Descendants of immigrants from Turkey and Middle East
  (origine_tous_g2 == 80 | origine_tous_g2 == 100 | origine_tous_g2 == 110) ~ 70, # Asian Immigrants
  (origine_tous_g2 == 88 | origine_tous_g2 == 111) ~ 77, # Descendants of Asian immigrants
  (origine_tous_g2 == 120 | origine_tous_g2 == 130 | origine_tous_g2== 140 | origine_tous_g2 == 150) ~ 80, # European immigrants
  (origine_tous_g2 == 121 | origine_tous_g2 == 131 | origine_tous_g2 == 141 | origine_tous_g2 == 151) ~ 88, # Descentant European immigrants 
  (origine_tous_g2 == 160) ~ 100, # Immigrants from other regions
  (origine_tous_g2 == 161) ~ 111, # Descendant of immigrants from other regions
  TRUE ~ NA_real_
))
## On filtre pour ne garder que la population d'intérêt
test_data<- test_data %>%  filter(origine_tous_g2bis != 1) 
########################################################################################


########################################################################################
# Création de la variable dépendante
test_data <- test_data %>% mutate(MentalH = case_when(
  s_deprim==1|s_noplais==1 ~ 1,
  TRUE ~ 0
))
########################################################################################



########################################################################################
# Création des variables d'intérêt
########################################################################################


# Nombre attribution
test_data <- test_data %>% mutate(Total_attributions = d_pqdisc_a + d_pqdisc_b + d_pqdisc_c + d_pqdisc_d + d_pqdisc_e +d_pqdisc_f + d_pqdisc_g + d_pqdisc_h + d_pqdisc_i + d_pqdisc_j + d_pqdisc_k + d_pqdisc_l + d_pqdisc_m)
test_data <- test_data %>% mutate (Nombre_attributions = case_when(
  Total_attributions == 1 ~ 1, # 1 raison
  Total_attributions > 1 ~ 2, # 2 raisons ou +
  TRUE ~ 0, # pas de raison
))

# Attributions simples
variables_discri <- paste0("d_pqdisc_", letters[1:13])  

# Dictionnaire des noms lisibles
labels_discri <- c(
  "Age",
  "Sexe",
  "Health/Disability",
  "Skin color",
  "Origins/Nationality",
  "Neighbourhood reputation/where you live",
  "Accent/Way of speaking",
  "Family situation",
  "Sexuality",
  "Religion",
  "Clothes",
  "Weight",
  "Other"
)

# On associe le nom des variables aux labels
nom_lisible <- setNames(labels_discri, variables_discri)

# Attribution ethnoraciale: création des variables
test_data <- test_data %>% mutate(attri_ethnorac = case_when(
  d_pqdisc_d==1 | d_pqdisc_e ==  1 ~ 1, 
  TRUE ~ 0
))

# Autre attribution ethnoraciale (quartier/accent)
test_data <- test_data %>% mutate(attri_ethnorac_other = case_when(
  d_pqdisc_f==1 | d_pqdisc_g ==  1 ~ 1, 
  TRUE ~ 0
))

# Attribution religion
test_data <- test_data %>% mutate(attri_relig = case_when(
  d_pqdisc_j==1 ~ 1, 
  TRUE ~ 0
))

# Attribution genre
test_data <- test_data %>% mutate(attri_gender = case_when(
  d_pqdisc_b == 1 ~ 1, 
  TRUE ~ 0
))

# Autre
test_data <- test_data %>% mutate(attri_other = case_when(
  d_pqdisc_a == 1|d_pqdisc_c == 1|d_pqdisc_h == 1|d_pqdisc_i == 1|d_pqdisc_k == 1|d_pqdisc_l == 1|d_pqdisc_m == 1 ~ 1, 
  TRUE ~ 0
))

# Attributions multiples

#plusieurs raison ethno-raciales
test_data <- test_data %>% mutate(plus_ER = case_when(
  d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >=2 ~1,
  TRUE ~0
))

# ER + religion
test_data <- test_data %>% mutate(ER_relig = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1) & d_pqdisc_j==1 ~1,
  TRUE ~0
))

# ER + gen
test_data <- test_data %>% mutate(ER_genre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1) & d_pqdisc_b==1 ~1,
  TRUE ~0
))

# ER + autres
test_data <- test_data %>% mutate(ER_autre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1) & (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k+ d_pqdisc_l + d_pqdisc_m >=1) ~1,
  TRUE ~0
))

# Religion + Genre
test_data <- test_data %>% mutate(relig_genre = case_when(
  d_pqdisc_b + d_pqdisc_j == 2 ~1,
  TRUE ~0
))

# Religion + autre
test_data <- test_data %>% mutate(relig_autre = case_when(
  d_pqdisc_j==1 & (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k +d_pqdisc_l + d_pqdisc_m >=1) ~1,
  TRUE ~0
))

# ER ou relig + genre
test_data <- test_data %>% mutate(ER_ou_relig_genre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1|d_pqdisc_j==1) & d_pqdisc_b==1 ~1,
  TRUE ~0
))

# ER ou relig + autre
test_data <- test_data %>% mutate(ER_ou_relig_autre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1|d_pqdisc_j==1) & (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k +d_pqdisc_l + d_pqdisc_m >=1) ~1,
  TRUE ~0
))
########################################################################################

















########################################################################################
# Modèles
########################################################################################


######### Modèles simples

### Nombre attribution
# Chi²
table_cross_nbr <- table(test_data$MentalH, test_data$Nombre_attributions)
print(table_cross_nbr)
chisq.test(table_cross_nbr) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions



# logit
# logit quantitatif
logit_nbr <- glm(MentalH ~ Nombre_attributions, family = binomial, data = test_data)
summary(logit_nbr)
exp(coef(logit_nbr))# Odds Ratios
exp(confint(logit_nbr)) # Intervalles de confiance des OR


# logit catégoriel
test_data$Nombre_attributions_f <- factor(test_data$Nombre_attributions)
test_data$Nombre_attributions_f <- relevel(test_data$Nombre_attributions_f, ref = "0")
levels(test_data$Nombre_attributions_f)
test_data$Nombre_attributions_f <- relevel(test_data$Nombre_attributions_f, ref = "1")
logit_nbr_cat <- glm(MentalH ~ Nombre_attributions_f, family = binomial, data = test_data)
summary(logit_nbr_cat)
exp(coef(logit_nbr_cat))# Odds Ratios
exp(confint(logit_nbr_cat)) # Intervalles de confiance des OR

# On regarde le AIC pour savoir quel modèle privilégier
AIC(logit_nbr_cat, logit_nbr) 
# Comme on pouvait l'anticiper, c'est le modèle catégoriel qui est préférable






### Attributions simples


# chi²
# ER
table_cross_ER <- table(test_data$MentalH, test_data$attri_ethnorac)
chisq.test(table_cross_ER) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# Autres ER
table_cross_ER_o <- table(test_data$MentalH, test_data$attri_ethnorac_other)
chisq.test(table_cross_ER_o) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions


# Religion
table_cross_relig <- table(test_data$MentalH, test_data$attri_relig)
chisq.test(table_cross_relig) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# Gender
table_cross_gender <- table(test_data$MentalH, test_data$attri_gender)
chisq.test(table_cross_gender) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# Autres
table_cross_other <- table(test_data$MentalH, test_data$attri_other)
chisq.test(table_cross_other) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions


#logit
logit_attri <- glm(MentalH ~ attri_ethnorac + attri_ethnorac_other + attri_relig + attri_gender + attri_other, 
                    family = binomial, data = test_data)
summary(logit_attri)
exp(coef(logit_attri))# Odds Ratios
exp(confint(logit_attri)) # Intervalles de confiance des OR
# seul le coefficient de attri_relig n'est pas significatif (pq ? p-ê confondu ou médié par d'autres variables)
# déviance résiduelle baisse bien par rapport au modèle nul, donc le modèle explique une part significative de la variabilité.

vif(logit_attri) # pas de problème sérieux de multicolinéarité






# Atributions multiples
# plusieurs ER
table_cross_ERx <- table(test_data$MentalH, test_data$plus_ER)
chisq.test(table_cross_ERx) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# ER + relig
table_cross_ERrelig <- table(test_data$MentalH, test_data$ER_relig)
chisq.test(table_cross_ERrelig) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# ER + genre
table_cross_ERgen <- table(test_data$MentalH, test_data$ER_genre)
chisq.test(table_cross_ERgen) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# ER + autre
table_cross_ERo <- table(test_data$MentalH, test_data$ER_autre)
chisq.test(table_cross_ERo) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# relig + genre
table_cross_religgen <- table(test_data$MentalH, test_data$relig_genre)
chisq.test(table_cross_religgen) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions (mais nettement plus élevée que les autres)

# relig + autre
table_cross_religo <- table(test_data$MentalH, test_data$relig_autre)
chisq.test(table_cross_religo) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

# (ER ou relig) + genre
table_cross_ERrelig_gen <- table(test_data$MentalH, test_data$ER_ou_relig_genre)
chisq.test(table_cross_ERrelig_gen) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions


# (relig + genre)ER ou relig) + autre
table_cross_ERrelig_o <- table(test_data$MentalH, test_data$ER_ou_relig_autre)
chisq.test(table_cross_ERrelig_o) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions


# logit
logit_multi <- glm(MentalH ~ plus_ER + ER_relig + ER_genre + ER_autre + relig_genre + relig_autre + ER_ou_relig_genre + ER_ou_relig_autre,
                   family = binomial, data = test_data)
summary(logit_multi)
exp(coef(logit_multi))# Odds Ratios
exp(confint(logit_multi)) # Intervalles de confiance des OR
# OR élevé pour ER_ou_relig_genre => peut indiquer un sous-groupe avec un impact très marqué, MAIS ATTENTION CI = très large => suggère une certaine incertitude (p-ê petit n).

vif(logit_multi) # Beaucoup trop de multicolinéarité clairement, on va subdivisre


############### Religion ###############
# On crée une variable adaptée pour éviter les pb de multicolinéarité
test_data <- test_data %>% mutate(multi_relig = case_when(
  d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g == 1 & d_pqdisc_j == 0 ~ 1,
  d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 2 & d_pqdisc_j == 0 ~ 2,
  (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) & d_pqdisc_j == 1 ~ 3,
  TRUE ~ 0
))

#chi²
table_cross_multirelig <- table(test_data$MentalH, test_data$multi_relig)
chisq.test(table_cross_multirelig) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

#logit quanti
logit_quanti_multirelig <- glm(MentalH ~ multi_relig, family = binomial, data = test_data)
summary(logit_quanti_multirelig)
exp(coef(logit_quanti_multirelig))# Odds Ratios
exp(confint(logit_quanti_multirelig)) # Intervalles de confiance des OR

# logit catégoriel
test_data$multi_relig_f <- factor(test_data$multi_relig)
logit_cat_multirelig <- glm(MentalH ~ multi_relig_f, family = binomial, data = test_data)
summary(logit_cat_multirelig)
exp(coef(logit_cat_multirelig))# Odds Ratios
exp(confint(logit_cat_multirelig)) # Intervalles de confiance des OR


# comparaison des deux modèles
anova(logit_quanti_multirelig, logit_cat_multirelig, test = "Chisq")
AIC(logit_quanti_multirelig, logit_cat_multirelig)
# modèle catégoriel, comme on pouvait s'y attendre
#######################################

############### Genre ###############
# On crée une variable adaptée pour éviter les pb de multicolinéarité
test_data <- test_data %>% mutate(multi_gen = case_when(
  d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g == 1 & d_pqdisc_b == 0 ~ 1,
  d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 2 & d_pqdisc_b == 0 ~ 2,
  (d_pqdisc_d + d_pqdisc_e + d_pqdisc_f + d_pqdisc_g >= 1) & d_pqdisc_b == 1 ~ 3,
  TRUE ~ 0
))

#chi²
table_cross_multigen <- table(test_data$MentalH, test_data$multi_gen)
chisq.test(table_cross_multigen) # test du chi² significatif au seuil de 1%, vaut le coup de faire des régressions

#logit quanti
logit_quanti_multigen <- glm(MentalH ~ multi_gen, family = binomial, data = test_data)
summary(logit_quanti_multigen)
exp(coef(logit_quanti_multigen))# Odds Ratios
exp(confint(logit_quanti_multigen)) # Intervalles de confiance des OR

# logit catégoriel
test_data$multi_gen_f <- factor(test_data$multi_gen)
logit_cat_multigen <- glm(MentalH ~ multi_gen_f, family = binomial, data = test_data)
summary(logit_cat_multigen)
exp(coef(logit_cat_multigen))# Odds Ratios
exp(confint(logit_cat_multigen)) # Intervalles de confiance des OR


# comparaison des deux modèles
anova(logit_quanti_multigen, logit_cat_multigen, test = "Chisq")
AIC(logit_quanti_multigen, logit_cat_multigen)
# modèle catégoriel, comme on pouvait s'y attendre
#######################################















######### Modèles avec variables de contrôle
## Origine: déjà codée pour le filtre
# Convertir en facteur et vérifier les niveaux
test_data$origine_tous_g2bis <- as.factor(test_data$origine_tous_g2bis)
levels(test_data$origine_tous_g2bis)  # pour vérifier les modalités

# Quelle valeur de référence choisir comme on a enlever la population majoritaire ?
# On choisit les G1 maghrébins (origine_tous_g2bis == 30)
test_data$origine_tous_g2bis <- relevel(test_data$origine_tous_g2bis, ref = "30")
levels(test_data$origine_tous_g2bis)


## Niveau d'études
# Recode f_dip en 3 niveaux
test_data <- test_data %>%
  mutate(
    dip_rec = case_when(
      f_dip %in% 1:4 ~ 1,      # <BAC
      f_dip %in% 5:6 ~ 2,      # BAC
      f_dip %in% 7:8 ~ 3,      # >BAC
      TRUE ~ 1                # NA ou autres => <BAC
    ),
    dip_rec = factor(dip_rec, levels = 1:3,
                     labels = c("<BAC", "BAC", ">BAC"))
  )
levels(test_data$dip_rec)
test_data$dip_rec <- relevel(test_data$dip_rec, ref = "BAC")
levels(test_data$dip_rec)

## Genre est déjà codé
test_data$sexee <- factor(test_data$sexee, levels = c(1,2), labels = c("Homme", "Femme"))
levels(test_data$sexee)
# référence = homme

# Nombre attributions
# logit
# logit quantitatif
logit_nbr_ctrl <- glm(MentalH ~ Nombre_attributions + sexee + origine_tous_g2bis + dip_rec, family = binomial, data = test_data)
summary(logit_nbr_ctrl)
exp(coef(logit_nbr_ctrl))# Odds Ratios
exp(confint(logit_nbr_ctrl)) # Intervalles de confiance des OR
# coeffs variable d'intérêt restent significatifs

# logit catégoriel
logit_nbr_cat_ctrl <- glm(MentalH ~ Nombre_attributions_f+ sexee + origine_tous_g2bis + dip_rec, family = binomial, data = test_data)
summary(logit_nbr_cat_ctrl)
exp(coef(logit_nbr_cat_ctrl))# Odds Ratios
exp(confint(logit_nbr_cat_ctrl)) # Intervalles de confiance des OR
# coeffs variable d'intérêt restent significatifs

# On regarde le AIC pour savoir quel modèle privilégier
AIC(logit_nbr_cat_ctrl, logit_nbr_ctrl)
anova(logit_nbr_ctrl, logit_nbr_cat_ctrl, test = "Chisq")
# Comme on pouvait l'anticiper, c'est le modèle catégoriel semble plus précis mais cette fois plus complexe (beaucoup de modalités pour variables de contrôle ?). Je pense que le modèle catégoriel reste préférable
# On regarde la colinéarité
vif(logit_nbr_cat_ctrl)
vif(logit_nbr_ctrl)
# Pas de problème de colinéarité





# Attributions simples
#logit
logit_attri_ctrl <- glm(MentalH ~ attri_ethnorac + attri_ethnorac_other + attri_relig + attri_gender + attri_other + sexee + origine_tous_g2bis + dip_rec, 
                   family = binomial, data = test_data)
summary(logit_attri_ctrl)
exp(coef(logit_attri_ctrl))# Odds Ratios
exp(confint(logit_attri_ctrl)) # Intervalles de confiance des OR
# que le coeff de la religion qui n'est pas significatif, comme avant l'ajout des variables de ctrl


# ATTENTION: depuis qu'on a rajouté les variables de ctrl, il apparaît qu'avoir un diplôme du supérieur semble être une modalité protectrice tcepa
vif(logit_attri)

# Attributions multiples
logit_multi_ctrl <- glm(MentalH ~ plus_ER + ER_relig + ER_genre + ER_autre + relig_genre + relig_autre + ER_ou_relig_genre + ER_ou_relig_autre + sexee + origine_tous_g2bis+dip_rec,
                   family = binomial, data = test_data)
# Comme sans variable de contrôle on avait déjà un pb de multicolinéarité, on regarde ça dès maintenant
vif(logit_multi_ctrl)
# Comme on pouvait s'y attendre, problème de multicolinéarité
# On reprend donc nos variables multi_relig et multi_gen

############### Religion ###############
#logit quanti
logit_quanti_multirelig_ctrl <- glm(MentalH ~ multi_relig + sexee + origine_tous_g2bis + dip_rec, family = binomial, data = test_data)
summary(logit_quanti_multirelig_ctrl)
exp(coef(logit_quanti_multirelig_ctrl))# Odds Ratios
exp(confint(logit_quanti_multirelig_ctrl)) # Intervalles de confiance des OR
# coeffs de variable d'intérêt restent significatifs

# logit catégoriel
logit_cat_multirelig_ctrl <- glm(MentalH ~ multi_relig_f + sexee + origine_tous_g2bis + dip_rec, family = binomial, data = test_data)
summary(logit_cat_multirelig_ctrl)
exp(coef(logit_cat_multirelig_ctrl))# Odds Ratios
exp(confint(logit_cat_multirelig_ctrl)) # Intervalles de confiance des OR
# coeffs de variable d'intérêt restent significatifs

# comparaison des deux modèles
anova(logit_quanti_multirelig_ctrl, logit_cat_multirelig_ctrl, test = "Chisq")
AIC(logit_quanti_multirelig_ctrl, logit_cat_multirelig_ctrl)
# modèle catégoriel, comme on pouvait s'y attendre

# On regarde la multicolinéarité
vif(logit_cat_multirelig_ctrl)
vif(logit_quanti_multirelig_ctrl)
# Pas de souci de multicolinéarité

#######################################

############### Genre ###############
#logit quanti
logit_quanti_multigen_ctrl <- glm(MentalH ~ multi_gen+ sexee + origine_tous_g2bis + dip_rec, family = binomial, data = test_data)
summary(logit_quanti_multigen_ctrl)
exp(coef(logit_quanti_multigen_ctrl))# Odds Ratios
exp(confint(logit_quanti_multigen_ctrl)) # Intervalles de confiance des OR

# logit catégoriel
logit_cat_multigen_ctrl <- glm(MentalH ~ multi_gen_f+ sexee + origine_tous_g2bis + dip_rec, family = binomial, data = test_data)
summary(logit_cat_multigen_ctrl)
exp(coef(logit_cat_multigen_ctrl))# Odds Ratios
exp(confint(logit_cat_multigen_ctrl)) # Intervalles de confiance des OR


# comparaison des deux modèles
anova(logit_quanti_multigen_ctrl, logit_cat_multigen_ctrl, test = "Chisq")
AIC(logit_quanti_multigen_ctrl, logit_cat_multigen_ctrl)
# modèle catégoriel, comme on pouvait s'y attendre (et cette fois pas moins parcimonieux que la modèle quanti)

# On regarde la multicolinéarité
vif(logit_cat_multigen_ctrl)
vif(logit_quanti_multigen_ctrl)
# Pas de problème de multicolinéarité
#######################################


library(broom)
library(dplyr)
library(knitr)
library(kableExtra)

# logit_nbr_cat
mod <- logit_nbr_cat

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_nbr_cat_output.tex")







# logit_attri
mod <- logit_attri

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_attri_output.tex")



# logit_cat_multirelig
mod <- logit_cat_multirelig

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_multirelig.tex")


# logit_cat_multigen
mod <- logit_cat_multigen

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_multigen.tex")




# logit_nbr_cat_ctrl
mod <- logit_nbr_cat_ctrl

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_nbr_ctrl.tex")


# logit_attri_ctrl
mod <- logit_attri_ctrl

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_attri_ctrl.tex")


# logit_multirelig_ctrl
mod <- logit_cat_multirelig_ctrl

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_multirelig_ctrl.tex")




# logit_multigen_ctrl
mod <- logit_cat_multigen_ctrl

# Extraire un tidy summary
tidy_mod <- broom::tidy(mod) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    p.value = signif(p.value, 3)
  ) %>%
  select(term, estimate, std.error, p.value, OR, conf.low, conf.high)

# Renommer colonnes pour la clarté
colnames(tidy_mod) <- c("Variable", "Coef.", "SE", "p-value", "OR", "IC 2.5%", "IC 97.5%")

# Générer la table LaTeX
latex_table <- kable(tidy_mod, format = "latex", booktabs = TRUE, digits = 3,
                     caption = "Résultats de la régression logistique") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Écrire la table dans un fichier .tex
writeLines(latex_table, "logit_multigen_ctrl.tex")


descriptive variables pour les nouvelles