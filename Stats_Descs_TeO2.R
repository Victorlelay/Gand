install.packages(c("dplyr", "weights", "Hmisc", "questionr"))
library(questionr)
library(dplyr)
library(weights) 
library(readr)
library(tidyr)
library(stringr)
library(survey)

# import the database
indiv <- read_delim("Bases de données/indiv.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# recoding of the variable origine_tous_g2
indiv <- test_data_final %>% mutate(origine_tous_g2bis = case_when(
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

# filter to only keep G1 and G2
indiv_immi<- indiv %>%  filter(origine_tous_g2bis != 1) 

# Discrimination (last 5 years)

# G1 vs G2
# Creation of a dummy variable in order to split the database between G1 and G2
indiv_immi <- indiv_immi %>% mutate(G1_G2 = case_when(
  origine_tous_g2bis %in% c(20, 30, 50, 60, 70, 80, 100) ~ 1, #G1
  origine_tous_g2bis %in% c(22, 33, 55, 66, 77, 88, 111) ~ 2, #G2
  TRUE ~ NA_real_
))

discri_origin_G1G2 <- wtd.table(indiv_immi$d_discri, indiv_immi$generation_4, weights = indiv_immi$poidsi) # weighted cross-table
discri_origin_G1G2_pct <- prop.table(discri_origin_G1G2,2)*100 # percentages: repartition of discrimination within the subgroups
discri_origin_G1G2_pct <- (round(discri_origin_G1G2_pct, 2)) 
print(discri_origin_G1G2_pct)

#G1
df_discri_origin_pct_G1 <- df_discri_origin_pct[1:5, ]
df_discri_origin_pct_G1 <- df_discri_origin_pct_G1 %>%  select(-Var2)

#G2
df_discri_origin_pct_G2 <- df_discri_origin_pct[6:10, ]
df_discri_origin_pct_G2 <- df_discri_origin_pct_G2 %>%  select(-Var2)

#G2_5
df_discri_origin_pct_G2_5 <- df_discri_origin_pct[11:15, ]
df_discri_origin_pct_G2_5 <- df_discri_origin_pct_G2_5 %>%  select(-Var2)

df_discri_origin_pct <- bind_cols(df_discri_origin_pct_G1, df_discri_origin_pct_G2, df_discri_origin_pct_G2_5)
write.csv2(as.data.frame(df_discri_origin_pct), "table_discrimination_G1G2.csv", row.names = TRUE) #export











# All sub-groups
discri_origin <- wtd.table(indiv_immi$d_discri, indiv_immi$origine_tous_g2bis, weights = indiv_immi$poidsi) # weighted cross-table
discri_origin_pct <- prop.table(discri_origin,2)*100 # percentages: repartition of discrimination within the subgroups
discri_origin_pct <- (round(discri_origin_pct, 2)) 
df_discri_origin_pct <- as.data.frame(discri_origin_pct)
print(df_discri_origin_pct)


df_pct_ori <- df_discri_origin_pct %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    names_prefix = "Freq_"
  )

labels_ori_en <- c(
  "1"   = "No direct migratory ancestry",
  "20"  = "From DOM-TOM",
  "22"  = "Descendant of DOM-TOM immigrants",
  "30"  = "Immigrant from Maghreb",
  "33"  = "Descendant of Maghreb immigrants",
  "50"  = "Immigrant from Sub-Saharan Africa",
  "55"  = "Descendant of Sub-Saharan African immigrants",
  "60"  = "Immigrant from Turkey/Middle East",
  "66"  = "Descendant of Turkey/Middle East immigrants",
  "70"  = "Immigrant from Asia",
  "77"  = "Descendant of Asian immigrants",
  "80"  = "Immigrant from Europe",
  "88"  = "Descendant of European immigrants",
  "100" = "Immigrant from other regions",
  "111" = "Descendant of immigrants from other regions"
)

rename_freq_columns <- function(colnames_vec, dict) {
  sapply(colnames_vec, function(col) {
    if (startsWith(col, "Freq_")) {
      code <- sub("Freq_", "", col)
      # Only replace if code exists in the dictionary
      if (!is.na(dict[code])) {
        return(dict[code])
      } else {
        return(col)
      }
    } else {
      return(col)
    }
  }, USE.NAMES = FALSE)
}

colnames(df_pct_ori) <- rename_freq_columns(colnames(df_pct_ori), labels_ori_en)

write.csv2(as.data.frame(df_pct_ori), "table_discrimination_ori.csv", row.names = TRUE) #export

indiv$f_diprec


###

indiv_immi <- indiv_immi %>% mutate(generation_4 = case_when(
  group1==5 ~ 1,
  group1==1|group1==2 ~ 2, # G1
  group2==31|group2==41~3, # G2
  group2==32|group2==33|group2==42|group2==43~4, # G2.5
  TRUE ~ NA_real_
))



table_a <- table(indiv_immi$d_pqdisc_a, indiv_immi$generation_4)
table_a <- table_a[2, , drop = FALSE]
colnames(table_a)[colnames(table_a) == '2'] <- 'n_G1'
colnames(table_a)[colnames(table_a) == '3'] <- 'n_G2'
colnames(table_a)[colnames(table_a) == '4'] <- 'n_G2_5'
print(table_a)

table_temp_a <- wtd.table(indiv_immi$d_pqdisc_a, indiv_immi$generation_4, 
                          weights = indiv_immi$poidsi)
  
table_pct_a <- round(prop.table(table_temp_a, 2) * 100, 2)
table_pct_a <- table_pct_a[2, , drop = FALSE]
colnames(table_pct_a)[colnames(table_pct_a) == '2'] <- 'pct_G1'
colnames(table_pct_a)[colnames(table_pct_a) == '3'] <- 'pct_G2'
colnames(table_pct_a)[colnames(table_pct_a) == '4'] <- 'pct_G2_5'
print(table_pct_a)


# Test du chi2 sur la table complète non pondérée
table_complete <- table(indiv_immi$d_pqdisc_a, indiv_immi$G1_G2)
test_chi2 <- chisq.test(table_complete)
p_value_a <- test_chi2$p.value

table_finale_a <- bind_cols(table_pct_a, table_a, p_value_a)
colnames(table_finale_a)[colnames(table_finale_a) == '...7'] <- 'p-value'
print(table_finale_a)

indiv$s_suic



########################################################################
# Creation of a variable that count the number of causes of discrimination according to the respondent
indiv_immi <- indiv_immi %>% mutate(Total_attributions = d_pqdisc_a + d_pqdisc_b + d_pqdisc_c + d_pqdisc_d + d_pqdisc_e +d_pqdisc_f + d_pqdisc_g + d_pqdisc_h + d_pqdisc_i + d_pqdisc_j + d_pqdisc_k + d_pqdisc_l + d_pqdisc_m)
indiv_immi <- indiv_immi %>% mutate (Nombre_attributions = case_when(
  Total_attributions == 1 ~ 1, # 1 reason
  Total_attributions > 1 ~ 2, # 2 reasons or more
  TRUE ~ 0, # No reason
))

# Cross table: number of attributions x gender/generation/origin

#Gender
table_gender <- wtd.table(indiv_immi$Nombre_attributions, indiv_immi$sexee, weights = indiv_immi$poidsi)
table_pct_gender <- round(prop.table(table_gender, 2) * 100, 2)
print(table_pct_gender)
table_gender_n <- table(indiv_immi$Nombre_attributions, indiv_immi$sexee)
print(table_gender_n)
df_pct_gender <- as.data.frame(table_pct_gender)
df_gender_n <- as.data.frame(table_gender_n)
df_pct_gender <- pivot_wider(df_pct_gender, names_from = Var1, values_from = Freq)
df_gender_n <- df_gender_n %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    names_prefix = "Freq_"
)
df_gender_n <- df_gender_n %>% select(-Var1)
df_gender_n <- as.data.frame(t(df_gender_n))

colnames(df_gender_n) <- c("n_0", "n_1", "n_seq2")
colnames(df_pct_gender) <- c("Gender", "0_reason", "1_reason", "seq2_reason")
df_gender_final <- bind_cols(df_pct_gender, df_gender_n)
print(df_gender_final)

# Test du chi² classique
df_complet <- table(indiv_immi$Nombre_attributions, indiv_immi$sexee)
test_chi2 <- chisq.test(df_complet)
p_value <- round(test_chi2$p.value, 4)
print(p_value)

# Système d’étoiles
stars <- if (p_value < 0.001) {
  "***"
} else if (p_value < 0.01) {
  "**"
} else if (p_value < 0.05) {
  "*"
} else if (p_value < 0.1) {
  "."
} else {
  ""
}


# Test du chi² Rao-Scott ajusté
# Définir le plan d'échantillonnage pondéré
dsgn <- svydesign(ids = ~1, data = indiv_immi, weights = ~poidsi)

# Test de Rao-Scott (chi² pondéré)
res <- svychisq(~ Nombre_attributions + sexee, design = dsgn)

# Récupérer la p-value ajustée
p_value_rao_scott_gender <- res$p.value
print(p_value_rao_scott_gender)

# Système d’étoiles
stars_RS <- if (p_value_rao_scott_gender < 0.001) {
  "***"
} else if (p_value_rao_scott_gender < 0.01) {
  "**"
} else if (p_value_rao_scott_gender < 0.05) {
  "*"
} else if (p_value_rao_scott_gender < 0.1) {
  "."
} else {
  ""
}

# Construction du tableau final pour la variable
df_gender_final <- bind_cols(df_gender_final,
                          significance = stars, significance_RS = stars_RS)
write.csv2(as.data.frame(df_gender_final), "df_discrimination_gender.csv", row.names = TRUE) #export




#Generation

table_gen <- wtd.table(indiv_immi$Nombre_attributions, indiv_immi$generation_4, weights = indiv_immi$poidsi)
table_pct_gen <- round(prop.table(table_gen, 2) * 100, 2)
print(table_pct_gen)
table_gen_n <- table(indiv_immi$Nombre_attributions, indiv_immi$generation_4)
print(table_gen_n)
df_pct_gen <- as.data.frame(table_pct_gen)
df_gen_n <- as.data.frame(table_gen_n)
df_pct_gen <- pivot_wider(df_pct_gen, names_from = Var1, values_from = Freq)
df_gen_n <- df_gen_n %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    names_prefix = "Freq_"
  )
df_gen_n <- df_gen_n %>% select(-Var1)
df_gen_n <- as.data.frame(t(df_gen_n))

colnames(df_gen_n) <- c("n_0", "n_1", "n_seq2")
colnames(df_pct_gen) <- c("Generation (G1/G2/G2_5)", "0_reason", "1_reason", "seq2_reason")
df_gen_final <- bind_cols(df_pct_gen, df_gen_n)
print(df_gen_final)

# Test du chi² classique
df_complet <- table(indiv_immi$Nombre_attributions, indiv_immi$generation_4)
test_chi2 <- chisq.test(df_complet)
p_value <- round(test_chi2$p.value, 4)
print(p_value)

# Système d’étoiles
stars <- if (p_value < 0.001) {
  "***"
} else if (p_value < 0.01) {
  "**"
} else if (p_value < 0.05) {
  "*"
} else if (p_value < 0.1) {
  "."
} else {
  ""
}


# Test du chi² Rao-Scott ajusté
# Définir le plan d'échantillonnage pondéré
dsgn <- svydesign(ids = ~1, data = indiv_immi, weights = ~poidsi)

# Test de Rao-Scott (chi² pondéré)
res <- svychisq(~ Nombre_attributions + generation_4, design = dsgn)

# Récupérer la p-value ajustée
p_value_rao_scott_gen <- res$p.value
print(p_value_rao_scott_gen)

# Système d’étoiles
stars_RS <- if (p_value_rao_scott_gen < 0.001) {
  "***"
} else if (p_value_rao_scott_gen < 0.01) {
  "**"
} else if (p_value_rao_scott_gen < 0.05) {
  "*"
} else if (p_value_rao_scott_gen < 0.1) {
  "."
} else {
  ""
}

# Construction du tableau final pour la variable
df_gen_final <- bind_cols(df_gen_final,
                             significance = stars, significance_RS = stars_RS)
write.csv2(as.data.frame(df_gen_final), "df_discrimination_gen.csv", row.names = TRUE) #export




#Origin
table_ori <- wtd.table(indiv_immi$Nombre_attributions, indiv_immi$origine_tous_g2bis, weights = indiv_immi$poidsi)
table_pct_ori <- round(prop.table(table_ori, 2) * 100, 2)
print(table_pct_ori)
table_ori_n <- table(indiv_immi$Nombre_attributions, indiv_immi$origine_tous_g2bis)
print(table_ori_n)
df_pct_ori <- as.data.frame(table_pct_ori)
df_ori_n <- as.data.frame(table_ori_n)
df_pct_ori <- pivot_wider(df_pct_ori, names_from = Var1, values_from = Freq)
df_ori_n <- df_ori_n %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    names_prefix = "Freq_"
  )
df_ori_n <- df_ori_n %>% select(-Var1)
df_ori_n <- as.data.frame(t(df_ori_n))

colnames(df_ori_n) <- c("n_0", "n_1", "n_seq2")
colnames(df_pct_ori) <- c("Origin", "0_reason", "1_reason", "seq2_reason")
df_ori_final <- bind_cols(df_pct_ori, df_ori_n)
print(df_ori_final)



# Test du chi² classique
df_complet <- table(indiv_immi$Nombre_attributions, indiv_immi$origine_tous_g2bis)
test_chi2 <- chisq.test(df_complet)
p_value <- round(test_chi2$p.value, 4)
print(p_value)

# Système d’étoiles
stars <- if (p_value < 0.001) {
  "***"
} else if (p_value < 0.01) {
  "**"
} else if (p_value < 0.05) {
  "*"
} else if (p_value < 0.1) {
  "."
} else {
  ""
}


# Test du chi² Rao-Scott ajusté
# Définir le plan d'échantillonnage pondéré
dsgn <- svydesign(ids = ~1, data = indiv_immi, weights = ~poidsi)

# Test de Rao-Scott (chi² pondéré)
res <- svychisq(~ Nombre_attributions + origine_tous_g2bis, design = dsgn)

# Récupérer la p-value ajustée
p_value_rao_scott_ori <- res$p.value
print(p_value_rao_scott_ori)

# Système d’étoiles
stars_RS <- if (p_value_rao_scott_ori < 0.001) {
  "***"
} else if (p_value_rao_scott_ori < 0.01) {
  "**"
} else if (p_value_rao_scott_ori < 0.05) {
  "*"
} else if (p_value_rao_scott_ori < 0.1) {
  "."
} else {
  ""
}

# Construction du tableau final pour la variable
df_ori_final <- bind_cols(df_ori_final,
                          significance = stars, significance_RS = stars_RS)
write.csv2(as.data.frame(df_ori_final), "df_discrimination_ori.csv", row.names = TRUE) #export


############# 3 ########################################################################
# Attribution ethnoraciale
indiv_immi <- indiv_immi %>% mutate(attri_ethnorac = case_when(
  d_pqdisc_d==1 | d_pqdisc_e ==  1 ~ 1, 
  TRUE ~ 0
))

# Autre attribution ethnoraciale (quartier/accent)
indiv_immi <- indiv_immi %>% mutate(attri_ethnorac_other = case_when(
  d_pqdisc_f==1 | d_pqdisc_g ==  1 ~ 1, 
  TRUE ~ 0
))

# Attribution religion
indiv_immi <- indiv_immi %>% mutate(attri_relig = case_when(
  d_pqdisc_j==1 ~ 1, 
  TRUE ~ 0
))

# Attribution genre
indiv_immi <- indiv_immi %>% mutate(attri_gender = case_when(
  d_pqdisc_b == 1 ~ 1, 
  TRUE ~ 0
))

# Autre
indiv_immi <- indiv_immi %>% mutate(attri_other = case_when(
  d_pqdisc_a == 1|d_pqdisc_c == 1|d_pqdisc_h == 1|d_pqdisc_i == 1|d_pqdisc_k == 1|d_pqdisc_l == 1|d_pqdisc_m == 1 ~ 1, 
  TRUE ~ 0
))

###################################################################################
# Recode f_dip en 3 niveaux
indiv_immi <- indiv_immi %>%
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

###################################################################################
# PCS 2 positions au lieu de 4 (extraction des deux premiers)
indiv_immi <- indiv_immi %>%
  mutate(
    cs_ego = substr(pcs_act, 1, 2),                       
    cs_ego = as.numeric(cs_ego),                          
    cs_ego = ifelse(cs_ego %in% c(0, 20, 30, 40, 50, 60), NA, cs_ego)  
  )
###################################################################################
# Recodage final
indiv_immi <- indiv_immi %>%
  mutate(
    classesoc = case_when(
      cs_ego %in% c(23, 31, 37, 38) ~ 11,  # Clsup-Eco
      cs_ego %in% c(33, 34, 35) ~ 12,      # Clsup-Cul
      cs_ego %in% c(12, 13, 21, 22, 47, 48) |
        (cs_ego %in% c(53, 55) & csnq_ego == 50) ~ 21,  # Clmoy-Eco
      cs_ego %in% c(42, 43, 44, 45, 46) |
        (cs_ego %in% c(52, 54, 56) & csnq_ego == 50) ~ 22,  # Clmoy-Cul
      csnq_ego %in% c(51, 60, 61) | cs_ego == 11 ~ 31,      # Clpop
      TRUE ~ 1  # Par défaut : Inactifs
    ),
    classesoc = factor(classesoc,
                       levels = c(11, 12, 21, 22, 31, 1),
                       labels = c("Clsup-Eco", "Clsup-Cul",
                                  "Clmoy-Eco", "Clmoy-Cul",
                                  "Clpop", "Inactifs"))
  )
###################################################################################
variables <- c("attri_ethnorac", "attri_ethnorac_other", "attri_relig", "attri_gender", "attri_other")

resultats <- list()

for (var in variables) {
  var_vals <- indiv_immi[[var]]
  sexee_vals <- indiv_immi$sexee
  poids_vals <- indiv_immi$poidsi
  
  # Table non pondérée : effectifs par sexee x var
  tab_n <- table(sexee_vals, var_vals)
  
  # Table pondérée : wtd.table(var, sexee)
  tab_pond <- wtd.table(var_vals, sexee_vals, weights = poids_vals)
  
  # Pourcentages pondérés par sexee
  tab_pct <- round(prop.table(tab_pond, 2) * 100, 2)
  
  # Vérifier modalités (0 et 1 attendus)
  modalities <- sort(unique(var_vals))
  if (!all(modalities %in% c(0,1))) {
    stop(paste("La variable", var, "n'a pas des modalités uniquement 0 et 1"))
  }
  
  # Construction du dataframe avec résultats % et n
  df <- data.frame(
    sexee = colnames(tab_n),   # sexee en colonnes dans tab_n
    pct_1 = tab_pct["1", ],    # % pondérés pour valeur 1
    n_1 = tab_n[, "1"],        # effectifs non pondérés pour 1
    pct_0 = tab_pct["0", ],    # % pondérés pour valeur 0
    n_0 = tab_n[, "0"]         # effectifs non pondérés pour 0
  )
  colnames(df)[-1] <- paste0(var, c("_pct_1", "_n_1", "_pct_0", "_n_0"))
  
  # Test du chi2 classique (non pondéré)
  tab_chi2 <- table(var_vals, sexee_vals)
  test_chi2 <- suppressWarnings(chisq.test(tab_chi2))
  p_value <- test_chi2$p.value
  
  # Système d'étoiles
  stars <- if (p_value < 0.001) {
    "***"
  } else if (p_value < 0.01) {
    "**"
  } else if (p_value < 0.05) {
    "*"
  } else if (p_value < 0.1) {
    "."
  } else {
    ""
  }
  
  # Ajouter colonne variable_significance avec les étoiles, répétée sur toutes les lignes
  df$variable_significance <- stars
  
  resultats[[var]] <- df
}

# Fusionner tous les tableaux par la variable sexee (0/1)
tableau_final <- Reduce(function(x, y) merge(x, y, by = "sexee"), resultats)

print(tableau_final)
df <- as.data.frame(tableau_final)

#########################################################################################################
# On crée une table avec les terciles des revenus
library(Hmisc)

# Calcul tercils
terciles <- wtd.quantile(
  x = indiv_immi$a_montan,
  weights = indiv_immi$poidsi,
  probs = c(1/3, 2/3),
  na.rm = TRUE
)

#  Création variable
indiv_immi$tercile_revenu <- cut(
  indiv_immi$a_montan,
  breaks = c(-Inf, terciles, Inf),
  labels = c("Bas", "Moyen", "Élevé"),
  include.lowest = TRUE,
  right = TRUE
)

test <- indiv_immi %>% select(ident, a_montan, tercile_revenu)

#########################################################################################################
#On refait les tercils

# 1. Recodage de a_estima en valeurs médianes pondérées (a_estima_rec)
indiv_immi <- indiv_immi %>%
  mutate(
    a_estima_rec = case_when(
      a_estima == 1  ~ 300,
      a_estima == 2  ~ 500,
      a_estima == 3  ~ 700,
      a_estima == 4  ~ 890,
      a_estima == 5  ~ 1050,
      a_estima == 6  ~ 1300,
      a_estima == 7  ~ 1600,
      a_estima == 8  ~ 1800,
      a_estima == 9  ~ 2100,
      a_estima == 10 ~ 2600,
      a_estima == 11 ~ 3300,
      a_estima == 12 ~ 4500,
      a_estima == 13 ~ 6700,
      a_estima == 14 ~ 11000,
      a_estima %in% c(98, 99) ~ 2700,
      TRUE ~ NA_real_
    )
  )

# 2. Création de a_montan_rec à partir de a_montan
indiv_immi <- indiv_immi %>%
  mutate(a_montan_rec = a_montan)

# 3. Remplacement de a_montan_rec par a_estima_rec si a_montan_drap == -1
indiv_immi <- indiv_immi %>%
  mutate(a_montan_rec = ifelse(a_montan_drap == -1, a_estima_rec, a_montan_rec))

# 4. Suppression des valeurs extrêmes (<495 ou >11000)
indiv_immi <- indiv_immi %>%
  mutate(a_montan_rec = ifelse(a_montan_rec <= 495 | a_montan_rec > 11000, NA, a_montan_rec))

# Calcul des seuils pondérés (33.3% et 66.6%)
terciles <- wtd.quantile(indiv_immi$a_montan_rec, weights = indiv_immi$poidsi, probs = c(1/3, 2/3), na.rm = TRUE)

# Création de la variable income3 selon les seuils pondérés
indiv_immi <- indiv_immi %>%
  mutate(
    income3 = case_when(
      is.na(a_montan_rec) ~ NA_integer_,
      a_montan_rec <= terciles[1] ~ 1,
      a_montan_rec <= terciles[2] ~ 2,
      TRUE ~ 3
    )
  )



table_subset <- indiv_immi %>%
  select(ident, a_estima, a_estima_rec, a_montan, a_montan_drap, a_montan_rec, poidsi, income3)

