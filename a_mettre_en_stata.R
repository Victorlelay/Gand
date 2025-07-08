library(dplyr)
library(tidyr)
library(survey)
library(questionr)

##################################################################################################
#2 attribution des discriminatons
##################################################################################################



##################################################################################################
# a. fréquences pondérées par génération et n non-pondéré
##################################################################################################
# recoding of the variable origine_tous_g2
indiv <- indiv %>% mutate(origine_tous_g2bis = case_when(
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
# Filtrer pour ne garder que le G1 et les G2 (retirer popu sans ascendance migratoire)
indiv_immi<- indiv %>%  filter(origine_tous_g2bis != 1) 

# On renomme les discriminations
# Ou en une fois avec dplyr
library(dplyr)

indiv_immi <- indiv_immi %>% 
  rename(
    Discrimination_age = d_pqdisc_a,
    Discrimination_genre = d_pqdisc_b,
    Discrimination_handi = d_pqdisc_c,
    Discrimination_peau = d_pqdisc_d,
    Discrimination_orig = d_pqdisc_e,
    Discrimination_quartier = d_pqdisc_f,
    Discrimination_accent = d_pqdisc_g,
    Discrimination_fami = d_pqdisc_h,
    Discrimination_orientsex = d_pqdisc_i,
    Discrimination_relig = d_pqdisc_j,
    Discrimination_habil = d_pqdisc_k,
    Discrimination_poids = d_pqdisc_l,
    Discrimination_autre = d_pqdisc_m)

# Création variable (avec G2.5)
indiv_immi <- indiv_immi %>% mutate(generation_4 = case_when(
  group1==5 ~ 1,
  group1==1|group1==2 ~ 2, # G1
  group2==31|group2==41~3, # G2
  group2==32|group2==33|group2==42|group2==43~4, # G2.5
  TRUE ~ NA_real_
))

# Liste des variables à traiter (a à m)
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

# On crée un tableau qui va acceuillir notre résultat final
tableau_final <- data.frame()

# Boucle principale: 
for (i in seq_along(variables_discri)) {
  var_name <- variables_discri[i]
  cat("Traitement de", var_name, "\n")
  
  if (var_name %in% names(indiv_immi)) {
    tryCatch({
      # Table avec les pourcentages pondérée
      table_a <- table(indiv_immi[[var_name]], indiv_immi$generation_4) #en fonction de la généraition donc
      table_a <- table_a[2, , drop = FALSE]
      colnames(table_a)[colnames(table_a) == '2'] <- 'n_G1' # on renomme les colonnes
      colnames(table_a)[colnames(table_a) == '3'] <- 'n_G2'
      colnames(table_a)[colnames(table_a) == '4'] <- 'n_G2_5'
      
      # Table n non-pondéré
      table_temp_a <- wtd.table(indiv_immi[[var_name]], indiv_immi$generation_4, weights = indiv_immi$poidsi)
      table_pct_a <- round(prop.table(table_temp_a, 2) * 100, 2)
      table_pct_a <- table_pct_a[2, , drop = FALSE]
      colnames(table_pct_a)[colnames(table_pct_a) == '2'] <- 'pct_G1'
      colnames(table_pct_a)[colnames(table_pct_a) == '3'] <- 'pct_G2'
      colnames(table_pct_a)[colnames(table_pct_a) == '4'] <- 'pct_G2_5'
      
      # Test du chi²
      table_complete <- table(indiv_immi[[var_name]], indiv_immi$generation_4)
      test_chi2 <- chisq.test(table_complete)
      p_value <- round(test_chi2$p.value, 4)
      
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
      
      # Construction du tableau final pour la variable
      table_finale_a <- bind_cols(table_pct_a, table_a, 
                                  p_value = p_value,
                                  significance = stars)
      
      # Ajout du label lisible
      table_finale_a <- table_finale_a %>%
        mutate(variable = nom_lisible[var_name], .before = 1)
      
      # Agrégation dans le tableau final
      tableau_final <- bind_rows(tableau_final, table_finale_a)
      
    }, error = function(e) {
      cat("Erreur pour", var_name, ":", e$message, "\n")
    })
  } else {
    cat("Variable", var_name, "non trouvée\n")
  }
}

# Affichage final
cat("=== TABLEAU FINAL COMPLET ===\n")
print(tableau_final)
df_final <- as.data.frame(tableau_final)

write.csv2(as.data.frame(df_final), "table_discrimination_types.csv", row.names = TRUE) #export



##################################################################################################
# b. Nombre attribution + tableau associé
##################################################################################################
# Creation d'une variable qui compte le nombre d'éléments ayant engendrés une discrimiation selon le répondant
indiv_immi <- indiv_immi %>% mutate(Total_attributions = d_pqdisc_a + d_pqdisc_b + d_pqdisc_c + d_pqdisc_d + d_pqdisc_e +d_pqdisc_f + d_pqdisc_g + d_pqdisc_h + d_pqdisc_i + d_pqdisc_j + d_pqdisc_k + d_pqdisc_l + d_pqdisc_m)
indiv_immi <- indiv_immi %>% mutate (Nombre_attributions = case_when(
  Total_attributions == 1 ~ 1, # 1 raison
  Total_attributions > 1 ~ 2, # 2 raisons ou +
  TRUE ~ 0, # pas de raison
))

# Table croisée

# Variables à analyser
var_list <- list(
  list(var = "sexee", label = "Gender"),
  list(var = "generation_4", label = "Generation (G1/G2/G2_5)"),
  list(var = "origine_tous_g2bis", label = "Origin")
)

# Plan de sondage pondéré
dsgn <- svydesign(ids = ~1, data = indiv_immi, weights = ~poidsi)

# Liste pour stocker les résultats
results_list <- list()

# Boucle sur les variables
for (v in var_list) {
  var_name <- v$var
  var_label <- v$label
  
  # Table pondérée
  table_w <- wtd.table(indiv_immi[[var_name]], indiv_immi$Nombre_attributions, weights = indiv_immi$poidsi)
  table_pct <- round(prop.table(table_w, 1) * 100, 2)
  
  # Table n-non pondéré
  table_n <- table(indiv_immi[[var_name]], indiv_immi$Nombre_attributions)
  
  # Mise en forme des %
  df_pct <- as.data.frame(table_pct) %>%
    pivot_wider(names_from = Var2, values_from = Freq)
  
  # Mise en forme des effectifs
  df_n <- as.data.frame(table_n) %>%
    pivot_wider(names_from = Var1, values_from = Freq, names_prefix = "Freq_") %>%
    select(-Var2) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  
  # Harmonisation des noms de colonnes
  colnames(df_n) <- c("n_0", "n_1", "n_seq2")
  colnames(df_pct) <- c(var_label, "0_reason", "1_reason", "seq2_reason")
  
  # Fusion des % et n
  df_final <- bind_cols(df_pct, df_n)
  
  # Test chi² classique
  chi2 <- chisq.test(table_n)
  p_value <- round(chi2$p.value, 4)
  
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
  
  # Test Rao-Scott
  form <- as.formula(paste("~", var_name, "+ Nombre_attributions"))
  test_rs <- svychisq(form, design = dsgn)
  p_rs <- round(test_rs$p.value, 4)
  
  stars_RS <- if (p_rs < 0.001) {
    "***"
  } else if (p_rs < 0.01) {
    "**"
  } else if (p_rs < 0.05) {
    "*"
  } else if (p_rs < 0.1) {
    "."
  } else {
    ""
  }
  
  # Ajouter les étoiles
  df_final <- bind_cols(df_final,
                        significance = stars,
                        significance_RS = stars_RS)
  
  # Stockage
  results_list[[var_label]] <- df_final
  
  # Affichage
  cat("\nRésultats pour :", var_label, "\n")
  print(df_final)
}



##################################################################################################
#3 Racisme
##################################################################################################


# Attribution ethnoraciale: création des variables
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

# PCS 2 positions au lieu de 4 (extraction des deux premiers)
indiv_immi <- indiv_immi %>%
  mutate(
    cs_ego = substr(pcs_act, 1, 2),                       
    cs_ego = as.numeric(cs_ego),                          
    cs_ego = ifelse(cs_ego %in% c(0, 20, 30, 40, 50, 60), NA, cs_ego)  
  )


# Recodage final
indiv <- indiv %>%
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


# Création de la table
individus <- c("sexee", "generation_4", "origine_tous_g2bis", "dip_rec", "classesoc")
variables <- c("attri_ethnorac", "attri_ethnorac_other", "attri_relig", "attri_gender", "attri_other")

resultats_separes <- list()

resultats_separes <- list()

for (indiv_var in individus) {
  for (var in variables) {
    var_vals <- indiv_immi[[var]]
    indiv_vals <- indiv_immi[[indiv_var]]
    poids_vals <- indiv_immi$poidsi
    
    # Nettoyage : suppression des NA
    valid_idx <- complete.cases(var_vals, indiv_vals, poids_vals)
    var_vals <- var_vals[valid_idx]
    indiv_vals <- indiv_vals[valid_idx]
    poids_vals <- poids_vals[valid_idx]
    
    # Table non pondérée
    tab_n <- table(indiv_vals, var_vals)
    
    # Table pondérée
    tab_pond <- wtd.table(var_vals, indiv_vals, weights = poids_vals)
    tab_pct <- round(prop.table(tab_pond, 2) * 100, 2)
    
    # Modalités du groupe (indiv_vals)
    group <- rownames(tab_n)
    
    # Extraction uniquement des colonnes où var = 1
    pct_1 <- tab_pct["1", group]
    n_1 <- tab_n[, "1"]
    
    # Construction du dataframe
    df <- data.frame(group = group)
    df[[paste0(var, "_pct_1")]] <- as.numeric(pct_1)
    df[[paste0(var, "_n_1")]] <- as.numeric(n_1)
    
    # Test du chi²
    tab_chi2 <- table(var_vals, indiv_vals)
    test_chi2 <- suppressWarnings(chisq.test(tab_chi2))
    p_value <- test_chi2$p.value
    
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
    
    df$variable_significance <- stars
    df$individu <- indiv_var
    
    # Stockage dans la liste
    resultats_separes[[paste0(indiv_var, "_", var)]] <- df
  }
}


# Création table finale
groupes_tables <- split(resultats_separes, sapply(names(resultats_separes), function(x) strsplit(x, "_")[[1]][1]))

resultats_groupes <- list()

for (indiv in names(groupes_tables)) {
  tables_list <- groupes_tables[[indiv]]
  
  base <- tables_list[[1]][, "group", drop = FALSE]
  reste <- lapply(tables_list, function(df) df[, !colnames(df) %in% c("group", "individu")])
  
  table_fusionnee <- bind_cols(base, do.call(bind_cols, reste))
  table_fusionnee$individu <- indiv
  
  resultats_groupes[[indiv]] <- table_fusionnee
}

# Empilement final
resultat_final <- bind_rows(resultats_groupes)



##################################################################################################
#4 Combinaisons d'attributions inter- / intra-catégories
##################################################################################################
#plusieurs raison ethno-raciales
indiv_immi <- indiv_immi %>% mutate(plus_ER = case_when(
  d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=2 ~1,
  TRUE ~0
))

# ER + religion
indiv_immi <- indiv_immi %>% mutate(ER_relig = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1) & d_pqdisc_j==1 ~1,
  TRUE ~0
))

# ER + gen
indiv_immi <- indiv_immi %>% mutate(ER_genre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1) & d_pqdisc_b==1 ~1,
  TRUE ~0
))

# ER + autres
indiv_immi <- indiv_immi %>% mutate(ER_autre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1) & (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k+ d_pqdisc_l + d_pqdisc_m >=1) ~1,
  TRUE ~0
))

# Religion + Genre
indiv_immi <- indiv_immi %>% mutate(relig_genre = case_when(
  d_pqdisc_b + d_pqdisc_j == 2 ~1,
  TRUE ~0
))

# Religion + autre
indiv_immi <- indiv_immi %>% mutate(relig_autre = case_when(
  d_pqdisc_j==1 & (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k +d_pqdisc_l + d_pqdisc_m >=1) ~1,
  TRUE ~0
))

# ER ou relig + genre
indiv_immi <- indiv_immi %>% mutate(ER_ou_relig_genre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1|d_pqdisc_j==1) & d_pqdisc_b==1 ~1,
  TRUE ~0
))

# ER ou relig + autre
indiv_immi <- indiv_immi %>% mutate(ER_ou_relig_autre = case_when(
  (d_pqdisc_d+d_pqdisc_e+d_pqdisc_f+d_pqdisc_g>=1|d_pqdisc_j==1) & (d_pqdisc_a + d_pqdisc_c + d_pqdisc_h + d_pqdisc_i + d_pqdisc_k +d_pqdisc_l + d_pqdisc_m >=1) ~1,
  TRUE ~0
))

# Création de la table
individus <- c("sexee", "generation_4", "origine_tous_g2bis", "dip_rec", "classesoc")
variables <- c("plus_ER","ER_relig", "ER_genre", "ER_autre", "relig_genre", "relig_autre", "ER_ou_relig_genre", "ER_ou_relig_autre")

resultats_separes <- list()

resultats_separes <- list()

for (indiv_var in individus) {
  for (var in variables) {
    var_vals <- indiv_immi[[var]]
    indiv_vals <- indiv_immi[[indiv_var]]
    poids_vals <- indiv_immi$poidsi
    
    # Nettoyage : suppression des NA
    valid_idx <- complete.cases(var_vals, indiv_vals, poids_vals)
    var_vals <- var_vals[valid_idx]
    indiv_vals <- indiv_vals[valid_idx]
    poids_vals <- poids_vals[valid_idx]
    
    # Table non pondérée
    tab_n <- table(indiv_vals, var_vals)
    
    # Table pondérée
    tab_pond <- wtd.table(var_vals, indiv_vals, weights = poids_vals)
    tab_pct <- round(prop.table(tab_pond, 2) * 100, 2)
    
    # Modalités du groupe (indiv_vals)
    group <- rownames(tab_n)
    
    # Extraction uniquement des colonnes où var = 1
    pct_1 <- tab_pct["1", group]
    n_1 <- tab_n[, "1"]
    
    # Construction du dataframe
    df <- data.frame(group = group)
    df[[paste0(var, "_pct_1")]] <- as.numeric(pct_1)
    df[[paste0(var, "_n_1")]] <- as.numeric(n_1)
    
    # Test du chi²
    tab_chi2 <- table(var_vals, indiv_vals)
    test_chi2 <- suppressWarnings(chisq.test(tab_chi2))
    p_value <- test_chi2$p.value
    
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
    
    df$variable_significance <- stars
    df$individu <- indiv_var
    
    # Stockage dans la liste
    resultats_separes[[paste0(indiv_var, "_", var)]] <- df
  }
}


# Création table finale
groupes_tables <- split(resultats_separes, sapply(names(resultats_separes), function(x) strsplit(x, "_")[[1]][1]))

resultats_groupes <- list()

for (indiv in names(groupes_tables)) {
  tables_list <- groupes_tables[[indiv]]
  
  base <- tables_list[[1]][, "group", drop = FALSE]
  reste <- lapply(tables_list, function(df) df[, !colnames(df) %in% c("group", "individu")])
  
  table_fusionnee <- bind_cols(base, do.call(bind_cols, reste))
  table_fusionnee$individu <- indiv
  
  resultats_groupes[[indiv]] <- table_fusionnee
}

# Empilement final
resultat_final <- bind_rows(resultats_groupes)
write.csv2(resultat_final, "df_discrimination_croisée.csv", row.names = TRUE) #export




# Corrélation
# Exemple de dataframe
vars <- indiv_immi[, c("d_pqdisc_d", "d_pqdisc_e", "d_pqdisc_f", "d_pqdisc_g")]  # sélectionne les variables voulues
poids <- indiv_immi$poidsi              # vecteur de poids

# Fonction pour corrélation pondérée matricielle
wtd_cor_matrix <- function(data, weights) {
  n <- ncol(data)
  mat <- matrix(NA, n, n)
  colnames(mat) <- rownames(mat) <- colnames(data)
  
  for (i in 1:n) {
    for (j in i:n) {
      pair_df <- data.frame(x = data[[i]], y = data[[j]])
      res <- weights::wtd.cor(pair_df, weight = weights)
      cor_val <- res$correlation[1, 2]  # valeur de corrélation entre x et y
      mat[i, j] <- mat[j, i] <- cor_val
    }
  }
  return(mat)
}

# Calcul
mat_corr_pond <- wtd_cor_matrix(vars, poids)
print(round(mat_corr_pond, 3))

cor_matrix_simple <- function(data) {
  n <- ncol(data)
  mat <- matrix(NA, n, n)
  colnames(mat) <- rownames(mat) <- colnames(data)
  
  for (i in 1:n) {
    for (j in i:n) {
      x <- data[[i]]
      y <- data[[j]]
      
      valid <- complete.cases(x, y)
      cor_val <- cor(x[valid], y[valid], use = "complete.obs")
      
      mat[i, j] <- mat[j, i] <- cor_val
    }
  }
  return(mat)
}
vars <- indiv_immi[, c("d_pqdisc_d", 
                       "d_pqdisc_e", "d_pqdisc_f", "d_pqdisc_g")]

mat_corr_non_pond <- cor_matrix_simple(vars)

round(mat_corr_non_pond, 2)

correl <- indiv_immi %>% select(ident, d_pqdisc_a, d_pqdisc_b, d_pqdisc_c ,d_pqdisc_d, d_pqdisc_e, d_pqdisc_f, d_pqdisc_g, d_pqdisc_h, d_pqdisc_i, d_pqdisc_j, d_pqdisc_k, d_pqdisc_l, d_pqdisc_m)
library(corrplot)

corrplot(cor(correl))


#Créer une variable : 0 (pas de discri) ; 
#1- seulement origine/natio ; 2- seulement couleur peau ; 
#3- seulement religion ; 4- origines + couleur peau ; 
#5- origines+ religion ; 6-couleur peau + religion, 
#7-anyethnoracial (origine/accent/quartier/couleur peau, religion) + 
#gender 8-autre 
# La nouvelle variable
indiv_immi <- indiv_immi %>% mutate(motif = case_when(
  Discrimination_age + Discrimination_genre + Discrimination_peau + Discrimination_orig + Discrimination_quartier + Discrimination_accent + Discrimination_fami + Discrimination_orientsex + Discrimination_relig + Discrimination_habil + Discrimination_autre + Discrimination_handi + Discrimination_poids == 0 ~ 0,
  Discrimination_orig == 1 & Discrimination_peau + Discrimination_genre + Discrimination_relig  == 0 ~ 1,
  Discrimination_peau == 1 & Discrimination_genre + Discrimination_quartier + Discrimination_accent + Discrimination_relig + Discrimination_orig == 0 ~ 2,
  Discrimination_relig == 1 & Discrimination_genre + Discrimination_quartier + Discrimination_accent + Discrimination_peau + Discrimination_orig == 0 ~ 3,
  Discrimination_orig+Discrimination_peau == 2 & Discrimination_relig + Discrimination_genre == 0 ~ 4,
  Discrimination_orig+Discrimination_relig == 2 & Discrimination_genre + Discrimination_peau == 0 ~ 5,
  Discrimination_peau + Discrimination_relig == 2 & Discrimination_genre + Discrimination_quartier + Discrimination_accent  + Discrimination_orig == 0 ~ 6,
  (Discrimination_peau==1|Discrimination_orig==1|Discrimination_quartier==1|Discrimination_accent==1|Discrimination_relig==1) & Discrimination_genre ==1  ~ 7,
  TRUE ~ 8
))


# Être religieux
indiv_immi <- indiv_immi %>%
  mutate(
    religious = r_relsoi == 1
  )

# Religion (version 1)
indiv_immi <- indiv_immi %>%
  mutate(
    religion = case_when(
      relego1 >= 10 & relego1 <= 19 ~ 1,   # Christian
      relego1 %in% c(20, 21) ~ 2,          # Muslim
      relego1 >= 30 ~ 3,                   # Other
      r_relsoi == 2 | relego1 == 1 ~ 4,    # No current religion
      TRUE ~ NA_real_
    )
  )

# Ajouter des labels si besoin (optionnel)
indiv_immi$religion <- factor(indiv_immi$religion,
                              levels = c(1, 2, 3, 4),
                              labels = c("Christian", "Muslim", "Other", "No current religion"))

# Religion avec refus / NSP mis dans la catégorie 4 (version alternative)
indiv_immi <- indiv_immi %>%
  mutate(
    religion_nomis = case_when(
      relego1 >= 10 & relego1 <= 19 ~ 1,                          # Christian
      relego1 %in% c(20, 21) ~ 2,                                 # Muslim
      relego1 >= 30 ~ 3,                                         # Other
      r_relsoi != 1 | relego1 == 1 ~ 4,                          # No current religion or no religion stated
      TRUE ~ NA_real_
    )
  )

# Labels également ici
indiv_immi$religion_nomis <- factor(indiv_immi$religion_nomis,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Christian", "Muslim", "Other", "No current religion or no religion stated"))



# Stats descriptives
# Création de la table
individus <- c("sexee", "generation_4", "origine_tous_g2bis", "dip_rec", "religion_nomis")
variables <- c("motif")

resultats_separes <- list()

resultats_separes <- list()

for (indiv_var in individus) {
  for (var in variables) {
    var_vals <- indiv_immi[[var]]
    indiv_vals <- indiv_immi[[indiv_var]]
    poids_vals <- indiv_immi$poidsi
    
    # Nettoyage : suppression des NA
    valid_idx <- complete.cases(var_vals, indiv_vals, poids_vals)
    var_vals <- var_vals[valid_idx]
    indiv_vals <- indiv_vals[valid_idx]
    poids_vals <- poids_vals[valid_idx]
    
    # Table non pondérée
    tab_n <- table(indiv_vals, var_vals)
    
    # Table pondérée
    tab_pond <- wtd.table(var_vals, indiv_vals, weights = poids_vals)
    tab_pct <- round(prop.table(tab_pond, 2) * 100, 2)
    
    # Modalités du groupe (indiv_vals)
    group <- rownames(tab_n)
    
    # Extraction uniquement des colonnes où var = 1
    pct_1 <- tab_pct["1", group]
    n_1 <- tab_n[, "1"]
    
    # Construction du dataframe
    df <- data.frame(group = group)
    df[[paste0(var, "_pct_1")]] <- as.numeric(pct_1)
    df[[paste0(var, "_n_1")]] <- as.numeric(n_1)
    
    # Test du chi²
    tab_chi2 <- table(var_vals, indiv_vals)
    test_chi2 <- suppressWarnings(chisq.test(tab_chi2))
    p_value <- test_chi2$p.value
    
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
    
    df$variable_significance <- stars
    df$individu <- indiv_var
    
    # Stockage dans la liste
    resultats_separes[[paste0(indiv_var, "_", var)]] <- df
  }
}


# Création table finale
groupes_tables <- split(resultats_separes, sapply(names(resultats_separes), function(x) strsplit(x, "_")[[1]][1]))

resultats_groupes <- list()

for (indiv in names(groupes_tables)) {
  tables_list <- groupes_tables[[indiv]]
  
  base <- tables_list[[1]][, "group", drop = FALSE]
  reste <- lapply(tables_list, function(df) df[, !colnames(df) %in% c("group", "individu")])
  
  table_fusionnee <- bind_cols(base, do.call(bind_cols, reste))
  table_fusionnee$individu <- indiv
  
  resultats_groupes[[indiv]] <- table_fusionnee
}

# Empilement final
resultat_final <- bind_rows(resultats_groupes)
write.csv2(resultat_final, "df_discrimination_croisée.csv", row.names = TRUE) #export
