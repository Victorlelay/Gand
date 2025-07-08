library(questionr)
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

# Export
write.csv2(resultat_final, "df_attributions.csv", row.names = FALSE)



#######################################
variables <- c("d_racism", "d_racpot")

resultats_separes <- list()

for (indiv_var in individus) {
  for (var in variables) {
    
    var_vals <- indiv_immi[[var]]
    indiv_vals <- indiv_immi[[indiv_var]]
    poids_vals <- indiv_immi$poidsi
    
    # Nettoyage : suppression NA
    valid_idx <- complete.cases(var_vals, indiv_vals, poids_vals)
    var_vals <- var_vals[valid_idx]
    indiv_vals <- indiv_vals[valid_idx]
    poids_vals <- poids_vals[valid_idx]
    
    # Effectifs non pondérés
    tab_n <- table(indiv_vals, var_vals)
    
    # Table pondérée
    tab_pond <- wtd.table(var_vals, indiv_vals, weights = poids_vals)
    tab_pct <- round(prop.table(tab_pond, 2) * 100, 2)
    
    # Construction du tableau dynamique
    group <- rownames(tab_n)
    df <- data.frame(group = group)
    
    for (val in sort(unique(var_vals))) {
      val_chr <- as.character(val)
      pct_col <- if (val_chr %in% rownames(tab_pct)) tab_pct[val_chr, group] else rep(NA, length(group))
      n_col <- if (val_chr %in% colnames(tab_n)) tab_n[, val_chr] else rep(NA, length(group))
      
      df[[paste0(var, "_pct_", val_chr)]] <- as.numeric(pct_col)
      df[[paste0(var, "_n_", val_chr)]] <- as.numeric(n_col)
    }
    
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
    
    resultats_separes[[paste0(indiv_var, "_", var)]] <- df
  }
}
# création table finale

# Étape 1 : Grouper les tableaux par variable "indiv_var"
groupes_tables <- split(resultats_separes, sapply(names(resultats_separes), function(x) strsplit(x, "_")[[1]][1]))

# Étape 2 : Coller côte à côte (bind_cols) les tableaux pour chaque indiv_var
resultats_groupes <- list()

for (indiv in names(groupes_tables)) {
  tables_list <- groupes_tables[[indiv]]
  
  # Extraire les dataframes et retirer les colonnes dupliquées ('group', 'individu')
  base <- tables_list[[1]][, c("group")]
  reste <- lapply(tables_list, function(df) df[, !colnames(df) %in% c("group", "individu")])
  
  # Coller les colonnes ensemble
  table_fusionnee <- bind_cols(base, do.call(bind_cols, reste))
  
  # Ajouter le nom de la variable individuelle
  table_fusionnee$individu <- indiv
  
  resultats_groupes[[indiv]] <- table_fusionnee
}

# Étape 3 : Empiler tous les blocs (sexee, generation_4, etc.)
resultat_final <- bind_rows(resultats_groupes)

# Afficher un aperçu
print(head(resultat_final, 10))
df <- as.data.frame(resultat_final)

write.csv2(resultat_final, "df_racisme.csv", row.names = FALSE)
