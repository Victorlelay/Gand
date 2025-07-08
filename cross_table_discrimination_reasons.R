library(questionr)
library(dplyr)

# Liste des variables à traiter
variables_discri <- paste0("d_pqdisc_", letters[1:13])  # a à m

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

# Associer noms techniques aux labels
nom_lisible <- setNames(labels_discri, variables_discri)

# Initialiser le tableau final
tableau_final <- data.frame()

# Boucle principale
for (i in seq_along(variables_discri)) {
  var_name <- variables_discri[i]
  cat("Traitement de", var_name, "\n")
  
  if (var_name %in% names(indiv_immi)) {
    tryCatch({
      # Table non pondérée
      table_a <- table(indiv_immi[[var_name]], indiv_immi$generation_4)
      table_a <- table_a[2, , drop = FALSE]
      colnames(table_a)[colnames(table_a) == '2'] <- 'n_G1'
      colnames(table_a)[colnames(table_a) == '3'] <- 'n_G2'
      colnames(table_a)[colnames(table_a) == '4'] <- 'n_G2_5'
      
      # Table pondérée
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
