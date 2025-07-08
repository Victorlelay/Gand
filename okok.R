library(questionr)
library(dplyr)
library(weights) 
library(Hmisc)
library(readr)
library(tidyr)

# Fonction pour créer une table avec test de significativité
creer_table_avec_test <- function(var_name) {
  
  # Table non pondérée pour les effectifs et le test
  table_a <- table(indiv_immi[[var_name]], indiv_immi$G1_G2)
  table_a <- table_a[2, , drop = FALSE]  # garder seulement "oui" à la discrimination
  rownames(table_a)[colnames(table_a) == '1'] <- 'n_G1'
  colnames(table_a)[colnames(table_a) == '2'] <- 'n_G2'
  
  # Table pondérée
  table_temp_a <- wtd.table(indiv_immi[[var_name]], indiv_immi$G1_G2, 
                            weights = indiv_immi$poidsi)
  
  # Pourcentages pondérés
  table_pct_a <- round(prop.table(table_temp_a, 2) * 100, 2)
  table_pct_a <- table_pct_a[2, , drop = FALSE]  # garder seulement "oui"
  colnames(table_pct_a)[colnames(table_pct_a) == '1'] <- 'pct_G1'
  colnames(table_pct_a)[colnames(table_pct_a) == '2'] <- 'pct_G2'
  
  # Test du chi2 sur la table complète non pondérée
  table_complete <- table(indiv_immi[[var_name]], indiv_immi$G1_G2)
  test_chi2 <- chisq.test(table_complete)
  p_value <- round(test_chi2$p.value, 4)
  
  # Significativité
  if (p_value < 0.001) {
    significativite <- "***"
  } else if (p_value < 0.01) {
    significativite <- "**"
  } else if (p_value < 0.05) {
    significativite <- "*"
  } else {
    significativite <- "ns"
  }
  
  # Combiner les résultats
  table_finale_a <- cbind(table_pct_a, table_a, 
                          p_value = p_value, 
                          significativite = significativite)
  
  return(table_finale_a)
}

# Liste des variables à traiter
variables_discri <- paste0("d_pqdisc_", letters[1:13])  # a à m

# Liste pour stocker tous les résultats
resultats_finaux <- list()

# Boucle for
for (var in variables_discri) {
  cat("Traitement de", var, "\n")
  
  # Vérifier que la variable existe
  if (var %in% names(indiv_immi)) {
    tryCatch({
      # Créer la table avec test
      table_result <- creer_table_avec_test(var)
      
      # Ajouter le nom de la variable comme nom de ligne
      rownames(table_result) <- var
      
      # Stocker dans la liste
      resultats_finaux[[var]] <- as.data.frame(table_result)
      
      # Afficher le résultat
      print(table_result)
      cat("\n")
      
    }, error = function(e) {
      cat("Erreur pour", var, ":", e$message, "\n")
    })
  } else {
    cat("Variable", var, "non trouvée dans les données\n")
  }
}

# Combiner tous les résultats en un seul data.frame
if (length(resultats_finaux) > 0) {
  tableau_final <- do.call(rbind, resultats_finaux)
  
  # Export
  write.csv2(tableau_final, "discrimination_detaillee_avec_tests.csv", row.names = TRUE)
  
  # Affichage du tableau final
  print("=== TABLEAU FINAL ===")
  print(tableau_final)
  
  # Export Excel si vous préférez
  # library(writexl)
  # write_xlsx(list("Discrimination_G1_G2" = tableau_final), "discrimination_G1_G2_complet.xlsx")
}

# Résumé des variables significatives
if (exists("tableau_final")) {
  cat("\n=== RÉSUMÉ SIGNIFICATIVITÉ ===\n")
  print(table(tableau_final$significativite))
  
  cat("\nVariables significatives (p < 0.05):\n")
  vars_significatives <- rownames(tableau_final)[tableau_final$significativite %in% c("*", "**", "***")]
  print(vars_significatives)
}