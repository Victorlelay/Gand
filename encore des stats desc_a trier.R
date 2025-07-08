library(dplyr)
library(tidyr)
library(survey)
library(questionr)

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
  
  # Table brute (n)
  table_n <- table(indiv_immi[[var_name]], indiv_immi$Nombre_attributions)
  
  # Mise en forme des % en long → large
  df_pct <- as.data.frame(table_pct) %>%
    pivot_wider(names_from = Var2, values_from = Freq)
  
  # Mise en forme des effectifs en long → large puis transposition
  df_n <- as.data.frame(table_n) %>%
    pivot_wider(names_from = Var1, values_from = Freq, names_prefix = "Freq_") %>%
    select(-Var2) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  
  # Harmonisation des noms de colonnes
  colnames(df_n) <- c("n_0", "n_1", "n_seq2")
  colnames(df_pct) <- c(var_label, "0_reason", "1_reason", "seq2_reason")
  
  # Fusion des % et effectifs
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




#####