library(tidyr)

# Table pondérée avec fonction helper
create_g1g2_table <- function(data, var_discri) {
  
  # Table pondérée
  weighted_table <- wtd.table(data[[var_discri]], data$G1_G2, weights = data$poidsi)
  
  # Conversion en dataframe propre
  df_table <- as.data.frame(weighted_table) %>%
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    rename(Response = Var1) %>%
    mutate(
      pct_G1 = round(G1_Immigrants / sum(G1_Immigrants) * 100, 2),
      pct_G2 = round(G2_Descendants / sum(G2_Descendants) * 100, 2)
    ) %>%
    select(Response, 
           freq_G1 = G1_Immigrants, pct_G1, 
           freq_G2 = G2_Descendants, pct_G2)
  
  return(df_table)
}

# Application à la discrimination générale
df_discri_G1G2 <- create_g1g2_table(indiv_immi, "d_discri")

# Affichage
print(df_discri_G1G2)

# Export
write.csv2(df_discri_G1G2, "table_discrimination_G1G2_clean.csv", row.names = FALSE)
