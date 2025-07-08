library(poLCA)
library(ggplot2)
library(tidyverse)
library(haven)
library(dplyr)

data <- sorana1

data <- data %>% 
  filter(
    d_accent %in% c("Oui", "Non"),
    d_nom    %in% c("Oui", "Non"),
    d_relig  %in% c("Oui", "Non"),
    d_natio  %in% c("Oui", "Non"),
    d_peau   %in% c("Oui", "Non"),
    d_apphy  %in% c("Oui", "Non")
  )


data <- data %>%
  mutate(across(c(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy),
                ~ as.integer(as_factor(.))))  # Convertit les labels en facteurs, puis en entiers



################# Exemple sans covariable####################################
# On choisit les variables explicatives
f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy) ~ 1










#############################################################################
# Choix du nombre de classes : FIT STATS
############################################################################

########################################################################
# Création des fits stats et tests sur différents nombres de catégories
########################################################################

# Dataframe des fits stats (initialisation)
model_results <- data.frame(
  Classes = integer(),
  AIC = numeric(),
  BIC = numeric(),
  SABIC = numeric(),
  df = integer()
)

N<- 11513

# Boucles : fit stats
for (k in 1:6) {
  cat("\n--- Modèle", k, "classes ---\n")
  res <- poLCA(f, data = data, nclass = k, nrep = 10, maxiter = 5000)
  
  p <- length(res$coeff)
  logL <- res$llik
  SABIC <- -2 * logL + log((N + 2) / 24) * p
  df <- res$resid.df  # degrés de liberté
  
  cat("AIC:", res$aic, "| BIC:", res$bic, "| SABIC:", SABIC, "| df:", df, "\n")
  
  # Ajouter les résultats au tableau
  model_results <- rbind(model_results, data.frame(
    Classes = k,
    AIC = res$aic,
    BIC = res$bic,
    SABIC = SABIC,
    df = df
  ))
}

# Affichage du tableau récapitulatif
print(model_results)

# Elbow plot (AIC, BIC, SABIC)
ggplot(model_results, aes(x = Classes)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red", linetype = "dashed") +
  geom_point(aes(y = AIC), color = "red") +
  geom_line(aes(y = SABIC), color = "green", linetype = "dotdash") +
  geom_point(aes(y = SABIC), color = "green") +
  labs(title = "Elbow Plot (LCA models)",
       y = "Information Criterion", x = "Classes") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:6) +
  theme(legend.position = "bottom")
#############################################################################

