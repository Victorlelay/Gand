install.packages("poLCA")
library(poLCA)
?poLCA
library(ggplot2)
library(tidyverse)
library(haven)

# chargement des data
data <- read_sav("C:/Users/vctrl/Desktop/Bases de données/data_LCA_reasons.sav")

data <- data %>%
  mutate(across(c(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy),
                ~ as.integer(as_factor(.))))  # Convertit les labels en facteurs, puis en entiers



################# Exemple sans covariable####################################
# On choisit les variables explicatives
f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy) ~ 1
mod <- poLCA(f, data = data, nclass = 3)
################# On crée les fit stats nécessaires##########################

# avec covariable
f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy) ~ sexee + origine_tous_g2bis
mod <- poLCA(f, data = data, nclass = 3, nrep=1, maxiter = 5000)







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




#############################################################################
# choix final du nombre de classes
#############################################################################



# On considère qu'on choisit le modèle à trois paramètres : 
# On fait tourner le modèle
mod_4class <- poLCA(f, data = data, nclass = 4, nrep = 10, maxiter = 5000)
# Nombre de classes
K <- 3 # nombre de classes choisies
#############################################################################



#############################################################################
# Diagnostic criteria, On suit la méthodologie de Weller et al. (2020)
#############################################################################
# En moy, quelle part de l'échantillon dans chaque gpe
avg_posterior <- colMeans(mod_4class$posterior)
print(avg_posterior)



###################################################
# On va regarder les average posteriori probality
###################################################
K <- 3  # nombre de classes dans ton modèle
assigned_class <- apply(mod$posterior, 1, which.max)
post <- mod$posterior

classification_matrix <- matrix(0, nrow = K, ncol = K)

for (true_class in 1:K) {
  idx <- which(assigned_class == true_class)
  if (length(idx) > 0) {
    classification_matrix[true_class, ] <- colMeans(post[idx, , drop = FALSE])
  }
}

rownames(classification_matrix) <- paste("Classe assignée", 1:K)
colnames(classification_matrix) <- paste("Probabilité classe", 1:K)

print(classification_matrix)

# Il faut des valeurs sur la diagonale > 0.8 et hors diagonale proches de 0


########################
# On regarde l'entropie
########################
entropy <- function(post) {
  N <- nrow(post)
  K <- ncol(post)
  log_post <- log(post + 1e-10)  # éviter log(0)
  entropy_raw <- -sum(post * log_post)
  entropy_norm <- 1 - (entropy_raw / (N * log(K)))
  return(entropy_norm)
}
entropy_val <- entropy(mod$posterior)
print(round(entropy_val, 4))  # pour un affichage lisible



# On regarde aussi la taille de nos classes
# Moyenne des probabilités postérieures par classe (proportion estimée de chaque cluster)
assigned_class <- apply(mod_4class$posterior, 1, which.max)
table(assigned_class)


#############################################################################





#############################################################################
# Analyse des différentes classes
#############################################################################

# On récupère les probabilités conditionnelles de réponse aux items
probs <- mod_4class$probs

# Crée un tableau long avec les probabilités
plot_data <- map_dfr(names(probs), function(varname) {
  var_probs <- probs[[varname]]  
  var_probs_t <- t(var_probs)  
  df <- as.data.frame(var_probs_t)
  df$modality <- rownames(df) 
  df$variable <- varname
  pivot_longer(df, cols = starts_with("class"),
               names_to = "class", values_to = "probability")
}, .id = "item_index")
plot_data <- plot_data %>%
  mutate(class = gsub("class (\\d+):", "Classe \\1", class))


# Réorganiser pour ggplot
plot_data_long <- pivot_longer(plot_data,
                               cols = starts_with("V"),
                               names_to = "class",
                               values_to = "probability")

# Nettoyage : renommer les classes proprement
#plot_data_long$class <- factor(plot_data_long$class,
                               #labels = paste("Classe", 1:mod_3class$nclass))


# Graphique final : pas du tout au point
plot_data_positive <- plot_data %>% filter(modality == "Pr(1)")
ggplot(plot_data_positive, aes(x = variable, y = probability, group = class, color = class)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Profils latents selon les types de discrimination",
       x = "Indicateurs de discrimination",
       y = "Probabilité conditionnelle",
       color = "Classe latente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


