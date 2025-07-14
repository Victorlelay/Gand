install.packages("poLCA")
library(poLCA)
?poLCA
library(ggplot2)
library(tidyverse)
library(haven)
library(dplyr)

# Men

# chargement des data
datam <- dataset_discri1

datam <- datam %>%
  rename(
    age = d_pqdisc_a,
    gender = d_pqdisc_b,
    health = d_pqdisc_c,
    skin = d_pqdisc_d,
    nat_ori = d_pqdisc_e,
    neighbourhood = d_pqdisc_f,
    accent = d_pqdisc_g,
    family = d_pqdisc_h,
    orient_sex = d_pqdisc_i,
    relig = d_pqdisc_j,
    clothes = d_pqdisc_k,
    weight = d_pqdisc_l,
    other = d_pqdisc_m
  ) 

datam <- datam %>%
  mutate(across(
    c(age, gender, health, skin, nat_ori, neighbourhood, accent, family, orient_sex, relig, clothes, weight, other),
    ~ case_when(
      . == "Modalité citée" ~ 1,
      . == "Modalité non citée" ~ 0,
      TRUE ~ NA_real_
    )))

datam <- datam %>% mutate(other_crit = case_when(
  age + health + family + orient_sex + weight + other > 0 ~ 1,
  TRUE ~ 0))

datam <- datam %>%
  mutate(across(c(gender, skin, nat_ori, neighbourhood, accent, relig, clothes, other_crit),
                ~ as.integer(as_factor(.))))  # Convertit les labels en facteurs, puis en entiers

datam <- datam %>% filter(sexee == "Masculin") 

################# Exemple sans covariable####################################
# On choisit les variables explicatives
f <- cbind(gender, skin, nat_ori, neighbourhood, accent, relig, clothes, other_crit) ~ 1




# On considère qu'on choisit le modèle à trois paramètres : 
# On fait tourner le modèle
mod_5class <- poLCA(f, data = datam, nclass = 5, nrep = 10, maxiter = 5000)
# Nombre de classes
K <- 5 # nombre de classes choisies
#############################################################################



#############################################################################
# Diagnostic criteria, On suit la méthodologie de Weller et al. (2020)
#############################################################################
# En moy, quelle part de l'échantillon dans chaque gpe
avg_posterior <- colMeans(mod_5class$posterior)
print(avg_posterior)



###################################################
# On va regarder les average posteriori probality
###################################################
K <- 5  # nombre de classes dans ton modèle
assigned_class <- apply(mod_5class$posterior, 1, which.max)
post <- mod_5class$posterior

classification_matrix5 <- matrix(0, nrow = K, ncol = K)

for (true_class in 1:K) {
  idx <- which(assigned_class == true_class)
  if (length(idx) > 0) {
    classification_matrix5[true_class, ] <- colMeans(post[idx, , drop = FALSE])
  }
}

rownames(classification_matrix5) <- paste("Classe assignée", 1:K)
colnames(classification_matrix5) <- paste("Probabilité classe", 1:K)

print(classification_matrix5)

# Taille
# Ajouter la classe latente à chaque individu
data_male_5 <- datam
data_male_5$classe_latente <- mod_5class$predclass

# Afficher la taille de chaque classe latente
table(data_male_5$classe_latente)
write_dta(data_male_5, "data_male_5.dta")




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
entropy_val <- entropy(mod_5class$posterior)
print(round(entropy_val, 4))  # pour un affichage lisible



# On regarde aussi la taille de nos classes
# Moyenne des probabilités postérieures par classe (proportion estimée de chaque cluster)
assigned_class <- apply(mod_4class$posterior, 1, which.max)
table(assigned_class)



write_dta(data_male_4, "data_male_5.dta")
#############################################################################





#############################################################################
# Analyse des différentes classes
#############################################################################

# On récupère les probabilités conditionnelles de réponse aux items
probs <- mod_5class$probs

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
plot_data_clean <- plot_data %>%
  rename(class_orig = class, probability_orig = probability)

plot_data_long <- pivot_longer(plot_data_clean,
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

plot_data_positive <- plot_data %>%
  filter(modality == "Pr(2)")

ggplot(plot_data_positive, aes(x = class, y = probability, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profils latents selon les types de discrimination",
       x = "Classe latente",
       y = "Probabilité conditionnelle",
       fill = "Indicateur de discrimination") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom")

