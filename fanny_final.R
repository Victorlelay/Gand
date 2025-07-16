install.packages("poLCA")
library(poLCA)
?poLCA
library(ggplot2)
library(tidyverse)
library(haven)

########################################################################################
# On enlève les Refus et les NSP
########################################################################################
# chargement des data
# origine
data <- test_data_final %>%
  mutate(origine_tous_g2bis = case_when(
    origine_tous_g2 %in% c(0, 10, 11) ~ 1,
    origine_tous_g2 == 20 ~ 20,
    origine_tous_g2 == 22 ~ 22,
    origine_tous_g2 %in% c(30, 40) ~ 30,
    origine_tous_g2 %in% c(33, 44) ~ 33,
    origine_tous_g2 %in% c(50, 60, 70) ~ 50,
    origine_tous_g2 %in% c(55, 66, 77) ~ 55,
    origine_tous_g2 == 90 ~ 60,
    origine_tous_g2 == 99 ~ 66,
    origine_tous_g2 %in% c(80, 100, 110) ~ 70,
    origine_tous_g2 %in% c(88, 111) ~ 77,
    origine_tous_g2 %in% c(120, 130, 140, 150) ~ 80,
    origine_tous_g2 %in% c(121, 131, 141, 151) ~ 88,
    origine_tous_g2 == 160 ~ 100,
    origine_tous_g2 == 161 ~ 111,
    TRUE ~ NA_real_  # équivalent de "." en Stata
  ))

data <- data %>% filter(origine_tous_g2bis!=1) 

# racism
data <- data %>%
  mutate(racism = 99,  # Initialisation à 99
         racism = ifelse(d_racism == 1, 1, racism),  # Cas 1 : déjà vécu du racisme
         racism = ifelse(d_racism == 2 & d_racpot == 1, 2, racism),  # Cas 2 : pas vécu mais pense que ça pourrait arriver
         racism = ifelse(d_racpot == 2 & racism == 99, 3, racism)  # Cas 3 : pense que ça n'arrivera jamais
  )

data <- data %>% filter(racism!=99) %>% filter(racism!=3) # on retire les refus et les NSP pour d_racism et d_racpot


# On enlève refus et NSP
data <- data %>% filter(d_nom %in% c(1,2))
data <- data %>% filter(d_peau %in% c(1,2))
data <- data %>% filter(d_apphy %in% c(1,2))
data <- data %>% filter(d_relig %in% c(1,2))
data <- data %>% filter(d_accent %in% c(1,2))
data <- data %>% filter(d_natio %in% c(1,2))

# bon type de variable
data_all <- data %>%
  mutate(across(c(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy, d_racaut),
                ~ as.integer(as_factor(.))))  # Convertit les labels en facteurs, puis en entiers



###### Fit statistics
### ALL
set.seed(1234)
f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy, d_racaut) ~ 1
N <- 11513

# Dataframe des fits stats (initialisation)
model_results <- data.frame(
  Classes = integer(),
  AIC = numeric(),
  BIC = numeric(),
  SABIC = numeric(),
  df = integer()
)

# Boucles : fit stats
for (k in 1:10) {
  cat("\n--- Modèle", k, "classes ---\n")
  res <- poLCA(f, data = data_all, nclass = k, nrep = 1, maxiter = 5000)
  
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
fit <- as.data.frame(model_results)

# Elbow plot (AIC, BIC, SABIC)
ggplot(model_results, aes(x = Classes)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red", linetype = "dashed") +
  geom_point(aes(y = AIC), color = "red") +
  geom_line(aes(y = SABIC), color = "green", linetype = "dotdash") +
  geom_point(aes(y = SABIC), color = "green") +
  labs(title = "Elbow Plot (LCA models) - Racism experienced/expected - d_racaut included",
       y = "Information Criterion", x = "Classes") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:20) +
  theme(legend.position = "bottom")

# Let's test 3,4,6 and 7 classes
#############################################################################


# Only discriminated
data_disc <- data_all %>% filter(racism==1) # n = 6550


# Fit statistics
N <- 6550
# Dataframe des fits stats (initialisation)
model_results <- data.frame(
  Classes = integer(),
  AIC = numeric(),
  BIC = numeric(),
  SABIC = numeric(),
  df = integer()
)

# Boucles : fit stats
for (k in 1:10) {
  cat("\n--- Modèle", k, "classes ---\n")
  res <- poLCA(f, data = data_disc, nclass = k, nrep = 1, maxiter = 5000)
  
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
fit <- as.data.frame(model_results)

# Elbow plot (AIC, BIC, SABIC)
ggplot(model_results, aes(x = Classes)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red", linetype = "dashed") +
  geom_point(aes(y = AIC), color = "red") +
  geom_line(aes(y = SABIC), color = "green", linetype = "dotdash") +
  geom_point(aes(y = SABIC), color = "green") +
  labs(title = "Elbow Plot (LCA models) - Racism experienced only - d_racaut included",
       y = "Information Criterion", x = "Classes") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:20) +
  theme(legend.position = "bottom")

# Let's test 3,4,5 and 6




########################################################################################################################################################


###### Diagnositc statistics

#################################
### Expected AND Experienced
#################################
N <- 11513
# 3 classes


f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy, d_racaut) ~ 1 # modèle
set.seed(1234)
mod <- poLCA(f, data = data_all, nclass = 3, nrep=1, maxiter = 5000) # on fait tourner

### Diagnostic criteria
K <- 3  # nombre de classes dans ton modèle

# Diagonal values
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
classification_matrix_all_3 <- classification_matrix
print(classification_matrix_all_3)

# Entropie
entropy <- function(post) {
  N <- nrow(post)
  K <- ncol(post)
  log_post <- log(post + 1e-10)  # éviter log(0)
  entropy_raw <- -sum(post * log_post)
  entropy_norm <- 1 - (entropy_raw / (N * log(K)))
  return(entropy_norm)
}
entropy_val3 <- entropy(mod$posterior)
print(round(entropy_val3, 4))  # pour un affichage lisible

# taille de chaque classe
data_LCA_recode_all <- data_all
data_LCA_recode_all$classe_latente <- mod$predclass
table(data_LCA_recode_all$classe_latente) # Bien: 1 : 4852 // 2 : 4642 // 3 : 2019

mod_all_3 <- mod


# On récupère les probabilités conditionnelles de réponse aux items
probs <- mod_all_3$probs

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

plot_data_positive <- plot_data %>%
  filter(modality == "Pr(1)")

ggplot(plot_data_positive, aes(x = class, y = probability, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profils latents selon les types de discrimination",
       x = "Classe latente",
       y = "Probabilité conditionnelle",
       fill = "Indicateur de discrimination") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom")







# 4 classes


f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy, d_racaut) ~ 1 # modèle
set.seed(1240)
mod <- poLCA(f, data = data_all, nclass = 4, nrep=1, maxiter = 5000) # on fait tourner

### Diagnostic criteria
K <- 4  # nombre de classes dans ton modèle

# Diagonal values
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
classification_matrix_all_4 <- classification_matrix
print(classification_matrix_all_4)

# Entropie
entropy <- function(post) {
  N <- nrow(post)
  K <- ncol(post)
  log_post <- log(post + 1e-10)  # éviter log(0)
  entropy_raw <- -sum(post * log_post)
  entropy_norm <- 1 - (entropy_raw / (N * log(K)))
  return(entropy_norm)
}
entropy_val4 <- entropy(mod$posterior)
print(round(entropy_val4, 4))  # pour un affichage lisible

# taille de chaque classe
data_LCA_recode_all <- data_all
data_LCA_recode_all$classe_latente <- mod$predclass
table(data_LCA_recode_all$classe_latente) # Bien: 1 : 4852 // 2 : 4642 // 3 : 2019
write_dta(data_LCA_recode_all, "data_LCA_recode_all.dta")
mod_all_4 <- mod

# On récupère les probabilités conditionnelles de réponse aux items
probs <- mod_all_4$probs

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

plot_data_positive <- plot_data %>%
  filter(modality == "Pr(1)")

ggplot(plot_data_positive, aes(x = class, y = probability, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profils latents selon les types de discrimination",
       x = "Classe latente",
       y = "Probabilité conditionnelle",
       fill = "Indicateur de discrimination") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom")


# 5 classes


f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy, d_racaut) ~ 1 # modèle
set.seed(1236)
mod <- poLCA(f, data = data_all, nclass = 5, nrep=1, maxiter = 5000) # on fait tourner

### Diagnostic criteria
K <- 5  # nombre de classes dans ton modèle

# Diagonal values
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
classification_matrix_all_5 <- classification_matrix
print(classification_matrix_all_5)

# Entropie
entropy <- function(post) {
  N <- nrow(post)
  K <- ncol(post)
  log_post <- log(post + 1e-10)  # éviter log(0)
  entropy_raw <- -sum(post * log_post)
  entropy_norm <- 1 - (entropy_raw / (N * log(K)))
  return(entropy_norm)
}
entropy_val5 <- entropy(mod$posterior)
print(round(entropy_val5, 4))  # pour un affichage lisible

# taille de chaque classe
data_LCA_recode_all <- data_all
data_LCA_recode_all$classe_latente <- mod$predclass
table(data_LCA_recode_all$classe_latente) # Bien: 1 : 4852 // 2 : 4642 // 3 : 2019
write_dta(data_LCA_recode_all, "data_LCA_recode_all.dta")
mod_all_5 <- mod


# On récupère les probabilités conditionnelles de réponse aux items
probs <- mod_all_5$probs

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

plot_data_positive <- plot_data %>%
  filter(modality == "Pr(1)")

ggplot(plot_data_positive, aes(x = class, y = probability, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profils latents selon les types de discrimination",
       x = "Classe latente",
       y = "Probabilité conditionnelle",
       fill = "Indicateur de discrimination") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom")



#################################
### Experienced ONLY
#################################
N <- 6550
# 3 classes


f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy, d_racaut) ~ 1 # modèle
set.seed(1234)
mod <- poLCA(f, data = data_disc, nclass = 3, nrep=1, maxiter = 5000) # on fait tourner

### Diagnostic criteria
K <- 3  # nombre de classes dans ton modèle

# Diagonal values
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
classification_matrix_disc_3 <- classification_matrix


# Entropie
entropy <- function(post) {
  N <- nrow(post)
  K <- ncol(post)
  log_post <- log(post + 1e-10)  # éviter log(0)
  entropy_raw <- -sum(post * log_post)
  entropy_norm <- 1 - (entropy_raw / (N * log(K)))
  return(entropy_norm)
}
entropy_val33 <- entropy(mod$posterior)
print(round(entropy_val3, 4))  # pour un affichage lisible

# taille de chaque classe
data_LCA_recode_all <- data_disc
data_LCA_recode_all$classe_latente <- mod$predclass
table(data_LCA_recode_all$classe_latente) # Bien: 1 : 4852 // 2 : 4642 // 3 : 2019

mod_disc_3 <- mod

# On récupère les probabilités conditionnelles de réponse aux items
probs <- mod_disc_3$probs

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

plot_data_positive <- plot_data %>%
  filter(modality == "Pr(1)")

ggplot(plot_data_positive, aes(x = class, y = probability, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profils latents selon les types de discrimination",
       x = "Classe latente",
       y = "Probabilité conditionnelle",
       fill = "Indicateur de discrimination") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom")




# 4 classes


f <- cbind(d_nom, d_peau, d_accent, d_relig, d_natio, d_apphy, d_racaut) ~ 1 # modèle
set.seed(1240)
mod <- poLCA(f, data = data_disc, nclass = 4, nrep=1, maxiter = 5000) # on fait tourner

### Diagnostic criteria
K <- 4  # nombre de classes dans ton modèle

# Diagonal values
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
classification_matrix_disc_4 <- classification_matrix


# Entropie
entropy <- function(post) {
  N <- nrow(post)
  K <- ncol(post)
  log_post <- log(post + 1e-10)  # éviter log(0)
  entropy_raw <- -sum(post * log_post)
  entropy_norm <- 1 - (entropy_raw / (N * log(K)))
  return(entropy_norm)
}
entropy_val44 <- entropy(mod$posterior)
print(round(entropy_val4, 4))  # pour un affichage lisible

# taille de chaque classe
data_LCA_recode_all <- data_disc
data_LCA_recode_all$classe_latente <- mod$predclass
table(data_LCA_recode_all$classe_latente) # Bien: 1 : 4852 // 2 : 4642 // 3 : 2019

mod_disc_4 <- mod


probs <- mod_disc_4$probs

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

plot_data_positive <- plot_data %>%
  filter(modality == "Pr(1)")

ggplot(plot_data_positive, aes(x = class, y = probability, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profils latents selon les types de discrimination",
       x = "Classe latente",
       y = "Probabilité conditionnelle",
       fill = "Indicateur de discrimination") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom")
