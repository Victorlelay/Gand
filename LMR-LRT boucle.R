for (k in 2:10){
  lmr_result <- calc_lrt(
    n = 14183,                                    # Votre taille d'échantillon
    null_ll = models[[k-1]]$llik,              # Log-vraisemblance 2 classes
    null_param = length(models[[k-1]]$coeff),  # Nb paramètres 2 classes
    null_classes = k-1,
    alt_ll = models[[k]]$llik,               # Log-vraisemblance 3 classes
    alt_param = length(models[[k]]$coeff),   # Nb paramètres 3 classes
    alt_classes = k
  )
  print(lmr_result)
}


lmr_result <- calc_lrt(
  n = 14183,                                    # Votre taille d'échantillon
  null_ll = models[[3]]$llik,              # Log-vraisemblance 2 classes
  null_param = length(models[[3]]$coeff),  # Nb paramètres 2 classes
  null_classes = 3,
  alt_ll = models[[4]]$llik,               # Log-vraisemblance 3 classes
  alt_param = length(models[[4]]$coeff),   # Nb paramètres 3 classes
  alt_classes = 4
)
print(lmr_result)

models <- list()
for (k in 1:10) {
  set.seed(123)  # pour la reproductibilité
  models[[k]] <- poLCA(
    f, 
    data = indiv, 
    nclass = k, 
    nrep = 1,       # plusieurs réplications pour une meilleure estimation
    maxiter = 5000, 
    verbose = FALSE
  )
}

entropy <- function(posterior) {
  N <- nrow(posterior)
  K <- ncol(posterior)
  log_post <- log(posterior + 1e-10)  # éviter log(0)
  entropy_raw <- -sum(posterior * log_post)
  entropy_norm <- 1 - (entropy_raw / (N * log(K)))
  return(entropy_norm)
}

entropy_values <- sapply(models, function(mod) entropy(mod$posterior))
print(round(entropy_values, 4))
