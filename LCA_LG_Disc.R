data <- d_ouraci %>% mutate(origine_tous_g2bis = case_when(
  (origine_tous_g2 == 0 | origine_tous_g2 == 10 | origine_tous_g2 == 11) ~ 1, # population sans ascendance migratoire directe
  (origine_tous_g2 == 20) ~ 20, # Originaires d'outre-mer
  (origine_tous_g2 == 22) ~ 22, # Descendant.es d'originaires d'outre-mer
  (origine_tous_g2 == 30 | origine_tous_g2 == 40) ~ 30, # Immigré.es du Maghreb
  (origine_tous_g2 == 33 |origine_tous_g2 == 44) ~ 33, # Descendant.es d'immigré.es originaires du Maghreb
  (origine_tous_g2 == 50 | origine_tous_g2 == 60 | origine_tous_g2 == 70) ~ 50, # Immigré.es originaires d'Afrique Subsaharienne
  (origine_tous_g2 == 55 | origine_tous_g2 == 66 | origine_tous_g2 == 77) ~ 55, # Descendant.es d'immigré.es originaires d'Afrique Subsaharienne
  (origine_tous_g2 == 90) ~ 60, # Immigré.es originaires de Turquie et du Moyen-Orient
  (origine_tous_g2 == 99) ~ 66, # Descendant.es d'immigré.es originaires de Turquie et du Moyen-Orient
  (origine_tous_g2 == 80 | origine_tous_g2 == 100 | origine_tous_g2 == 110) ~ 70, # Immigré.es originaires du reste de l'Asie
  (origine_tous_g2 == 88 | origine_tous_g2 == 111) ~ 77, # Descendant.es d'immigré.es originaires de reste de l'Asie
  (origine_tous_g2 == 120 | origine_tous_g2 == 130 | origine_tous_g2== 140 | origine_tous_g2 == 150) ~ 80, # Immigré.es originaires d'Europe
  (origine_tous_g2 == 121 | origine_tous_g2 == 131 | origine_tous_g2 == 141 | origine_tous_g2 == 151) ~ 88, # Descendant.es d'originaires d'immigré.es d'Europe 
  (origine_tous_g2) == 160 ~ 100, # Immigré.es d'autres pays
  (origine_tous_g2) == 161 ~ 111, # Descendant.es d'immigré.es d'autres pays
  TRUE ~ NA_real_
))  

data <- data %>% filter(origine_tous_g2bis!=1) #20 231
data <- data %>%  filter(!is.na(d_pqdisc_nbval)) # 20 146
data <- data %>%  filter(d_pqdisc_nbval!=0) # 5267


# recode discrimination reasons
data <- data %>%
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

data <- data %>% mutate(other = case_when(
  age + health + family + orient_sex + weight + other > 0 ~ 1,
  age + health + family + orient_sex + weight + other == 0 ~ 0,
  TRUE ~ NA_real_))


data_f <- data %>%  filter(sexee==2) # female
data_m <- data %>%  filter(sexee==1) # male

write.csv(data, "data1_discri_all_LG.csv", row.names = FALSE)
write.csv(data_f, "data1_discri_f_LG.csv", row.names = FALSE)
write.csv(data_m, "data1_discri_m_LG.csv", row.names = FALSE)


# LCA
lg_metrics <- tribble(
  ~modele, ~BIC,        ~AIC,        ~AIC3,
  1,       37586.0915,  37533.5378,  37541.5378,
  2,       37006.6309,  36894.9542,  36911.9542,
  3,       36403.7387,  36232.9391,  36258.9391,
  4,       36035.1955,  35805.2729,  35840.2729,
  5,       35858.5576,  35569.5121,  35613.5121,
  6,       35727.9371,  35379.7686,  35432.7686,
  7,       35566.0162,  35158.7248,  35220.7248,
  8,       35566.7118,  35100.2974,  35171.2974,
  9,       35560.8513,  35035.3140,  35115.3140,
  10,       35606.1223,  35021.4621,  35110.4621
)

lg_metrics %>%
  pivot_longer(c(BIC, AIC, AIC3), names_to = "critere", values_to = "valeur") %>%
  ggplot(aes(modele, valeur, color = critere)) +
  geom_line() + geom_point() + scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(x = "Nombre de classes (k)", y = "Critère d'information",
       title = "Elbow plot — BIC, AIC, AIC3", color = "Critère") +
  theme_minimal()

library(dplyr)
library(tidyr)
library(stringr)

# ---- lecture adaptée (espaces, 1 ligne de titre "Classification" à sauter)
read_lg_ws <- function(path) {
  df <- read.table(
    file = path,
    header = TRUE,   # la 2e ligne est l’en-tête
    skip   = 1,      # on saute la 1re ligne ("Classification")
    sep    = "",     # n'importe quel nb d'espaces comme séparateur
    fill   = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    comment.char = ""
  )
  
  cluster_cols <- grep("^Cluster\\d+$", names(df), value = TRUE)
  if (length(cluster_cols) == 0) stop("Colonnes Cluster1..K introuvables.")
  if (!all(c("ObsFreq","Modal") %in% names(df))) stop("Colonnes ObsFreq/Modal manquantes.")
  
  df <- df %>%
    mutate(
      ObsFreq = as.numeric(ObsFreq),
      Modal   = as.integer(Modal)
    ) %>%
    mutate(across(all_of(cluster_cols), ~ as.numeric(.x)))
  
  list(df = df, cluster_cols = cluster_cols)
}

# ---- métriques façon Weller
lg_app_metrics <- function(df, cluster_cols) {
  long <- df %>%
    select(Modal, ObsFreq, all_of(cluster_cols)) %>%
    pivot_longer(all_of(cluster_cols), names_to = "ClassLab", values_to = "PostProb") %>%
    mutate(Class = as.integer(str_extract(ClassLab, "\\d+")))
  
  app_matrix <- long %>%
    group_by(Modal, Class) %>%
    summarise(APP = weighted.mean(PostProb, w = ObsFreq, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Class, values_from = APP, names_prefix = "Class_") %>%
    arrange(Modal)
  
  avepp <- long %>%
    filter(Modal == Class) %>%
    group_by(Class) %>%
    summarise(
      AvePP = weighted.mean(PostProb, w = ObsFreq, na.rm = TRUE),
      N     = sum(ObsFreq, na.rm = TRUE),
      .groups = "drop"
    )
  
  sizes <- df %>%
    group_by(Modal) %>%
    summarise(N_class = sum(ObsFreq, na.rm = TRUE), .groups = "drop") %>%
    mutate(Total = sum(N_class), Prop = N_class / Total) %>%
    rename(Class = Modal)
  
  min_offdiag <- long %>%
    filter(Modal != Class) %>%
    summarise(min_off = min(PostProb, na.rm = TRUE)) %>%
    pull(min_off)
  
  avepp <- avepp %>%
    left_join(sizes, by = "Class") %>%
    mutate(Meets_.80 = AvePP >= 0.80) %>%
    arrange(Class)
  
  list(app_matrix = app_matrix, avepp = avepp, min_offdiag = min_offdiag)
}

# ---- pipeline
analyze_lg_model_ws <- function(path) {
  r <- read_lg_ws(path)
  lg_app_metrics(r$df, r$cluster_cols)
}

# === utilisation ===
res_7c <- analyze_lg_model_ws("classif_discri_all_7classes.txt")
res_7c$avepp
res_7c$app_matrix
res_7c$min_offdiag

res_6c <- analyze_lg_model_ws("classif_discri_all_6classes.txt")
res_6c$avepp
res_6c$app_matrix
res_6c$min_offdiag

res_5c <- analyze_lg_model_ws("classif_discri_all_5classes.txt")
res_5c$avepp
res_5c$app_matrix
res_5c$min_offdiag

res_4c <- analyze_lg_model_ws("classif_discri_all_4classes.txt")
res_4c$avepp
res_4c$app_matrix
res_4c$min_offdiag