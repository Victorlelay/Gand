####### Final dataset ???
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)

data <- X27_8

data <-  data %>% mutate(Region_of_Origin = case_when(
  GENSTATUS == "1.5 generation" | GENSTATUS == "first generation" ~ regionnaise,
  GENSTATUS == "2nd generation" & regionnaism!= "Refus, NSP" ~ regionnaism,
  GENSTATUS == "2nd generation" & regionnaism == "Refus, NSP" ~ regionnaisp, 
  group2_teo2 %in% c(32, 42) ~ regionnaism,
  group2_teo2 %in% c(33, 43) ~ regionnaisp,
  GENSTATUS== "3th generation" & !is.na(regionnaisepgmm) & regionnaisepgmm != "Refus, NSP" & regionnaisepgmm != "France, y compris DROM-COM" ~ regionnaisepgmm,
  GENSTATUS== "3th generation" & (is.na(regionnaisepgmm)|regionnaisepgmm == "Refus, NSP"|regionnaisepgmm == "France, y compris DROM-COM") & ((!is.na(regionnaisepgpm)&regionnaisepgpm != "Refus, NSP"&regionnaisepgpm != "France, y compris DROM-COM")) ~ regionnaisepgpm,
  GENSTATUS== "3th generation" & (is.na(regionnaisepgmm)|regionnaisepgmm == "Refus, NSP"|regionnaisepgmm == "France, y compris DROM-COM")&(is.na(regionnaisepgpm)|regionnaisepgpm == "Refus, NSP"|regionnaisepgpm == "France, y compris DROM-COM") & ((!is.na(regionnaisepgmp)&regionnaisepgmp != "Refus, NSP"&regionnaisepgmp != "France, y compris DROM-COM")) ~ regionnaisepgmp,
  GENSTATUS== "3th generation" & (is.na(regionnaisepgmm)|regionnaisepgmm == "Refus, NSP"|regionnaisepgmm == "France, y compris DROM-COM")&(is.na(regionnaisepgpm)|regionnaisepgpm == "Refus, NSP"|regionnaisepgpm == "France, y compris DROM-COM") & ((is.na(regionnaisepgmp)|regionnaisepgmp == "Refus, NSP"|regionnaisepgmp == "France, y compris DROM-COM")) & ((!is.na(regionnaisepgpp)&regionnaisepgpp != "Refus, NSP"&regionnaisepgpp != "France, y compris DROM-COM")) ~ regionnaisepgpp,
  GENSTATUS== "3th generation" & (is.na(regionnaisepgmm)|regionnaisepgmm == "Refus, NSP"|regionnaisepgmm == "France, y compris DROM-COM")&(is.na(regionnaisepgpm)|regionnaisepgpm == "Refus, NSP"|regionnaisepgpm == "France, y compris DROM-COM") & ((is.na(regionnaisepgmp)|regionnaisepgmp == "Refus, NSP"|regionnaisepgmp == "France, y compris DROM-COM")) & ((is.na(regionnaisepgpp)|regionnaisepgpp == "Refus, NSP"|regionnaisepgpp == "France, y compris DROM-COM")) & dromcomgp!= 1 ~ "No info gp",
  GENSTATUS== "3th generation" & (is.na(regionnaisepgmm)|regionnaisepgmm == "Refus, NSP"|regionnaisepgmm == "France, y compris DROM-COM")&(is.na(regionnaisepgpm)|regionnaisepgpm == "Refus, NSP"|regionnaisepgpm == "France, y compris DROM-COM") & ((is.na(regionnaisepgmp)|regionnaisepgmp == "Refus, NSP"|regionnaisepgmp == "France, y compris DROM-COM")) & ((is.na(regionnaisepgpp)|regionnaisepgpp == "Refus, NSP"|regionnaisepgpp == "France, y compris DROM-COM")) & dromcomgp== 1 ~ "DROM-COM",
  TRUE ~ "other"
))

# there is one case of a person coded as "France métropolitaine" and "G2" even though is father is from DROM COM and his mother from 'France métropolitaine', let's put him in the right categories (DROM COM and G2.5)
data <- data %>% mutate(GENSTATUS_rec = case_when(
  Region_of_Origin != "France m\xe9tropolitaine" ~ GENSTATUS,
  TRUE ~ "2.5 generation"
))

data <- data %>% mutate(GENSTATUS2_rec = case_when(
  Region_of_Origin != "France m\xe9tropolitaine" ~ GENSTATUS2,
  TRUE ~ "2.5 and 3rd generation"
))

data <- data %>% mutate(Region_of_Origin_rec = case_when(
  Region_of_Origin != "France m\xe9tropolitaine" ~ Region_of_Origin,
  TRUE ~ "DROM-COM"
))
# okay, now the recode is done ! We can export the table


data <- data %>% mutate(Region_of_Origin2 = case_when(
  Region_of_Origin_rec %in% c("Alg\xe9rie", "Maroc, Tunisie") ~ "Maghreb",
  Region_of_Origin_rec %in% c("Afrique guin\xe9enne ou centrale", "Afrique sah\xe9lienne", "Autres pays d'Afrique") ~ "Subsaharian Africa",
  Region_of_Origin_rec %in% c("Autres pays d'Europe", "Autres pays de l'UE 15 (hors Royaume-Uni)", "Autres pays de l'UE 27", "Espagne, Italie", "Portugal") ~ "Europe",
  Region_of_Origin_rec == "Autres pays d'Asie" ~ "Asia",
  Region_of_Origin_rec == "DROM-COM" ~ "French Overseas Territories",
  Region_of_Origin_rec == "Turquie, Moyen-Orient" ~ "Turkey / M-E",
  Region_of_Origin_rec == "Am\xe9rique, Oc\xe9anie" ~ "Other",
  TRUE ~ NA_character_
))

write_csv(data, "LCA_racism_final.csv")






############ The LCA
elbow1 <- elbow1 %>%
  rename(
    BIC = `BIC(LL)`,
    AIC = `AIC(LL)`,
    AIC3 = `AIC3(LL)`
  ) %>%
  mutate(k = row_number()) 

elbow1 %>%
  pivot_longer(c(BIC, AIC, AIC3), names_to = "critere", values_to = "valeur") %>%
  ggplot(aes(k, valeur, color = critere, group = critere)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(x = "Nombre de classes (k)", y = "Critère d'information",
       title = "Elbow plot — BIC, AIC, AIC3", color = "Critère") +
  theme_minimal()


##### Weller
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
res_7c <- analyze_lg_model_ws("Model7.txt")
res_7c$avepp
res_7c$app_matrix
res_7c$min_offdiag

m7 <- as.data.frame(res_7c$app_matrix)


res_6c <- analyze_lg_model_ws("Model6.txt")
res_6c$avepp
res_6c$app_matrix
res_6c$min_offdiag

m6 <- as.data.frame(res_6c$app_matrix)

res_5c <- analyze_lg_model_ws("Model5.txt")
res_5c$avepp
res_5c$app_matrix
res_5c$min_offdiag

m5 <- as.data.frame(res_5c$app_matrix)

res_4c <- analyze_lg_model_ws("Model4.txt")
res_4c$avepp
res_4c$app_matrix
res_4c$min_offdiag

m4 <- as.data.frame(res_4c$app_matrix)