library(questionr)
library(dplyr)
library(tidyr)


# Multi ethnoracial
indiv_immi<-indiv_immi %>% mutate(multiple_ethnoracial=case_when(
  Discri_age + Discri_genre + Discri_peau + Discri_orig + Discri_quart + Discri_accent + Discri_fami + Discri_orientsex + Discri_relig + Discri_habi + Discri_autre + Discri_handi + Discri_poids == 0 ~ 0,
  Discri_orig==1 & Discri_relig + Discri_peau == 0 ~ 1,
  Discri_peau==1 & Discri_orig + Discri_accent+ Discri_quart + Discri_relig == 0 ~2,
  Discri_relig==1 & Discri_orig + Discri_accent + Discri_quart + Discri_peau == 0 ~3,
  Discri_orig + Discri_peau == 2 & Discri_relig==0 ~ 4,
  Discri_orig + Discri_relig == 2 & Discri_peau == 0 ~5,
  Discri_peau + Discri_relig == 2 & Discri_orig + Discri_accent + Discri_quart == 0 ~6,
  (Discri_orig ==1 | Discri_accent==1 | Discri_quart==1) & Discri_relig + Discri_peau == 2 ~7,
  TRUE ~ 8
))

# On crée la table
# On fait une table par modalité
indiv_immi0 <- indiv_immi %>% filter(multiple_ethnoracial==0)
indiv_immi1 <- indiv_immi %>% filter(multiple_ethnoracial==1)
indiv_immi2 <- indiv_immi %>% filter(multiple_ethnoracial==2)
indiv_immi3 <- indiv_immi %>% filter(multiple_ethnoracial==3)
indiv_immi4 <- indiv_immi %>% filter(multiple_ethnoracial==4)
indiv_immi5 <- indiv_immi %>% filter(multiple_ethnoracial==5)
indiv_immi6 <- indiv_immi %>% filter(multiple_ethnoracial==6)
indiv_immi7 <- indiv_immi %>% filter(multiple_ethnoracial==7)
indiv_immi8 <- indiv_immi %>% filter(multiple_ethnoracial==8)


motifs <- c("indiv_immi0", "indiv_immi1", "indiv_immi2", "indiv_immi3", "indiv_immi4", "indiv_immi5", "indiv_immi6", "indiv_immi7", "indiv_immi8")
variables <- c("origine_tous_g2bis", "dip_rec", "religion_nomis", "sexee")

# Liste finale des tables combinées PAR indiv_immi
tables_combinees <- list()

for (df_name in motifs) {
  df <- get(df_name)
  resultats_var <- list()
  
  for (var in variables) {
    # Table non pondérée
    n_tab <- table(df[[var]], df$multiple_ethnoracial)
    df_n <- as.data.frame.matrix(n_tab)
    df_n$modalite <- rownames(df_n)
    df_n <- pivot_longer(df_n, -modalite, names_to = "motif", values_to = "n")
    
    # Table pondérée
    pond_tab <- wtd.table(df[[var]], df$multiple_ethnoracial, weights = df$poidsi)
    df_pct <- as.data.frame.matrix(round(prop.table(pond_tab, 2) * 100, 2))
    df_pct$modalite <- rownames(df_pct)
    df_pct <- pivot_longer(df_pct, -modalite, names_to = "motif", values_to = "pct")
    
    # Jointure sur modalite et motif
    tab <- left_join(df_n, df_pct, by = c("modalite", "motif"))
    
    # Ajout variable + source
    tab$variable <- var
    tab$source_df <- df_name
    
    resultats_var[[var]] <- tab
    
    # Test du chi²
    chi2_pval <- chisq.test(n_tab)$p.value
    assign(paste0("chi2_", var, "_", df_name), chi2_pval)
  }
  
  # Fusion verticale des résultats pour chaque dataframe
  tables_combinees[[df_name]] <- bind_rows(resultats_var)
}


table2 <- tables_combinees$indiv_immi2
write.csv(table2, "modalite22.csv", row.names = FALSE)

################################################################################################################################
# Multiple_ethnoracial

# popu générale ###

# Orig
orig_ME <- table(indiv_immi$multiple_ethnoracial, indiv_immi$origine_tous_g2bis)
orig_ME <- t(orig_ME)
orig_ME
write.csv(orig_ME, "morig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(orig_ME)$p.value

orign_pond <- wtd.table(indiv_immi$multiple_ethnoracial, indiv_immi$origine_tous_g2bis,weights = indiv_immi$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme
dip_ME <- table(indiv_immi$multiple_ethnoracial, indiv_immi$dip_rec)
dip_ME <- t(dip_ME)
dip_ME
write.csv(dip_ME, "dip_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(dip_ME)$p.value

dip_pond <- wtd.table(indiv_immi$multiple_ethnoracial, indiv_immi$dip_rec,weights = indiv_immi$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion
relig_ME <- table(indiv_immi$multiple_ethnoracial, indiv_immi$religion_nomis)
relig_ME <- t(relig_ME)
relig_ME
write.csv(relig_ME, "relig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(relig_ME)$p.value

relig_pond <- wtd.table(indiv_immi$multiple_ethnoracial, indiv_immi$religion_nomis,weights = indiv_immi$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe
sexe_ME <- table(indiv_immi$multiple_ethnoracial, indiv_immi$sexee)
sexe_ME <- t(sexe_ME)
sexe_ME
write.csv(sexe_ME, "sexe_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(sexe_ME)$p.value

sexe_pond <- wtd.table(indiv_immi$multiple_ethnoracial, indiv_immi$sexee,weights = indiv_immi$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(indiv_immi$multiple_ethnoracial,weights = indiv_immi$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)


# Discriminé.es ###

# Orig

orign_pond <- wtd.table(discri$multiple_ethnoracial, discri$origine_tous_g2bis,weights = discri$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme

dip_pond <- wtd.table(discri$multiple_ethnoracial, discri$dip_rec,weights = discri$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion

relig_pond <- wtd.table(discri$multiple_ethnoracial, discri$religion_nomis,weights = discri$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe

sexe_pond <- wtd.table(discri$multiple_ethnoracial, discri$sexee,weights = discri$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(discri$multiple_ethnoracial,weights = discri$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)



########################################################################################################################################
# Number_multiple_ethnoracial
indiv_immi<-indiv_immi %>% mutate(number_multiple_ethnoracial=case_when(
  multiple_ethnoracial==0~"No discri",
  multiple_ethnoracial>0&multiple_ethnoracial<4~"One discri.",
  multiple_ethnoracial>3&multiple_ethnoracial<8~"At least 2",
  multiple_ethnoracial==8~"Others"
))

# popu générale ###

# Orig
orig_ME <- table(indiv_immi$number_multiple_ethnoracial, indiv_immi$origine_tous_g2bis)
orig_ME <- t(orig_ME)
orig_ME
write.csv(orig_ME, "morig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(orig_ME)$p.value

orign_pond <- wtd.table(indiv_immi$number_multiple_ethnoracial, indiv_immi$origine_tous_g2bis,weights = indiv_immi$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme
dip_ME <- table(indiv_immi$number_multiple_ethnoracial, indiv_immi$dip_rec)
dip_ME <- t(dip_ME)
dip_ME
write.csv(dip_ME, "dip_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(dip_ME)$p.value
chi2_pval

dip_pond <- wtd.table(indiv_immi$number_multiple_ethnoracial, indiv_immi$dip_rec,weights = indiv_immi$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion
relig_ME <- table(indiv_immi$number_multiple_ethnoracial, indiv_immi$religion_nomis)
relig_ME <- t(relig_ME)
relig_ME
write.csv(relig_ME, "relig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(relig_ME)$p.value
chi2_pval

relig_pond <- wtd.table(indiv_immi$number_multiple_ethnoracial, indiv_immi$religion_nomis,weights = indiv_immi$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe
sexe_ME <- table(indiv_immi$number_multiple_ethnoracial, indiv_immi$sexee)
sexe_ME <- t(sexe_ME)
sexe_ME
write.csv(sexe_ME, "sexe_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(sexe_ME)$p.value
chi2_pval

sexe_pond <- wtd.table(indiv_immi$number_multiple_ethnoracial, indiv_immi$sexee,weights = indiv_immi$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(indiv_immi$number_multiple_ethnoracial,weights = indiv_immi$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)


# Discriminé.es ###

# Orig

orign_pond <- wtd.table(discri$number_multiple_ethnoracial, discri$origine_tous_g2bis,weights = discri$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme

dip_pond <- wtd.table(discri$number_multiple_ethnoracial, discri$dip_rec,weights = discri$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion

relig_pond <- wtd.table(discri$number_multiple_ethnoracial, discri$religion_nomis,weights = discri$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe

sexe_pond <- wtd.table(discri$number_multiple_ethnoracial, discri$sexee,weights = discri$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(discri$number_multiple_ethnoracial,weights = discri$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)




#########################################################################################################
# Indicatrices

# origine
indiv_immi <- indiv_immi %>% mutate(ethno_racial = case_when(
  Discri_accent+Discri_quart+Discri_orig>0 ~1,
  TRUE ~ 0
))

# peau
indiv_immi <- indiv_immi %>% mutate(skin_colour = case_when(
  Discri_peau>0 ~1,
  TRUE ~ 0
))


indiv_immi <- indiv_immi %>% mutate(religion = case_when(
  Discri_relig>0 ~1,
  TRUE ~ 0
))
####################################################################################

##### ETHNO_RACIAL

# popu générale ###

# Orig
orig_ME <- table(indiv_immi$ethno_racial, indiv_immi$origine_tous_g2bis)
orig_ME <- t(orig_ME)
orig_ME
write.csv(orig_ME, "morig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(orig_ME)$p.value
chi2_pval

orign_pond <- wtd.table(indiv_immi$ethno_racial, indiv_immi$origine_tous_g2bis,weights = indiv_immi$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme
dip_ME <- table(indiv_immi$ethno_racial, indiv_immi$dip_rec)
dip_ME <- t(dip_ME)
dip_ME
write.csv(dip_ME, "dip_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(dip_ME)$p.value
chi2_pval

dip_pond <- wtd.table(indiv_immi$ethno_racial, indiv_immi$dip_rec,weights = indiv_immi$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion
relig_ME <- table(indiv_immi$ethno_racial, indiv_immi$religion_nomis)
relig_ME <- t(relig_ME)
relig_ME
write.csv(relig_ME, "relig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(relig_ME)$p.value
chi2_pval

relig_pond <- wtd.table(indiv_immi$ethno_racial, indiv_immi$religion_nomis,weights = indiv_immi$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe
sexe_ME <- table(indiv_immi$ethno_racial, indiv_immi$sexee)
sexe_ME <- t(sexe_ME)
sexe_ME
write.csv(sexe_ME, "sexe_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(sexe_ME)$p.value
chi2_pval

sexe_pond <- wtd.table(indiv_immi$ethno_racial, indiv_immi$sexee,weights = indiv_immi$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(indiv_immi$ethno_racial,weights = indiv_immi$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)


# Discriminé.es ###

# Orig

orign_pond <- wtd.table(discri$ethno_racial, discri$origine_tous_g2bis,weights = discri$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme

dip_pond <- wtd.table(discri$ethno_racial, discri$dip_rec,weights = discri$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion

relig_pond <- wtd.table(discri$ethno_racial, discri$religion_nomis,weights = discri$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe

sexe_pond <- wtd.table(discri$ethno_racial, discri$sexee,weights = discri$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(discri$ethno_racial,weights = discri$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)

####################################################################################

##### SKIN_COLOUR

# popu générale ###

# Orig
orig_ME <- table(indiv_immi$skin_colour, indiv_immi$origine_tous_g2bis)
orig_ME <- t(orig_ME)
orig_ME
write.csv(orig_ME, "morig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(orig_ME)$p.value
chi2_pval

orign_pond <- wtd.table(indiv_immi$skin_colour, indiv_immi$origine_tous_g2bis,weights = indiv_immi$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme
dip_ME <- table(indiv_immi$skin_colour, indiv_immi$dip_rec)
dip_ME <- t(dip_ME)
dip_ME
write.csv(dip_ME, "dip_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(dip_ME)$p.value
chi2_pval

dip_pond <- wtd.table(indiv_immi$skin_colour, indiv_immi$dip_rec,weights = indiv_immi$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion
relig_ME <- table(indiv_immi$skin_colour, indiv_immi$religion_nomis)
relig_ME <- t(relig_ME)
relig_ME
write.csv(relig_ME, "relig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(relig_ME)$p.value
chi2_pval

relig_pond <- wtd.table(indiv_immi$skin_colour, indiv_immi$religion_nomis,weights = indiv_immi$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe
sexe_ME <- table(indiv_immi$skin_colour, indiv_immi$sexee)
sexe_ME <- t(sexe_ME)
sexe_ME
write.csv(sexe_ME, "sexe_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(sexe_ME)$p.value
chi2_pval

sexe_pond <- wtd.table(indiv_immi$skin_colour, indiv_immi$sexee,weights = indiv_immi$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(indiv_immi$skin_colour,weights = indiv_immi$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)


# Discriminé.es ###

# Orig

orign_pond <- wtd.table(discri$skin_colour, discri$origine_tous_g2bis,weights = discri$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme

dip_pond <- wtd.table(discri$skin_colour, discri$dip_rec,weights = discri$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion

relig_pond <- wtd.table(discri$skin_colour, discri$religion_nomis,weights = discri$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe

sexe_pond <- wtd.table(discri$skin_colour, discri$sexee,weights = discri$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(discri$skin_colour,weights = discri$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)

####################################################################################

##### RELIGION

# popu générale ###

# Orig
orig_ME <- table(indiv_immi$religion, indiv_immi$origine_tous_g2bis)
orig_ME <- t(orig_ME)
orig_ME
write.csv(orig_ME, "morig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(orig_ME)$p.value
chi2_pval

orign_pond <- wtd.table(indiv_immi$religion, indiv_immi$origine_tous_g2bis,weights = indiv_immi$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme
dip_ME <- table(indiv_immi$religion, indiv_immi$dip_rec)
dip_ME <- t(dip_ME)
dip_ME
write.csv(dip_ME, "dip_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(dip_ME)$p.value
chi2_pval

dip_pond <- wtd.table(indiv_immi$religion, indiv_immi$dip_rec,weights = indiv_immi$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion
relig_ME <- table(indiv_immi$religion, indiv_immi$religion_nomis)
relig_ME <- t(relig_ME)
relig_ME
write.csv(relig_ME, "relig_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(relig_ME)$p.value
chi2_pval

relig_pond <- wtd.table(indiv_immi$religion, indiv_immi$religion_nomis,weights = indiv_immi$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe
sexe_ME <- table(indiv_immi$religion, indiv_immi$sexee)
sexe_ME <- t(sexe_ME)
sexe_ME
write.csv(sexe_ME, "sexe_ME.csv", row.names = FALSE)
chi2_pval <- chisq.test(sexe_ME)$p.value
chi2_pval

sexe_pond <- wtd.table(indiv_immi$religion, indiv_immi$sexee,weights = indiv_immi$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(indiv_immi$religion,weights = indiv_immi$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)


# Discriminé.es ###

# Orig

orign_pond <- wtd.table(discri$religion, discri$origine_tous_g2bis,weights = discri$poidsi)
orig_pct <- round(prop.table(orign_pond, 2) * 100, 2)
orig_pct <- t(orig_pct)
orig_pct
write.csv(orig_pct, "orig_pct.csv", row.names = FALSE)

# Diplôme

dip_pond <- wtd.table(discri$religion, discri$dip_rec,weights = discri$poidsi)
dip_pct <- round(prop.table(dip_pond, 2) * 100, 2)
dip_pct <- t(dip_pct)
dip_pct
write.csv(dip_pct, "dip_pct.csv", row.names = FALSE)

# Religion

relig_pond <- wtd.table(discri$religion, discri$religion_nomis,weights = discri$poidsi)
relig_pct <- round(prop.table(relig_pond, 2) * 100, 2)
relig_pct <- t(relig_pct)
relig_pct
write.csv(relig_pct, "relig_pct.csv", row.names = FALSE)

# Sexe

sexe_pond <- wtd.table(discri$religion, discri$sexee,weights = discri$poidsi)
sexe_pct <- round(prop.table(sexe_pond, 2) * 100, 2)
sexe_pct <- t(sexe_pct)
sexe_pct
write.csv(sexe_pct, "sexe_pct.csv", row.names = FALSE)

# pct total
pond <- wtd.table(discri$religion,weights = discri$poidsi)
pct <- round(prop.table(pond) * 100, 2)
pct <- t(pct)
pct
write.csv(pct, "pct.csv", row.names = FALSE)