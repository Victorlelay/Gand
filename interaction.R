# Création de variables (motifs racisme)

# Foreign Markers
indiv_immi <- indiv_immi %>% mutate(For_Mark = case_when(
  d_nom==1|d_accent==1 ~ 1,
  TRUE ~ 0
))

# Skin color or physical appearance
indiv_immi <- indiv_immi %>% mutate(skin_phyap = case_when(
  d_peau==1|d_apphy==1 ~ 1,
  TRUE ~ 0
))

# Origin/Nationality
indiv_immi <- indiv_immi %>% mutate(ori_nat = case_when(
  d_natio==1 ~ 1,
  TRUE ~ 0
))

# Religion
indiv_immi <- indiv_immi %>% mutate(relig = case_when(
  d_relig==1 ~ 1,
  TRUE ~ 0
))

# Other
indiv_immi <- indiv_immi %>% mutate(other = case_when(
  d_racaut==1 ~ 1,
  TRUE ~ 0
))

############################################################
# Foreigness/ori + skin color
indiv_immi <- indiv_immi %>% mutate(ori_skin = case_when(
  For_Mark==1 & skin_phyap==1 ~ 1,
  TRUE ~ 0
))

# Foreigness/ori + religion
indiv_immi <- indiv_immi %>% mutate(ori_skin = case_when(
  For_Mark==1 & relig==1 ~ 1,
  TRUE ~ 0
))

# skin color + religion
indiv_immi <- indiv_immi %>% mutate(ori_skin = case_when(
  relig==1 & skin_phyap==1 ~ 1,
  TRUE ~ 0
))

# Foreigness/ori + skin color
indiv_immi <- indiv_immi %>% mutate(ori_skin = case_when(
  For_Mark==1 & skin_phyap==1 ~ 1,
  TRUE ~ 0
))