# Load data

source(here::here("code","01_load_data.R"))

# Set theme

gtsummary::theme_gtsummary_compact()

# Create table

patientchars <- PWASL %>% 
  dplyr::select(age, sex, handedness, education, 
                stroke_type, stroke_laterality, stroke_history, stroke_area,
                stroke_size, old_lesion_load, NIHSS_total, 
                volume_LHC, volume_RHC, CD_LHC, CD_RHC, 
                ScreeLing_1_tot, ScreeLing_1_dpo, 
                ANTAT_2_A, ANTAT_2_dpo, ASL, VSL, VML, 
                ANTAT_3_A, ANTAT_3_dpo) %>%
  mutate_if(is.character, ~factor(.)) %>%
  gtsummary::tbl_summary(type = list(c(sex, handedness, stroke_type, stroke_laterality, stroke_history) ~ "dichotomous"),
                         value = list(sex ~ "female", handedness ~ "right-handed", stroke_type ~ "ischemia", stroke_laterality ~ "left", stroke_history ~ "yes"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})_{median} ({min} - {max})",
                                          all_categorical() ~ "{n}"),
                         digits = list(c(age, education) ~ 1, c(NIHSS_total, ScreeLing_1_dpo, ANTAT_2_dpo, ANTAT_3_dpo) ~ 0, c(ScreeLing_1_tot, ANTAT_2_A, ANTAT_3_A, VSL, VML, stroke_size, old_lesion_load) ~ 2), 
                         label  = list(age ~ "Age (years)",
                                       sex ~ "Sex (female/male)",
                                       handedness ~ "Handedness (right-handed/other)",
                                       education ~ "Education (years)",
                                       stroke_type ~ "Stroke type (ischemia/hemorrhage)",
                                       stroke_laterality ~ "Stroke laterality (left/bilateral)",
                                       stroke_history ~ "History of stroke (yes/no)",
                                       stroke_size ~ "Acute lesion volume (cc)", 
                                       old_lesion_load ~ "Old lesion load (cc)",
                                       ASL ~ "Subacute auditory SL score (/32)",
                                       volume_LHC ~ "Acute volume left hippocampus (mm^3^)", 
                                       volume_RHC ~ "Acute volume right hippocampus (mm^3^)",
                                       CD_LHC ~ "Acute FBC left hippocampus (a.u.)",
                                       CD_RHC ~ "Acute FBC right hippocampus (a.u.)",
                                       ScreeLing_1_tot ~ "Initial language score (/72)",
                                       ANTAT_2_A ~ "Subacute functional language outcome (/50)", 
                                       ANTAT_3_A ~ "Chronic functional language outcome (/50)")) %>%
  modify_footnote(all_stat_cols() ~ "Descriptives printed for continuous variables: M (SD); Descriptives printed for categorical variables: n. Note: NIHSS = National Institutes of Health Stroke Scale (a higher score corresponds to a more severe stroke), FBC = Fiber Bundle Capacity, SL = statistical learning") %>%
  modify_header(list(label ~ "**Variable**")) %>%
  bold_labels() %>%
  gtsummary::as_tibble() %>%
  dplyr::rename(Variable=1) %>%
  dplyr::rename(N=2) %>%
  separate(N, into=c("N","Median"), sep="_") %>% 
  filter(!Variable %in% "Unknown") %>%
  mutate(N = ifelse(Variable == "__Sex (female/male)__", paste0(N,"/",33-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__Handedness (right-handed/other)__", paste0(N,"/",33-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__Stroke type (ischemia/hemorrhage)__", paste0(N,"/",33-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__Stroke laterality (left/bilateral)__", paste0(N,"/",33-as.numeric(N)),N)) %>%
  mutate(N = ifelse(Variable == "__History of stroke (yes/no)__", paste0(N,"/",33-as.numeric(N)),N)) %>%
  add_row(.after = 8, Variable = "ACM/ACP/Avert/Abas/AchorA/multifocal", N = "22/5/1/1/1/3", Median = "") %>%
  filter(!Variable %in% c("arteria cerebri media","arteria cerebri posterior","arteria vertebralis",
                          "arteria basilaris","anterior choroidal artery","multifocal")) %>%
  mutate(Unknown = as.factor(ifelse(Variable=="__Education (years)__",1, 
                                    ifelse(Variable=="__Subacute days post stroke__",1, 
                                     ifelse(Variable=="__Initial language score (/72)__",2, 
                                      ifelse(Variable=="__Subacute functional language outcome (/50)__",1, 
                                       ifelse(Variable=="__Chronic functional language outcome (/50)__",3, 
                                        ifelse(Variable=="__Subacute visual SL score (/32)__",3, 
                                         ifelse(Variable=="__Subacute auditory SL score (/32)__",5, 
                                          ifelse(Variable=="__Subacute visuomotor SL score (z-score difference sequence-random)__",1, 
                                           ifelse(Variable=="__Acute volume left hippocampus (mm^3^)__",4, #if >10% damage
                                            ifelse(Variable=="__Acute volume right hippocampus (mm^3^)__",1, #if >10% damage
                                             ifelse(Variable=="__Acute FBC left hippocampus (a.u.)__",5, 
                                              ifelse(Variable=="__Acute FBC right hippocampus (a.u.)__",2, NA)))))))))))))) %>%
  dplyr::rename(`N = 33`=2) %>%
  dplyr::rename(`Median (Range)`=3) %>%
  dplyr::rename(`Missing values (N)` = Unknown) %>%
  mutate(Variable = recode_factor(Variable, "__Education (years)__" = "Education (years)",
                                  "__Age (years)__" = "Age (years)", 
                                  "__stroke_area__" = "Affected circulation area",
                                  "__Sex (female/male)__" = "Sex (female/male)", 
                                  "__Handedness (right-handed/other)__" = "Handedness (right-handed/other)",
                                  "__Stroke type (ischemia/hemorrhage)__" = "Stroke type (ischemia/hemorrhage)", 
                                  "__Stroke laterality (left/bilateral)__" = "Stroke laterality (left/bilateral)", 
                                  "__Acute lesion volume (cc)__" = "Acute lesion volume (cm3)",
                                  "__Old lesion load (cc)__" = "Old lesion load (cm3)",
                                  "__History of stroke (yes/no)__" = "History of stroke (yes/no)",
                                  "__Acute NIHSS total score__" = "Acute NIHSS total score",
                                  "__Acute volume left hippocampus (mm^3^)__" = "Acute volume left hippocampus (mm3)",
                                  "__Acute volume right hippocampus (mm^3^)__" = "Acute volume right hippocampus (mm3)",
                                  "__Acute FBC left hippocampus (a.u.)__" = "Acute FBC left hippocampus (a.u.)",
                                  "__Acute FBC right hippocampus (a.u.)__" = "Acute FBC right hippocampus (a.u.)",
                                  "__Initial language score (/72)__" = "Initial language score (/72)",
                                  "__Acute days post stroke__" = "Acute days post stroke",
                                  "__Subacute functional language outcome (/50)__" = "Subacute functional language outcome (/50)",
                                  "__Subacute days post stroke__" = "Subacute days post stroke",
                                  "__Subacute visual SL score (/32)__" = "Subacute visual SL score (/32)",
                                  "__Subacute auditory SL score (/32)__" = "Subacute auditory SL score (/32)",
                                  "__Subacute visuomotor SL score (z-score difference sequence-random)__" = "Subacute visuomotor SL score",
                                  "__Chronic functional language outcome (/50)__" = "Chronic functional language outcome (/50)",
                                  "__Chronic days post stroke__" = "Chronic days post stroke"))

table_patientchars <- flextable(patientchars) %>% 
  autofit() %>%
  theme_booktabs(bold_header = T) %>%
  flextable::footnote(part="header",
                      i=1,j=2,
                      value=as_paragraph("N is reported for categorical variables; M (SD) is reported for continuous variables"),
                      ref_symbols = "a") %>%
  flextable::footnote(part="header",
                      i=1,j=4,
                      value=as_paragraph("N indicates the number of participants for which the corresponding data are missing"),
                      ref_symbols = "b") %>%
  flextable::footnote(part="body",
                      i=17,j=1,
                      value=as_paragraph("as measured by the ScreeLing (Doesborgh et al., 2003)"),
                      ref_symbols = "c") %>%
  flextable::footnote(part="body",
                      i=c(19,24),j=1,
                      value=as_paragraph("as measured by the Amsterdam-Nijmegen Everyday Language Test, A-scale (Blomert et al., 1994)"),
                      ref_symbols = "d") %>%
  flextable::footnote(part="body",
                      i=23,j=1,
                      value=as_paragraph("z-score difference sequence-random"),
                      ref_symbols = "e") %>%
  flextable::footnote(part="body",
                      i=2,j=2,
                      value=as_paragraph("Note. ACM = arteria cerebri media, ACP = arteria cerebri posterior, Avert = arteria vertebralis, Abas = arteria basilaris,  AchorA = anterior choroidal artery, NIHSS = National Institutes of Health Stroke Scale (a higher score corresponds to a more severe stroke), FBC = Fiber Bundle Capacity, SL = statistical learning."),
                      ref_symbols = "") %>%
  align(i=1:25, j=2:4, align = "center", part="body") %>%
  align(j=2:4, align = "center", part="header") %>%
  set_table_properties(layout = "autofit") %>%
  compose(part="body", i=13, j=1, value=as_paragraph("Acute volume left hippocampus (mm", as_sup("3"), ")")) %>%
  compose(part="body", i=14, j=1, value=as_paragraph("Acute volume right hippocampus (mm", as_sup("3"), ")")) %>%
  compose(part="body", i=10, j=1, value=as_paragraph("Acute lesion volume (cm", as_sup("3"), ")")) %>%
  compose(part="body", i=11, j=1, value=as_paragraph("Old lesion load (cm", as_sup("3"), ")"))

table_patientchars