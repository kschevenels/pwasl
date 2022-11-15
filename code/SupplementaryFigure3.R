# Load data

source(here::here("code","01_load_data.R"))

# Check assumptions

shapiro.test(PWASL$ScreeLing_1_tot) 
shapiro.test(PWASL$stroke_size) 
shapiro.test(PWASL$volume_LHC) 
shapiro.test(PWASL$volume_RHC) 
shapiro.test(PWASL$CD_LHC) 
shapiro.test(PWASL$CD_RHC) 
shapiro.test(PWASL$ANTAT_2_A) 
shapiro.test(PWASL$ANTAT_3_A) 
shapiro.test(PWASL$old_lesion_load) 
shapiro.test(PWASL$age) 
shapiro.test(PWASL$education) 
shapiro.test(PWASL$NIHSS_total) 
shapiro.test(PWASL$NIHSS_language) 

# Plot big correlation matrix

allcor <- ggcorrmat(
  data = PWASL,
  cor.vars = c ("ScreeLing_1_tot", "stroke_size", "volume_LHC", "volume_RHC", "CD_LHC", "CD_RHC", "ASL", "VSL", "VML", "ANTAT_2_A", "ANTAT_3_A", "old_lesion_load", "age", "education", "NIHSS_total", "NIHSS_language"), 
  cor.vars.names = c("Initial ScreeLing score", "Acute lesion volume", "Acute volume left hippocampus", "Acute volume right hippocampus", "Acute FBC left hippocampus", "Acute FBC right hippocampus", "Auditory statistical learning", "Visual statistical learning", "Visuomotor statistical learning", "Short-term ANELT outcome", "Long-term ANELT outcome", "Old lesion load", "Age", "Years of education", "Acute NIHSS total score", "Acute NIHSS language score"),
  type = "nonparametric",
  ggtheme = ggplot2::theme_light(), 
  colors = c(cbbPalette[8], "white", cbbPalette[6]),
  partial = F, 
  p.adjust.method = "none",
  sig.level = 0.01,
  caption = "For description of the variables, we refer to the main text of the study. Note: Higher NIHSS scores correspond to a more severe stroke."
)

ggsave(filename=here("figs","Supplementary_Figure3.pdf"), allcor, width=11, height=8.5, dpi=600)
