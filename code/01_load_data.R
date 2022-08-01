# Load libraries --------------------------------------------------------------

# Assuming these packages are already locally installed
library(here)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
library(captioner)
library(xlsx)
library(readr)
library(XML)
library(sjmisc)
library(BayesFactor)
library(bayestestR)
library(ggstatsplot)
library(gtsummary)
library(flextable)
library(labelled)
library(mice)
library(VIM)
library(dplyr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(officer)
library(vctrs)
library(qwraps2)
library(matrixStats)
library(car)
library(psych)
library(ppcor)
library(magick)
library(performance)
library(tibble)
library(lsr)
library(cowplot)
library(rstatix)
library(papaja)

# Helper functions --------------------------------------------------------

f_pvalue = function(p.value, symbol = " = "){
  p.value <- round(p.value, digits = 2)
  if (p.value == 0) {
    return("< .001")
  } else {
    return(paste0(symbol, round(p.value, digits = 2)))
  }
}

numformat <- function(x, digits = 2) { 
  ncode <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rms <- function(x1, x2, x3){
  sqrt(((x1^2)+(x2^2)+(x3^2))/3)
}

# Load data --------------------------------------------------------------

PWASL <- read.csv(here("data", "pwasl_tidy.csv"), header = TRUE, sep = ",", dec = ".", check.names=T)
HASL <- read.csv(here("data", "hasl_tidy.csv"), header = TRUE, sep = ",", dec = ".", check.names=T)

# Create value labels
var_label(PWASL) <- list(
  ScreeLing_1_tot = "Initial language score", ANTAT_2_A = "Subacute functional language outcome", ANTAT_3_A = "Chronic functional language outcome",
  volume_LHC = "Volume left hippocampus", volume_RHC = "Volume right hippocampus",
  residual_LHC = "Residual volume left hippocampus", residual_RHC = "Residual volume right hippocampus",
  CD_LHC = "FBC left hippocampus", CD_RHC = "FBC right hippocampus",
  ScreeLing_1_dpo = "Acute days post stroke", ANTAT_2_dpo = "Subacute days post stroke", ANTAT_3_dpo = "Chronic days post stroke",
  VSL = "Subacute visual SL score (/32)", VML = "Subacute visuomotor SL score (z-score difference sequence-random)",
  stroke_size = "Acute lesion volume", old_lesion_load = "Old lesion load", eTIV = "Estimated total intracranial volume",
  age = "Age", sex = "Sex", handedness = "Handedness", education = "Education", 
  NIHSS_total = "Acute NIHSS total score", NIHSS_language = "Acute NIHSS language score", 
  stroke_area = "Affected circulation area", stroke_type = "Stroke type", 
  stroke_laterality = "Stroke laterality", stroke_history = "History of stroke"
)

PWASL <- PWASL %>% mutate_if(is.character, ~factor(.)) %>% mutate(patient = as.character(patient))
HASL <- HASL %>% mutate_if(is.character, ~factor(.)) %>% mutate(subject = as.character(subject))
