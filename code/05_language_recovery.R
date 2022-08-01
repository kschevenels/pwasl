# Load data

source(here::here("code","01_load_data.R"))

# Calculate achieved, potential and factual recovery for both time points 

PWASL <- PWASL %>% mutate(potential_recovery_ScreeLing = 72 - ScreeLing_1_tot, 
         potential_recovery_ANTAT = 50 - ANTAT_1_A,
         achieved_recovery_subacute_ScreeLing = ScreeLing_2_tot - ScreeLing_1_tot,
         achieved_recovery_subacute_ANTAT = ANTAT_2_A - ANTAT_1_A,
         factual_recovery_subacute_ScreeLing = achieved_recovery_subacute_ScreeLing/potential_recovery_ScreeLing,
         factual_recovery_subacute_ANTAT = achieved_recovery_subacute_ANTAT/potential_recovery_ANTAT,
         achieved_recovery_chronic_ScreeLing = ScreeLing_3_tot - ScreeLing_1_tot, 
         achieved_recovery_chronic_ANTAT = ANTAT_3_A - ANTAT_1_A,
         factual_recovery_chronic_ScreeLing = achieved_recovery_chronic_ScreeLing/potential_recovery_ScreeLing,
         factual_recovery_chronic_ANTAT = achieved_recovery_chronic_ANTAT/potential_recovery_ANTAT)

# How much of the potential recovery was reached on average at each time point?

factual_t2_ScreeLing <- PWASL %>% summarize(mean = mean(factual_recovery_subacute_ScreeLing, na.rm = TRUE)) 
factual_t2_ANTAT <- PWASL %>% summarize(mean = mean(factual_recovery_subacute_ANTAT, na.rm = TRUE)) 
factual_t3_ScreeLing <- PWASL %>% summarize(mean = mean(factual_recovery_chronic_ScreeLing, na.rm = TRUE)) 
factual_t3_ANTAT <- PWASL %>% summarize(mean = mean(factual_recovery_chronic_ANTAT, na.rm = TRUE)) 

# Correlation between achieved and potential recovery at each time point?

shapiro.test(PWASL$potential_recovery_ANTAT) # not normally distributed 
shapiro.test(PWASL$potential_recovery_ScreeLing) # not normally distributed

recov_subacute_ScreeLing <- corr.test(PWASL$potential_recovery_ScreeLing, PWASL$achieved_recovery_subacute_ScreeLing, method = c("spearman"))
recov_subacute_ANTAT <- corr.test(PWASL$potential_recovery_ANTAT, PWASL$achieved_recovery_subacute_ANTAT, method = c("spearman"))

recov_chronic_ScreeLing <- corr.test(PWASL$potential_recovery_ScreeLing, PWASL$achieved_recovery_chronic_ScreeLing, method = c("spearman"))
recov_chronic_ANTAT <- corr.test(PWASL$potential_recovery_ANTAT, PWASL$achieved_recovery_chronic_ANTAT, method = c("spearman"))

# Significant improvement between time points? 

shapiro.test(PWASL$ScreeLing_t2t1_tot) # not normally distributed
shapiro.test(PWASL$ScreeLing_t3t1_tot) # not normally distributed
shapiro.test(PWASL$ScreeLing_t3t2_tot) # not normally distributed
shapiro.test(PWASL$ANTAT_t2t1_A) 
shapiro.test(PWASL$ANTAT_t3t1_A) 
shapiro.test(PWASL$ANTAT_t3t2_A) 

ScreeLing_t2t1 <- wilcox.test(PWASL$ScreeLing_t2t1_tot, alternative=c("greater"), mu=8.5) 
SL_eff_t2t1 <- PWASL %>% wilcox_effsize(ScreeLing_t2t1_tot ~ 1, alternative = "greater", mu = 8.5) 

ScreeLing_t3t1 <- wilcox.test(PWASL$ScreeLing_t3t1_tot, alternative=c("greater"), mu=8.5) 
SL_eff_t3t1 <- PWASL %>% wilcox_effsize(ScreeLing_t3t1_tot ~ 1, mu = 8.5, alternative=c("greater"))

ScreeLing_t3t2 <- wilcox.test(PWASL$ScreeLing_t3t2_tot, alternative=c("greater"), mu=8.5) 
SL_eff_t3t2 <- PWASL %>% wilcox_effsize(ScreeLing_t3t2_tot ~ 1, mu = 8.5, alternative=c("greater")) 

ANTAT_t2t1_A <- t.test(PWASL$ANTAT_t2t1_A, alternative=c("greater"), mu=7) 
ANTAT_t3t1_A <- t.test(PWASL$ANTAT_t3t1_A, alternative=c("greater"), mu=7) 
ANTAT_t3t2_A <- t.test(PWASL$ANTAT_t3t2_A, alternative=c("greater"), mu=7) 

# critical difference ANTAT A = 7 points 
# critical difference ScreeLing = 8.5 points 
