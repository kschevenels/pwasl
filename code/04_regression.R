# Load data

source(here::here("code","01_load_data.R"))
load(here::here("data", "PWASL_complete.RData"))

var_label(PWASL$sex) <- "Sex"

# Bayesian regression analyses

## Predictive model for short-term language recovery 

### Step 1: Traditional short-term model

weighted_posterior_t2 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_2_A ~ ScreeLing_1_tot + stroke_size, data = dataset)
  as.data.frame.matrix(summary(weighted_posteriors(model, iterations = 10000)))
})

mod_basic_t2 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_2_A ~ ScreeLing_1_tot + stroke_size, data = dataset)
  bf <- bf_inclusion(model)
  Predictor = rownames(bf)
  BFincl = exp(bf$log_BF)
  data.frame(Predictor, BFincl)
})

BFincl_basic_t2 <- data.frame(do.call(rbind, mod_basic_t2), row.names = NULL) %>% dplyr::group_by(Predictor) %>% 
  summarise("BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren")) %>%
  mutate(Predictor = unlist(var_label(PWASL[Predictor]))) #summary dataframe

r2_t2 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_2_A ~ ScreeLing_1_tot + stroke_size, data = dataset)
  r2_bayes(model, average = T)
})

r2_t2 <- data.frame(do.call(rbind, r2_t2), row.names = NULL) %>%
  mutate(R2_Bayes = as.numeric(R2_Bayes)) %>%
  summarise(R2 = mean_sd(R2_Bayes, denote_sd = "paren")) 

save(weighted_posterior_t2, mod_basic_t2, BFincl_basic_t2, r2_t2, file = here("data", "basict2.RData"))

### Step 2: Other predictors

predictors_noi_t2 = c("ANTAT_2_dpo", "old_lesion_load", "age", "sex", "education", "NIHSS_total", "NIHSS_language", "eTIV")

mod_noi_t2 <- lapply(PWASL_complete, function(dataset){ #across the 10 datasets and predictors, calculate BF incl
  lapply(predictors_noi_t2, function(predictor){ #loop across predictors
    formula = as.formula(paste0("ANTAT_2_A ~ ScreeLing_1_tot + stroke_size + ", predictor)) #traditional model
    data = dataset %>% drop_na(predictor)
    model <- generalTestBF(formula, data)
    bf <- bf_inclusion(model)
    Predictor = rownames(bf)
    BFincl = exp(bf$log_BF)
    data.frame(Predictor, BFincl)
  })
})

BFincl_noi_t2 <- data.frame() #get the mean and sd across the 10 datasets
for (i in 1:length(predictors_noi_t2)){
  x <- data.frame(do.call(rbind, do.call(rbind, mod_noi_t2)[,i]), row.names = NULL) # ith element of each sublist
  y <- x %>% dplyr::group_by(Predictor) %>% summarise("BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren"))
  BFincl_noi_t2 <- rbind(BFincl_noi_t2, y)
}

BFincl_noi_t2 <- BFincl_noi_t2 %>% filter(!(Predictor %in% c("ScreeLing_1_tot", "stroke_size"))) %>%
  mutate(Predictor = unlist(var_label(PWASL[Predictor]))) #summary dataframe

### Step 3: Hippocampal predictors

predictors_oi = c("volume_LHC", "volume_RHC", "CD_LHC", "CD_RHC")

mod_coi_t2 <- lapply(PWASL_complete, function(dataset){ #across the 10 datasets and predictors, calculate BF incl
  lapply(predictors_oi, function(predictor){ #loop across predictors
    formula = as.formula(paste0("ANTAT_2_A ~ ScreeLing_1_tot + stroke_size + ", predictor)) # traditional model 
    data = dataset %>% drop_na(predictor)
    model <- generalTestBF(formula, data)
    bf <- bf_inclusion(model)
    Predictor = rownames(bf)
    BFincl = exp(bf$log_BF)
    data.frame(Predictor, BFincl)
  })
})

BFincl_coi_t2 <- data.frame() #get the mean and sd across the 10 datasets
  for (i in 1:length(predictors_oi)){ 
    x <- data.frame(do.call(rbind, do.call(rbind, mod_coi_t2)[,i]), row.names = NULL) #ith element of each sublist
    y <- x %>% dplyr::group_by(Predictor) %>% summarise("BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren"))
    BFincl_coi_t2 <- rbind(BFincl_coi_t2, y)
  }

BFincl_coi_t2 <- BFincl_coi_t2 %>% filter(!(Predictor %in% c("ScreeLing_1_tot", "stroke_size"))) %>%
                 mutate(Predictor = unlist(var_label(PWASL[Predictor]))) #summary dataframe

### Final short-term model (idem to traditional model)

bf_inclusion_t2 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_2_A ~ ScreeLing_1_tot + stroke_size, data = dataset)
  bf_inclusion(model)
})

save(mod_noi_t2, BFincl_noi_t2, mod_coi_t2, BFincl_coi_t2, bf_inclusion_t2, file = here("data", "finalt2.RData"))

## Predictive model for long-term language recovery 

### Step 1: Traditional long-term model 

basic_weighted_posterior_t3 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_3_A ~ ScreeLing_1_tot + stroke_size, data = dataset)
  as.data.frame.matrix(summary(weighted_posteriors(model, iterations = 10000)))
})

mod_basic_t3 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_3_A ~ ScreeLing_1_tot + stroke_size, data = dataset)
  bf <- bf_inclusion(model)
  Predictor = rownames(bf)
  BFincl = exp(bf$log_BF)
  data.frame(Predictor, BFincl)
})

BFincl_basic_t3 <- data.frame(do.call(rbind, mod_basic_t3), row.names = NULL) %>% dplyr::group_by(Predictor) %>% 
  summarise("BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren")) %>%
  mutate(Predictor = unlist(var_label(PWASL[Predictor]))) #summary dataframe

basic_r2_t3 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_3_A ~ ScreeLing_1_tot + stroke_size, data = dataset)
  r2_bayes(model, average = T)
})

basic_r2_t3 <- data.frame(do.call(rbind, basic_r2_t3), row.names = NULL) %>%
  mutate(R2_Bayes = as.numeric(R2_Bayes)) %>%
  summarise(R2 = mean_sd(R2_Bayes, denote_sd = "paren")) 

save(basic_weighted_posterior_t3, mod_basic_t3, BFincl_basic_t3, basic_r2_t3, file = here("data", "basict3.RData"))

### Step 2: Other predictors

predictors_noi_t3 = c("ANTAT_3_dpo", "old_lesion_load", "age", "sex", "education", "NIHSS_total", "NIHSS_language", "eTIV")

mod_noi_t3 <- lapply(PWASL_complete, function(dataset){ #across the 10 datasets and predictors, calculate BF incl
  lapply(predictors_noi_t3, function(predictor){ #loop across predictors
    formula = as.formula(paste0("ANTAT_3_A ~ ScreeLing_1_tot + stroke_size +", predictor)) #traditional model
    data = dataset %>% drop_na(predictor)
    model <- generalTestBF(formula, data)
    bf <- bf_inclusion(model)
    Predictor = rownames(bf)
    BFincl = exp(bf$log_BF)
    data.frame(Predictor, BFincl)
  })
})

BFincl_noi_t3 <- data.frame() #get the mean and sd across the 10 datasets
for (i in 1:length(predictors_noi_t3)){ 
  x <- data.frame(do.call(rbind, do.call(rbind, mod_noi_t3)[,i]), row.names = NULL) #ith element of each sublist
  y <- x %>% dplyr::group_by(Predictor) %>% summarise("BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren"))
  BFincl_noi_t3 <- rbind(BFincl_noi_t3, y)
}

BFincl_noi_t3 <- BFincl_noi_t3 %>% filter(!(Predictor %in% c("ScreeLing_1_tot", "stroke_size"))) %>%
  mutate(Predictor = unlist(var_label(PWASL[Predictor]))) #summary dataframe

### Step 3: Hippocampal predictors

mod_coi_t3 <- lapply(PWASL_complete, function(dataset){ #across the 10 datasets and predictors, calculate BF incl
  lapply(predictors_oi, function(predictor){ #loop across predictors
    formula = as.formula(paste0("ANTAT_3_A ~ ScreeLing_1_tot + stroke_size +", predictor)) #traditional model 
    data = dataset %>% drop_na(predictor)
    model <- generalTestBF(formula, data)
    bf <- bf_inclusion(model)
    Predictor = rownames(bf)
    BFincl = exp(bf$log_BF)
    data.frame(Predictor, BFincl)
  })
})

BFincl_coi_t3 <- data.frame() #get the mean and sd across the 10 datasets
  for (i in 1:length(predictors_oi)){ 
    x <- data.frame(do.call(rbind, do.call(rbind, mod_coi_t3)[,i]), row.names = NULL) #ith element of each sublist
    y <- x %>% dplyr::group_by(Predictor) %>% summarise("BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren"))
    BFincl_coi_t3 <- rbind(BFincl_coi_t3, y)
  }

BFincl_coi_t3 <- BFincl_coi_t3 %>% filter(!(Predictor %in% c("ScreeLing_1_tot", "stroke_size"))) %>%
  mutate(Predictor = unlist(var_label(PWASL[Predictor]))) #summary dataframe

### Extra analysis 

mod_HC_t3 <- lapply(PWASL_complete, function(dataset){ #across the 10 datasets and predictors, calculate BF incl
  lapply(predictors_noi_t3, function(predictor){ #loop across predictors
    formula = as.formula(paste0("ANTAT_3_A ~ ScreeLing_1_tot + stroke_size + volume_LHC +", predictor)) #new model
    data = dataset %>% drop_na(predictor, volume_LHC)
    model <- generalTestBF(formula, data)
    bf <- bf_inclusion(model)
    Predictor = rownames(bf)
    BFincl = exp(bf$log_BF)
    data.frame(Predictor, BFincl)
  })
})

BFincl_HC_t3 <- data.frame() #get the mean and sd across the 10 datasets
for (i in 1:length(predictors_noi_t3)){ 
  x <- data.frame(do.call(rbind, do.call(rbind, mod_HC_t3)[,i]), row.names = NULL) #ith element of each sublist
  y <- x %>% dplyr::group_by(Predictor) %>% summarise("BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren"))
  BFincl_HC_t3 <- rbind(BFincl_HC_t3, y)
}

BFincl_HC_t3 <- BFincl_HC_t3 %>% filter(!(Predictor %in% c("ScreeLing_1_tot", "stroke_size"))) %>%
  mutate(Predictor = unlist(var_label(PWASL[Predictor]))) #summary dataframe

### Final long-term model

weighted_posterior_t3 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_3_A ~ ScreeLing_1_tot + stroke_size + volume_LHC, data = dataset %>% drop_na(volume_LHC))
  as.data.frame.matrix(summary(weighted_posteriors(model, iterations = 10000)))
})

bf_inclusion_t3 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_3_A ~ ScreeLing_1_tot + stroke_size + volume_LHC, data = dataset %>% drop_na(volume_LHC))
  bf_inclusion(model)
})

final_r2_t3 <- lapply(PWASL_complete, function(dataset){
  model <- generalTestBF(ANTAT_3_A ~ ScreeLing_1_tot + stroke_size + volume_LHC, data = dataset %>% drop_na(volume_LHC))
  r2_bayes(model, average = T)
})

final_r2_t3 <- data.frame(do.call(rbind, final_r2_t3), row.names = NULL) %>%
  mutate(R2_Bayes = as.numeric(R2_Bayes)) %>%
  summarise(R2 = mean_sd(R2_Bayes, denote_sd = "paren"))

save(mod_noi_t3, BFincl_noi_t3, mod_coi_t3, BFincl_coi_t3, mod_HC_t3, BFincl_HC_t3, weighted_posterior_t3, bf_inclusion_t3, final_r2_t3, file = here("data", "finalt3.RData"))

# Assumption check (check_model(lm))

# 1. There must be a linear relationship between the outcome variable and the independent variables. Scatterplots can show whether there is a linear or curvilinear relationship. Also check for outliers. OK
# 2. Multivariate Normality. Multiple regression assumes that the residuals are normally distributed. Q-Q plot, histogram of residuals or Kolmogorov-Smirnov/Shapiro-Wilk test on the residuals. OK
# 3. No Multicollinearity. Multiple regression assumes that the independent variables are not highly correlated with each other. This assumption is tested using Variance Inflation Factor (VIF) values. Or check correlations between IVs (should be < .8). OK
# 4. Homoscedasticity. This assumption states that the variance of error terms are similar across the values of the independent variables.  A plot of standardized residuals versus predicted values can show whether points are equally distributed across all values of the independent variables (no clear pattern)/Breush Pagan Test (bptest)/NCV Test. OK 