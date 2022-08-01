# Load data

source(here::here("code","01_load_data.R"))

# Create subset of data for missing data imputation

PWASL_miss <- PWASL %>% dplyr::select(sex, age, education, stroke_type, stroke_size, stroke_laterality, 
                                      stroke_history, old_lesion_load, NIHSS_total, NIHSS_language, 
                                      ScreeLing_1_tot, ANTAT_1_A, ScreeLing_2_tot, ANTAT_2_A, ScreeLing_3_tot, ANTAT_3_A)

not <- names(PWASL %>% dplyr::select(-patient, -sex, -age, -education, 
                                     -stroke_type, -stroke_size, -stroke_laterality, -stroke_history, -old_lesion_load, -NIHSS_total, -NIHSS_language, 
                                     -ScreeLing_1_sem, -ScreeLing_1_fon, -ScreeLing_1_synt, -ANTAT_1_A, 
                                     -ScreeLing_2_sem, -ScreeLing_2_fon, -ScreeLing_2_synt, -ANTAT_2_A, 
                                     -ScreeLing_3_sem, -ScreeLing_3_fon, -ScreeLing_3_synt, -ANTAT_3_A)) 

# PWASL <- PWASL %>% mutate(education = as.numeric(education), ScreeLing_1_sem = as.numeric(ScreeLing_1_sem), ScreeLing_1_synt = as.numeric(ScreeLing_1_synt),
#                  ScreeLing_t2t1_sem = as.numeric(ScreeLing_t2t1_sem), ScreeLing_t2t1_synt = as.numeric(ScreeLing_t2t1_synt), 
#                  ScreeLing_t3t1_sem = as.numeric(ScreeLing_t3t1_sem), ScreeLing_t3t1_synt = as.numeric(ScreeLing_t3t1_synt),
#                  ANTAT_1_dpo = as.numeric(ANTAT_1_dpo), ScreeLing_1_dpo = as.numeric(ScreeLing_1_dpo))

# Visualize missing data pattern

missing_pattern_plot <- aggr(PWASL_miss, col = c('skyblue', 'red'),
   numbers = TRUE, sortVars = TRUE, bars = TRUE, combined = TRUE,
   labels = names(PWASL_miss), cex.axis = .62, ylab = c("Missing data pattern"))

# Summarize the missing data pattern

fluxplot(PWASL_miss) #higher outflux = more powerful predictors <-> higher influx = depend strongly on the imputation model

# Create predictor matrix 

ini <- mice(PWASL, maxit=0, print=F) # to get initial predictor matrix
pred <- ini$pred # save as an object
pred[,c(not)] <- 0 # eliminate the influence of all variables in vector "not" in imputing missing values

# Create methods matrix

meth <- ini$method # save the imputation method per variable as an object
meth[c(not)] <- "" # to state that the variables in vector "not" should not be imputed
meth["ScreeLing_1_tot"]<- "~I(ScreeLing_1_fon + ScreeLing_1_sem + ScreeLing_1_synt)" # ! the tot variable has to come after the subscores in the dataset ! 
meth["ScreeLing_2_tot"]<- "~I(ScreeLing_2_fon + ScreeLing_2_sem + ScreeLing_2_synt)" # ! the tot variable has to come after the subscores in the dataset !
meth["ScreeLing_3_tot"]<- "~I(ScreeLing_3_fon + ScreeLing_3_sem + ScreeLing_3_synt)" # ! the tot variable has to come after the subscores in the dataset !
meth["ScreeLing_t2t1_fon"]<- "~I(ScreeLing_2_fon - ScreeLing_1_fon)"  
meth["ScreeLing_t2t1_sem"]<- "~I(ScreeLing_2_sem - ScreeLing_1_sem)" 
meth["ScreeLing_t2t1_synt"]<- "~I(ScreeLing_2_synt - ScreeLing_1_synt)" 
meth["ScreeLing_t2t1_tot"]<- "~I(ScreeLing_2_tot - ScreeLing_1_tot)"  
meth["ScreeLing_t3t1_fon"]<- "~I(ScreeLing_3_fon - ScreeLing_1_fon)"  
meth["ScreeLing_t3t1_sem"]<- "~I(ScreeLing_3_sem - ScreeLing_1_sem)" 
meth["ScreeLing_t3t1_synt"]<- "~I(ScreeLing_3_synt - ScreeLing_1_synt)" 
meth["ScreeLing_t3t1_tot"]<- "~I(ScreeLing_3_tot - ScreeLing_1_tot)"  
meth["ScreeLing_t3t2_fon"]<- "~I(ScreeLing_3_fon - ScreeLing_2_fon)"  
meth["ScreeLing_t3t2_sem"]<- "~I(ScreeLing_3_sem - ScreeLing_2_sem)" 
meth["ScreeLing_t3t2_synt"]<- "~I(ScreeLing_3_synt - ScreeLing_2_synt)" 
meth["ScreeLing_t3t2_tot"]<- "~I(ScreeLing_3_tot - ScreeLing_2_tot)" 
meth["ANTAT_t2t1_A"]<- "~I(ANTAT_2_A - ANTAT_1_A)" 
meth["ANTAT_t3t1_A"]<- "~I(ANTAT_3_A - ANTAT_1_A)" 
meth["ANTAT_t3t2_A"]<- "~I(ANTAT_3_A - ANTAT_2_A)" 

# Impute missing values 

imputed_MICE <- mice(PWASL, m = 10, maxit = 50, method = meth, seed = 500, pred = pred) 
# imputed_MICE$loggedEvents #should be NULL (if not: remove 'out' variables from dataframe)
# summary(imputed_MICE)
# imputed_MICE$imp #to check imputed values 
# View(complete(imputed_MICE, 1, include = F) %>% select(patient,ScreeLing_1_tot, ScreeLing_1_fon, ScreeLing_1_sem, ScreeLing_1_synt)) # to check whether relations are correct

# Save complete dataframes in a list

PWASL_complete <- lapply(1:10, function(x) {
  y <- complete(imputed_MICE, x)
  z <- PWASL
  z[names(y)] <- y # Replace columns with missing data in original dataset with imputed values 
  z
})

save(PWASL_complete, file = here("data", "PWASL_complete.RData"))
