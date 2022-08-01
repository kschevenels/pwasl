# Load data

load(here::here("data", "basict2.Rdata"))
load(here::here("data", "basict3.Rdata"))
load(here::here("data", "finalt2.Rdata"))
load(here::here("data", "finalt3.Rdata"))

# Extract posterior parameters for every weighted posterior model

extract_posterior_t2 <- lapply(weighted_posterior_t2, function(model){
  Parameter = str_trim(names(model)) #get variable names (trim removes white space)  
  Beta = str_trim(sub('.{8}', '', model[4,])) #get mean values (sub removes first 8 characters)
  Q1 = str_trim(sub('.{8}', '', model[2,])) #get first quantile 
  Q3 = str_trim(sub('.{8}', '', model[5,])) #get third quantile
  data.frame(Parameter, Beta, Q1, Q3)[c(1,2,5),] #retain only the two predictors 
})

# Extract prior and posterior probabilities and BF inclusion for every model

get_BFincl_t2 <- lapply(bf_inclusion_t2, function(model){
  Parameter = c("mu", rownames(model)) #add intercept
  BFincl = c(1, exp(model$log_BF)) #add intercept
  Prior = c(1, model$p_prior) #add intercept
  Posterior = c(1, model$p_posterior)
  data.frame(Prior, Posterior, BFincl)
})

# Calculate mean parameters across imputed dataframes and put them in a dataframe 

mean_posterior_t2 <- data.frame(do.call(rbind, extract_posterior_t2), row.names = NULL) %>%
  cbind(data.frame(do.call(rbind, get_BFincl_t2), row.names = NULL)) %>%
  mutate(Beta = as.numeric(Beta), Q1 = as.numeric(Q1), 
         Q3 = as.numeric(Q3), BFincl = as.numeric(BFincl)) %>%
  mutate(Parameter = as.factor(Parameter)) %>% 
  mutate(Parameter = recode_factor(Parameter, "mu" = "Intercept",  
                                   "ScreeLing_1_tot" = "Initial language score", "stroke_size" = "Acute lesion volume")) %>%
  group_by(Parameter) %>% summarise(Beta = qwraps2::mean_sd(Beta, denote_sd = "paren"), 
                                    "Q1" = qwraps2::mean_sd(Q1, denote_sd = "paren"), 
                                    "Q3" = qwraps2::mean_sd(Q3, denote_sd = "paren"),
                                    "P(prior)" = qwraps2::mean_sd(Prior, denote_sd = "paren"), 
                                    "P(posterior)" = qwraps2::mean_sd(Posterior, denote_sd = "paren"), 
                                    "BF inclusion" = qwraps2::mean_sd(BFincl, denote_sd = "paren"))

# Create table

flextable(mean_posterior_t2) %>% 
  theme_vanilla %>% 
  set_table_properties(layout = "autofit") %>%
  mk_par(j = 2, part = "header", value = as_paragraph(as_i(("β")))) %>% 
  flextable::footnote(part="body", i=2, j=1, ref_symbols = "*", 
                      value=as_paragraph("BFs inclusion > 3 are marked with an asterisk.")) 
