# Load data

source(here::here("code","01_load_data.R"))

# Compare gender distribution between patients with aphasia and healthy older controls

gender = data.frame(HASL = as.numeric(summary(HASL$gender)),
                    PWASL = as.numeric(summary(PWASL$sex)))

sex <- chisq.test(gender, correct = T) #use the correction when at least one cell has a count <10 # significant
prop.test(gender$HASL, gender$HASL+gender$PWASL, correct = T) # significant

# Compare age between patients with aphasia and healthy older controls

shapiro.test(HASL$age) # not normally distributed
shapiro.test(PWASL$age) # normally distributed

age <- wilcox.test(HASL$age, PWASL$age)

# Compare education between patients with aphasia and healthy older controls

shapiro.test(HASL$education) # not normally distributed
shapiro.test(PWASL$education) # normally distributed

education <- wilcox.test(HASL$education, PWASL$education) # not significant

# Auditory SL

shapiro.test(PWASL$ASL) # not normally distributed
shapiro.test(HASL$ASL) # normally distributed
effsizeASL <- data.frame(ASL = c(PWASL$ASL,HASL$ASL), 
                         study = c(rep("PWASL", length(PWASL$ASL)), rep("HASL", length(HASL$ASL)))) %>%
                mutate(ASL = as.numeric(ASL), study = as.factor(study))

comparison_ASL <- wilcox.test(PWASL$ASL, HASL$ASL)
cASL_eff <- effsizeASL %>% wilcox_effsize(ASL ~ study) 
bay_ASL <- extractBF(ttestBF(na.omit(PWASL$ASL), na.omit(HASL$ASL))) 
group_ASL <- wilcox.test(PWASL$ASL, alternative=c("greater"), mu=16)
gASL_eff <- PWASL %>% wilcox_effsize(ASL ~ 1, mu = 16) 
ind_ASL <- wilcox.test(PWASL$ASL, alternative=c("greater"), mu=21) 
iASL_eff <- PWASL %>% wilcox_effsize(ASL ~ 1, mu = 21)

# Visual SL 

shapiro.test(PWASL$VSL) # normally distributed
shapiro.test(HASL$VSL) # normally distributed

comparison_VSL <- t.test(PWASL$VSL, HASL$VSL)
bay_VSL <- extractBF(ttestBF(na.omit(PWASL$VSL), na.omit(HASL$VSL))) 
group_VSL <- t.test(PWASL$VSL, alternative=c("greater"), mu=16) 
ind_VSL <- t.test(PWASL$VSL, alternative=c("greater"), mu=21) 

# Visuomotor SL 

shapiro.test(PWASL$VML) # normally distributed
shapiro.test(HASL$VML) # normally distributed

comparison_VML <- t.test(PWASL$VML, HASL$VML)
bay_VML <- extractBF(ttestBF(na.omit(PWASL$VML), na.omit(HASL$VML)))
group_VML <- t.test(PWASL$VML, alternative=c("greater"), mu=0) 

# Correlations SL with HC volume and connection density

shapiro.test(PWASL$volume_LHC) # normally distributed
shapiro.test(PWASL$volume_RHC) # normally distributed
shapiro.test(PWASL$CD_LHC) # not normally distributed
shapiro.test(PWASL$CD_RHC) # normally distributed

HC_SL_cor <- corr.test(PWASL %>% select(VML, VSL), PWASL %>% select(volume_LHC, volume_RHC, CD_LHC, CD_RHC), adjust = "holm")

# Partial correlations with HC volume and connection density corrected for eTIV and age

shapiro.test(PWASL$eTIV) # normally distributed
shapiro.test(PWASL$age) # normally distributed

partial <- partial.r(PWASL, c("VML", "volume_LHC", "volume_RHC", "CD_LHC", "CD_RHC"), cs(eTIV,age), method = "pearson")
x <- c(28-2,31-2,27-2,30-2) # number of observations for correlations of interest,  n is set to n - s (where s is the number of variables partialed out)
p.adj <- corr.p(as.matrix(partial[2:5,1]), n = as.matrix(x), adjust="holm", alpha=0.05, ci=T) # to find the confidence intervals of a correlation
