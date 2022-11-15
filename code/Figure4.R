# Load data

source(here::here("github", "code","01_load_data.R"))
source(here::here("github", "code","03_statistical_learning.R"))

# Prepare dataframe for summary table

SLresultsdf <- PWASL %>%
  dplyr::select(patient, ASL, VSL, VML) %>% 
  pivot_longer(cols = !patient,
               names_to = "task",
               values_to = "score") %>% 
  filter(!(is.na(score))) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Assessment = recode_factor(task, "ASL" = "Auditory SL (/32)", "VSL" = "Visual SL (/32)", "VML" = "Visuomotor SL*"), Time = "subacute") %>% 
  group_by(Assessment, Time) %>%
  summarise(Mean = round(mean(score),2), SD = round(sd(score),2), n = length(unique(patient))) %>%
  as.data.frame()

# Prepare dataframe for figure HC vs PWA 

combinedresults <- PWASL %>% mutate(subject = patient, group = "Patients with aphasia") %>% 
  select(subject, group, ASL, VSL, VML, lesion_LHC, lesion_RHC) %>%
  rbind(HASL %>% mutate(group = "Older healthy controls", lesion_LHC = 0, lesion_RHC = 0) %>% 
          select(subject, group, ASL, VSL, VML, lesion_LHC, lesion_RHC)) %>%
  pivot_longer(cols = c("ASL", "VSL", "VML"),
               names_to = "task",
               values_to = "score") %>% mutate(group = as.factor(group))

lesion1 <- combinedresults %>% filter(lesion_LHC > 3 |lesion_RHC > 3)

# Make figures HC vs PWA 

auditorytask <- combinedresults %>% filter(task == "ASL") %>% filter(lesion_LHC<3&lesion_RHC<3) %>% 
  ggplot(aes(x = group, y = score, fill = group)) + 
  geom_violin(aes(fill = NULL), position = position_dodge()) +
  geom_point(aes(group = group), position = position_jitterdodge(jitter.width = 1), size = 2) + #fill werkt niet?
  geom_point(data = lesion1 %>% filter(task == "ASL"), color = cbbPalette[c(8)], position = position_jitterdodge(jitter.width = 1), size = 2) +
  theme_classic() + 
  labs(title = "Auditory statistical learning", x=NULL, y="accuracy (/32)") +
  ylim(0,32) +
  theme(legend.position = "none") +
  geom_hline(yintercept=16, linetype="solid", color = "grey", size=1) +
  geom_hline(yintercept=21, linetype="dashed", color = "grey", size=1) +
  scale_fill_manual(values = c("#bdbdbd", "#636363")) 

visualtask <- combinedresults %>% filter(task == "VSL") %>% filter(lesion_LHC<3&lesion_RHC<3) %>% 
  ggplot(aes(x = group, y = score, fill = group)) + 
  geom_violin(aes(fill = NULL), position = position_dodge()) +
  geom_point(aes(group = group), position = position_jitterdodge(jitter.width = 1), size = 2) +
  geom_point(data = lesion1 %>% filter(task == "VSL"), color = cbbPalette[c(8)], position = position_jitterdodge(jitter.width = 1), size = 2) +
  theme_classic() + 
  labs(title = "Visual statistical learning", x=NULL, y="accuracy (/32)") +
  ylim(0,32) +
  theme(legend.position = "none") +
  geom_hline(yintercept=16, linetype="solid", color = "grey", size=1) +
  geom_hline(yintercept=21, linetype="dashed", color = "grey", size=1) +
  scale_fill_manual(values = c("#bdbdbd", "#636363")) 

# Make VML figure on item level 

itemHC <- readRDS(file = here("data", "itemVML_HC.rds")) %>% 
  select(subject, condition, accuracy, zscore) %>%
  mutate(group = "HC", lesion_LHC = NA, lesion_RHC = NA)

itemPWA <- readRDS(file = here("data", "itemVML_PWA.rds")) %>% 
  mutate(subject = patient, group = "PWA") %>%
  select(subject, condition, accuracy, zscore, group) 

itemPWA$condition <- factor(itemPWA$condition, levels = c("sequenceA", "sequenceB", "sequenceC", "sequenceD", "random"))
itemPWA <- merge(itemPWA, PWASL %>% mutate(subject = patient) %>% select(subject, lesion_LHC, lesion_RHC), by = 'subject')

figureVML <- rbind(itemHC, itemPWA) %>% 
  filter(!(is.na(zscore))) %>% #leave NA's out of data 
  mutate_at(c('condition'), as.factor) %>%
  group_by(condition, group, subject) %>% 
  summarise(zscore = mean(zscore), lesion_LHC = mean(lesion_LHC), lesion_RHC = mean(lesion_RHC))

meanVML <- rbind(itemHC, itemPWA) %>%  
  filter(!(is.na(zscore))) %>% #leave NA's out of data 
  mutate_at(c('condition'), as.factor) %>%
  group_by(group, condition) %>% 
  summarise(zscore = mean(zscore))

lines <- c("HC" = "solid", "PWA" = "dotted")

lesion2 <- figureVML %>% filter(lesion_LHC > 3|lesion_RHC > 3) 

plotPMz <- ggplot(figureVML, aes(x=condition, y=zscore, linetype=group)) +
  geom_line(aes(group=subject), alpha=.3) +
  geom_line(aes(group=subject), data=lesion2, color=cbbPalette[c(8)],size = 1.3) +
  geom_point(aes(group=group),data=meanVML, alpha=.8, size=3) +
  geom_line(aes(group=group),data=meanVML, alpha=.8, size=1.5) + 
  theme_classic() +
  labs(title = "Visuomotor statistical learning", x=NULL, y="z-score", color=NULL) +
  theme(legend.position = c(0.78, 0.95)) + theme(legend.title=element_blank()) +
  theme(legend.text = element_text(colour = "black")) +
  theme(axis.text.x = element_text(face = c('plain', 'plain', 'plain', 'bold', 'bold'))) +
  scale_linetype_manual(values = lines, labels=c("Older healthy controls", "Patients with aphasia"))

# Put plots together and save 

SLresults <- ggdraw() +
  draw_plot(auditorytask, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(visualtask, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(plotPMz, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))

png(here("figs", "Figure4.png"), units="mm", width=180, height=180, res=600)
SLresults
dev.off()