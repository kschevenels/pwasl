# Load data

source(here::here("code","01_load_data.R"))
source(here::here("code","05_language_recovery.R"))

# Create long dataset to plot

PWASL$ScreeLing_1_tot <- as.numeric(PWASL$ScreeLing_1_tot)

PWASL_long <- PWASL %>% 
  dplyr::select("patient", "ScreeLing_1_tot", "ScreeLing_2_tot", "ScreeLing_3_tot", "ANTAT_1_A", "ANTAT_2_A", "ANTAT_3_A") %>%
  pivot_longer(cols = !patient, 
               names_to = c("Assessment", "Time"), 
               names_pattern = "(.*)_(.)_.*", 
               values_to = "score") %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(Assessment = recode_factor(Assessment, "ANTAT" = "ANELT (range 10-50)", "ScreeLing" = "ScreeLing (range 0-72)"))

# Plot language evolution over time

cutoff <- data.frame(Assessment = as.factor(c("ANELT (range 10-50)", "ScreeLing (range 0-72)")), cutoff = c(49, 68))

Individual_evolution_linechart <- PWASL_long %>%
  filter(!(is.na(score))) %>%
  group_by(Time, Assessment) %>%
  summarize(mean_score = mean(score), sd_score = sd(score)) %>% 
  ggplot(mapping = aes(x = as.numeric(as.character(Time)), y = score, color = Assessment)) + #Time needs to be 1, 2, 3 (instead of characters) in order to draw the lines
  geom_path(data = PWASL_long, aes(group = patient), alpha = .2) +
  geom_line(aes(y = mean_score, color = Assessment), size = 1.5, alpha = .8) +
  geom_point(aes(y = mean_score, color = Assessment), size = 3, alpha = .8) +
  geom_errorbar(aes(y = mean_score, ymin = mean_score - sd_score, ymax = mean_score + sd_score), width = .2, size = 1, alpha = .8) +
  geom_point(data = PWASL_long, size = 1.5) + 
  theme_classic() + 
  labs(x = "Time", y = "Language outcome") + 
  scale_colour_manual(values = cbbPalette[c(4,7)]) +
  scale_x_discrete(limits = c("acute", "subacute", "chronic")) +
  facet_grid(. ~ Assessment) + 
  geom_hline(data = cutoff, aes(yintercept = cutoff), color = cbbPalette[1], size = 0.7, linetype="dashed") +
  theme(legend.position="none")

tiff(here("figs", "Supplementary_Figure2.tiff"), units="mm", width=200, height=150, res=600, compression="lzw")
Individual_evolution_linechart
dev.off()
