# Load data

source(here::here("code","01_load_data.R"))
source(here::here("code","03_statistical_learning.R"))

# Create long dataframe for figure

HC_SL_cor_long <- PWASL %>% 
  dplyr::select(VSL, VML, volume_LHC, volume_RHC, CD_LHC, CD_RHC) %>% 
  pivot_longer(!c("VML", "VSL"), names_to = c(".value", "laterality"), names_pattern = "(.*)_([^HC])") %>% # get laterality as variable
  mutate(laterality = recode_factor(laterality, "L" = "left", "R" = "right")) %>%
  pivot_longer(cols = c("volume", "CD"),
               names_to = "measure", 
               values_to = "value") %>%
  pivot_longer(cols = c("VML", "VSL"),
             names_to = "task",
             values_to = "score") %>%
  mutate(measure = recode_factor(measure, "CD" = "FBC"), 
         task = recode_factor(task, "VSL" = "visual statistical learning", "VML" = "visuomotor statistical learning"))

# Code for text on figure

HC_SL_cor_text <- data.frame(
  label = c(paste0("Left: r = ", numformat(HC_SL_cor$r[2,1]),", ",
                   "t(",HC_SL_cor$n[2,1]-2,") = ", numformat(HC_SL_cor$t[2,1]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[2,1]), ", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[2,1]),", ",
                   numformat(HC_SL_cor$ci.adj[2,3]), "]"), 
            paste0("Right: r = ", numformat(HC_SL_cor$r[2,2]),", ",
                   "t(",HC_SL_cor$n[2,2]-2,") = ", numformat(HC_SL_cor$t[2,2]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[2,2]), ", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[4,1]),", ",
                   numformat(HC_SL_cor$ci.adj[4,3]), "]"),
            paste0("Left: r = ", numformat(HC_SL_cor$r[2,3]),", ",
                   "t(",HC_SL_cor$n[2,3]-2,") = ", numformat(HC_SL_cor$t[2,3]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[2,3]),", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[6,1]),", ",
                   numformat(HC_SL_cor$ci.adj[6,3]), "]"),
            paste0("Right: r = ", numformat(HC_SL_cor$r[2,4]),", ",
                   "t(",HC_SL_cor$n[2,4]-2,") = ", numformat(HC_SL_cor$t[2,4]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[2,4]),", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[8,1]),", ",
                   numformat(HC_SL_cor$ci.adj[8,3]), "]"),
            paste0("Left: r = ", numformat(HC_SL_cor$r[1,1]),", ",
                   "t(",HC_SL_cor$n[1,1]-2,") = ", numformat(HC_SL_cor$t[1,1]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[1,1]),", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[1,1]),", ",
                   numformat(HC_SL_cor$ci.adj[1,3]), "]"),
            paste0("Right: r = ", numformat(HC_SL_cor$r[1,2]),", ",
                   "t(",HC_SL_cor$n[1,2]-2,") = ", numformat(HC_SL_cor$t[1,2]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[1,2]),", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[3,1]),", ",
                   numformat(HC_SL_cor$ci.adj[3,3]), "]"),
            paste0("Left: r = ", numformat(HC_SL_cor$r[1,3]),", ",
                   "t(",HC_SL_cor$n[1,3]-2,") = ", numformat(HC_SL_cor$t[1,3]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[1,3]),", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[5,1]),", ",
                   numformat(HC_SL_cor$ci.adj[5,3]), "]"),
            paste0("Right: r = ", numformat(HC_SL_cor$r[1,4]),", ",
                   "t(",HC_SL_cor$n[1,4]-2,") = ", numformat(HC_SL_cor$t[1,4]),", ",
                   "p = ", printp(HC_SL_cor$p.adj[1,4]),", ",
                   "95% CI [",numformat(HC_SL_cor$ci.adj[7,1]),", ",
                   numformat(HC_SL_cor$ci.adj[7,3]), "]")),
  task = c(rep("visual statistical learning",4), rep("visuomotor statistical learning",4)),
  measure = rep(c("volume", "volume", "FBC", "FBC"),2),
  laterality = rep(c("left", "right"),4),
  x = c(21, 21, 20.5, 21, 0.24, 0.26, 0.23, 0.24),
  y = c(4200, 4100, 6, 5.7, 4200, 4100, 6, 5.7)
)

# Code for figure

corr_matrix <- HC_SL_cor_long %>%  
  ggplot(aes(x=score, y=value, color=laterality)) +
  geom_point(size=2.5) +
  theme_classic() +
  scale_colour_manual(values = cbbPalette[c(4,7)]) +
  geom_smooth(method=lm, se=TRUE, alpha = .1) +
  guides(colour = "none", shape = "none", size = "none", fill = "none") +
  labs(x = "statistical learning score", y = "hippocampal measure (volume in mm³ and FBC in a.u.)") +
  facet_grid(measure ~ task, scales = "free") +
  geom_text(
    data = HC_SL_cor_text, size = 4,
    mapping = aes(x = x, y = y, label = label)
  )

tiff(here("figs", "Figure2.tiff"), units="mm", width=250, height=250, res=600, compression="lzw")
corr_matrix
dev.off()
