# Load data

source(here::here("code","01_load_data.R"))

# Create figure 

Y_resid <- resid(lm(ANTAT_3_A ~ ScreeLing_1_tot + stroke_size, data = PWASL, na.action = na.exclude))
X_resid <- PWASL$volume_LHC
data<-data.frame(X_resid, Y_resid)

hccor <- ggplot(data, aes(x=X_resid, y=Y_resid)) +
  geom_point(size=3, color = cbbPalette[7]) +
  labs(x="volume of the left hippocampus (mm³)", y = "residual long-term language outcome")+
  theme_classic() + 
  geom_smooth(method=lm, size=1, color = cbbPalette[7]) 

tiff(here("figs", "Supplementary_Figure4.tiff"), units="mm", width=200, height=150, res=600, compression="lzw")
hccor
dev.off()

