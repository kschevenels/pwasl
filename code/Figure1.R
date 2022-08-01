# Create some magick

LOM <- image_read(here("figs", "overlay_lesion_acute_mricron.png"))
legend <- image_read(here("figs", "legend_mricron.PNG"))
legend_inset <- image_scale(legend, "80%x") 

LOM_with_legend <- LOM %>% image_composite(
  legend_inset,
  operator = "Atop",
  gravit = "SouthWest",
  offset = "+130+2"
)

# Write some magick
image_write(LOM_with_legend, here("figs", "Figure1.png"), format = "png", quality = 100, depth = 16)
