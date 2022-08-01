# Load data

load(here::here("data", "basict2.Rdata"))
load(here::here("data", "basict3.Rdata"))
load(here::here("data", "finalt2.Rdata"))
load(here::here("data", "finalt3.Rdata"))

# Create dataframes for table

dataframe_t2_modelbuilding <- rbind(
  c("Step 1: Traditional predictors", "BF inclusion: mean (SD)"),
  BFincl_basic_t2,
  c("Step 2: Adding other predictors", "BF inclusion: mean (SD)"), #as separator between both
  BFincl_noi_t2, #create one dataframe of coi and noi
  c("Step 3: Adding hippocampal predictors", "BF inclusion: mean (SD)"), #as separator between both
  BFincl_coi_t2) 

# Create table

t2_modelbuilding <- flextable(dataframe_t2_modelbuilding) %>%
  autofit () %>%
  theme_vanilla %>% 
  delete_part(part = "header") %>%
  surround(i = 1, border.top = fp_border(width = 2)) %>% 
  surround(i = 1, border.bottom = fp_border(width = 2)) %>% 
  bold(i = 1, bold = TRUE, part = "body") %>%
  surround(i = 4, border.top = fp_border(width = 2)) %>% #
  surround(i = 4, border.bottom = fp_border(width = 2)) %>% 
  bold(i = 4, bold = TRUE, part = "body") %>%
  surround(i = 13, border.top = fp_border(width = 2)) %>% 
  surround(i = 13, border.bottom = fp_border(width = 2)) %>% 
  bold(i = 13, bold = TRUE, part = "body") %>%
  bold(part = "footer") %>%
  bold(part = "header") %>%
  flextable::footnote(part="body", i=7, j=1, ref_symbols = "", 
         value=as_paragraph("Note. BF inclusion indicates, given the data, how many times more likely a model is including a specific predictor than a model without that specific predictor.")) %>%
  set_table_properties(layout = "autofit") #autofit table to length of longest line

t2_modelbuilding

# Create dataframes for table

dataframe_t3_modelbuilding <- rbind(
  c("Step 1: Traditional predictors", "BF inclusion: mean (SD)"),
  BFincl_basic_t3,
  c("Step 2: Adding other predictors", "BF inclusion: mean (SD)"), #as separator between both
  BFincl_noi_t3, #create one dataframe of coi and noi
  c("Step 3: Adding hippocampal predictors", "BF inclusion: mean (SD)"), #as separator between both
  BFincl_coi_t3)

# Create table

t3_modelbuilding <- flextable(dataframe_t3_modelbuilding) %>%
  autofit() %>%
  theme_vanilla %>% 
  delete_part(part = "header") %>%
  surround(i = 1, border.top = fp_border(width = 2)) %>% 
  surround(i = 1, border.bottom = fp_border(width = 2)) %>% 
  bold(i = 1, bold = TRUE, part = "body") %>%
  surround(i = 4, border.top = fp_border(width = 2)) %>% 
  surround(i = 4, border.bottom = fp_border(width = 2)) %>% 
  bold(i = 4, bold = TRUE, part = "body") %>%
  surround(i = 13, border.top = fp_border(width = 2)) %>% 
  surround(i = 13, border.bottom = fp_border(width = 2)) %>% 
  bold(i = 13, bold = TRUE, part = "body") %>%
  bold(part = "footer") %>%
  bold(part = "header") %>%
  flextable::footnote(part="body", i=14, j=1, ref_symbols = "*",
         value=as_paragraph("BF inclusion > 3, providing moderate evidence for the predictor of interest.")) %>%
  set_table_properties(layout = "autofit") #autofit table to length of longest line

t3_modelbuilding
