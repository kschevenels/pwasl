# Create flowchart
flowchart <- grViz("digraph flowchart {
      node [fontname = Helvetica, shape = rectangle, fontsize = 16]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5'] 
      tab6 [label = '@@6']
      tab7 [label = '@@7'] [style = bold]
      tab8 [label = '@@8']
      tab9 [label = '@@9'] [style = bold]
      tab10 [label = '@@10'] 
      tab11 [label = '@@11'] [style = bold]
      tab12 [label = '@@12'] [style = bold]
      tab1 -> tab2;
      tab1 -> tab3; 
      tab3 -> tab4;
      tab3 -> tab5;
      tab5 -> tab6;
      tab5 -> tab7;
      tab7 -> tab8;
      tab7 -> tab9;
      tab8 -> tab9;
      tab8 -> tab10;
      tab9 -> tab11;
      tab10 -> tab11;
      {tab7; tab9; tab11} -> tab12
      }
      
      [1]: ' 787 patients admitted to the stroke unit \\nbetween October 2018 and March 2020 '
      [2]: ' 387 patients not eligible/\\nno consent for screening/\\nnot screened due to practical reasons '
      [3]: ' 400 stroke patients screened with LAST-NL '
      [4]: ' 297 stroke patients not eligible/\\nno consent for further assessment '  
      [5]: ' 103 stroke patients assessed with ScreeLing '
      [6]: ' 38 stroke patients not eligible/\\nno consent for longitudinal study '  
      [7]: ' 65 PWA included in acute phase \\n(13 no MRI due to practical reasons, \\nno consent or contraindications) '
      [8]: ' 10 PWA deceased, \\n13 PWA refused follow-up '
      [9]: ' 42 PWA included in subacute phase '
      [10]: '1 PWA lost to follow-up reincluded '
      [11]: '43 PWA included in chronic phase '
      [12]: '33 PWA with acute MRI and \\nat least one behavioral follow-up moment'
      ")

# Export & save
flowchart %>% export_svg() %>% 
  charToRaw() %>% rsvg::rsvg_png(here("figs", "Supplementary_Figure1.png"))
