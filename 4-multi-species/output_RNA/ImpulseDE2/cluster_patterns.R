pattern_mapping <- tribble(
  ~species, ~cluster, ~pattern,
  "Pacuta", 1, "Early Dip (3h)",
  "Pacuta", 2, "Early Peak (3h)",
  "Pacuta", 3, "Sustained Down (3h)",
  "Pacuta", 4, "Sustained Up (3h)",
  "Pacuta", 5, "Gradual Down",
  "Pacuta", 6, "Sustained Up (12h)",
  "Mcap", 1, "Sustained Down (12h)",
  "Mcap", 2, "Sustained Up (12h)",
  "Mcap", 3, "Sustained Up (3h)",
  "Mcap", 4, "Early Dip (3h)",
  "Mcap", 5, "U-shaped Dip (12h)",
  "Mcap", 6, "Gradual Down",
  "Pcomp", 1, "Gradual Down",
  "Pcomp", 2, "Sustained Down (12h)",
  "Pcomp", 3, "Sustained Down (3h)",
  "Pcomp", 4, "U-shaped Dip (12h)",
  "Pcomp", 5, "Sustained Up (3h)",
  "Pcomp", 6, "Sustained Up (12h)",
)

mfuzz_cols <- c(paletteer::paletteer_d("MetBrewer::VanGogh2"))
names(mfuzz_cols) <- unique(pattern_mapping$Mfuzz_pattern)[!(is.na(unique(pattern_mapping$Mfuzz_pattern)))]

logical_order <- c("Early Peak (3h)", "Sustained Up (3h)", "Sustained Up (12h)", 
                   "Early Dip (3h)", "U-shaped Dip (12h)", "Sustained Down (3h)", 
                   "Sustained Down (12h)", "Gradual Down")

# manual rearrange
names(mfuzz_cols) <- logical_order