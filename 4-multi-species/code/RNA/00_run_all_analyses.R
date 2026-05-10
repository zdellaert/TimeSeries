# Run analysis pipeline for all species
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(rmarkdown)

species_list <- c("Pacuta", "Mcap", "Pcomp")

# Create output directories
for (sp in species_list) {
  dir.create(file.path("../../output_RNA/reports",sp), recursive = TRUE, showWarnings = FALSE)
}

for (sp in species_list) {
  output_dir <- normalizePath(paste0("../../output_RNA/reports/", sp))
  
#Run each analysis script for this species
  render("01_preprocessing.Rmd",
       params = list(species = sp),
       output_format = "github_document",
       output_dir = output_dir)
  
  render("02_ImpulseDE.Rmd",
         params = list(species = sp, run_ImpulseDE2 = FALSE),
         output_format = "github_document",
         output_dir = output_dir)
  
  render("03_WGCNA.Rmd",
         params = list(species = sp, TestParams = FALSE, run_WGCNA = FALSE),
         output_format = "github_document",
         output_dir = output_dir)
}
