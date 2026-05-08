# Species specific parameters and output paths are defined here
# See README.md for setup instructions

# Path to annotation repository (relative to code/RNA/)
annot_dir <- "../../../../HI_genome_annotations/annotation"

# Global parameters

global_params <- list(
    seed = 42, 
    n_proc = 16,
    padj_threshold = 0.05,
    
    # pOverA filtering
    pOverA_proportion = 0.07,
    pOverA_counts = 10,

    # WGCNA defaults
    wgcna_default = list(
        min_module_size = 30,
        merge_cut_height = 0.25,
        deep_split = 1
    )
)

# Species specific parameters
species_params <- list(
    Pacuta = list(
        full_name = "Pocillopora acuta",
        count_matrix = "POC_PacutaV2_gene_count_matrix.csv",
        SwissProt = "Pocillopora_acuta_HIv2_Swissprot_GO.tsv",

        outlier_samples = c(),

        #for wgcna
        soft_power = 12,

        #for mfuzz
        n_clusters = 6
),

    Mcap = list(
        full_name = "Montipora capitata",
        count_matrix = "MON_MCapV3_gene_count_matrix.csv",
        SwissProt = "Montipora_capitata_HIv3_Swissprot_GO.tsv",

        outlier_samples = c(),
        #outlier_samples = c("MON_R72_H1", "MON_R72_H2"),

        #for wgcna
        soft_power = 10,

        #for mfuzz
        n_clusters = 6
        ),

    Pcomp = list(
        full_name = "Porites compressa",
        count_matrix = "POR_Pcomp_gene_count_matrix.csv",
        SwissProt = "Porites_compressa_HIv1_Swissprot_GO.tsv",

        outlier_samples = c(),

        #for wgcna
        soft_power = 8,

        #for mfuzz
        n_clusters = 6)
)

get_params <- function(species) {species_params[[species]]}

print_config <- function(species) {
  config <- get_params(species)
  cat("Species: ", species, "\n", sep = "")
  cat("Count matrix: ", config$count_matrix, "\n", sep = "")
  cat("Outliers: ", 
      ifelse(length(config$outlier_samples) == 0, "None", 
             paste(config$outlier_samples, collapse = ", ")), "\n", sep = "")
  cat("WGCNA power: ", config$soft_power, "\n", sep = "")
  cat("Mfuzz clusters: ", config$n_clusters, "\n", sep = "")
  cat("\nOutput: ", file.path("../../output_RNA", species), "\n", sep = "")
}