Genome Annotation
================
Zoe Dellaert
2025-12-07

# Genome annotation

## Load packages

``` r
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)
library(tidyr)

# set up some universal vectors
species <- c("Mcap","Pacuta","Pcomp")
```

## SwissProt of ALL genes

Swissprot annotation method based on [E5
Annotations](https://github.com/urol-e5/deep-dive/blob/main/D-Apul/code/20-Apul-gene-annotation.Rmd)
and
[here](https://github.com/urol-e5/deep-dive/blob/main/F-Pmea/code/20-Pmea-gene-annotation.Rmd)
and Steven Robert’s [notebook post
here](https://sr320.github.io/tumbling-oysters/posts/sr320-27-go/)

#### Step 1: Gather protein fasta files

``` bash
cd ../references

# copy all genome fasta files here
cp  /work/pi_hputnam_uri_edu/HI_Genomes/*/*pep.faa .
```

#### Step 2: Download swissprot database

``` bash
mkdir blast_dbs
cd blast_dbs

# download swissprot database
curl -O https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/uniprot_sprot.fasta.gz
curl -O https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/reldate.txt

gunzip uniprot_sprot.fasta.gz

cat reldate.txt
```

UniProt Knowledgebase Release 2025_04 consists of:

- UniProtKB/Swiss-Prot Release 2025_04 of 08-Oct-2025
- UniProtKB/TrEMBL Release 2025_04 of 08-Oct-2025

#### Step 3: Rename file with release date info and confirm contents

``` bash
mv uniprot_sprot.fasta uniprot_sprot_r2025_10_08.fasta

head uniprot_sprot_r2025_10_08.fasta
echo "Number of Sequences"
grep -c ">" uniprot_sprot_r2025_10_08.fasta
# 573661 sequences
```

#### Step 4: Create BLAST protein database from swissprot fasta file

``` bash
module load blast-plus/2.14.1

makeblastdb \
-in uniprot_sprot_r2025_10_08.fasta \
-dbtype prot \
-out uniprot_sprot_r2025_10_08
```

#### Step 5: Run BLAST of geneome protein FASTAs against swissprot BLAST database

``` bash
cd ../scripts
nano 00_blastp_SwissProt.sh
```

``` bash
#!/bin/bash
#SBATCH -t 18:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=500GB
#SBATCH --export=NONE
#SBATCH --error=outs_errs/"%x_error.%j" #write out slurm error reports
#SBATCH --output=outs_errs/"%x_output.%j" #write out any program outpus
#SBATCH --mail-type=BEGIN,END,FAIL #email you when job starts, stops and/or fails

module load blast-plus/2.14.1

cd ../references/
mkdir -p annotation

for fasta in *.pep.faa; do
    base=$(basename "$fasta" .pep.faa)

    blastp \
    -query "$fasta" \
    -db blast_dbs/uniprot_sprot_r2024_10_02 \
    -out "annotation/${base}_SwissProt_out.tab" \
    -evalue 1E-05 \
    -num_threads 48 \
    -max_target_seqs 1 \
    -max_hsps 1 \
    -outfmt 6
done

echo "Blast complete!" $(date)
```

#### Step 6: Rename results files

``` bash
cd ../references/annotation/

ls

#Montipora_capitata_HIv3.genes_SwissProt_out.tab
#Porites_compressa_HIv1.genes_SwissProt_out.tab
#Pocillopora_acuta_HIv2.genes_SwissProt_out.tab

tr '|' '\t' <  Montipora_capitata_HIv3.genes_SwissProt_out.tab >  Montipora_capitata_HIv3_SwissProt_out_sep.tab
tr '|' '\t' <  Pocillopora_acuta_HIv2.genes_SwissProt_out.tab > Pocillopora_acuta_HIv2_SwissProt_out_sep.tab
tr '|' '\t' <  Porites_compressa_HIv1.genes_SwissProt_out.tab >  Porites_compressa_HIv1_SwissProt_out_sep.tab

rm *_SwissProt_out.tab
```

#### Step 7: Download Swissprot GO annotation information

- Previous file was just the sequences + accession numbers, this
  contains the metadata and other information we need about each protein

``` bash
curl -H "Accept: text/plain; format=tsv" "https://rest.uniprot.org/uniprotkb/stream?fields=accession%2Creviewed%2Cid%2Cprotein_name%2Cgene_names%2Corganism_name%2Clength%2Cgo_p%2Cgo%2Cgo_id%2Cgo_c%2Cgo_f&format=tsv&query=%28reviewed%3Atrue%29" -o SwissProt-Annot-GO_20251207.tsv

wc -l SwissProt-Annot-GO_20251207.tsv
# 573662
```

#### Step 8: Join Swissprot GO annotations to BLAST results

``` r
# load in GO annotations
spgo <- read.csv("../references/annotation/SwissProt-Annot-GO_20251207.tsv", sep = '\t', header = TRUE)

genomes <- list.files("../references", pattern = "*.pep.faa", full.names = FALSE, ignore.case = TRUE)
genomes <- str_remove(genomes, ".genes.pep.faa")

for (genome in genomes){
  bltabl <- read.csv(paste0("../references/annotation/",genome,"_SwissProt_out_sep.tab"), sep = '\t', header = FALSE)

  annot_tab <- left_join(bltabl, spgo, by = c("V3" = "Entry")) %>%
    select(
      query = V1,
      blast_hit = V3,
      evalue = V13,
      ProteinNames = Protein.names,
      BiologicalProcess = Gene.Ontology..biological.process.,
      GeneOntologyIDs = Gene.Ontology.IDs,
      CellularComponent = Gene.Ontology..cellular.component.,
      MolecularFunction = Gene.Ontology..molecular.function.,
    )

  write.table(annot_tab, 
            file = paste0("../references/annotation/",genome,"_Swissprot_GO.tsv"), 
            sep = "\t", 
            row.names = FALSE, 
            quote = FALSE)
}

rm(spgo)
rm(bltabl)
rm(annot_tab)
```

## Load in SwissProt Annotations

``` r
Mcap_SwissP <- read.delim("../references/annotation/Montipora_capitata_HIv3_Swissprot_GO.tsv") %>% dplyr::rename(GOs = GeneOntologyIDs)
Pacuta_SwissP <- read.delim("../references/annotation/Pocillopora_acuta_HIv2_Swissprot_GO.tsv") %>% dplyr::rename(GOs = GeneOntologyIDs)
Pcomp_SwissP <- read.delim("../references/annotation/Porites_compressa_HIv1_Swissprot_GO.tsv") %>% dplyr::rename(GOs = GeneOntologyIDs)
```

## Heat Stress Genes

``` r
for (i in 1:length(species)){
HeatStressGenes <- read_csv(paste0("/project/pi_hputnam_uri_edu/zdellaert/snRNA_analysis/multi-sp-snRNA/reference_genes/genes_of_interest/HeatStressGenes_", species[i] ,".csv")) %>%
  dplyr::select(-1) %>%
  dplyr::rename(query = paste0(species[i],"_gene")) %>%
  dplyr::select(query,everything())

assign(paste0(species[i],"_HeatStressGenes"),HeatStressGenes)

HeatStressGenes_unique <- HeatStressGenes %>%
  group_by(query) %>%
  summarize(gene_id = paste(unique(gene_id), collapse = ";"),
            gene_name = paste(unique(gene_name), collapse = ";"),
            response_type = paste(unique(response_type), collapse = ";"),
            category = paste(unique(category), collapse = ";")
            ) %>%
  left_join(get(paste0(species[i],"_SwissP")))

assign(paste0(species[i],"_HeatStressGenes_unique"),HeatStressGenes_unique)

  rm(HeatStressGenes)
  rm(HeatStressGenes_unique)
}
```

``` r
for_natalie <- Pacuta_HeatStressGenes_unique %>% filter(grepl("BAK",gene_id,ignore.case = TRUE)|
                                                        grepl("BAX",gene_id,ignore.case = TRUE)|
                                                        grepl("Bcl-2",gene_id,ignore.case = TRUE)|
                                                        grepl("AMPK",gene_id,ignore.case = TRUE)|  
                                                        grepl("OGG1",gene_id,ignore.case = TRUE)|
                                                        grepl("Foxo3",gene_id,ignore.case = TRUE)| 
                                                        grepl("HO-1",gene_id,ignore.case = TRUE)|
                                                        grepl("Nrf2",gene_id,ignore.case = TRUE)|
                                                        grepl("BI-1",gene_id,ignore.case = TRUE)|
                                                        grepl("HSP",gene_id,ignore.case = TRUE)
                                                          )

for_natalie_swissP <- Pacuta_SwissP %>% filter(grepl("Bcl-2 homologous antagonist/killer",ProteinNames,ignore.case = TRUE)|
                                                        grepl("Apoptosis regulator BAX",ProteinNames,ignore.case = TRUE)|
                                                        grepl("Apoptosis regulator Bcl-2",ProteinNames,ignore.case = TRUE)|
                                                        grepl("5'-AMP-activated protein kinase",ProteinNames,ignore.case = TRUE)|  
                                                        grepl("8-oxoguanine DNA glycosylase",ProteinNames,ignore.case = TRUE)|
                                                        grepl("Forkhead box protein O3",ProteinNames,ignore.case = TRUE)| 
                                                        grepl("Heme oxygenase",ProteinNames,ignore.case = TRUE)|
                                                        grepl("Nuclear factor erythroid 2",ProteinNames,ignore.case = TRUE)|
                                                        grepl("Bax inhibitor 1 (BI-1)",ProteinNames,ignore.case = TRUE)|
                                                        grepl("Heat shock protein 70 A2",ProteinNames,ignore.case = TRUE)|
                                                        grepl("Heat shock protein HSP 90",ProteinNames,ignore.case = TRUE)
                                                          )

for_natalie_swissP_missing <- for_natalie_swissP %>% filter(!(query %in% for_natalie$query)) %>%
  mutate(gene_id = "",gene_id = "",gene_name = "",response_type = "",category = "")

for_natalie_added <- rbind(for_natalie,for_natalie_swissP_missing) 

#write.csv(for_natalie_added, file = "../references/annotation/NC_HeatStress_Genes.csv",row.names = FALSE, quote = FALSE)

Natalie_list <- read.csv(file = "../references/annotation/NC_HeatStress_Genes.csv")
```

## Membrane_Channels

### Find coral IDs for human membrane channel sequences

Based on [Bhattacharya et al 2016](https://doi.org/10.7554/eLife.13288)
elife-13288-fig2-data1-v1.docx

#### Step 1: Gather protein fasta files from Human Accession numbers

[NCBI Fasta Download](https://github.com/kblin/ncbi-acc-download)

``` bash

pip install ncbi-acc-download

mkdir human_channels
cd human_channels

ncbi-acc-download --molecule protein --format fasta #HUMAN ACCESSION NUMBERS HERE

cat * > sensing_protein_seqs.fasta
```

#### I am actually going to use this FASTA file from Hollie: `../references/sensing_protein_seqs.fasta`

#### Step 2: Create BLAST protein database for each coral species from protein fasta file

``` bash
salloc
cd references/
module load blast-plus/2.14.1

makeblastdb -in Pocillopora_acuta_HIv2.genes.pep.faa \
-dbtype prot \
-out blast_dbs/Pacuta_prot

makeblastdb -in Montipora_capitata_HIv3.genes.pep.faa \
-dbtype prot \
-out blast_dbs/Mcap_prot

makeblastdb -in Porites_compressa_HIv1.genes.pep.faa \
-dbtype prot \
-out blast_dbs/Pcomp_prot
```

#### Step 3: Run BLAST of geneome protein FASTAs against swissprot BLAST database

``` bash
cd ../scripts
nano 00_blastp_channels.sh
```

``` bash
#!/bin/bash
#SBATCH -t 18:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=500GB
#SBATCH --export=NONE
#SBATCH --error=outs_errs/"%x_error.%j" #write out slurm error reports
#SBATCH --output=outs_errs/"%x_output.%j" #write out any program outpus
#SBATCH --mail-type=BEGIN,END,FAIL #email you when job starts, stops and/or fails

module load blast-plus/2.14.1

cd ../references/blast_dbs/

for database in *_prot.pin; do
    species=$(basename "$database" _prot.pin)

    blastp \
    -query "../sensing_protein_seqs.fasta" \
    -db ${species}_prot \
    -out "../annotation/${species}_channels_BLAST_out.tab" \
    -evalue 1E-05 \
    -num_threads 48 \
    -max_target_seqs 1 \
    -max_hsps 1 \
    -outfmt 6
done

echo "Blast complete!" $(date)
```

#### Step 4: Process results

``` r
Bhattacharya2016 <- read.csv("../references/2_Gene_list7.csv",header = TRUE) %>%
  #remove genes from this list that are not relevant here: SLC3
    # SLC3A2 = amino acid transporter, not even mentioned in the Bhattacharya paper
    # consider removing:
      # SLC4 = very important, but mostly for calcification (also SLC26, another bicarbonate transporter)
      # ABCC6 = not mentioned in the Bhattacharya paper
  filter(!grepl("^SLC3A2$|^SLC4|^SLC26|^ABCC6", Name.HS,ignore.case = TRUE))

for (sp in species){
  bltabl <- read.csv(paste0("../references/annotation/",sp,"_channels_BLAST_out.tab"), sep = '\t', header = FALSE)
  bltabl <-  bltabl %>% mutate(V1 = str_extract(V1, "[A-Z]{2}_[0-9.]+"))
  colnames(bltabl) <- c("qseqid","sseqid", "pident","length","mismatch", "gapopen", "qstart", "qend",
                       "sstart", "send", "evalue", "bitscore")
  
  # add gene ID info
  annot_tab <- bltabl %>% left_join(Bhattacharya2016, join_by("qseqid"=="Protein.HS")) 
  
  # for proteins that have their best BLAST match as being more than one coral match, keep only the best coral match
  annot_tab <- annot_tab %>% group_by(sseqid) %>% arrange(evalue, desc(bitscore)) %>% slice(1) %>% ungroup()
  
  assign(paste0(sp,"_channels_BLAST"),annot_tab)
}

rm(bltabl)
rm(annot_tab)
```

### manual search

``` r
channel_list <- tibble::tribble(
  ~name,            ~search_pattern,              ~SwissProt_column,
  "aquaporin",     "Aquaporin",                   "ProteinNames",
  "TRP",           "transient receptor potential","ProteinNames",
  "Mechanosensory","Mechanosens",                 "ProteinNames",
  "calcium",       "calcium ion transport",       "BiologicalProcess",
  "ER_calcium", "endoplasmic reticulum calcium ion homeostasis",       "BiologicalProcess",
  "Golgi_calcium", "Golgi calcium ion",       "BiologicalProcess",
  "SLC24",           "solute carrier family 24",              "ProteinNames",
  "SLC25",           "uncoupl",              "ProteinNames",
  "PMCA", "plasma membrane calcium", "ProteinNames",
  "Sodium_calcium_exchanger", "Sodium/calcium exchanger", "ProteinNames"
)


for (i in 1:length(species)){

  Swissprot <- get(paste0(species[i], "_SwissP"))
  
  channel_df <- channel_list %>%
  mutate(
    query = map2(
      search_pattern, SwissProt_column,
      \(pattern, column) {
        Swissprot$query[
          str_detect(Swissprot[[column]], regex(pattern, ignore_case = TRUE))
        ]
      }
    )
  ) %>%
  select(name, query) %>%
  unnest(query) %>%
  mutate(Bhattacharya_ID="") %>%
  rbind(get(paste0(species[i], "_channels_BLAST")) %>% mutate(name="Bhattacharya2016") %>% select(name, query=sseqid,Bhattacharya_ID=Name.HS)) %>%
  dplyr::rename(gene_set = name) %>%
  left_join(Swissprot) %>% select(-Bhattacharya_ID,everything()) %>%
  group_by(query) %>%
  summarize(
    gene_set = paste(unique(gene_set), collapse = ", "),
    Bhattacharya_ID = paste(unique(Bhattacharya_ID[Bhattacharya_ID != ""]), collapse = ", "),
    across(-c(gene_set, Bhattacharya_ID), first),
    .groups = "drop"
  ) %>%
  mutate(short_name = str_replace(ProteinNames, "\\s+\\(.*", ""),
         short_name = str_replace(short_name, "\\s+\\[.*", ""),
         short_name = ifelse(Bhattacharya_ID=="", short_name, Bhattacharya_ID))
  
  assign(paste0(species[i],"_membrane_channels"),channel_df)
  
  write_csv(channel_df,paste0("../references/annotation/",species[i],"_membrane_channels.csv"))
}
```
