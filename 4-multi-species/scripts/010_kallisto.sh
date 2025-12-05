#!/usr/bin/env bash
#SBATCH --export=NONE
#SBATCH --ntasks=1 --cpus-per-task=20
#SBATCH --mem=100GB
#SBATCH -t 03:59:00 --qos=short
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --no-requeue

data_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/trimmed"
out_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/kallisto/POR_Pcomp"

mkdir -p "${out_dir}"
cd "${out_dir}"

# load modules 
module load kallisto/0.50.1
module load gffread/0.12.7

# Extract transcriptome from your genome + GFF
#gffread -w transcripts.fa -g /work/pi_hputnam_uri_edu/HI_Genomes/Pcompressa/Porites_compressa_HIv1.assembly.fasta /work/pi_hputnam_uri_edu/HI_Genomes/Pcompressa/Porites_compressa_HIv1.gtf

# Build kallisto index
#kallisto index -i kallisto_index transcripts.fa

# Quantify
kallisto quant -i kallisto_index \
               -o POR_R120_H2_kallisto \
               -t 10 \
               -b 100 \
               "${data_dir}"/POR_R120_H2_S46_R1_trim.fastq.gz \
               "${data_dir}"/POR_R120_H2_S46_R2_trim.fastq.gz

# Check the mapping rate
echo "Mapping rate:"
cat POR_R120_H2_kallisto/run_info.json | grep "p_pseudoaligned"
echo ""
echo "Number of reads processed:"s
cat POR_R120_H2_kallisto/run_info.json | grep "n_processed"
