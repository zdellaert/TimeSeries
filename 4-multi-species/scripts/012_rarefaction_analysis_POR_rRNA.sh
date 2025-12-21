#!/usr/bin/env bash
#SBATCH --export=NONE
#SBATCH --ntasks=1 --cpus-per-task=8
#SBATCH --mem=24GB
#SBATCH -t 12:00:00
#SBATCH --array=0-41
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --no-requeue

## Set up directory paths for whole script
data_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/trimmed/combined_files"
cleaned_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/rRNA_decomp_paired"
out_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/rRNA_rarefaction"
rrna_ref="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/references/Plutea_rRNA.fasta"
kallisto_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/kallisto/POR_Pcomp"

mkdir -p "${cleaned_dir}"
mkdir -p "${out_dir}"

## Run rRNA decontamination with paired-end output

### Get array of sample files for decontamination
samples=(${data_dir}/POR*R1_trim.fastq.gz)
R1_file="${samples[$SLURM_ARRAY_TASK_ID]}"
sample_name=$(basename "${R1_file}" _R1_trim.fastq.gz)
R2_file="${data_dir}/${sample_name}_R2_trim.fastq.gz"

echo "Processing ${sample_name}..."
echo "R1: ${R1_file}"
echo "R2: ${R2_file}"

### Run BBDuk to match against rRNA, output paired reads
module load bbmap/39.01

cd "${cleaned_dir}"
bbduk.sh in1="${R1_file}" \
         in2="${R2_file}" \
         ref="${rrna_ref}" \
         outm1="${sample_name}_rRNA_R1.fq.gz" \
         outm2="${sample_name}_rRNA_R2.fq.gz" \
         out1="${sample_name}_clean_R1.fq.gz" \
         out2="${sample_name}_clean_R2.fq.gz" \
         stats="${sample_name}_stats.txt" \
         k=31 hdist=1 threads=4 overwrite=t
         
echo "bbduk completed for ${sample_name}"

## Create kallisto index

### load modules 
#module load kallisto/0.50.1
#module load gffread/0.12.7

#mkdir -p "${kallisto_dir}"

### Extract transcriptome from your genome + GFF
#cd "${kallisto_dir}"
#gffread -w transcripts.fa -g /work/pi_hputnam_uri_edu/HI_Genomes/Pcompressa/Porites_compressa_HIv1.assembly.fasta /work/pi_hputnam_uri_edu/#HI_Genomes/Pcompressa/Porites_compressa_HIv1.gtf

### Build kallisto index
#kallisto index -i kallisto_index transcripts.fa

## Prepare for subsampling and mapping: find rRNA-cleaned samples
cd "${cleaned_dir}"

clean_R1_array=(${cleaned_dir}/*_clean_R1.fq.gz)
sample_R1="${clean_R1_array[$SLURM_ARRAY_TASK_ID]}"
sample_base=$(basename "${sample_R1}" _clean_R1.fq.gz)

R1="${cleaned_dir}/${sample_base}_clean_R1.fq.gz"
R2="${cleaned_dir}/${sample_base}_clean_R2.fq.gz"

echo "Processing sample: ${sample_base}"
echo "R1: ${R1}"
echo "R2: ${R2}"

### check that read counts match

r1_count=$(zcat ${R1} | wc -l); r1_count=$((r1_count/4))
r2_count=$(zcat ${R2} | wc -l); r2_count=$((r2_count/4))

if [[ "$r1_count" != "$r2_count" ]]; then
    echo "ERROR - Read counts differ for ${sample_base}: R1=$r1_count, R2=$r2_count" >&2
    exit 1
fi

echo "Total paired reads: ${r1_count}"

## Subsampling and mapping time

### load modules
module load bbmap/39.01
module load kallisto/0.50.1

### Create output directory and output csv file for each sample

sample_out_dir="${out_dir}/${sample_base}"
mkdir -p "${sample_out_dir}"

results_csv="${sample_out_dir}/${sample_base}_rarefaction.csv"
echo "sample,depth,pairs_processed,pairs_mapped,genes_detected" > "${results_csv}"

### Subsampling depths (fraction of total read pairs)
depths=(0.01 0.02 0.05 0.1 0.2 0.5 1.0)

### subsample and map with kallisto

for depth in "${depths[@]}"; do
    echo "Subsampling depth = ${depth}"

    R1_subsampled="${sample_out_dir}/${sample_base}_${depth}_R1.fq.gz"
    R2_subsampled="${sample_out_dir}/${sample_base}_${depth}_R2.fq.gz"
    depth_dir="${sample_out_dir}/kallisto_${depth}"
    mkdir -p "${depth_dir}"

    # Subsample reads
    reformat.sh in1="${R1}" in2="${R2}" \
        out1="${R1_subsampled}" out2="${R2_subsampled}" \
        sampleseed=42 samplerate="${depth}" overwrite=t

    # Run kallisto quant
    kallisto quant -i "${kallisto_dir}"/kallisto_index \
        -o "${depth_dir}" \
        -t 8 \
        "${R1_subsampled}" "${R2_subsampled}"

    # Get stats from kallisto run
    processed=$(jq -r '.num_processed' "${depth_dir}/run_info.json")
    mapped=$(jq -r '.num_mapped' "${depth_dir}/run_info.json")

    # Count number of transcripts detected (TPM > 0.5)
    detected=$(awk -F'\t' 'NR>1 && $5>0.5 {c++} END{print c+0}' "${depth_dir}/abundance.tsv")

    echo "${sample_base},${depth},${processed},${mapped},${detected}" >> "${results_csv}"
done

echo "Finished sample ${sample_base}"
echo "Results written to: ${results_csv}"
