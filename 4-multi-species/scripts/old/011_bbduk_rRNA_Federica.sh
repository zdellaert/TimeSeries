#!/usr/bin/env bash
#SBATCH --export=NONE
#SBATCH --ntasks=1 --cpus-per-task=4
#SBATCH --mem=16GB
#SBATCH -t 03:59:00
#SBATCH --array=0-1
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --no-requeue

data_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/Fede_reads/"
out_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/rRNA_decomp/Fede_reads_results"
rrna_ref="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/references/Plutea_rRNA.fasta"

mkdir -p "${out_dir}"
cd "${out_dir}"

module load bbmap/39.01

# Get array of sample files
samples=(${data_dir}/trim.Pcomp_G8*)
R1_file="${samples[$SLURM_ARRAY_TASK_ID]}"
sample_name=$(basename "${R1_file}" _R1_001.fastq.gz)
R2_file="${data_dir}/${sample_name}_R2_001.fastq.gz"

echo "Processing ${sample_name}..."
echo "R1: ${R1_file}"
echo "R2: ${R2_file}"

# Run BBDuk to match against rRNA

bbduk.sh in1="${R1_file}" \
         in2="${R2_file}" \
         ref="${rrna_ref}" \
         outm="${sample_name}_rRNA.fq.gz" \
         outu="${sample_name}_clean.fq.gz" \
         stats="${sample_name}_stats.txt" \
         k=31 \
         hdist=1 \
         threads=4 \
         overwrite=t

echo "Completed ${sample_name}"
