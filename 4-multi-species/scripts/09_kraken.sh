#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --no-requeue
#SBATCH --mem=200GB
#SBATCH -t 03:59:00 --qos=short
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file

# load modules required
module load kraken2/2.1.2

# make and enter output directory
scratch_dir="/scratch4/workspace/zdellaert_uri_edu-shared_TimeSeries/TimeSeries"
data_dir="${scratch_dir}/trimmed/combined_files/"
out_dir="${scratch_dir}/kraken"

mkdir -p "${out_dir}"

trimmed=( "${data_dir}"*"R1_trim.fastq.gz" )

for R1_file in "${trimmed[@]}"; do
  # extract sample name
  sample_name=$(basename "${R1_file}" "_R1_trim.fastq.gz")

  # define R2 file
  R2_file="${data_dir}${sample_name}_R2_trim.fastq.gz"

  kraken2 \
    --db /datasets/bio/kraken2/kraken2-db/ \
    --threads 16 \
    --paired "$R1_file" "$R2_file" \
    --use-names \
    --report "${out_dir}/${sample_name}.report.txt" \
    --output "${out_dir}/${sample_name}.kraken"
done
