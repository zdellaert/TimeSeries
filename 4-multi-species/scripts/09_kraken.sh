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
data_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/trimmed"
out_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/kraken"

mkdir -p "${out_dir}"

samples=(
  "MON_R72_H1_S3"
  "MON_R72_H2_S4"
  "run-2-MON_R72_H2_S14"
  "MON_R72_H3_S44"
  "run-2-MON_R72_H3_S28"
)

for s in "${samples[@]}"; do
    R1="${data_dir}/${s}_R1_trim.fastq.gz"
    R2="${data_dir}/${s}_R2_trim.fastq.gz"

    kraken2 \
      --db /datasets/bio/kraken2/kraken2-db/ \
      --threads 16 \
      --paired "$R1" "$R2" \
      --use-names \
      --report "${out_dir}/${s}.report.txt" \
      --output "${out_dir}/${s}.kraken"
done
