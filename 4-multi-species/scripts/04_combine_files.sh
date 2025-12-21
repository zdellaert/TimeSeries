#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --no-requeue
#SBATCH --mem=16GB
#SBATCH -t 03:59:00 --qos=short
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file

# enter trimmed data directory
cd /scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/trimmed/

# make a directory to move combined files (and files from samples with only one run) into
mkdir -p combined_files

# get unique sample id
samples=$(ls *R1_trim.fastq.gz | \
  sed -E 's/^run-[0-9][-_]//; s/_S.*//' | \
  sort -u)

for sample in $samples; do
  # define output names
  combinedR1="combined_files/${sample}_R1_trim.fastq.gz"
  combinedR2="combined_files/${sample}_R2_trim.fastq.gz"

  # gather all matching R1 and R2 files across runs
  r1_files=( *${sample}*R1_trim.fastq.gz )
  r2_files=( *${sample}*R2_trim.fastq.gz )
  
  echo "Combining sample: $sample from files ${r1_files[@]} and ${r2_files[@]}"

  # concatenate the multiple r1 files (if there are multiple) and the multiple r2 files (if there are multiple)
    cat "${r1_files[@]}" > "$combinedR1"
    cat "${r2_files[@]}" > "$combinedR2"
done