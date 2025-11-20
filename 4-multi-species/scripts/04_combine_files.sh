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
mkdir combined_files

for f in *R1_trim.fastq.gz; do
  # skip "run-2" files
  if [[ "$f" == run-2-* ]]; then
    continue
  fi

  # extract the sample ID for the file, which is the first three underscore-sep. fields (ex: POR_R1_C2)
  sample=$(echo "$f" | sed -E 's/^run-2-//; s/_S.*//')

  # define output names
  combinedR1="combined_files/${sample}_R1_trim.fastq.gz"
  combinedR2="combined_files/${sample}_R2_trim.fastq.gz"

  # gather all matching R1 and R2 files (run-1 + run-2)
  r1_files=( *${sample}*R1_trim.fastq.gz )
  r2_files=( *${sample}*R2_trim.fastq.gz )

  echo "Combining sample: $sample from files ${r1_files[@]} and ${r2_files[@]}"

  # concatenate the two r1 files (if there are two) and the two r2 files (if there are two)
    cat "${r1_files[@]}" > "$combinedR1"
    cat "${r2_files[@]}" > "$combinedR2"
done
