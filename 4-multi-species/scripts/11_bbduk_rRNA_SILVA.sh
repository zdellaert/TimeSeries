#!/usr/bin/env bash
#SBATCH --export=NONE
#SBATCH --ntasks=1 --cpus-per-task=4
#SBATCH --mem=160GB
#SBATCH -t 6:00:00
#SBATCH --array=0-125
#SBATCH --error=../scripts/outs_errs/%x_%a.error
#SBATCH --output=../scripts/outs_errs/%x_%a.output
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --no-requeue

data_dir="/scratch4/workspace/zdellaert_uri_edu-shared_TimeSeries/TimeSeries/trimmed/combined_files"
out_dir="/scratch4/workspace/zdellaert_uri_edu-shared_TimeSeries/TimeSeries/rRNA_decomp_SILVA"
rrna_ref="/scratch4/workspace/zdellaert_uri_edu-shared_TimeSeries/TimeSeries/references"

mkdir -p "${out_dir}"
cd "${out_dir}"

module load bbmap/39.01

# Get array of sample files
samples=(${data_dir}/*R1_trim.fastq.gz)
R1_file="${samples[$SLURM_ARRAY_TASK_ID]}"
sample_name=$(basename "${R1_file}" _R1_trim.fastq.gz)
R2_file="${data_dir}/${sample_name}_R2_trim.fastq.gz"

echo "Processing ${sample_name}..."
echo "R1: ${R1_file}"
echo "R2: ${R2_file}"

# Run BBDuk to match against rRNA Large Subunit Database

bbduk.sh -Xmx136g in1="${R1_file}" \
         in2="${R2_file}" \
         ref="${rrna_ref}/SILVA_138.2_LSURef_NR99_tax_silva_trunc.fasta.gz" \
         outm="${sample_name}_rRNA_LSU.fq.gz" \
         outu="${sample_name}_clean_LSU.fq.gz" \
         stats="${sample_name}_stats_LSU.txt" \
         k=31 \
         hdist=1 \
         threads=4 \
         overwrite=t

# Run BBDuk to match against rRNA Small Subunit Database against file cleaned in above code

bbduk.sh -Xmx136g in="${sample_name}_clean_LSU.fq.gz" \
         ref="${rrna_ref}/SILVA_138.2_SSURef_NR99_tax_silva_trunc.fasta.gz" \
         outm="${sample_name}_rRNA_SSU.fq.gz" \
         outu="${sample_name}_clean.fq.gz" \
         stats="${sample_name}_stats_SSU.txt" \
         k=31 hdist=1 rskip=2 threads=4 overwrite=t

# Combine fastq and stats files
cat "${sample_name}_rRNA_LSU.fq.gz" "${sample_name}_rRNA_SSU.fq.gz" > "${sample_name}_rRNA.fq.gz"
cat "${sample_name}_stats_LSU.txt" "${sample_name}_stats_SSU.txt" > "${sample_name}_stats.txt"

# Cleanup
rm "${sample_name}"*_[LS]SU.fq.gz

echo "Completed ${sample_name}"