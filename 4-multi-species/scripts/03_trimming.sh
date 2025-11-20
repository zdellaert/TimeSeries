#!/usr/bin/env bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --signal=2
#SBATCH --no-requeue
#SBATCH --mem=80GB
#SBATCH -t 03:59:00 --qos=short
#SBATCH --mail-type=BEGIN,END,FAIL #email you when job starts, stops and/or fails
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file

# load modules needed
module load parallel/20240822
module load fastp/0.23.4

# make and define directories needed
mkdir -p /scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/trimmed/
mkdir -p /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/output_RNA/trimmed_qc/

data_dir="/project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/data_RNA/"
out_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries/trimmed/"
qc_dir="/project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/output_RNA/trimmed_qc/"

# create an list of fastq files to process
R1_files=(${data_dir}*_R1_001.fastq.gz)

echo "There are ${#R1_files[@]} samples to process"
echo "Starting trimming at $(date)"

# define fastp function - with more than 300 files we need to parallelize

run_fastp() {
  R1_file=$1
  data_dir=$2
  out_dir=$3
  qc_dir=$4

  # extract sample name
  sample_name=$(basename "$R1_file" "_R1_001.fastq.gz")

  # define R2 file
  R2_file="${data_dir}${sample_name}_R2_001.fastq.gz"

  # fastp
  fastp --in1 "$R1_file" --in2 "$R2_file" \
        --out1 "${out_dir}${sample_name}_R1_trim.fastq.gz" \
        --out2 "${out_dir}${sample_name}_R2_trim.fastq.gz" \
        --detect_adapter_for_pe \
        --qualified_quality_phred 20 \
        --trim_poly_g \
        --trim_front1 10 --trim_front2 10 \
        --length_required 20 \
        --thread 2 \
        --overrepresentation_analysis \
        --html "${qc_dir}${sample_name}_fastp.html" \
        --json "${qc_dir}${sample_name}_fastp.json"

  echo "trimming of "${sample_name}" complete at $(date)"
}

export -f run_fastp

# run fastp in parallel
parallel -j 10 run_fastp {} "$data_dir" "$out_dir" "$qc_dir" ::: "${R1_files[@]}"

# now move onto qc
echo "Starting fastqc on trimmed files at" $(date)

# load modules needed
module load fastqc/0.12.1
module load uri/main
module load MultiQC/1.12-foss-2021b

# create an list of fastq files to process
trimmed_files=(${out_dir}*trim.fastq.gz)

# Run fastqc in parallel
parallel -j 20 "fastqc {} -o "${qc_dir}" && echo 'Processed {}'" ::: "${trimmed_files[@]}"
echo "fastQC done." $(date)

#Compile MultiQC report from FastQC files
echo "Running MultiQC"
cd "${qc_dir}"
multiqc --interactive .

echo "QC of trimmed RNA-seq data complete." $(date)
