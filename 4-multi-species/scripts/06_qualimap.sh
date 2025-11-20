#!/usr/bin/env bash
#SBATCH --export=NONE
#SBATCH --ntasks=1 --cpus-per-task=20
#SBATCH --mem=100GB
#SBATCH --time=24:00:00
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --no-requeue

species=$1
genome=$2
gtf_path=$3

# load modules needed
module load qualimap/2.2.1

# list and make required directories
scratch_dir="/scratch3/workspace/zdellaert_uri_edu-shared/TimeSeries"
alignments_dir="${scratch_dir}/aligned/${species}_${genome}"

qc_dir="/project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/output_RNA/alignment_qc/${species}_${genome}"

# make the output directory if it does not exist (-p checks for this)
mkdir -p "${qc_dir}"

cd "${alignments_dir}"

for f in *Aligned.sortedByCoord.out.bam; do
	sample_name=$(echo "$f" | sed -E 's/_Aligned.*//')

	echo "Running Qualimap on ${sample_name}..."

	qualimap rnaseq \
	    --java-mem-size=8G \
    	    -gtf "${gtf_path}" \
	    -pe \
	    --sequencing-protocol strand-specific-reverse \
	    -bam "${f}"  \
	    -outdir "${qc_dir}"/"${sample_name}"
done

# load modules needed for multiqc
module purge
module load uri/main
module load MultiQC/1.12-foss-2021b

cd "${qc_dir}"

multiqc . "${alignments_dir}"

echo "MultiQC report of STAR and qualimap outputs generated in "${qc_dir}"/multiqc_report"