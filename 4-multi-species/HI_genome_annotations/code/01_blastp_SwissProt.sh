#!/bin/bash
#SBATCH -t 18:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=500GB
#SBATCH --export=NONE
#SBATCH --error=outs_errs/"%x_error.%j" #write out slurm error reports
#SBATCH --output=outs_errs/"%x_output.%j" #write out any program outpus
#SBATCH --mail-type=BEGIN,END,FAIL #email you when job starts, stops and/or fails

module load blast-plus/2.14.1

cd ../references/
mkdir -p ../annotation

for fasta in *.pep.faa; do
    base=$(basename "$fasta" .pep.faa)

    blastp \
    -query "$fasta" \
    -db blast_dbs/uniprot_sprot_r2024_10_02 \
    -out "../annotation/${base}_SwissProt_out.tab" \
    -evalue 1E-05 \
    -num_threads 48 \
    -max_target_seqs 1 \
    -max_hsps 1 \
    -outfmt 6
done

echo "Blast complete!" $(date)