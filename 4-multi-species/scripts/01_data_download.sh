#!/usr/bin/env bash
#SBATCH --cpus-per-task=1
#SBATCH --mem=24GB
#SBATCH --time 24:00:00
#SBATCH --error=../scripts/outs_errs/%x_error.%j
#SBATCH --output=../scripts/outs_errs/%x_output.%j
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80

#load AWS module
module load awscli-v2/2.15.53

#enter raw data directory
cd /project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp/

# sync data from aws bucket
aws s3 sync s3://genohub###### . --no-progress

#compute md5sum after sync is complete
md5sum *.fastq.gz > 20251118_URI.md5
