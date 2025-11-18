# Time Series RNA-seq Bioinformatic Processing

Script Written By: Zoe Dellaert
Last Updated: 11/17/2024

## Quick directory references

- raw data is located in `/project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp`
- all other project files are located in `/project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species`

## Project info

- Sample prep: https://github.com/zdellaert/TimeSeries/blob/main/protocols/Sampling.md
- RNA extraction protocol: https://github.com/zdellaert/TimeSeries/blob/main/protocols/Bulk_DNA_RNA_Extractions_Zymo_Quick_Miniprep.md
- RNA plate sent: https://zdellaert.github.io/ZD_Putnam_Lab_Notebook/TimeSeries-Plate-Prep/
  - **Note to self update 2 re-extracted samples sent 10/22/2025**
- RNA extractions: 
- Sample list: https://github.com/zdellaert/TimeSeries/blob/main/4-multi-species/data/completed_bulk_RNA_extractions_3species.csv

## Sequencing information

- Library prep and sequencing done by Genohub Service Provider: Oklahoma Medical Research Foundation NGS Core
- Library type: Illumina - RNA (poly-A selected)
- Library prep kit: Watchmaker Genomics mRNA kit
- Instrument: Illumina NovaSeq X Plus - 25B - PE 150 Cycle
- Read length: 2 x 150bp (Paired End)
- Number of samples: 126
- Guaranteed number of pass filter PE reads/sample: 30M (15M in each direction)

## Make directory structure on Unity

```
#Make directory for raw data
mkdir /project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp

#Make directory for processed data, scripts, outputs, and symlinked raw data - clone repo
cd /project/pi_hputnam_uri_edu/zdellaert/
git clone https://github.com/zdellaert/TimeSeries.git

#Enter project directory
cd /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species

#Make folder for scripts, script outputs, raw data, and output
mkdir scripts
mkdir scripts/outs_errs
mkdir data_RNA
mkdir output_RNA
```

## Transfer data from genohub using AWS

1. First, on unity login node, run `module load awscli-v2/2.15.53`
2. Then follow the prompts and enter the AWS Access Key ID and Secret Key as prompted and instructed in the genohub instructions
3. This writes your config information into a file in your home directory, located at `~/.aws/config`
4. Now we can download the data using a script without having to enter any other access info
5. To view what's in the bucket:
   1. `module load awscli-v2/2.15.53`
   2. `aws s3 ls s3://genohub####### --recursive`

```
cd /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/scripts
nano 01_data_download.sh

#enter text in next code chunk
```

### Script: 01_data_download.sh

```
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
aws s3 sync s3://genohub####### . --no-progress

#compute md5sum after sync is complete
md5sum *.fastq.gz > 20251118_URI.md5
```

## Check integrity of data transfer

raw data is located in `/project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp`

```
cd /project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp/

#concatenate genohub-provided md5s
cat *gz.md5 > genohub.md5

#use diff command to see if there is a differnece between the checksums
# using -w to ignore spaces (the URI file is formatted slightly differently) 

diff -w genohub.md5 20251118_URI.md5 

# no output, so no difference between the md5s. verified by inspecting manually.

# copy both md5s to data directory
cp genohub.md5 /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/data_RNA
cp 20251118_URI.md5  /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/data_RNA
```

Data appears to have been transferred successfully from genohub.

## Symlink raw data files into data_RNA

```
ln -s /project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp/*.fastq.gz /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/data_RNA
```

## QC raw files

```
cd /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/scripts
nano 02_raw_qc.sh

#enter text in next code chunk
```

```
#!/usr/bin/env bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --signal=2
#SBATCH --no-requeue
#SBATCH --mem=200GB
#SBATCH -t 12:00:00
#SBATCH --mail-type=BEGIN,END,FAIL #email you when job starts, stops and/or fails
#SBATCH --error=../scripts/outs_errs/%x_error.%j #if your job fails, the error report will be put in this file
#SBATCH --output=../scripts/outs_errs/%x_output.%j #once your job is completed, any final job report comments will be put in this file

# load modules needed
module load parallel/20240822
module load fastqc/0.12.1
module load uri/main
module load MultiQC/1.12-foss-2021b

#go into directory with raw data (symlinks)
cd /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/data_RNA

#make raw_qc output folder
mkdir -p ../output_RNA/raw_qc/

# Create an list of fastq files to process
files=( *.fastq.gz )

# Run fastqc in parallel
echo "Starting fastqc..." $(date)
parallel -j 20 "fastqc {} -o ../output_RNA/raw_qc/ && echo 'Processed {}'" ::: "${files[@]}"
echo "fastQC done." $(date)

#Compile MultiQC report from FastQC files
cd ../output_RNA/raw_qc/
multiqc --interactive .

echo "Initial QC of RNA-seq data complete." $(date)
```