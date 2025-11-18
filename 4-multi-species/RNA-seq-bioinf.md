# Time Series RNA-seq Bioinformatic Processing

Script Written By: Zoe Dellaert
Last Updated: 11/17/2024

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

#Make directory for processed data, scripts, outputs, and symlinked raw data
mkdir /project/pi_hputnam_uri_edu/zdellaert/TimeSeries

#Enter project directory
cd /project/pi_hputnam_uri_edu/zdellaert/TimeSeries

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
cd /project/pi_hputnam_uri_edu/zdellaert/TimeSeries/scripts
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

cd /project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp/
aws s3 sync s3://genohub####### . --no-progress
```


raw data is located in `/project/pi_hputnam_uri_edu/raw_sequencing_data/20251117_Timeseries_3sp`

