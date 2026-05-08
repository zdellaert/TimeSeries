# Bulk RNA-seq Time Series Analysis

## Setup

### 1. Clone the annotation repository

I have a [central repository](https://github.com/zdellaert/HI_genome_annotations.git) for the genome annotations for the three species I have been studying throughout my dissertation. My annotation files are stored there, and this code is written so you clone this repository in the same folder as the TimeSeries repository. If you clone the repo at the same level as this project, you should be all set! All annotation code is available in that repository as well.

```
# Navigate to parent folder (where TimeSeries/ is located)
#If you are in the working directory of this Rmd file (TimeSeries/4-multi-species/code/RNA), you need to move out four levels:

pwd
cd ../../../../

#confirm correct parent directory
ls
# one of the directories should be "TimeSeries"

git clone https://github.com/zdellaert/HI_genome_annotations.git
#ssh: git clone git@github.com:zdellaert/HI_genome_annotations.git

ls
# now there should be both "TimeSeries" and "HI_genome_annotations" directories.
```