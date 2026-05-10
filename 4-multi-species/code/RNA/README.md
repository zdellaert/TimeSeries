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

## QC Decisions

### Sample Exclusions

Samples were excluded based on:
- Hierarchical clustering (outlier branches)
- Low mapping rates (<10%)
- High rRNA/bacterial contamination
- Visual confirmation of mortality

#### *Pocillopora acuta* (POC)

No samples excluded.

#### *Porites compressa* (POR)

- see [bioinformatic processing doc](https://github.com/zdellaert/TimeSeries/blob/main/4-multi-species/scripts/README.md) for rRNA contamination screen

| Sample | Mapping % | rRNA % | Reason | Color Score Image |
|--------|-----------|--------|--------|-------------------|
| POR_R24_H1 | 7.7% | 82% | High rRNA | <img src="https://github.com/zdellaert/TimeSeries/blob/main/1-Pcom/data/Images/20250626/IMG_4309.jpeg" width="300"> |
| POR_R72_H1 | 6.4% | 84% | Visible mortality, High rRNA | <img src="https://github.com/zdellaert/TimeSeries/blob/main/1-Pcom/data/Images/20250628/IMG_4330.jpeg" width="300"> |
| POR_R72_H2 | 8.8% | 81% | Visible mortality, High rRNA | <img src="https://github.com/zdellaert/TimeSeries/blob/main/1-Pcom/data/Images/20250628/IMG_4333.jpeg" width="300"> |

**Impact:** R24 heat n=2, R72 heat n=1

#### *Montipora capitata* (MON)

- see [bioinformatic processing doc](https://github.com/zdellaert/TimeSeries/blob/main/4-multi-species/scripts/README.md) for bacterial contamination screen

| Sample | Mapping % | Bacterial % | Reason | Color Score Image |
|--------|-----------|-------------|--------|-------------------|
| MON_R72_H1 | 0.5% | 10.6% | Visible mortality, bacterial contamination | <img src="https://github.com/zdellaert/TimeSeries/blob/main/2-Mcap/data/Images/20250705/IMG_4516.jpeg" width="300"> |
| MON_R72_H2 | 0.5% | 32.0% | Visible mortality, bacterial contamination | <img src="https://github.com/zdellaert/TimeSeries/blob/main/2-Mcap/data/Images/20250705/IMG_4480.jpeg" width="300"> |

**Impact:** R72 heat n=1
