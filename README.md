# PRICLE_resampling

## general information
Data and code for resampling community composition at the Prairie Invasion and 
Climate Experiment (PRICLE)

## author
Nick Smith (nick.gregory.smith@gmail.com)

## DOI badge
[![DOI](https://zenodo.org/badge/169658762.svg)](https://zenodo.org/badge/latestdoi/169658762)

## folder descriptions

### /data
Folder containing all data used in the analyses.

### /analysis
Folder containing analysis scripts.

## file descriptions

### /data/cover_pft_data_.csv
This file contains cover data from 2012-2019.

- *pft*: plant functional type (c3g = c3 grass, c4g = c4 grass, cf = clonal forb, 
nf = nitrogen fixing forb, of = other forb)
- *plot*: the plot number (1-12)
- *Sum*: the summed estimate cover (%)
- *year*: year of measurement
- *nitrogen*: nitrogen treatment (1 = added nitrogen, 0 = ambient)
- *precipitation*: precipitation treatment (1 = increased rainfall variability, 0 = ambient)
- *block*: experimental block

### /data/diversity_data_.csv
This file contains diversity data from 2012-2019.

- *plot*: the plot number (1-12)
- *D*: Simpson's plant diversity index
- *R*: Plant species richness
- *E*: Simpson's plant evenness index
- *year*: year of measurement
- *nitrogen*: nitrogen treatment (1 = added nitrogen, 0 = ambient)
- *precipitation*: precipitation treatment (1 = increased rainfall variability, 0 = ambient)
- *block*: experimental block

### /analysis/resample_analysis.R
R script for data analyses of resampled data.

### /analysis/resample_plots.R
R script to make plots of resampled data.

### /analysis/plots
Plots of resampled data.

### /analysis/tables
Tables with statistical results.
