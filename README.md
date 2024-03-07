# gamlss

## Overview
 - Target of Research: Iwabuchi Water Gate (Arakawa)
 - Dataset: annual max outflow (1980-2099) calculated by CaMa-Flood (ssp2-4.5)
 - Language: R
   - Library: GAMLSS (non-stationary) and extRemes 2.0 (stationary)

## Methods
### Environment Setup for R
 - server: to install r-env
```bash
 conda install conda-forge::r-renv
```

 - local: to install Rstudio - 
   download from here: https://cran.r-project.org/

![RA_gamlss (2)](https://github.com/sparkrones/gamlss/assets/82133070/16966149-d761-49a1-99f9-79649cc98998)


### 1. GAMLSS fitting
1. find (ix, iy) of the research point
   - Please replace "LAT" and "LON" with latitude and longitude of your own research point
 ```bash
 cd /work/a06/stakahashi/glb_06min/src_param
 ```
 ```bash
 ./get_rivinfo latlon "LAT" "LON"
 ```

2. extract annual max outflow (AMAX), which is calculated by CaMa-Flood, from 1980 to 2100
  ```bash
 python /work/a06/stakahashi/workspace/01_max_extract.py <IX> <IY> <OUTPUT_FILE_PATH>
 ```
 > example
 ```bash
 python /work/a06/stakahashi/workspace/01_max_extract.py 3197 542 /work/a06/stakahashi/data/max_y125.bin
 ```

3. estimate the 100-year flood benchmark from stationary gumbel distribution
4. fit 120 years AMAX to GAMLSS model (gumbel distribution in linear)   # 02_gamlss.R
   - non-exceedance probabilities of the benchmark 100-year flood calculated above: pGU()
   - return period (corresponding to non-exceedance prob)
   - to estimate the 100-year flood: qGU()
 ```bash
 Rscript /work/a06/stakahashi/workspace/02_gamlss.R <DATA_PATH>
 ```
 > example
 ```bash
 Rscript /work/a06/stakahashi/workspace/02_gamlss.R /work/a06/stakahashi/data/max_y120.bin
 ```

  
### 2. OSSE
1. generate complemented 3000 dataset (execute only once): rGU()
 ```bash
 Rscript /work/a06/stakahashi/workspace/03_osse2csv.R --output-path="OUTPUT_PATH"
 ```
 > example
 ```bash
 Rscript /work/a06/stakahashi/workspace/03_osse2csv.R --output-path="/work/a06/stakahashi/data/osse_df.csv"
 ```

(2. plot generated osse data)
 ```bash
 Rscript /work/a06/stakahashi/workspace/04_osse_plot.R
 ```

3. calculate error values in several ensembles and plot error distribution (n=100) in each ensemble
 ```bash 
 Rscript /work/a06/stakahashi/workspace/05_evaluation.R "PATH_TO_OSSE"

 ```


## About each file
 - 00_func.R: all functions
    - osse(mu, sigma): to generate osse dataset used in 03_osse2csv.R
    - gamlss_mdl(osse_data, e): to try the designated number of ensembles in gamlss
    - gumbel_mdl(osse_data, e): to try the designated number of ensembles in stationary gumbel
    - evaluation(truth_model, gamlss e_df, stationary e_df, e): to calculate error values in the designated number of ensembles and their standard deviations, normally used in eval_dist()
    - eval_dist(n,  osse_data, e, model): to calculate error distribution of the designated number of samples
      
 - 01_max_extract.py: to extract annual max outflows (1980-2100) from designated point (ix, iy)
 - 02_gamlss.R: to fit annual max outflows to gamlss
 - 03_osse2csv.R: to export the osse dataset in the designated number of ensembles to a csv/binary file
 - 04_evaluation.R: to examine error distributions and their standard deviation of gamlss and stationary estimations

### Output dataset's variables
 - df: year, outflow, f_hundred, r, nonx_prob
 - e_df: year, outflow, f_hundred
