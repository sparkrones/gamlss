# gamlss

## code explanation
 - 00_max_extract.py: to extract annual max outflows (1980-2100) from designated point (ix, iy)
 - 01_func.R: all functions
    - osse(mu, sigma): to generate osse dataset used in 03_osse2csv.R
    - gamlss_mdl(osse_data, e): to try the designated number of ensembles in gamlss
    - gumbel_mdl(osse_data, e): to try the designated number of ensembles in stationary gumbel
    - evaluation(truth_model, gamlss e_df, stationary e_df, e): to calculate error values in the designated number of ensembles and their standard deviations, normally used in eval_dist()
    - eval_dist(n,  osse_data, e, model): to calculate error distribution of the designated number of samples
      
 - 02_gamlss.R: to fit annual max outflows to gamlss
 - 03_osse2csv.R: to export the osse dataset in the designated number of ensembles to a csv/binary file
 - 04_evaluation.R: to examine error distributions and their standard deviation of gamlss and stationary estimations

### output dataset's variables
 - df: year, outflow, f_hundred, r, nonx_prob
 - e_df: year, outflow, f_hundred



## Overview

## Methods
### Environment Setup for R
 - server: to install r-env

Example:
```bash
Rscript /work/a06/stakahashi/workspace/code/01_func.R
```
      
 - local: to install Rstudio

### GAMLSS fitting
1. find (ix, iy) of the research point
   - Please replace "LAT" and "LON" with latitude and longitude of your own research point
 ```bash
 cd /work/a06/stakahashi/workspace/glb_06min/src_param
 ```
 ```bash
 ./get_rivinfo latlon "LAT" "LON"
 ```

2. extract annual max outflow (AMAX), which is calculated by CaMa-Flood, from 1980 to 2100
 ```bash
 python /work/a06/stakahashi/workspace/code/00_max_extract.py
 ```

3. estimate 100-year flood benchmark from stationary gumbel distribution
4. fit 120 years AMAX to GAMLSS model (gumbel distribution in linear)   # 02_gamlss.R
   - non-exceedance probabilities of the benchmark 100-year flood calculated above: pGU()
   - return period (corresponding to non-exceedance prob)
   - to estimate the 100-year flood: qGU()
 ```bash
 Rscript /work/a06/stakahashi/workspace/code/02_gamlss.R
 ```

  
### OSSE
1. generate complemented 5000 dataset (execute only once): rGU()
 ```bash
 Rscript /work/a06/stakahashi/workspace/code/03_osse2csv.R
 ```

2. calculate error values in several ensembles and plot error distribution (n=100) in each ensemble
 ```bash
 Rscript /work/a06/stakahashi/workspace/code/04_evaluation.R
 ```
