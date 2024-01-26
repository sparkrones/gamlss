# gamlss

## code explanation
 - 00_max_extract.py: to extract annual max outflows (1980-2100) from designated point (ix, iy)
 - 01_func.R: all functions
    - osse(mu, sigma): for 03_osse2csv.R
    - gamlss_mdl(osse_data, e): e_df
    - gumbel_mdl(osse_data, e): e_df
    - evaluation(truth_model, gamlss e_df, stationary e_df, e): used in eval_dist()
    - eval_dist(n,  osse_data, e, model): gam_list, gum_list   # error calcuation
 - 02_gamlss.R: gamlss fitting
 - 03_osse2csv.R: export the osse dataset to csv/binary files

 - 04_evaluation.R: error calculation of gamlss and stationary estimations

### output dataset's variables
 - df: year, outflow, f_hundred, r, nonx_prob
 - e_df: year, outflow, f_hundred



## Overview

## Methods
### Environment Setup for R
 - server: to install r-env

Example:
```bash
Rscript /work/a06/stakahashi/workspace/code/01_gamlss.R
```
      
 - local: to install Rstudio

### GAMLSS fitting
1. find (ix, iy) of the research point (serverにあるglbなんちゃら)
2. extract annual max outflow (AMAX), which is calculated by CaMa-Flood, from 1980 to 2100   # 00_max_extract.py
3. estimate 100-year flood benchmark from stationary gumbel distribution
4. fit 120 years AMAX to GAMLSS model (gumbel distribution in linear)   # 02_gamlss.R
   - non-exceedance probabilities of the benchmark 100-year flood calculated above: pGU()
   - return period (corresponding to non-exceedance prob)
   - to estimate the 100-year flood: qGU()
  
### OSSE
1. generate complemented 10000 dataset (only once): rGU()   # 03_osse2csv.R
2. calculate error values in several ensembles and plot error distribution (n=100) in each ensemble   # 04_evaluation.R
