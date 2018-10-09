# Data and code to accompany paper "Polling bias and undecided voter allocations: US Presidential elections, 2004 - 2016"

Cite as: JJ Bon, T Ballard, B Baffour (2018). "Polling bias and undecided voter allocations: US Presidential elections, 2004 - 2016". Journal of the Royal Statistical Society (Series A). Early view: http://dx.doi.org/10.1111/rssa.12414.

 - Corresponding Author: Joshua J Bon
 - Email: joshuajbon@gmail.com
 - Web: https://joshuabon.com
 - Github: https://github.com/bonStats/undecided-voters-us-pres-elections/
 - arXiv: https://arxiv.org/abs/1703.09430

## Directory description

- Top: contains all `.R` code for running models and reproducing plots and tables in the paper
- `data/`: Contain the state-level polling and voting data
- `stan_models/`: contains `.stan` code that define (and estimate by HMC) the models
- `fitted_models/`: Folder for fitted `.stan` models and summary outputs from those models
- `eda/`: Contains example(s) of exploratory data analysis, including Figure 1 in the paper.

The `fitted_models/` folder may be empty due to large size of files. Run the models and posterior calculations to populate.

## Data description
Two data sets are in the `data/` directory.

### Election results: `us-pres-state-voting-2004-2016.*` 
This data contains the election *results* for the 2004, 2008, 2012, and 2016 US presidential election by state. It is in both `.csv` and `.rds` (`tibble`) format. It has columns:

- `state`: State names and Washington D.C. (e.g. `"washington-d-c"`)
- `year`: Presidential election year: `2004`, `2008`, `2012`, `2016`
- `state_year`: Concatenation of `state` and `year`: (e.g. `washington-d-c_2016`)
- `state_year_id`: Unique integer ids for `state_year`
- `Dem_vote`: Vote percentage won by Democratic candidate (`0`-`100`)
- `Rep_vote`: Vote percentage won by Republican candidate (`0`-`100`)
- `short_state`: Two character state id (e.g. `DC`)
- `result_margin6`: Category for margin of voting *result*. Strong Dem. win (margin > 6%), Strong Rep. win (margin > 6%), or close margin (margin < 6%)
- `year_id`: Unique integer ids for `year`

### Pre-election polls: `us-pres-state-polling-2004-2016.*` 
This data contains the election *polls* for the 2004, 2008, 2012, and 2016 US presidential election by state. It is in both `.csv` and `.rds` (`tibble`) format. It has columns:

- `Dem_poll`: Polled percentage support for Democratic candidate (`0`-`100`)
- `Rep_poll`: Polled percentage support for Republican candidate (`0`-`100`)
- `Undecided`: Polled percentage of undecided voters (`0`-`100` and `NA`)
- `sample_size`: Reported sample size of poll
- `mean_days_to_election`: Number of days until election, measured as mean of start and end date of poll
- `start_days_to_election`: Number of days until election, measured from start date of poll 
- `end_days_to_election`: Number of days until election, measured from end date of poll 
- `state`: State names and Washington D.C. (e.g. `"washington-d-c"`)
- `year`: Presidential election year: `2004`, `2008`, `2012`, `2016`
- `state_year`: Concatenation of `state` and `year`: (e.g. `washington-d-c_2016`)
- `pollster`: Original name of polling agency or agencies
- `state_year_id`: Unique integer ids for `state_year`
- `pollster2`: Cleaned name of polling agency or agencies
- `year_id`: Unique integer ids for `year`
- `result_margin6`: Category for margin of voting *result*. Strong Dem. win (margin > 6%), Strong Rep. win (margin > 6%), or close margin (margin < 6%)
- `rmargin_year`: `result_margin6` concatenated with `year`
- `rmargin_year_id`: Unique integer ids for `rmargin_year`
- `pollster_grp`: Further cleaned and grouped polling agencies or institutes
- `pollster_id`:  Unique integer ids for `pollster_grp`

## R code description

- `state-polls-original-model.R`: Fit original SRGG model
- `state-polls-extended-model-proportionate.R`: Fit extended SRGG model with baseline *proportionate* split of undecided voters
- `state-polls-extended-model-even.R`: Fit extended SRGG model with baseline *even* split of undecided voters
- `posterior-calcs.R`: Calculate additional posterior quantities from the model
- `paper-outputs.R`: Reproduce all plots and tables for the paper

## Session Info
``` 
sessionInfo()
#> R version 3.5.1 (2018-07-02)
#> Platform: x86_64-apple-darwin15.6.0 (64-bit)
#> Running under: macOS High Sierra 10.13.6
#> 
#> Matrix products: default
#> BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#> LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
#> 
#> locale:
#> [1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8
#> 
#> attached base packages:
#> [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] shinystan_2.5.0    shiny_1.1.0        gtools_3.8.1       plyr_1.8.4        
#>  [5] rstan_2.17.3       StanHeaders_2.17.2 rv_2.3.2           stringr_1.3.1     
#>  [9] scales_1.0.0       ggplot2_3.0.0      bindrcpp_0.2.2     dplyr_0.7.6       
#> 
#> loaded via a namespace (and not attached):
#>  [1]  Rcpp_0.12.18      lattice_0.20-35   zoo_1.8-4         assertthat_0.2.0  digest_0.6.16
#>  [6]  utf8_1.1.4        mime_0.5          R6_2.2.2          ggridges_0.5.0    stats4_3.5.1
#>  [11] colourpicker_1.0  pillar_1.3.0      rlang_0.2.2       lazyeval_0.2.1    miniUI_0.1.1.1
#>  [16] rstudioapi_0.7    DT_0.4            shinythemes_1.1.1 shinyjs_1.0       devtools_1.13.6
#>  [21] readr_1.1.1       htmlwidgets_1.2   igraph_1.2.2      munsell_0.5.0     compiler_3.5.1
#>  [26] httpuv_1.4.5      pkgconfig_2.0.2   base64enc_0.1-3   htmltools_0.3.6   tidyselect_0.2.4 
#>  [31] tibble_1.4.2      gridExtra_2.3     threejs_0.3.1     fansi_0.3.0       crayon_1.3.4     
#>  [36] withr_2.1.2       later_0.7.4       grid_3.5.1        xtable_1.8-3      gtable_0.2.0     
#>  [41] magrittr_1.5      cli_1.0.0         stringi_1.2.4     reshape2_1.4.3    promises_1.0.1   
#>  [46] dygraphs_1.1.1.6  xts_0.11-1        tools_3.5.1       glue_1.3.0        markdown_0.8     
#>  [51] purrr_0.2.5       hms_0.4.2         crosstalk_1.0.0   rsconnect_0.8.8   yaml_2.2.0       
#>  [56] inline_0.3.15     colorspace_1.3-2  bayesplot_1.6.0   memoise_1.1.0     bindr_0.1.1 
```