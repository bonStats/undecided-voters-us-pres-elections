# Data and code to accompany paper "Polling bias and undecided voter allocations: US Presidential elections, 2004 - 2016"

Cite as: JJ Bon, T Ballard, B Baffour (2018). "Polling bias and undecided voter allocations: US Presidential elections, 2004 - 2016". Journal of the Royal Statistical Society (Series A). Forthcoming.

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
