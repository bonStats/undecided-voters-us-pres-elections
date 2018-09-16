# Data and code to accompany paper "Polling bias and undecided voter allocations: US Presidential elections, 2004 - 2016"

Cite as: JJ Bon, T Ballard, B Baffour (2018). "Polling bias and undecided voter allocations: US Presidential elections, 2004 - 2016". Journal of the Royal Statistical Society (Series A). Forthcoming.

 - Corresponding Author: Joshua J Bon
 - Email: joshuajbon@gmail.com
 - Web: https://joshuabon.com
 - Git: https://github.com/bonStats/undecided-voters-us-pres-elections/

## Directory description

- Top: contains all `R` code for running models and reproducing plots and tables in the paper
- `data/`: Contain the state-level polling and voting data
- `stan_models/`: contains `.stan` code that define (and estimate by HMC) the models
- `fitted_models/`: Folder for fitted `.stan` models and summary outputs from those models 

The `fitted_models/` folder may be empty due to large size of files. Run the models and posterior calculations to populate.

## Data description
Two data sets are in the `data/` directory.

### us-pres-state-voting-2004-2016 
This data contains the election *results* for the 2004, 2008, 2012, and 2016 US presidential election by state. Is in both `.csv` and `.rds` format. It has columns:

- state
- year
- state_year
- state_year_id
- Dem_vote
- Rep_vote
- swing_state_year4
- short_state
- result_margin6
- year_id

### us-pres-state-polling-2004-2016 
This data contains the election polls for the 2004, 2008, 2012, and 2016 US presidential election by state. Is in both `.csv` and `.rds` format. It has columns:

- Dem_poll
- Rep_poll
- Undecided
- sample_size
- mean_days_to_election
- start_days_to_election
- end_days_to_election
- state
- year
- state_year
- pollster
- state_year_id
- n
- fnd
- rpl
- pollster2
- year_id
- result_margin6
- rmargin_year
- rmargin_year_id
- npollsterpolls
- pollster_grp
- pollster_id 

## R code description

- `state-polls-original-model.R`:
- `state-polls-extended-model-proportionate.R`:
- `state-polls-extended-model-even.R`:
- `posterior-calcs.R`:
- `paper-outputs.R`: