
#### Libraries ####

  library(dplyr)
  library(ggplot2)
  library(scales)
  library(stringr)

####

#### Data ####

  poll_d <- readRDS("data/us-pres-state-polling-2004-2016-extra.rds")
  short_state_names <- c("washington-d-c" = "DC",
                         "alabama" = "AL",
                         "alaska"= "AK",
                         "arizona" = "AZ",
                         "arkansas" = "AR",
                         "california" = "CA",
                         "colorado" = "CO",
                         "connecticut" = "CT",
                         "delaware" = "DE",
                         "florida" = "FL",
                         "georgia" = "GA",
                         "hawaii" = "HI",
                         "idaho" = "ID",
                         "illinois" = "IL",
                         "indiana" = "IN",
                         "iowa" = "IA",
                         "kansas" = "KS",
                         "kentucky" = "KY",
                         "louisiana" = "LA",
                         "maine" = "ME",
                         "maryland" = "MD",
                         "massachusetts" = "MA",
                         "michigan" = "MI",
                         "minnesota" = "MN",
                         "mississippi" = "MS",
                         "missouri" = "MO",
                         "montana" = "MT",
                         "nebraska" = "NE",
                         "nevada" = "NV",
                         "new-hampshire" = "NH",
                         "new-jersey" = "NJ",
                         "new-mexico" = "NM",
                         "new-york" = "NY",
                         "north-carolina" = "NC",
                         "north-dakota" = "ND",
                         "ohio" = "OH",
                         "oklahoma" = "OK",
                         "oregon" = "OR",
                         "pennsylvania" = "PA",
                         "rhode-island" = "RI",
                         "south-carolina" = "SC",
                         "south-dakota" = "SD",
                         "tennessee" = "TN",
                         "texas" = "TX",
                         "utah" = "UT",
                         "vermont" = "VT",
                         "virginia" = "VA",
                         "washington" = "WA",
                         "west-virginia" = "WV",
                         "wisconsin" = "WI",
                         "wyoming" = "WY")
  
  short_state_tbl <- tibble(short_state = short_state_names, state = names(short_state_names))
  
  poll_d <- poll_d %>% left_join(short_state_tbl)
  
####

#### Summarise by state ####
  
  state_avg <- poll_d %>% group_by(state,year,short_state) %>% summarise(abs_mean_error = mean(abs(poll_error), na.rm = T), 
                                                                         und_mean = mean(Undecided, na.rm = T)/100,  
                                                                         result_margin6 = first(result_margin6),
                                                                         vote_outcome = first(ifelse(Dem_vote > Rep_vote, "Dem.", "Rep."))
                                                                         )
  
####

y_format <- function(x) {
  if (length(x) == 0)
    return(character())
  x <- plyr::round_any(x, scales:::precision(x)/100)
  x <- sprintf("%0.1f", x*100)
  paste0(x)
}

x_format <- function(x) {
  if (length(x) == 0)
    return(character())
  x <- plyr::round_any(x, scales:::precision(x)/100)
  x <- sprintf("%0.1f", x*100)
  paste0(x)
}

#### Figure 1 ####
  
  # # annotate close 2016 only
  ann_move <- 0.01 
  
  ann_dat <- state_avg %>% 
    filter(!(year == 2008 & state == "rhode-island"),!(year == 2012 & state == "hawaii")) %>% 
    filter(year == 2016, result_margin6 == "Close margin") %>% 
    ungroup() %>%
    mutate(und_mean = und_mean + c(0,ann_move/3,0,0,0,0,0,-ann_move/3,0,0,0,0,0),
           abs_mean_error = abs_mean_error + c(0,-ann_move/10,0,0,0,ann_move/5,0,-ann_move/10,0,0,0,0,0))
    
    # mutate(und_mean = und_mean + c(ann_move,ann_move,-ann_move,-ann_move,-ann_move,-ann_move*1.25,ann_move*1.25,-ann_move,ann_move,-ann_move,-ann_move,ann_move,-ann_move),
    #        abs_mean_error = abs_mean_error + c(0,0,0,0,0,0,0,-ann_move/6,0,0,0,0,0))
  
  state_avg %>% filter(!(year == 2008 & state == "rhode-island"),!(year == 2012 & state == "hawaii")) %>% 
    ggplot() + 
    geom_point(aes(y = abs_mean_error, x = und_mean), size = 1.5, alpha = 0.5) + 
    geom_smooth(aes(y = abs_mean_error, x = und_mean), method = "lm", se = F, fullrange = F, colour = "black", linetype = 1) +
    geom_text(aes(y = abs_mean_error, x = und_mean, label = short_state), data = ann_dat, size = 4.25, alpha = 0.55) + 
    facet_grid(result_margin6~year, scales = "free_y") +
    theme_bw() +
    scale_y_continuous("Mean absolute error (%)", labels = y_format) + 
    scale_x_continuous("Mean undecided voters (%)", labels = x_format) + 
    expand_limits(y = 0, x = 0) +
    theme(axis.text=element_text(size=18), axis.title = element_text(size=20), strip.text = element_text(size=16))

####
  
#### Animation: Figure 1 ####
  
  library(gganimate)
  
p <- state_avg %>% filter(!(year == 2008 & state == "rhode-island"),!(year == 2012 & state == "hawaii")) %>% 
    filter(result_margin6 == "Close margin") %>%
    ggplot() + 
    geom_smooth(aes(y = abs_mean_error, x = und_mean), method = "lm", se = F, fullrange = F, colour = "black", linetype = 1) +
    geom_point(aes(y = abs_mean_error, x = und_mean, colour = vote_outcome), size = 2, alpha = 0.85) + 
    #geom_text(aes(y = abs_mean_error, x = und_mean, label = short_state), size = 4.25, alpha = 0.55) + 
    theme_bw() + 
    guides(colour=FALSE)+
    scale_y_continuous("Mean absolute error (%)", labels = y_format) + 
    scale_x_continuous("Mean undecided voters (%)", labels = x_format) + 
    scale_color_manual(values = c("Rep." = "red", "Dem." = "blue")) +
    expand_limits(y = 0, x = 0) +
    theme(plot.margin = unit(c(1,1,0.25,1),"cm"), axis.text=element_text(size=14), axis.title = element_text(size=16, margin = margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm")), title = element_text(size=18), plot.caption = element_text(hjust = 0, size=12, margin = margin(t = 0.75, r = 0, b = 0, l = 0, unit = "cm"))) +
    transition_states(year, transition_length = 1, state_length = 2, wrap = F) +
    labs(title = "{next_state} state undecided voters vs. polling error", caption = "In US presidential races where margin of victory was \u2264 6%") 

animate(p,length = 20, width = 525, height = 400)
 
####

