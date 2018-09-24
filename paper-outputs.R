########################################################################################
# Reproducible model outputs - Undecided voters and polling bias in US presidential 
# elections
# Author: Joshua J Bon
# Notes: 
# - To appear in JRSS-A article by Bon, Ballard & Baffour
#
# - arXiv: https://arxiv.org/abs/1703.09430
#
# - Models: 
#   - Original SRGG model for 2004, 2008, 2012, 2016 US presidential elections
#   - Extended SRGG with bias away from proportional allocation of undecided voters
#   - Extended SRGG with bias away from even allocation of undecided voters
#
# - SRGG: Shirani-Mehr, H., Rothschild, D., Goel, S., & Gelman, A. (2018). 
#   Disentangling bias and variance in election polls. 
#   Journal of the American Statistical Association, 1-23
#
############################################################################################

#### Directory ####

  setwd("~/Dropbox/Research/Code/undecided-voters-us-pres-elections/")

####

#### Libraries ####

  library(dplyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  # devtools::install_version("rv", version="2.3.2", repos="http://cran.r-project.org")
  library(rv)

####


#### Load RVs from models ####

 SRGG <- readRDS("fitted-models/SRGG-summary-rvs.rds")
 
 ext_prop <- readRDS("fitted-models/extended-SRGG-prop-summary-rvs.rds")
 
 ext_even <- readRDS("fitted-models/extended-SRGG-even-summary-rvs.rds")
 
####
 
#### Extra rv functions ####
 
 rv_sm <- function(rr, cnames = NULL){
   xx <- rbind(
     round(rvmean(rr), digits = 1),
     round(rvsd(rr), digits = 2)
   )
   
   dimnames(xx) <- list(c("mean", "sd"), 1:ncol(xx))
   
   if(!is.null(cnames)) colnames(xx) <- cnames
   
   return(xx)
 }
 
####

#### Table 1 ####
 
 SRGG$y_id_ord <- SRGG$vote_data %>% select(year,year_id) %>% unique() %>% arrange(year_id) %>% select(year) %>% collect() %>% .[[1]]
 
 # Table 1: overall 
 rv_sm(SRGG$mabs_b_r * 100)
 rv_sm(SRGG$mabs_b_r_e * 100)
 rv_sm(SRGG$m_sd_r * 100)
 
 
 # Table 1: yearly
 rv_sm(SRGG$yearly_mabs_b_r * 100, cnames = SRGG$y_id_ord)
 rv_sm(SRGG$yearly_mabs_b_r_e * 100, cnames = SRGG$y_id_ord) 
 rv_sm(SRGG$yearly_m_sd_r * 100, cnames = SRGG$y_id_ord) 
 
 ####
 
#### Table 2 ####

  # Table 2: overall 
  rv_sm(ext_prop$mabs_b_r * 100)
  rv_sm(ext_prop$mabs_b_r_e * 100)
  rv_sm(ext_prop$mabs_b_r_u * 100)
  rv_sm(ext_prop$mabs_b_r_h * 100)
  rv_sm(ext_prop$m_sd_r * 100)
  rv_sm(mean(ext_prop$alpha_und) * 100)


  # Table 2: yearly
  rv_sm(ext_prop$yearly_mabs_b_r * 100, cnames = ext_prop$y_id_ord)
  rv_sm(ext_prop$yearly_mabs_b_r_e * 100, cnames = ext_prop$y_id_ord) 
  rv_sm(ext_prop$yearly_mabs_b_r_u * 100, cnames = ext_prop$y_id_ord)
  rv_sm(ext_prop$yearly_mabs_b_r_h * 100, cnames = ext_prop$y_id_ord)
  rv_sm(ext_prop$yearly_m_sd_r * 100, cnames = ext_prop$y_id_ord)
  rv_sm(ext_prop$phi_und * 100, cnames = ext_prop$y_id_ord)
  
  # others
  rv_sm(ext_prop$yearly_mabs_b_r_0uh * 100, cnames = ext_prop$y_id_ord)
  rv_sm(ext_prop$yearly_mabs_b_r_e_0uh * 100, cnames = ext_prop$y_id_ord)

#### 

#### Figure 4 ####
  
  rmargin_year_id_df <- ext_prop$poll_data %>% select(rmargin_year_id, result_margin6, year_id, year) %>% unique() %>% arrange(rmargin_year_id)
  rmargin_year_id_df
  
  as.rvsummary(ext_prop$gamm)
  
  gamm_summary <- summary(as.rvsummary(ext_prop$gamm)) %>% 
    bind_cols(rmargin_year_id_df) %>% 
    mutate(Year = ordered(year, levels = c(2004,2008,2012,2016)),
           Resultmargin = ordered(result_margin6, levels = c("Strong Rep.", "Close margin", "Strong Dem.")))
  
  gamm_summary
  
  # FIG4_gamma_CI_ext_prop_2004-2016
  ggplot(gamm_summary) + 
    geom_segment(aes(y = Resultmargin, yend = Resultmargin, x = `2.5%`, xend = `97.5%`)) +
    geom_segment(aes(y = Resultmargin, yend = Resultmargin, x = `25%`, xend = `75%`), size = 1.5) + 
    geom_vline(xintercept = 0, linetype = "dotted") + 
    scale_x_continuous("Gamma credible intervals") + 
    theme_bw()  +
    theme(axis.text=element_text(size=18), axis.title = element_text(size=22), strip.text = element_text(size=18), 
          strip.text.y = element_text(angle = 0), panel.spacing = unit(0, "lines"), 
          strip.background = element_blank()) +
    facet_grid(Year ~ .,  scales = "free_x", space = "free_x") +
    ylab("Election result margin")
  
  #ggsave(filename = "FIG4_gamma_CI_ext_prop_2004-2016.pdf", device = "pdf", width = 30, height = 28, units = "cm")
  
####

#### Figure 5 ####  

  ext_prop$state_measures <- tibble(
    mean_abs_undecided_bias = abs(rvmean(ext_prop$b_r_u)),
    m_eld_und = rvmean(ext_prop$alpha_und),
    year = ext_prop$vote_data$year,
    state = ext_prop$vote_data$state
  )
  
  #FIG5_state_abs_bias_hist_ext_prop
  ggplot(ext_prop$state_measures, aes(x = mean_abs_undecided_bias)) + 
    geom_histogram(alpha = 0.7, colour = "black", binwidth = 0.002, position="identity") + facet_wrap(~year) + 
    scale_x_continuous("Mean absolute bias from undecided voters", labels=percent) +
    theme_bw() + ylab("Number of states") + 
    theme(legend.position="bottom") + guides(fill = guide_legend(label.position = "bottom")) + 
    theme(axis.text=element_text(size=18), axis.title = element_text(size=22), legend.text=element_text(size=14), strip.text = element_text(size=20))
  
  #ggsave(filename = "FIG5_state_abs_bias_hist_ext_prop.pdf", device = "pdf", width = 30, height = 21, units = "cm")
  
####
  
#### Figure 6 ####
  
  ext_even <- within(ext_even,{
    states_eld_bias_summary <- b_r_e %>% summary() %>% 
      mutate(state_year_id = 1:nrow(.)) %>% 
      left_join(vote_data, by = "state_year_id") %>%
      mutate(contains_0_in_95_percent = (`2.5%` < 0 & `97.5%` > 0))
  })
  
  ext_even$states_eld_bias_summary
  
  ext_prop <- within(ext_prop,{
    states_eld_bias_summary <- b_r_e %>% summary() %>% 
      mutate(state_year_id = 1:nrow(.)) %>% 
      left_join(vote_data, by = "state_year_id")  %>%
      mutate(contains_0_in_95_percent = (`2.5%` < 0 & `97.5%` > 0))
  })
  
  ext_comp_eld_all <- rbind(
    ext_even$states_eld_bias_summary %>%  mutate(model = "Even"),
    ext_prop$states_eld_bias_summary %>% mutate(model = "Proportional")
    )
  
  new_order <- ext_comp_eld_all %>% filter(model == "Proportional", year == 2016) %>%  with(order(`2.5%`,decreasing = T))
  ext_comp_eld_all <- ext_comp_eld_all %>% mutate(state = factor(state,levels = unique(state)[new_order]))
  
  nice_state_names <- function(x){
    
    out <- gsub(pattern = "-", replacement = " ", x = x)
    out <- str_to_title(out)
    out <- gsub(pattern = "D C", replacement = "DC", x = out)
    out
  }
  
  ext_comp_eld_all <- ext_comp_eld_all %>% mutate(State = nice_state_names(state)) %>% 
    mutate(State = factor(State,levels = unique(State)[new_order]))
  
  # FIG6-ELD_by_model_type 
  
  ext_comp_eld_all %>% filter(year %in% c(2016)) %>% ggplot(aes(x = State, ymin = `2.5%`, ymax = `97.5%`, colour = model)) + 
    geom_linerange(position = position_dodge(width = 0.8), size = 1.2) + 
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_bw() + scale_x_discrete("State") +
    scale_y_continuous("Election day bias", label = percent, limits = c(-0.08,0.06), breaks = seq(from = -0.08, to = 0.06, by = 0.02)) +
    theme(legend.position = "top") + scale_colour_grey("Allocation:", start = 0, end = 0.6) +
    coord_flip() +
    theme(axis.text=element_text(size=18), axis.title = element_text(size=22), 
          legend.text =  element_text(size=22), 
          legend.title = element_text(size=22))
  
  #ggsave(filename = "FIG6-ELD_by_model_type.pdf", device = "pdf", width = 30, height = 40, units = "cm")
  
####
  
#### Figure 7 ####
  
  house_id_df <- ext_prop$poll_data %>% group_by(pollster_id, pollster_grp) %>% dplyr::summarise(n_polls = n()) %>% arrange(pollster_id)
  house_id_df %>% summarise(sum(n_polls))
  
  hbias_summary <- summary(ext_prop$kappa) %>% 
    bind_cols(house_id_df) %>% filter(pollster_id != 1) %>% #filter(abs(mean) > 0.0075) %>% 
    mutate(pollster_grp = str_replace(pollster_grp, "\\(R\\)|\\(DAmericansUnitedforChange\\)","")) %>% 
    mutate(Pollster_grp = ordered(pollster_grp, levels = pollster_grp[order(-mean)]))
  
  mean(hbias_summary$n_polls)
  range(hbias_summary$n_polls)
  
  ggplot(hbias_summary) +
    geom_segment(aes(y = Pollster_grp, yend = Pollster_grp, x = `2.5%`, xend = `97.5%`)) +
    geom_segment(aes(y = Pollster_grp, yend = Pollster_grp, x = `25%`, xend = `75%`), size = 1.5) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_x_continuous("Kappa credible intervals", limits = c(-0.105,0.105)) + ylab("Polling agency or group") +
    theme_bw()  +
    theme(axis.text.x=element_text(size=16), axis.text.y=element_text(size=12), axis.title = element_text(size=22))
  
  #ggsave(filename = "FIG7_kappa_CI_ext_prop_2004-2016.pdf", device = "pdf", width = 21, height = 30, units = "cm")
  
####
  
#### Table 5 ####
  
  # look at worst performers:
  hbias_percent <- summary(ext_prop$b_h * 100) %>%
    bind_cols(house_id_df) %>% dplyr::select(`mean`,`sd`,pollster_grp) %>% filter(pollster_grp != "0_None")
  
  hbias_percent %>% filter(abs(mean) > 0.5) %>% select(pollster_grp,mean,sd) %>% xtable::xtable()
  
  
####
  
#### Figure 8 ####
  
  rmargin_year_id_df2 <- ext_even$poll_data %>% select(rmargin_year_id, result_margin6, year_id, year) %>% unique() %>% arrange(rmargin_year_id)
  rmargin_year_id_df2
  
  gamm_summary2 <- summary(as.rvsummary(ext_even$gamm)) %>% 
    bind_cols(rmargin_year_id_df2) %>% 
    mutate(Year = ordered(year, levels = c(2004,2008,2012,2016)),
           Resultmargin = ordered(result_margin6, levels = c("Strong Rep.", "Close margin", "Strong Dem.")))
  
  gamm_summary2
  
  # FIG8_gamma_CI
  ggplot(gamm_summary2) + 
    geom_segment(aes(y = Resultmargin, yend = Resultmargin, x = `2.5%`, xend = `97.5%`)) +
    geom_segment(aes(y = Resultmargin, yend = Resultmargin, x = `25%`, xend = `75%`), size = 1.5) + 
    geom_vline(xintercept = 0, linetype = "dotted") + 
    scale_x_continuous("Gamma credible intervals") + 
    theme_bw()  +
    theme(axis.text=element_text(size=18), axis.title = element_text(size=22), strip.text = element_text(size=18), 
          strip.text.y = element_text(angle = 0), panel.spacing = unit(0, "lines"), 
          strip.background = element_blank()) +
    facet_grid(Year ~ .,  scales = "free_x", space = "free_x") +
    ylab("Election result margin")
  
  #ggsave(filename = "FIG8_gamma_CI_ext_50_2004-2016.pdf", device = "pdf", width = 30, height = 28, units = "cm")
  
####
  
#### Table 6 ####
  
  # Table 2: overall 
  rv_sm(ext_even$mabs_b_r * 100)
  rv_sm(ext_even$mabs_b_r_e * 100)
  rv_sm(ext_even$mabs_b_r_u * 100)
  rv_sm(ext_even$mabs_b_r_h * 100) #
  rv_sm(ext_even$m_sd_r * 100)
  rv_sm(mean(ext_even$alpha_und) * 100)
  
  
  # Table 2: yearly
  rv_sm(ext_even$yearly_mabs_b_r * 100, cnames = ext_even$y_id_ord)
  rv_sm(ext_even$yearly_mabs_b_r_e * 100, cnames = ext_even$y_id_ord) 
  rv_sm(ext_even$yearly_mabs_b_r_u * 100, cnames = ext_even$y_id_ord)
  rv_sm(ext_even$yearly_mabs_b_r_h * 100, cnames = ext_even$y_id_ord) #
  rv_sm(ext_even$yearly_m_sd_r * 100, cnames = ext_even$y_id_ord)
  rv_sm(ext_even$phi_und * 100, cnames = ext_even$y_id_ord)
  
  # others
  rv_sm(ext_even$yearly_mabs_b_r_0uh * 100, cnames = ext_even$y_id_ord)
  rv_sm(ext_even$yearly_mabs_b_r_e_0uh * 100, cnames = ext_even$y_id_ord)
  
  
####
  
#### Figure 9 ####
  
  ext_even$state_measures <- tibble(
    mean_abs_undecided_bias = abs(rvmean(ext_even$b_r_u)),
    m_eld_und = rvmean(ext_even$alpha_und),
    year = ext_even$vote_data$year,
    state = ext_even$vote_data$state
  )
  
  ggplot(ext_even$state_measures, aes(x = mean_abs_undecided_bias)) + 
    geom_histogram(alpha = 0.7, colour = "black", binwidth = 0.002, position="identity") + facet_wrap(~year) + 
    scale_x_continuous("Mean absolute bias from undecided voters", labels=percent) +
    theme_bw() + ylab("Number of states") + 
    theme(legend.position="bottom") + guides(fill = guide_legend(label.position = "bottom")) + 
    theme(axis.text=element_text(size=18), axis.title = element_text(size=22), legend.text=element_text(size=14), strip.text = element_text(size=20))
  
  #ggsave(filename = "FIG9_state_abs_bias_hist_ext_50.pdf", device = "pdf", width = 30, height = 21, units = "cm")
  
####
  