########################################################################################
# Reproducible paper outputs - Undecided voters and polling bias in US presidential 
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



#### Table 2 ####

# Table 2: overall 
rv_sm(ext_model$mabs_b_r * 100)
rv_sm(ext_model$mabs_b_r_e * 100)
rv_sm(ext_model$mabs_b_r_u * 100)
rv_sm(ext_model$mabs_b_r_h * 100)
rv_sm(ext_model$m_sd_r * 100)
rv_sm(mean(ext_model$postrv$alpha_und) * 100)


# Table 2: yearly
rv_sm(ext_model$yearly_mabs_b_r * 100, cnames = ext_model$y_id_ord)
rv_sm(ext_model$yearly_mabs_b_r_e * 100, cnames = ext_model$y_id_ord) 
rv_sm(ext_model$yearly_mabs_b_r_u * 100, cnames = ext_model$y_id_ord)
rv_sm(ext_model$yearly_mabs_b_r_h * 100, cnames = ext_model$y_id_ord)
rv_sm(ext_model$yearly_m_sd_r * 100, cnames = ext_model$y_id_ord)
rv_sm(ext_model$postrv$phi_und * 100, cnames = ext_model$y_id_ord)

rv_sm(ext_model$yearly_mabs_b_r_0uh * 100, cnames = ext_model$y_id_ord)
rv_sm(ext_model$yearly_mabs_b_r_e_0uh * 100, cnames = ext_model$y_id_ord)

#### 



#### V1 ####  

#### Table 1 ####

SRGG$y_id_ord <- SRGG$vote_data %>% select(year,year_id) %>% unique() %>% arrange(year_id) %>% select(year) %>% collect() %>% .[[1]]

# Table 1: overall 
rv_sm(models$SRGG$mabs_b_r * 100)
rv_sm(models$SRGG$mabs_b_r_e * 100)
rv_sm(models$SRGG$m_sd_r * 100)


# Table 1: yearly
rv_sm(SRGG$yearly_mabs_b_r * 100, cnames = SRGG$y_id_ord)
rv_sm(SRGG$yearly_mabs_b_r_e * 100, cnames = SRGG$y_id_ord) 
rv_sm(SRGG$yearly_m_sd_r * 100, cnames = SRGG$y_id_ord) 

####

#save.image(file = "ws/posterior_calcs_20180225.RData")

#### Undecided + bias ####

ext_model$state_measures <- tibble(
  mean_abs_undecided_bias = abs(rvmean(ext_model$b_r_u)),
  m_eld_und = rvmean(ext_model$postrv$alpha_und),
  year = ext_model$vote_data$year,
  state = ext_model$vote_data$state
)

#FIG5

ggplot(ext_model$state_measures, aes(x = mean_abs_undecided_bias)) + 
  geom_histogram(alpha = 0.7, colour = "black", binwidth = 0.002, position="identity") + facet_wrap(~year) + 
  scale_x_continuous("Mean absolute bias from undecided voters", labels=percent) +
  theme_bw() + ylab("Number of states") + 
  theme(legend.position="bottom") + guides(fill = guide_legend(label.position = "bottom")) + 
  theme(axis.text=element_text(size=18), axis.title = element_text(size=22), legend.text=element_text(size=14), strip.text = element_text(size=20))

#ggsave(filename = "figs/updated/FIG5_state_abs_bias_hist_ext_prop.pdf", device = "pdf", width = 30, height = 21, units = "cm")

#### Undecideds ####

all(diff(ext_model$vote_data$state_year_id) == 1)

state_eld_und_bias <- ext_model$b_r_u %>% as.rvsummary() %>% as.data.frame() %>% bind_cols(ext_model$vote_data)

view_states <- c("florida","michigan","pennsylvania","wisconsin")

state_eld_und_bias %>% filter(year == 2016, state %in% view_states) %>% View()

ext_model$state_measures %>% summarise(mean = mean(m_eld_und), min = min(m_eld_und), max = max(m_eld_und))

rmargin_year_id_df <- ext_model$stan_poll_data %>% select(rmargin_year_id, result_margin6, year_id, year) %>% unique() %>% arrange(rmargin_year_id)
rmargin_year_id_df

as.rvsummary(ext_model$postrv$gamm)

gamm_summary <- summary(as.rvsummary(ext_model$postrv$gamm)) %>% 
  bind_cols(rmargin_year_id_df) %>% 
  mutate(Year = ordered(year, levels = c(2004,2008,2012,2016)),
         Resultmargin = ordered(result_margin6, levels = c("Strong Rep.", "Close margin", "Strong Dem.")))

gamm_summary

# FIG4_gamma_CI
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

#ggsave(filename = "figs/updated/FIG4_gamma_CI_ext_prop_2004-2016.pdf", device = "pdf", width = 30, height = 28, units = "cm")

#### House effects ####

house_id_df <- ext_model$stan_poll_data %>% group_by(pollster_id, pollster_grp) %>% dplyr::summarise(n_polls = n()) %>% arrange(pollster_id)
house_id_df %>% summarise(sum(n_polls))

hbias_summary <- summary(ext_model$postrv$kappa) %>% 
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

#ggsave(filename = "figs/updated/FIG7_kappa_CI_ext_prop_2004-2016.pdf", device = "pdf", width = 21, height = 30, units = "cm")


# look at worst performers:
hbias_percent <- summary(ext_model$b_h * 100) %>%
  bind_cols(house_id_df) %>% dplyr::select(`mean`,`sd`,pollster_grp) %>% filter(pollster_grp != "0_None")

hbias_percent %>% filter(abs(mean) > 0.5) %>% select(pollster_grp,mean,sd) %>% xtable::xtable()

####

#### V2 ####

#### Undecided + bias ####
library(scales)

rmargin_year_id_df <- ext_50$stan_poll_data %>% select(rmargin_year_id, result_margin6, year_id, year) %>% unique() %>% arrange(rmargin_year_id)
rmargin_year_id_df

gamm_summary <- summary(as.rvsummary(ext_50$postrv$gamm)) %>% 
  bind_cols(rmargin_year_id_df) %>% 
  mutate(Year = ordered(year, levels = c(2004,2008,2012,2016)),
         Resultmargin = ordered(result_margin6, levels = c("Strong Rep.", "Close margin", "Strong Dem.")))

gamm_summary

# FIG8_gamma_CI
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

#ggsave(filename = "figs/updated/FIG8_gamma_CI_ext_50_2004-2016.pdf", device = "pdf", width = 30, height = 28, units = "cm")



# FIG9
ext_50$state_measures <- tibble(
  mean_abs_undecided_bias = abs(rvmean(ext_50$b_r_u)),
  m_eld_und = rvmean(ext_50$postrv$alpha_und),
  year = ext_50$state_year_vote_details$year,
  state = ext_50$state_year_vote_details$state
)

ggplot(ext_50$state_measures, aes(x = mean_abs_undecided_bias)) + 
  geom_histogram(alpha = 0.7, colour = "black", binwidth = 0.002, position="identity") + facet_wrap(~year) + 
  scale_x_continuous("Mean absolute bias from undecided voters", labels=percent) +
  theme_bw() + ylab("Number of states") + 
  theme(legend.position="bottom") + guides(fill = guide_legend(label.position = "bottom")) + 
  theme(axis.text=element_text(size=18), axis.title = element_text(size=22), legend.text=element_text(size=14), strip.text = element_text(size=20))

#ggsave(filename = "figs/updated/FIG9_state_abs_bias_hist_ext_50.pdf", device = "pdf", width = 30, height = 21, units = "cm")


####

#### State comparisons, proportional vs 50/50 ####

library(broom)

## ELD bias

with(ext_50,{
  states_eld_bias_summary <- b_r_e %>% summary() %>% 
    mutate(state_year_id = 1:nrow(.)) %>% 
    left_join(state_year_vote_details, by = "state_year_id") %>%
    mutate(contains_0_in_95_percent = (`2.5%` < 0 & `97.5%` > 0))
})

ext_50$states_eld_bias_summary

# rather than read in entire dataset & redo calculations.
ext_prop <- new.env()
ext_prop$b_r_e <- readRDS("figs/ext_prop_election_day_bias_20180225.rds")
ext_prop$state_year_vote_details <- ext_50$state_year_vote_details

with(ext_prop,{
  states_eld_bias_summary <- b_r_e %>% summary() %>% 
    mutate(state_year_id = 1:nrow(.)) %>% 
    left_join(state_year_vote_details, by = "state_year_id")  %>%
    mutate(contains_0_in_95_percent = (`2.5%` < 0 & `97.5%` > 0))
})


tail(ext_50$states_eld_bias_summary$Rep_vote)
tail(ext_prop$states_eld_bias_summary$Rep_vote)

# the more that contain 0, the better the method
ext_50$states_eld_bias_summary %>% group_by(year) %>% summarise(sum(contains_0_in_95_percent)/nrow(.)) -
  ext_prop$states_eld_bias_summary %>% group_by(year) %>% summarise(sum(contains_0_in_95_percent)/nrow(.))
# Proportional allocating performs marginally in 2004, 2012, 2016.

# compare ELD confints for 2016

ext_50_eld_2016 <- ext_50$states_eld_bias_summary %>% filter(year == 2016) %>% mutate(model = "50/50")
ext_prop_eld_2016 <- ext_prop$states_eld_bias_summary %>% filter(year == 2016) %>% mutate(model = "Prop")

ext_comp_eld_2016 <- rbind(ext_50_eld_2016,ext_prop_eld_2016)


ggplot(ext_comp_eld_2016, aes(x = state, ymin = `2.5%`, ymax = `97.5%`, colour = model)) + 
  geom_linerange(position = position_dodge(width = 0.2)) + 
  coord_flip()

# compare all

ext_50_eld_all <- ext_50$states_eld_bias_summary %>%  mutate(model = "Even")
ext_prop_eld_all <- ext_prop$states_eld_bias_summary %>% mutate(model = "Proportional")

ext_comp_eld_all <- rbind(ext_50_eld_all,ext_prop_eld_all)


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

ggplot(ext_comp_eld_all, aes(x = state, ymin = `2.5%`, ymax = `97.5%`, colour = model)) + 
  geom_linerange(position = position_dodge(width = 0.2)) + 
  coord_flip() + facet_wrap(~year) + theme_bw()

ext_comp_eld_all %>% filter(year %in% c(2008,2016)) %>% ggplot(aes(x = State, ymin = `2.5%`, ymax = `97.5%`, colour = model)) + 
  geom_linerange(position = position_dodge(width = 0.5), size = 1.5) + 
  facet_wrap(~year) + theme_bw() + scale_x_discrete("State") +
  theme(legend.position = "bottom") + scale_colour_discrete("Allocation:") +
  coord_flip() 

# FIG6 - 

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


#ggsave(filename = "figs/updated/FIG6-ELD_by_model_type.pdf", device = "pdf", width = 30, height = 40, units = "cm")

## ELD Und bias

with(ext_50,{
  states_eld_und_bias_summary <- eld_und_bias %>% summary() %>% 
    mutate(state_year_id = 1:nrow(.)) %>% 
    left_join(state_year_vote_details, by = "state_year_id") %>%
    mutate(contains_0_in_95_percent = (`2.5%` < 0 & `97.5%` > 0))
})

with(ext_prop,{
  states_eld_und_bias_summary <- eld_und_bias %>% summary() %>% 
    mutate(state_year_id = 1:nrow(.)) %>% 
    left_join(state_year_vote_details, by = "state_year_id")  %>%
    mutate(contains_0_in_95_percent = (`2.5%` < 0 & `97.5%` > 0))
})

# compare all

ext_50_eld_und_all <- ext_50$states_eld_und_bias_summary %>%  mutate(model = "Even")
ext_prop_eld_und_all <- ext_prop$states_eld_und_bias_summary %>% mutate(model = "Proportional")

ext_comp_eld_und_all <- rbind(ext_50_eld_all,ext_prop_eld_all)

ggplot(ext_comp_eld_und_all, aes(x = state, ymin = `2.5%`, ymax = `97.5%`, colour = model)) + 
  geom_linerange(position = position_dodge(width = 0.2)) + 
  coord_flip() + facet_wrap(~year) + theme_bw()

# 2016 key/important states < 2% in favour of Trump

ims <- c("michigan", "wisconsin", "pennsylvania", "florida" )

ext_prop$state_measures %>% filter(year == 2016, state %in% ims) %>% 
  select(state, m_abs_eld_bias, m_und, m_abs_eld_und_bias)


ext_50$state_year_vote_details %>% filter(year == 2016, state %in% ims) %>% select(state,state_year_id)
ims_syid <- ext_50$state_year_vote_details %>% filter(year == 2016, state %in% ims) %>% with(state_year_id)

ext_prop$eld_bias[ims_syid]
ext_prop$eld_und[ims_syid]

ext_prop$old_eld_bias[ims_syid]
ext_prop$eld_und_bias[ims_syid]

####