# Code log to document correction to data made.
# Don't run this.

library(dplyr)
library(readr)

voting <- read_csv("data/us-pres-state-voting-2004-2016.csv") %>%
  select(-X1)
  
# check incorrectly coded
voting %>% transmute(result_margin6, Dem_vote > Rep_vote) %>% table()

rm_cor <- function(x){
  switch(x,
    "Close margin" = "Close margin",
    "Strong Dem." = "Strong Rep.",
    "Strong Rep." = "Strong Dem."
  )
}

rm_correction <- Vectorize(rm_cor)

# 
voting2 <- voting %>% 
  mutate(
    result_margin6 = rm_correction(result_margin6)
      )

# check correctly coded
voting2 %>% transmute(result_margin6, Dem_vote > Rep_vote) %>% table()

# save
# voting2 %>% write_csv("data/us-pres-state-voting-2004-2016.csv")

# saveRDS(voting2, "data/us-pres-state-voting-2004-2016.rds")
