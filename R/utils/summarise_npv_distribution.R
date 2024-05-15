library(tidyverse)

# write a function to quickly extract summary stats from NPV loss distributions
summarise_npv_distribution <- function(data = NULL) {
  
  # if there is no column with ISO3 codes (= lossfactors are for entire portfolio), we add a generic one
  if(!"iso3" %in% names(data)) data$iso3 <- "Full portfolio"
  
  # calculate summary stats across MC runs for each scenario x portfolio x iso3 x discount regime pairing
  data %>%
    
    # subset to variables of interest
    select(rcp, ssp, portfolio, persist, amoc, iso3, mc_samplesize, tip, tdamage, base_year, end_year, starts_with("lossfactor_pv")) %>%
    
    # convert to LONG format (one row per Monte Carlo run and discount rate regime, i.e., with or without country risk premiums)
    pivot_longer(cols = contains("lossfactor_pv"), names_to = "discountrate", values_to = "lossfactor_pv") %>%
    
    # clean up discount rate labels
    mutate(discountrate = str_remove(discountrate, "^lossfactor_pv_")) %>%
    
    # group by all relevant meta variables on the META model run, the discount rate, and the iso3 variable and calculate summary stats
    group_by(rcp, ssp, portfolio, persist, amoc, mc_samplesize, tip, tdamage, base_year, end_year, discountrate, iso3) %>%
    
    summarise_at(vars(lossfactor_pv), .f = list(mean = mean,
                                                median = median,
                                                sd = sd,
                                                min = min,
                                                max = max,
                                                perc01 = ~ quantile(.x, 0.01),
                                                perc05 = ~ quantile(.x, 0.05),
                                                perc10 = ~ quantile(.x, 0.10),
                                                perc25 = ~ quantile(.x, 0.25),
                                                perc75 = ~ quantile(.x, 0.75),
                                                perc90 = ~ quantile(.x, 0.90),
                                                perc95 = ~ quantile(.x, 0.95),
                                                perc99 = ~ quantile(.x, 0.99))) %>%
    
    # ungroup the data
    ungroup()

}
