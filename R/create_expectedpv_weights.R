rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(readxl)
library(dtplyr)

# read in core input parameters (CPI, ERP, risk-free rate, etc.)
inputs <- read_excel(file.path("data", "Calibration.xlsx"),
                             sheet = "Core Inputs") %>% as.list()

# read in portfolio weights calculated via MSCI index data
df_portfolioweights <- read_excel(file.path("data", "Calibration.xlsx"),
                                  sheet = "Portfolio",
                                  n_max = 195) %>%
  
  # discard the portfolio based on World Bank market cap data (not used in the manuscript)
  select(-contains("wbmarketcap")) %>%
  
  # discount helper columns (marked by "_raw" suffix)
  select(-ends_with("raw"))

# # set parameters manually for debugging the function (commented out - uncomment for debugging)
# data = df_portfolioweights
# base_year = 2024
# end_year = 2100

# write a function that calculates the expected present value of a country's dividends based on GDP growth as per SSPs and investor discount rate
create_expectedpv_timeseries <- function(data = NULL, # data with the portfolio weights for each country
                                         inputs_provided = inputs, # a list storing the key inputs for PV calculations (inflation rate, risk free rate, ERP, base year)
                                         base_year_selected = 2024, # base year for PV calculations
                                         end_year_selected = 2100 # last year considered in PV calculations
                                         ) {
  
  # check inputs
  if(base_year_selected < 2011) stop("Currently used GDP growth data starts in 2011, so base years prior to this year are not supported")
  
  # create a tibble called df which features gdp_growth with all rows with a value of years that is at least base_year
  # and merge in each country's portfolio weights
  # NOTE: 'gdp_growth.rds' is created in a previous R script, see ReadMe
  df <- read_csv("data/intermediate/gdp_growth.csv") %>%
    
    # we discard years before 2011 because we do not have GDP growth data for them currently
    filter(year > 2010) %>%
    
    # merge in the portfolio weights
    left_join(data, by = "iso3") %>%
    
    # add external inputs as columns
    mutate(risk_free_rate = inputs_provided$risk_free_rate,
           erp = inputs_provided$erp,
           inflation_rate_cpi = inputs_provided$inflation_rate_cpi,
           base_year = base_year_selected,
           end_year = end_year_selected)
    
  
  # create a vector with countries in initial equity share that have a value of zero (identified via the 'has_any_weight' column in 'data/Calibration.xlsx')
  countries_in_portfolio <- unique(df$iso3[df$has_any_weight])
  
  # create the country weights in the index' expected PV
  df_out <- df %>%
    
    # use dtplyr to speed up the process by converting tibble to data.table and initiating lazy_dt()
    data.table::as.data.table() %>% dtplyr::lazy_dt() %>%
    
    # discard countries that never feature in any portfolio - replace NA values with zero weights
    filter(iso3 %in% countries_in_portfolio) %>%
    
    # cut down the period under consideration from base_year to end_year
    filter(year >= base_year & year <= end_year) %>%
    
    # create a new column called gdp_growth_factor which is 1 + gdp_growth (e.g., 1.05 for a 5% year-to-year growth rate)
    # NOTE: in the base year, dividends do not grow yet, so growth factor is 1
    mutate(gdp_growth_factor = if_else(year == base_year, 1, 1 + gdp_growth)) %>%
    
    # convert to long format such that we have one row per SSP x portfolio x country x year
    pivot_longer(cols = starts_with("weight_"), values_to = "initialequityshare", names_to = "portfolio") %>%
    
    # remove the "weight_" prefix from the portfolio names (i.e., 'msciworld' instead of 'weight_msciworld')
    mutate(portfolio = str_remove(portfolio, "^weight_")) %>%
    
    # sort by SSP, portfolio (= stock index), country, year (ascending)
    arrange(ssp, portfolio, iso3, year) %>%
    
    # calculate cumulative GDP growth factors by multiplying over time via the cumprod() function
    group_by(ssp, portfolio, iso3) %>% mutate(gdp_growth_factor_cum = cumprod(gdp_growth_factor)) %>% ungroup() %>%
    
    # derive current equity income per year, market & portfolio
    # NOTE: this step implicitly assumes that the equity income in the base year equals one (since weights add up to one)
    mutate(currentequity = initialequityshare * gdp_growth_factor_cum) %>%
    
    # calculate total income and the share of each market
    group_by(ssp, portfolio, year) %>%
    
    # current equity income per year and portfolio (from all markets) is the sum across markets; for each market, we also save the share in the year's total equity income
    mutate(currentequity_total = sum(currentequity),
           currentequityshare = currentequity / currentequity_total) %>% ungroup() %>%
    
    # calculate annual discount factors and multiply them over time
    # NOTE: discount factor equals one for base_year
    mutate(discount_factor_damodaran = if_else(year == base_year, 1, ((1 + risk_free_rate + crp_damodaran + erp)/(1+inflation_rate_cpi))^(-1)),
           discount_factor_damodaran_nocrp = if_else(year == base_year, 1, ((1 + risk_free_rate + erp)/(1+inflation_rate_cpi))^(-1))) %>%
    
    # accumulate discount factors multiplicatively over years
    # NOTE: we group by SSP, portfolio and country to ensure that the discount factors are multiplied over time for each country
    group_by(ssp, portfolio, iso3) %>%
    mutate(discount_factor_damodaran_cum = cumprod(discount_factor_damodaran),
           discount_factor_damodaran_nocrp_cum = cumprod(discount_factor_damodaran_nocrp)) %>% ungroup() %>%
    
    # calculate present-value equity income by multiplying the current equity income (value of 1 = base year income) by the accumulated discount factor
    mutate(presentequity_damodaran = currentequity * discount_factor_damodaran_cum,
           presentequity_damodaran_nocrp = currentequity * discount_factor_damodaran_nocrp_cum) %>% ungroup() %>%

    # add the input values used as separate columns for meta data
    mutate(base_year = base_year_selected, end_year = end_year_selected) %>%
    
    # collect output by converting from data.table to tibble
    as_tibble()

  # return the output tibble and reorder columns
  return(df_out %>% select(ssp, portfolio, iso3, base_year, end_year, everything()))
}

# write a function to calculate the expected PV weights of each year in the total PV by each market/country
create_pvweights_market_byyear <- function(data = NULL # the tibble returned by the create_expectedpv_timeserie() function defined above
                                           ) {
  df_out <- data %>%
    
    # initiate lazy_dt() for speed gains
    data.table::as.data.table() %>% lazy_dt() %>%
    
    # to calculate the weight of a year in the country's total PV, we group by SSP, portfolio, country and the base_year and end_year of the period under consideration
    # NOTE: we no longer use the Dietz discount rate specification, so the weights of each year in the overall PV are no longer portfolio-specific. part can be removed in future code iterations XX
    group_by(ssp, portfolio, iso3, base_year, end_year) %>%
    
    # calculate present-value of equity income for the different discount rate regimes considered
    mutate(expected_pv_damodaran = sum(presentequity_damodaran),
           expected_pv_damodaran_nocrp = sum(presentequity_damodaran_nocrp)) %>%
    
    # calculate weight of each year in the total PV
    # NOTE: we can only do this for markets that have a non-zero weight in the respective portfolio because otherwise expected_pv_damodaran (= the denominator) is zero and this throws an error
    mutate(weight_countrypv_damodaran = if_else(expected_pv_damodaran > 0, presentequity_damodaran/expected_pv_damodaran, 0),
           weight_countrypv_damodaran_nocrp = if_else(expected_pv_damodaran_nocrp > 0, presentequity_damodaran_nocrp/expected_pv_damodaran_nocrp, 0)) %>%
    ungroup() %>%
    
    # select variables of interest and collect output via as_tibble()
    select(ssp, portfolio, iso3, base_year, end_year, year, starts_with("weight")) %>%
    as_tibble()
  
  # check that weights add up to 100%
  # NOTE: we round to 10th digit to avoid false positives due to precision issues$
  # NOTE: above, we impute weight_countrypv_damodaran with zero values if the country does not feature in the respective index. So the sum of weights can be exactly zero for these country-portfolio combinations
  df_check <- df_out %>% group_by(ssp, portfolio, iso3) %>% summarise_at(vars(starts_with("weight")), ~ round(sum(.x), 10))
  
  if(mean(df_check$weight_countrypv_damodaran %in% c(0, 1)) != 1) stop("Expected PV weights for Damodaran discounting do not add to 100%")
  if(mean(df_check$weight_countrypv_damodaran_nocrp %in% c(0, 1)) != 1) stop("Expected PV weights for Damodaran discounting (no CRP) do not add to 100%")

  # return the weights
  return(df_out)          
}

# export the weights for the base years of interest (which are used in subsequent scripts to calculate the PV loss due to climate change's GDP impacts)
years_of_interest <- tibble(base_year = c(2024, 2024, 2024, 2034, 2044, 2054), # base years of interest
                            end_year = c(2100, 2050, 2075, 2110, 2120, 2130) # final years for PV calculations of interest
                            )

# loop through the tibble's combinations of base_year and end_year
for(jj in 1:nrow(years_of_interest)) {
  
  # print out the loop iteration to keep track
  print(jj)
  
  # save out the weights for the respective base year and end year and save them out under 'data/weights/' with the respective base and end year in the file name
  # NOTE: to create the weights we first create the time series of present-value equity income using create_expectedpv_timeseries(), then we calculate the PV weights
  saveRDS(create_pvweights_market_byyear(create_expectedpv_timeseries(df_portfolioweights,
                                                                      base_year_selected = years_of_interest$base_year[jj],
                                                                      end_year_selected = years_of_interest$end_year[jj])), 
          file.path("data", "weights",
                    paste0("pvweights_market_byyear_",
                           years_of_interest$base_year[jj], "_",
                           years_of_interest$end_year[jj], ".rds")))
}

# save out an example time series which can be used for illustrative charts
create_expectedpv_timeseries(df_portfolioweights,
                             base_year_selected = 2024,
                             end_year_selected = 2100) %>%
  saveRDS(file.path("data", "weights", "pvtimeseries_2024_2100.rds"))

# create weights with alternative ERPs (which change the discount factors and hence the weight of years in the total PV)
for(erp_jj in seq(0.01, 0.2, by = 0.01)) {
  
  # print out the ERP considered to track progress
  print(paste0("ERP: ", erp_jj))
  
  # create a modified input list with the ERP of interest by taking the original input values and overwriting the 'erp' entry of the list with erp_jj
  inputs_modified <- inputs
  inputs_modified$erp <- erp_jj
  
  # run the functions 
  saveRDS(create_pvweights_market_byyear(create_expectedpv_timeseries(df_portfolioweights,
                                                                      inputs_provided = inputs_modified)), 
          file.path("data", "weights",
                    paste0("pvweights_market_byyear_2024_2100_erp", erp_jj, ".rds")))
  
}

# for the MSCI World, plot the weights of each year in a country's PV across time for visual inspection
readRDS(file.path("data", "weights", "pvweights_market_byyear_2024_2100.rds")) %>%
  
  # subset to SSP2 & MSCI World
  filter(ssp == "SSP2", portfolio == "msciworld") %>%
  
  # subset to countries that actually feature in the MSCI World (= sum of weights is non-zero)
  group_by(iso3, ssp, portfolio) %>% mutate(sum_weights = sum(weight_countrypv_damodaran)) %>%
  filter(sum_weights > 0) %>%
  
  # plot the weights of each year in the PV across time
  ggplot(aes(year, weight_countrypv_damodaran)) + geom_line(aes(color = iso3)) +
  
  # label the axes and use % values for the y-axis values
  labs(x = "Year", y = "Weight in PV (%)", color = NULL) +
  
  scale_y_continuous(labels = scales::percent)
