# clean the environment
rm(list = ls())

# load packages
library(tidyverse)
library(dtplyr)
library(janitor)
library(readxl)

# read in portfolio weights calculated via MSCI index data
df_portfolioweights <- read_excel(file.path("data", "Calibration.xlsx"),
                                  sheet = "Portfolio",
                                  n_max = 195) %>%
  
  # discard the portfolio based on World Bank market cap data (instead of actual stock indices)
  # NOTE: this portfolio is currently not used in our analysis, hence we discard it
  select(-contains("wbmarketcap")) %>%
  
  # discount helper columns (marked by "_raw" suffix) and the 'comment' column
  select(-ends_with("raw"), -c(comment))

# create LONG-format version of the portfolio weights
df_weights_long <- df_portfolioweights %>% select(iso3, starts_with("weight")) %>%
  
  pivot_longer(cols = starts_with("weight_"), names_to = "portfolio", values_to = "weight") %>%
  
  # remove the 'weight_' in the portfolio names
  mutate(portfolio = str_remove(portfolio, "^weight\\_")) %>%
  
  # drop all countries with zero weight in the respective portfolio
  filter(weight > 0)


# throw an error if the number of countries with non-zero weights is not 68
if(unique(df_weights_long$iso3) %>% length() != 68) stop("Number of countries with non-zero portfolio weights is not 68")

# # set parameters manually for debugging the function (commented out - uncomment for debugging)
# run_identifier_selected <- list(rcp_selected = "RCP45",
#                        ssp_selected = "SSP2",
#                        persist_selected = "Distribution",
#                        mc_samplesize = 2000,
#                        tip_selected = "all",
#                        tdamage_selected = "bhm_distribution",
#                        omh_selected = "default",
#                        return_output = FALSE
# )
# base_year = 2024
# end_year = 2100
# return_output = FALSE
# additional_suffix = NULL

# define the function to calculate the NPV losses by market
calculate_pvloss_bymarket <- function(base_year = 2024, # base year for the dividend NPV
                                      end_year = 2100, # final year considered for the dividend NPV
                                      rcp_selected = "RCP45", # RCP scenario selected
                                      ssp_selected = "SSP2", # SSP scenario selected
                                      persist_selected = "1.0", # persistence of temperature impacts selected (1 = none, 0 = 100% persistence)
                                      mc_samplesize = 2000, # number of MC runs used to calculate the loss distribution
                                      tip_selected = "all", # tipping point modules selected (all/none/individual TPs)
                                      tdamage_selected = "coacch_central", # GDP-temperature damage function selected
                                      omh_selected = "none", # which version of the ocean methane hydrate module to use (default/none)
                                      amoc_selected = "none", # which version of the AMOC module to use (default/none/Cai)
                                      additional_suffix = NULL,
                                      return_output = FALSE,
                                      run_identifier_selected = NULL,
                                      dir_meta_mc_outputs = "data/META MC results/lossfactors/lossfactor_conspc", # directory where input lossfactors from META runs are stored
                                      dir_out_npv_bymarket = file.path("data", "portfolio_simulation", "df_npv_bymarket"), # output directory where to save dividend NPVs by country
                                      dir_out_npv_fullportfolio = file.path("data", "portfolio_simulation", "df_npv_fullportfolio") # output directory where to save dividend NPVs by portfolio
                                      ) {
  
  # create the target directories if they do not exist yet
  if(!dir.exists(dir_out_npv_bymarket)) dir.create(dir_out_npv_bymarket, recursive = TRUE)
  if(!dir.exists(dir_out_npv_fullportfolio)) dir.create(dir_out_npv_fullportfolio, recursive = TRUE)
  
  # if user provides a run identifier list, overwrite function arguments with it
  if(!is.null(run_identifier_selected)) {
    
    rcp_selected = run_identifier_selected$rcp_selected
    ssp_selected = run_identifier_selected$ssp_selected
    persist_selected = run_identifier_selected$persist_selected
    mc_samplesize = run_identifier_selected$mc_samplesize
    tip_selected = run_identifier_selected$tip_selected
    tdamage_selected = run_identifier_selected$tdamage_selected
    omh_selected = run_identifier_selected$omh_selected
    amoc_selected = run_identifier_selected$amoc_selected
    
  }
  
  # check inputs
  if(base_year < 2011) stop("Currently used GDP growth data starts in 2011, so base years prior to this year are not supported")
  
  # map the file holding the weights of each year in a country's expected dividend present value (in the absence of CC impacts) 
  filepath_pvweights_market_byyear <- file.path("data", "weights",
                                                paste0("pvweights_market_byyear_", base_year, "_", end_year,
                                                       additional_suffix,  ".rds"))
  
  # throw an error if the file does not exist
  if(!file.exists(filepath_pvweights_market_byyear)) stop(paste0("No PV weights by market and year exist for base year ",
                                                                 base_year, " and end year ", end_year))
  
  # otherwise, load the file
  df_pvweights_market_byyear <- readRDS(filepath_pvweights_market_byyear)
  
  # identify all countries with a non-zero weight somewhere
  countries_in_portfolio <- unique(df_pvweights_market_byyear$iso3)
  
  # define the identifier of the targeted META model run
  identifier_string <- paste0("n", mc_samplesize, "_", rcp_selected, ssp_selected,
                              "_persist", persist_selected, "_tip", tip_selected, "_tdamage", tdamage_selected, "_omh", omh_selected, "_amoc", amoc_selected)
  
  
  # load the consumption loss factors
  # NOTE: format is one row per year x MC run pairing; one column per country (named by ISO3 code); additional columns for meta variables on the model specification used
  lossfactor_conspc <- read_csv(paste0(dir_meta_mc_outputs, "/lossfactor_conspc_", identifier_string, ".csv"),
                                show_col_types = F) %>%
    
    # initiate lazy_dt()
    data.table::as.data.table() %>% dtplyr::lazy_dt() %>%
    
    # select portfolio, MC run, year and the lossfactor of all relevant countries
    select(ssp, mc_run, year, all_of(countries_in_portfolio)) %>%
    
    # convert to LONG format (one row per country-year-MC draw pairing)
    pivot_longer(cols = -c(mc_run, year, ssp), names_to = "iso3", values_to = "lossfactor_conspc") %>%
    
    # convert from data.table to tibble
    as_tibble()
  
  
  # impute Taiwan with Hong Kong damages
  lossfactor_conspc <- lossfactor_conspc %>%
    
    # extract Hong Kong values and overwrite ISO3 to Taiwan
    filter(iso3 == "HKG") %>% mutate(iso3 = "TWN") %>%
    
    # bind this back to the original data excl. the actual Taiwan entry
    bind_rows(lossfactor_conspc %>% filter(iso3 != "TWN")) %>%
    
    # sort by MC run, country and year
    arrange(mc_run, iso3, year)
  
  
  # identify countries with NA damages in gdp_damagefactor
  countries_with_na_damages <- lossfactor_conspc %>%
    
    # filter for countries in the portfolio that have NA lossfactors in the base year or after
    filter(is.na(lossfactor_conspc), iso3 %in% countries_in_portfolio, year >= base_year) %>% 
    
    # extract the unique ISO3 codes
    pull(iso3) %>% unique()
  
  # explore the NA pattern if there is at least one country with NA damages
  if(length(countries_with_na_damages) >= 1) {
    
    df_nadamages <- lossfactor_conspc %>%
      
      # initiate lazy_dt()
      data.table::as.data.table() %>% dtplyr::lazy_dt() %>%
      
      # filter for countries with NA damages after the base year
      filter(iso3 %in% countries_with_na_damages, year >= base_year) %>%
      
      # group by MC run and country, calculate the share of NA values
      group_by(mc_run, iso3) %>% summarise(na_share = mean(is.na(lossfactor_conspc))) %>% ungroup() %>%
      
      # group by country and share of NA values, collect the MC runs
      group_by(iso3, na_share) %>% summarise(mc_run = list(mc_run)) %>% as_tibble() %>%
      
      rowwise() %>% mutate(mc_run_count = length(mc_run)) %>% ungroup() %>%
      
      mutate(rcp = rcp_selected, ssp = ssp_selected, persist = persist_selected, mc_samplesize = mc_samplesize,
             tip = tip_selected, tdamage = tdamage_selected, omh = omh_selected, amoc = amoc_selected)
    
  } else {
    
    df_nadamages <- NULL
  
  }
  
  # if there are countries with NA damages, print a warning with some overview of the issue
  if(length(countries_with_na_damages) > 0) {
    
    warning(paste("There are", length(countries_with_na_damages),
                  "out of", length(countries_in_portfolio),
                  "portfolio countries in lossfactor_conspc that have NA damages after the base year:\n",
                  paste0(countries_with_na_damages, collapse = ", ")))
    
    print("Distribution of NA shares across MC runs (averaged across countries):")
    
    df_nadamages %>% group_by(na_share) %>% summarise(mc_run_count = mean(mc_run_count)) %>% print()
    
  } 
  
  # merge the lossfactor_conspc data into df for the selected SSP and only for markets where we have a portfolio share
  df_npv_bymarket <- lossfactor_conspc %>%
    
    # initiate lazy_dt()
    data.table::as.data.table() %>% lazy_dt() %>%
    
    # subset to investment horizon defined by user
    filter(year >= base_year, year <= end_year) %>%
    
    # merge in the weight of each year in a country's overall dividend PV (in the absence of CC impacts)
    left_join(df_pvweights_market_byyear %>% select(-base_year, -end_year),
              by = c("year", "iso3", "ssp"), relationship = "many-to-many") %>%
    
    # calculate PV loss as dot product between years' lossfactor and their weight in a country's dividend PV
    group_by(ssp, mc_run, iso3, portfolio) %>%
    
    # NOTE: this step is carried out separately for different discountrate regimes (since the weights differ)
    summarise(lossfactor_pv_damodaran = sum((1-lossfactor_conspc) * weight_countrypv_damodaran),
              control_damodaran = round(sum(weight_countrypv_damodaran), 10), 
              
              lossfactor_pv_damodaran_nocrp = sum((1-lossfactor_conspc) * weight_countrypv_damodaran_nocrp),
              control_damodaran_norcp = round(sum(weight_countrypv_damodaran_nocrp), 10),
              
              .groups = "drop") %>%
    
    # convert results from data.table to tibble
    as_tibble()
  
  # ensure that weights added up to one
  if(all.equal(c(df_npv_bymarket$control_damodaran, df_npv_bymarket$control_damodaran_norcp) %>% unique() %>%
               sort(), c(0,1)) != TRUE) {
    
    stop("There are values other than 0 or 1 in the sums of the country weights")
    
  }
  
  # select required columns and add meta information to the file
  df_npv_bymarket <- df_npv_bymarket %>%
    
    select(ssp, portfolio, iso3, mc_run, lossfactor_pv_damodaran, lossfactor_pv_damodaran_nocrp) %>%
    
    mutate(rcp = rcp_selected,
           persist = persist_selected,
           mc_samplesize = mc_samplesize,
           tip = tip_selected,
           tdamage = tdamage_selected,
           omh = omh_selected,
           amoc = amoc_selected,
           base_year = base_year,
           end_year = end_year)
  
  # write out the results on NPV changes by country as CSV file
  write_csv(df_npv_bymarket,
            file.path(dir_out_npv_bymarket,
                      paste0("df_npv_bymarket_", identifier_string,
                             "_base", base_year, "_end", end_year,
                             additional_suffix, ".csv")
                      )
            )
  
  # calculate lossfactor for the entire portfolio
  df_npv_fullportfolio <- df_npv_bymarket %>%
    
    # merge in the market cap weights of each country for the respective portfolio
    left_join(df_weights_long, by = c("portfolio", "iso3")) %>%
    
    # discard rows with zero lossfactor (meaning that the country does not feature in the portfolio)
    filter(lossfactor_pv_damodaran != 0) %>%
    
    # calculate the weighted average lossfactor for the entire portfolio as a dot product
    group_by(ssp, portfolio, mc_run) %>%
    
    summarise(lossfactor_pv_damodaran = sum(lossfactor_pv_damodaran * weight),
              lossfactor_pv_damodaran_nocrp = sum(lossfactor_pv_damodaran_nocrp * weight),
              
              # ensure that weights add up to 100% (barring precision issues)
              control_weight = round(sum(weight), 10),
              .groups = "drop") %>%
    
    # rank the Monte Carlo runs by the lossfactor
    group_by(ssp, portfolio) %>% arrange(portfolio, desc(lossfactor_pv_damodaran )) %>% mutate(rank_relative_damodaran = 1:n()) %>% ungroup() %>%
    
    # add meta information as separate columns
    mutate(rcp = rcp_selected,
           persist = persist_selected,
           mc_samplesize = mc_samplesize,
           tip = tip_selected,
           tdamage = tdamage_selected,
           omh = omh_selected,
           amoc = amoc_selected,
           base_year = base_year,
           end_year = end_year)
  
  # ensure that weights added up to one
  if(unique(df_npv_fullportfolio$control_weight) != 1) stop("The control_weight column does not sum to 1 in all cases. Please inspect")
  
  # discard the control variables and write out
  write_csv(df_npv_fullportfolio %>% select(-starts_with("control_")),
            file.path(dir_out_npv_fullportfolio,
                      paste0("df_npv_fullportfolio_", identifier_string, "_base", base_year, "_end", end_year,
                             additional_suffix, ".csv")
                      )
            )
  
  # if return_output is set to TRUE, return the data frames in a list - otherwise simply a TRUE
  if(return_output) {
    
    list(df_npv_bymarket = df_npv_bymarket,
         df_npv_fullportfolio = df_npv_fullportfolio,
         df_nadamages = df_nadamages)
    
  } else {
    
    return(T)
  }
  
}

# # test the function (commented out - uncomment to test the function manually)
# calculate_pvloss_bymarket(mc_samplesize = 2000)

## map all the relevant combinations of input parameters for calculate_pvloss_bymarket() into one map
# NOTE: the output will be a tibble where each row corresponds to one Monte Carlo batch of a META model specification (e.g., w/o AMOC and OMH, with BHM damage function, 2000 Monte Carlo runs)

# first, read out all files in the 'lossfactor_conspc' folder (where META runs save their lossfactors)
meta_specs_available <- tibble(identifier_string = file.path("data", "META MC results", "lossfactors", "lossfactor_conspc") %>%
                                 list.files()) %>%
  
  # extract all relevant metadata based on the respective filename using regular expressions
  mutate(mc_samplesize = str_extract(identifier_string, "(?<=\\_n)[:digit:]+(?=_RCP)") %>% as.integer(),
         rcp_selected = str_extract(identifier_string, "(?<=\\_)RCP[^_]+(?=SSP[:digit:])"),
         ssp_selected = str_extract(identifier_string, "SSP[:digit:]"),
         persist_selected = str_extract(identifier_string, "(?<=SSP[:digit:]\\_persist)[^_]+(?=\\_tip)"),
         tip_selected = str_extract(identifier_string, "(?<=\\_tip)[:alpha:]+(?=_tdamage)"),
         tdamage_selected = str_extract(identifier_string, "(?<=\\_tdamage).+(?=_omh)"),
         omh_selected = str_extract(identifier_string, "(?<=\\_omh)[:alpha:]+(?=|_amoc$)"),
         amoc_selected = str_extract(identifier_string, "(?<=\\_amoc)[:alpha:]+(?=\\.csv$)"))

## subset to specific combinations of interest if we do not want to re-run the entire batch
# a) COACCH runs
meta_specs_coacch <- meta_specs_available %>%
  
  filter(tdamage_selected == "coacch_central" & persist_selected == "1.0" & omh_selected == "none" & amoc_selected %in% c("none", "Cai"))

# b) Burke et al.  
meta_specs_bhm <- meta_specs_available %>%
  
  filter(tdamage_selected == "bhm_distribution" & persist_selected %in% c("0.0", "Distribution") &
           omh_selected == "none" & amoc_selected %in% c("none", "Cai") & tip_selected %in% c("all", "none") &
           rcp_selected == "RCP45")

# bind the selections together
meta_specs_selected <- bind_rows(meta_specs_coacch, meta_specs_bhm)


### a) main runs  

# apply the calculate_pvloss_bymarket() function to the different specifications in question
for(jj in 1:nrow(meta_specs_selected)) {

  # print out the current specification to track progress
  print(meta_specs_selected[jj, ])

  # use the inputs in the jj-th row of the meta_specs_selected tibble to calculate the lossfactors
  calculate_pvloss_bymarket(rcp_selected = meta_specs_selected$rcp_selected[jj],
                            ssp_selected = meta_specs_selected$ssp_selected[jj],
                            persist_selected = meta_specs_selected$persist_selected[jj],
                            tip_selected = meta_specs_selected$tip_selected[jj],
                            tdamage_selected = meta_specs_selected$tdamage_selected[jj],
                            omh_selected = meta_specs_selected$omh_selected[jj],
                            amoc_selected = meta_specs_selected$amoc_selected[jj],
                            mc_samplesize = meta_specs_selected$mc_samplesize[jj]
  )

}


### b) runs with alternative tipping point calibration (= using Dietz et al 2021 specification w/o modifications); used for SI analyses
# NOTE: these META runs are located in an additional folder 'alternative_tipping_points' and PV losses are stored in a similar subfolder as well

# capture the alternative tipping point calibration for the SI
# a) w/o tipping points incl. w/o SAF
calculate_pvloss_bymarket(tip_selected = "none",
                          omh_selected = "none",
                          amoc_selected = "none",
                          mc_samplesize = 2000,
                          dir_meta_mc_outputs = file.path("data", "META MC results", "alternative_tipping_points", "lossfactors", "lossfactor_conspc"),
                          dir_out_npv_bymarket = file.path("data", "portfolio_simulation", "df_npv_bymarket", "alternative_tipping_points"),
                          dir_out_npv_fullportfolio = file.path("data", "portfolio_simulation", "df_npv_fullportfolio", "alternative_tipping_points"))

# b) w/ tipping points incl. SAF, using IPSL AMOC damages and default OMH spec (Whiteman et al)
calculate_pvloss_bymarket(tip_selected = "all",
                          omh_selected = "default",
                          amoc_selected = "IPSL",
                          mc_samplesize = 2000,
                          dir_meta_mc_outputs = file.path("data", "META MC results", "alternative_tipping_points", "lossfactors", "lossfactor_conspc"),
                          dir_out_npv_bymarket = file.path("data", "portfolio_simulation", "df_npv_bymarket", "alternative_tipping_points"),
                          dir_out_npv_fullportfolio = file.path("data", "portfolio_simulation", "df_npv_fullportfolio", "alternative_tipping_points"))


### c) runs with alternative time horizons (used in Figure 3)

# run the main spec with alternative time horizons considered
years_of_interest <- tibble(base_year = c(2024, 2024, 2024, 2034, 2044),
                            end_year = c(2100, 2050, 2075, 2110, 2120))

for(jj in 1:nrow(years_of_interest)) {
  years_of_interest[jj, ]

  # with TPs
  calculate_pvloss_bymarket(base_year = years_of_interest$base_year[jj],
                            end_year = years_of_interest$end_year[jj]
                            )

  # without TPs
  calculate_pvloss_bymarket(base_year = years_of_interest$base_year[jj],
                            end_year = years_of_interest$end_year[jj],
                            tip = "none"
  )

}


### d) runs with alternative equity risk premiums (used in Figure 3)

# loop through alternative equity risk premiums
for(erp_jj in seq(0.01, 0.15, by = 0.01)) {
  
  # print out the current ERP value to track progress
  print(paste0("ERP: ", erp_jj))
  
  # calculate the loss factor w/ TPs
  calculate_pvloss_bymarket(tip = "all",
                            additional_suffix = paste0("_erp", erp_jj))
  
  gc()
  
  # w/o TPs
  calculate_pvloss_bymarket(tip = "none",
                            additional_suffix = paste0("_erp", erp_jj))

  gc()
}


### e) clean up resulting file names to ensure that they are consistent

##  need for amoc and omh markers if tipping points are 'none'

# first, map all files with tipping points set to 'none'
files_notp <- file.path("data", "portfolio_simulation", "df_npv_fullportfolio") %>% list.files(full.names = T) %>%
  str_subset("tipnone")

# loop through them and remove the omh and amoc markers
for(file_jj in files_notp) {
  
  # create the new file name as a string
  newname <- file_jj %>% str_remove("_omh[:alpha:]+(?=\\_)") %>% str_remove("_amoc[:alpha:]+(?=\\_)")
  
  # rename the file
  file.rename(file_jj, newname)
  
  # empty the 'newname' object
  rm(newname)
  
}

# follow the same procedure for the loss factors by market (above we did it for the full portfolio)
files_notp_bymarket <- file.path("data", "portfolio_simulation", "df_npv_bymarket") %>% list.files(full.names = T) %>%
  str_subset("tipnone")

for(file_jj in files_notp_bymarket) {
  
  newname <- file_jj %>% str_remove("_omh[:alpha:]+(?=\\_)") %>% str_remove("_amoc[:alpha:]+(?=\\_)")
  
  file.rename(file_jj, newname)
  
  rm(newname)
}
