# The risks of climate tipping points for financial investors

This repository processes output data derived from a modified version of the META integrated assessment model (Dietz et al., 2021, PNAS; for our modifications regarding the GDP-temperature damage function, the calibration of a catastrophic AMOC collapse and the omission of ocean methane hydrates and an AMOC slowdown, see Methods and the Supplementary Information), which is publicly available in Julia. The R code in this repository then performs dividend discount modeling based on the GDP reductions by country and year in META's output data to calculate the impacts of climate change GDP impacts with and without climate tipping points on present values (PVs) of future dividends at the country and stock index level.

*NOTE: This clean repository serves for the review process only to allow for transparency regarding the scripts & analyses underlying the submitted manuscript. It does not feature the full output files from META's Julia version (which, as of May 2024, amount to several hundred GB) or the full output files created by the `R/calculate_pvloss_by_market.R` script in this repository (which, as of May 2024, amount to over 30 GB). Therefore, the R scripts are executable using this stand-alone repository alone until the `R/calculate_pvloss_by_market.R` script, which requires the full output of META's Julia version to be available. An extended Zenodo repository that also includes all larger output files will be published upon acceptance.*

## Overview of folder structure

1. `R`: hosts all R scripts described below.
2. `data`: hosts raw input files sourced from other sources (with date stamps in the filename indicating the download date), as well as the `data/Calibration.xlsx` Excel workbook, in which we calculate the weight of countries in the market capitalization of the different stock indices and the current market capitalization of each index based on stock index composition data provided by MSCI. `data/Calibration.xlsx` also features the key inputs used to calculate PVs of future dividends (risk-free rate, equity risk premium, inflation rate) and the country risk premium data from the Damodaran database, which are used in the `R/create_expectedpv_weights.R` script. The folder also contains the `Berkeley Earth GWL Calculation.xlsx` workbook, in which we calculate the baseline temperature for the COACCH damage functions (see Supplementary Information).
3. `data/COACCH_damagefunctions`: hosts the COACCH damage function parameters (`data/COACCH_damagefunctions/COACCH.csv`) from the COACCH Zenodo repository and matching files taken from the MIMOSA GitHub repo used to assign each country in META to one of the regions in `data/COACCH_damagefunctions/COACCH.csv` (based on the IMAGE model regions). These files are used in the `R/prepare_coacch_betas.R` script.
4. `data/META AMOC`: hosts the `AMOCparams.csv` file taken directly from the `/data` directory of the META GitHub repository, which is used in the `R/prepare_amoc_gdp_impacts.R` script.
5. `data/META MC results`: hosts all relevant outputs from the META model's Julia version (annual reductions in GDP under `lossfactors
/lossfactor_conspc`, global warming under `T_AT`, global sea level rise under `SLR`, GDP trajectories in the absence of climate change under `ssp_countrydata_nocc`). The `alternative_tipping_points` subfolder hosts the GDP reductions based on the alternative calibration of climate tipping points, where we use the exact calibration by Dietz et al. (2021) instead of our conservative modifications described in our Supplementary Information.
6. `data/intermediate`: hosts intermediate data objects created as part of the R pipeline, such as the GDP growth data created based on the raw SSP input files, which is created by the `R/calculate_gdpgrowthrates.R` script and used by `R/create_expectedpv_weights.R`.
7. `data/output`: hosts CSV files created by `R/prepare_coacch_betas.R` and `R/prepare_amoc_gdp_impacts.R` that are fed back into the META model's Julia version to calibrate model input parameters (for the COACCH GDP-temperature damage function and to calibrate the GDP impacts of a catastrophic AMOC collapse).
8. `data/portfolio_simulation`: hosts the PV loss distributions returned by `R/calculate_pvloss_by_market.R`, with different subfolders for the PV reduction by country (`data/portfolio_simulation/df_npv_bymarket`) and by stock index (`data/portfolio_simulation/df_npv_fullportfolio`).
9. `data/weights`: hosts the weights of a given year in a country's overall PV of future dividends calculated by `R/create_expectedpv_weights.R` as RDS files. Note that one file of weights will be created for each combination of the time horizon considered and the investor discount rate parameters.
10. `graphs`: directory where all figures (created by the `R/analyse_portfolioimpacts.R` script) will be saved.
11. `tables`: directory where all tables (created by the `R/analyse_portfolioimpacts.R` script) will be saved in TEX format.


## Overview of scripts

The following scripts provide inputs for the modified META model version and, therefore, must be executed before executing the Monte Carlo runs in META.
1.  `R/prepare_coacch_betas.R`: loads the region-specific COACCH damage functions (excl. sea level rise impacts because these are already covered in META by a different damage function) and assigns them to countries based on matching files from the COACCH Zenodo repository and the MIMOSA GitHub repository (Van der Wijst et al., 2023, NCC). We impute the COACCH region for the following countries due to missing data: Andorra, Liechtenstein and San Marino are assigned to Western Europe (WEU), Palestine is assigned to the Middle East (ME), Tuvalu and Nauru are assigned to Oceania (OCE).
2.  `R/prepare_amoc_gdp_impacts.R`: calculates how the 15% global GDP impact in case of an AMOC collapse from the central specification of Cai et al. (2016, NCC) is allocated to countries based on AMOC-related temperature shifts in META's AMOC module.

The following scripts process the outputs of the modified META model version and, therefore, must be executed in the specified order after all Monte Carlo runs in META have been carried out:

3. `R/calculate_gdpgrowthrates.R`: loads country-level GDP trajectories in the absence of climate change as per META, extrapolates the values for Hong Kong to Taiwan (which is missing in the META output), and saves year-to-year growth rates under `data/intermediate/gdp_growth.rds`. Values are used by `R/create_expectedpv_weights.R` to calculate the weight of different years in a country's PV of future dividends under different discount rate regimes (i.e., with and without a country risk premium).
4. `R/create_expectedpv_weights.R`: projects the growth of dividends based on GDP growth and calculates the share of a given year's dividends in a country's total PV of future dividends, based on user-specified parameters for the investor discount rate and the time horizon under consideration. These weights are used by `R/calculate_pvloss_by_market.R` to translate the reductions in future GDP returned by META's Monte Carlo runs into reductions of future dividends and, ultimately, the PV of future dividends at the country level or for different stock indices.
6. `R/calculate_pvloss_by_market.R`: uses the reductions in GDP from META's Monte Carlo runs and the PV weights created in `R/create_expectedpv_weights.R` to calculate reductions in the PV of future dividends due to climate change impacts on GDP. Damages for Taiwan (which is missing in META) are extrapolated based on Hong Kong. To reduce runtime and avoid redundant calculations, PV reductions for a given country are calculated as the dot product of each year's weight in a country's PV and the respective year's reductions in GDP; portfolio-level PV reductions are calculated as the dot product of countries' current share in the index's market cap and their PV reduction. Results for country-level and index-level PV reductions are saved under `data/portfolio_simulation/df_npv_bymarket` and `data/portfolio_simulation/df_npv_fullportfolio`, respectively.
7. `R/analyse_portfolioimpacts.R`: calculates the relevant summary statistics of PV loss distributions based on the output created by `R/calculate_pvloss_by_market.R` for different model specifications (e.g., with and without climate tipping points) and RCP-SSP scenarios and creates all charts, tables, and summary statistics in the main manuscript and the Supplementary Information, sourcing helper functions such as `R/summarize_npv_distribution.R`. Charts are saved under `/graphs`, tables under `/tables`.

The following scripts are helper functions sourced and used in `R/analyse_portfolioimpacts.R` and do NOT need to be executed by the user:

8. `R/utils/summarize_npv_distribution.R`: calculates mean/median/sd/min/max and quantiles of the PV loss distribution created by `R/calculate_pvloss_by_market.R`.
9. `R/utils/clean_value_labels.R`: cleans character labels of country names, model specifications, climate tipping points, RCP scenarios, and stock indices.

## System requirements

All scripts were executed on a local machine with 32GB RAM and an i7 processor. The runtime for `R/calculate_pvloss_by_market.R` was approximately 20min, with all other scripts requiring a few minutes or less to execute. Note that due to the size of the output files from META's Monte Carlo runs, the scripts may not work on local machines with lower memory. Versions of all R packages used are listed below.


## sessionInfo() in R

```
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: Europe/Zurich
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] waterfalls_1.0.0        xtable_1.8-4            rnaturalearthdata_0.1.0 rnaturalearth_0.3.4     ggpubr_0.6.0           
 [6] PupillometryR_0.0.5     rlang_1.1.1             dtplyr_1.3.1            sf_1.0-15               readxl_1.4.3           
[11] janitor_2.2.0           lubridate_1.9.3         forcats_1.0.0           stringr_1.5.1           dplyr_1.1.3            
[16] purrr_1.0.2             readr_2.1.4             tidyr_1.3.0             tibble_3.2.1            ggplot2_3.5.0          
[21] tidyverse_2.0.0        

loaded via a namespace (and not attached):
 [1] gtable_0.3.4       rstatix_0.7.2      lattice_0.21-8     tzdb_0.4.0         vctrs_0.6.3        tools_4.3.1       
 [7] generics_0.1.3     proxy_0.4-27       fansi_1.0.5        pkgconfig_2.0.3    KernSmooth_2.23-21 data.table_1.14.8 
[13] lifecycle_1.0.4    compiler_4.3.1     munsell_0.5.0      carData_3.0-5      snakecase_0.11.1   class_7.3-22      
[19] pillar_1.9.0       car_3.1-2          classInt_0.4-10    abind_1.4-5        tidyselect_1.2.0   stringi_1.7.12    
[25] grid_4.3.1         colorspace_2.1-0   cli_3.6.1          magrittr_2.0.3     utf8_1.2.4         broom_1.0.5       
[31] e1071_1.7-13       withr_2.5.2        scales_1.3.0       backports_1.4.1    sp_2.1-2           timechange_0.2.0  
[37] httr_1.4.7         ggsignif_0.6.4     cellranger_1.1.0   hms_1.1.3          Rcpp_1.0.11        glue_1.6.2        
[43] DBI_1.1.3          rstudioapi_0.15.0  jsonlite_1.8.7     R6_2.5.1           units_0.8-5   
```
