# clean environment
rm(list=ls())

# load tidyverse package
library(tidyverse)

# read in the CSV file under data/META MC results/ssp_countrydata_nocc/gdp_SSP2.csv
# and assign it to the object gdp
# NOTE: these objects are created by exporting country-level GDP data from META to a CSV file - XX: @PAUL ADD JULIA SCRIPT THAT DOES THIS
gdp_ssp2 <- read_csv("data/META MC results/ssp_countrydata_nocc/gdp_SSP2.csv")
gdp_ssp5 <- read_csv("data/META MC results/ssp_countrydata_nocc/gdp_SSP5.csv")

# convert the data to long format with one row per country and year
gdp_long <- gdp_ssp2 %>%
  
  # convert all columns except for 'years' (= ISO3 country codes) to long format, putting values in a new 'gdp' column and ISO3 codes in 'country'
  pivot_longer(cols = -years, names_to = "country", values_to = "gdp") %>%
  
  # add a column 'ssp' with the value "SSP2" to indicate the Shared Socioeconomic Pathway
  mutate(ssp = "SSP2") %>%
  
  # do the same steps with SSP5 and bind the two data frames together
  bind_rows(gdp_ssp5 %>%
              pivot_longer(cols = -years, names_to = "country", values_to = "gdp") %>%
              mutate(ssp = "SSP5"))

# sort by country and year (ascending), group by country and calculate the growth rate of GDP by dividing gdp by lag(gdp) and subtracting 1
gdp_growth <- gdp_long %>%
  
  arrange(ssp, country, years) %>%
  
  group_by(ssp, country) %>%
  
  # calculate growth rate (e.g., 0.05 in a year with 5% year-to-year growth)
  mutate(gdp_growth = gdp / lag(gdp) - 1) %>%
  
  # rename year and country columns
  rename(year = "years",
         iso3 = "country") %>%
  
  # ungroup the data
  ungroup()

# remove the zero GDP values (and NA growth rates) for Taiwan (TWN) and use the Hong Kong (HKG) values for it instead
# NOTE: this is valid because growth rates in META are regionally homogeneous and would be identical for TWN and HKG
gdp_growth <- gdp_growth %>%
  
  # subset to Hong Kong values
  filter(iso3 == "HKG") %>%
  
  # overwrite ISO3 with TWN
  mutate(iso3 = "TWN") %>%
  
  # bind together with the original data excluding the original (NA) data for Taiwan
  bind_rows(gdp_growth %>% filter(iso3 != "TWN"))

# save out as a CSV file under data/intermediate/
write_csv(gdp_growth, "data/intermediate/gdp_growth.csv")

# inspect value range
gdp_growth %>% filter(year != 2010) %>% summary()
# -> growth rates range between 0.1% and 9.5%

# inspect NA patterns
gdp_growth %>% filter(year != 2010) %>% filter(is.na(gdp_growth)) %>%
  count(iso3)
# -> several small markets with only NAs but none of them have positive portfolio weights in our indices (see data/Calibration.xlsx, column 'has_any_weight')