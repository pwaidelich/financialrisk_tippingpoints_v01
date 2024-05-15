# clean the environment
rm(list = ls())

# load packages
library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)

# map the directory of the META model repository
dir_meta <- "//gess-fs.d.ethz.ch/home$/pwaidelich/Documents/ETH/GitHub/META-2021"

# choose AMOC specification used to allocate global GDP impacts of Cai et al. (2016) to different countries
# NOTE: we use default specification by Dietz et al. (2021): 'IPSL'
amoc_selected <- "IPSL"

# read in the country-specific temperature shifts due to an AMOC slowdown from the META model
# NOTE: for more background info on the AMOC slowdown model, see our SI and the SI of Dietz et al. (2021, PNAS)
# NOTE: the line commented out pulls the file directly from the META repository. However, a local copy is included in this repository and can be used instead
#df_amocparams_raw <- read_csv(file.path(dir_meta, "data", "AMOCparams.csv"))
df_amocparams_raw <- read_csv(file.path("data", "META AMOC", "AMOCparams.csv"))

# select the respective specification
df_amocparams <- df_amocparams_raw %>% rename(AMOC_deltaT = amoc_selected)

# load a country shapefile from the rnaturalearth package to make map charts
world <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium")

# set ggplot2 theme
mytheme <- theme_classic() + theme(legend.position = "bottom",
                                   #legend.box = "vertical",
                                   axis.text = element_text(size = 5),
                                   axis.title = element_text(size = 5),
                                   legend.text = element_text(size = 5),
                                   legend.title = element_text(size = 5),
                                   legend.key.height = unit(0.3, 'cm'),
                                   legend.key.width = unit(0.3, 'cm'),
                                   legend.margin=margin(0, 0, 0, 0, "cm"),
                                   axis.line = element_line(linewidth = 0.3),
                                   axis.ticks = element_line(linewidth = 0.3),
                                   strip.text = element_text(size = 5),
                                   #strip.background = element_rect(color = "black", linewidth = 0.5)
                                   strip.background = element_blank())

theme_set(mytheme)

# # plot the temperature shifts due to AMOC slowdown for META's different specifications
# world %>%
#   
#   # merge in the temperature shifts due to AMOC slowdown for all four specifications in META
#   left_join(df_amocparams_raw %>% pivot_longer(cols = c("Hadley", "BCM", "IPSL", "HADCM"),
#                                                names_to = "amoc_spec", values_to = "T_delta"),
#             by = c("adm0_a3" = "Country code")) %>%
#   
#   # remove territories with no temperature shift data
#   filter(!is.na(T_delta)) %>%
#   
#   # plot a map and use the temperature shifts as fill color
#   ggplot() + geom_sf(aes(fill = T_delta)) +
#   
#   # set the fill color scale and create a 2x2 facet chart (one for each AMOC slowdown specification in META)
#   scale_fill_gradient2(low = "darkblue", high = "darkred") +
#   facet_wrap(~ amoc_spec, nrow = 2, ncol = 2)
# 
# 
# # export as a .pdf chart
# ggsave(file.path("graphs", paste0(Sys.Date(), " SI_AMOCparams_map_allspecifications.pdf")),
#        width = 10, height = 8, units = "cm")

# calculate an absolute value of the temperature shifts
# NOTE: we treat temperature deviations equally, regardless of the direction
df_amocparams <- df_amocparams %>% mutate(DeltaT_abs = abs(AMOC_deltaT))

# map the selected maximum temperature shift due to AMOC slowdown by country
left_join(world, df_amocparams, by = c("adm0_a3" = "Country code")) %>%
  
  # make a map chart using the selected specification's temperature shift as fill color
  ggplot() + geom_sf(aes(fill = AMOC_deltaT), size = 0.1) +
  
  # set the fill color scale and set the color legend title
  scale_fill_gradient2(breaks = c(0, -0.5)) +
  labs(fill = "Maximum change in national temperature\ndue to an AMOC slowdown (°C)")


# export as a .pdf chart
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_", amoc_selected, "_temperature_shifts.pdf")),
       width = 10, height = 7, units = "cm")

# read in 2010 baseline year values for GDP
# NOTE: the initial 'gdp_SSP2.csv' file has one column per country and one row per year
gdp_ssp2 <- read_csv("data/META MC results/ssp_countrydata_nocc/gdp_SSP2.csv") %>%
  
  # subset to 2010
  filter(years == 2010) %>%
  
  # convert to long format (one row per country identified via the new 'iso3' column)
  pivot_longer(cols = -years, names_to = "iso3", values_to = "gdp_2010") %>%
  
  # calculate the share in global GDP for 2010 as per META's output
  mutate(gdp_weight = gdp_2010/sum(gdp_2010))


# ensure that weights sum up to one
if(!sum(gdp_ssp2$gdp_weight) == 1) stop("GDP weights do not sum to 1")

# calculate the scaling factor by country
df_out <- df_amocparams %>%
  
  # merge in the GDP weight for each country
  left_join(gdp_ssp2 %>% select(iso3, gdp_weight), by = c("Country code" = "iso3")) %>%
  
  # for countries with missing temperature shift, we assume a zero impact
  mutate(DeltaT_abs = replace_na(DeltaT_abs, 0)) %>%
  
  # calculate the scaling factor for each country as per the respective Supplementary note
  mutate(scale = sum(DeltaT_abs * gdp_weight),
         scaling_country = DeltaT_abs/scale)


# ensure that if we insert a -0.2 (-20%) impact, this is what we get GDP-weighted
# NOTE: we round to the 15th digit to avoid precision issues with the check
if((df_out %>% summarise(globalimpact = sum(gdp_weight * scaling_country * (-0.2))) %>%
  pull(globalimpact) %>% round(digits = 15)) != -0.2) stop("weights are not correctly implemented, inspect")

# inspect US as the main market
df_out %>% filter(`Country code` == "USA")  

# based on the 'data/Calibration.xlsx' workbook, load all countries with a non-zero weight in the 
# main three indices and save their ISO3 code into a vector
countries_in_portfolios <- read_excel(file.path("data", "Calibration.xlsx"),
                                  sheet = "Portfolio",
                                  n_max = 195) %>%
  
  # subset to countries that features in at least one of the MSCI World, EM or FM
  filter(weight_msciem > 0 | weight_msciworld > 0 | weight_mscifm > 0) %>%
  
  # extract the iso3 code
  pull(iso3)


## create a map with the scaling factor
# merge df_out into the country-level shapefile
left_join(world, df_out, by = c("adm0_a3" = "Country code")) %>%
  
  # calculate the GDP impact by multiplying the global impact (-15%) by the country-specific scaling factor
  mutate(gdp_impact = scaling_country*(-0.15)) %>%
  
  # map and use gdp impact for fill color for all countries that are included in the countries_in_portfolios vector
  ggplot() +
  geom_sf(aes(fill = ifelse(adm0_a3 %in% countries_in_portfolios, gdp_impact, NA)), size = 0.1) +

  # use percent for the fill color scales and set the color legend title
  scale_fill_gradient2(labels = scales::percent) +
  labs(fill = "Maximum GDP impact due\nto an AMOC collapse")


# export as a .pdf chart
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_AMOCcollapse_GDPimpact.pdf")),
       width = 4, height = 3)

# subset df_out to the relevant columns and rename them
df_out <- df_out %>% select(ISO = "Country code", gdp_impact_proportional = "scaling_country")

# export directly into the META model repository
df_out %>% write_csv(file.path("data", "output", "AMOCparams_GDPimpacts.csv"))
