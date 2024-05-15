# clean the environment
rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(readxl)

# map the baseline temperature for the COACCH damage function (calculated in 'Berkeley Earth GWL Calculation.xlsx')
# for background information on the COACCH baseline temperature, see Supplementary Note 1
coacch_baseline_temperature <- 0.776462745

# load the COACCH damage function data
df_coacch <- read_csv("data/COACCH_damagefunctions/COACCH.csv")

# load a region definition file for IMAGE taken from the MIMIOSA GitHub repository
df_iso <- read_csv("data/COACCH_damagefunctions/ISO_IMAGE_regions_R5_regions.csv")

# load the matching file between IMAGE regions and COACCH taken from the MIMIOSA GitHub repository
df_imageregions <- read_csv("data/COACCH_damagefunctions/IMAGE26_COACCH.csv")

# ensure that all IMAGE regions (identified via the 'I_' prefix) in df_coacch also feature in df_imageregions
if(mean((df_coacch$region %>% str_subset("^I_")) %in% df_imageregions$COACCH) != 1) stop("Not all IMAGE-based regions in df_coacch are featured in df_imageregions")

# ensure that all regions in df_iso feature in df_imageregions
if(mean(df_iso$Region %in% df_imageregions$IMAGE26) != 1) stop("Not all regions in df_iso feature in df_imageregions")

# load the ISO3 codes featured in the META model based on an example output file
gid0_meta <- read_csv("data/META MC results/ssp_countrydata_nocc/gdp_SSP2.csv",
                      n_max = 10) %>%
             select(-years) %>% names()

# identify countries in META that are not covered by the matching files
gid0_meta_notcovered <- gid0_meta[!gid0_meta %in% df_iso$Country]
gid0_meta_notcovered
# -> Andorra, Liechtenstein, Nauru, Palestine, San Marino, Tuvalu

# We impute the IMAGE regions for these small markets based on the following classification map
# https://models.pbl.nl/image/Region_classification_map

# create a tibble that uses imputed IMAGE regions for the markets in question
df_impute <- tibble(Country = gid0_meta_notcovered) %>%

  # impute the regions based on visual inspections of the IMAGE region map linked above
  mutate(Region = case_when(Country %in% c("AND", "LIE", "SMR") ~ "WEU",
                            # Palestine is Middle East
                            Country %in% c("PSE") ~ "ME",
                            # Tuvalu and Nauru are Oceania
                            Country %in% c("TUV", "NRU") ~ "OCE"
                            ))

# bind df_iso together with the imputed countries that were missing in df_iso and merge in COACCH damage function parameters
df_out <- bind_rows(df_iso, df_impute) %>%

  # rename columns
  select(iso = "Country", imageregion = "Region") %>%

  # merge in the COACCH region indicated by df_imageregions for the respective IMAGE region of a country
  left_join(df_imageregions %>% rename(imageregion = "IMAGE26", coacchregion = "COACCH"), by = "imageregion") %>%

  # merge in the COACCH damage function parameters based on the COACCH region merged in above
  # NOTE: we use the 'NoSLR' specification because sea level rise impacts are already covered by the Diaz (2016) damage function in META
  left_join(df_coacch %>% select(coacchregion = "region",
                                 starts_with("NoSLR_a"),
                                 starts_with("NoSLR_b")), by = "coacchregion") %>%

  # use the clean_names() function from the janitor package
  clean_names()

# read in portfolio weights of each country in different portfolios/indices calculated via MSCI index data
df_portfolioweights <- read_excel(file.path("data", "Calibration.xlsx"),
                                  sheet = "Portfolio",
                                  n_max = 195) %>%

  # discard the portfolio based on World Bank market cap data (not used in the manuscript)
  select(-contains("wbmarketcap")) %>%

  # discount helper columns (marked by "_raw" suffix) and the 'comment' column
  select(-ends_with("raw"), -c(comment))

# create LONG-format version of the portfolio weights
df_weights_long <- df_portfolioweights %>%

  # subset to ISO3 code and the weights
  select(iso3, starts_with("weight")) %>%

  # covert to LONG format (= one row per country and portfolio)
  pivot_longer(cols = starts_with("weight_"), names_to = "portfolio", values_to = "weight") %>%

  # remove the 'weight_' prefix in the new 'portfolio' column (i.e., 'msciworld' instead of 'weight_msciworld')
  mutate(portfolio = str_remove(portfolio, "^weight\\_")) %>%

  # remove all country-portfolio combinations with a zero weight (e.g., India for the MSCI World or US for MSCI Emerging Markets)
  filter(weight > 0)

# subset to top10 markets by portfolio to
df_weights_top10 <- df_weights_long %>%

  # group by index and take 10 rows with largest weight, then ungroup
  group_by(portfolio) %>% slice_max(weight, n = 10) %>% ungroup() %>%

  # subset to the three main indices in the manuscript
  filter(portfolio %in% c("msciworld", "msciem", "mscifm"))

# create a chart label for each COACCH region with the format: "Region (ISO3, ISO3, ...)" with all top-10 markets in the parenthesis
df_chartlabel_for_regions <- df_out %>%

  # subset to the top 10 markets for the 3 main indices identified above
  filter(iso %in% df_weights_top10$iso3) %>%

  # group by COACCH region and collapse all top-10 countries' ISO3 codes for the respective region into a comma-separated string
  group_by(coacchregion) %>%
  summarise(iso_collapsed = paste(iso, collapse = ", ")) %>%

  # lastly, combine region name with linebreak, parentheses, and collapsed ISO3 codes
  mutate(chart_label = paste0(coacchregion, "\n(", iso_collapsed, ")"))

## plot the COACCH damage function by region

# create a tibble with global mean temperature increases from the COACCH baseline to +7C, in steps of 0.1C
crossing(temp = seq(coacch_baseline_temperature, 7, by = 0.1),
         df_out %>% select(coacchregion, no_slr_b1, no_slr_b2)) %>%

  # calculate the GDP loss for each region at each temperature level
  mutate(gdploss = no_slr_b1*(temp - coacch_baseline_temperature) + no_slr_b2*(temp - coacch_baseline_temperature)^2) %>%

  # merge in the COACCH region labels with the collapsed ISO3 codes of top 10 markets created above
  left_join(df_chartlabel_for_regions %>% select(coacchregion, chart_label), by = "coacchregion") %>%

  # if the chart label is NA, simply use the COACCH region name
  mutate(chart_label = if_else(is.na(chart_label), coacchregion, chart_label)) %>%

  # create a line chart with the GDP loss on the y-axis and the global warming on the x-axis, with color by COACCH region
  ggplot(aes(temp, gdploss)) +
    geom_line(aes(color = chart_label)) +

  # label axes
  labs(x = "Global warming over pre-industrial levels",
       y = "GDP loss", color = "Region in the COACCH data") +

  # set y-axis labels to % and x-axis labels to degree Celsius
  scale_y_continuous(labels = ~ paste0(.x, "%")) +
  scale_x_continuous(labels = ~ paste0("+", .x, "°C")) +

  # one facet per region; with 5 facets per row
  facet_wrap(~ chart_label,  scales = "free_x", ncol = 5) +

  # omit the legend
  theme(legend.position = "none")

# save the plot as pdf chart
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_COACCH_functions_by_region_top10_markets.pdf")),
                 width = 7, height = 6)

# confirm that b3 is always NA (i.e., the damage functions are not cubic for any region)
if(mean(is.na(df_out$no_slr_b3)) != 1) stop("no_slr_b3 is not always NA in df_out. Please inspect")

# subset df_out to the countries that actually feature in META and remove the NA cubic term
df_out <- df_out %>% filter(iso %in% gid0_meta) %>% select(-no_slr_b3)

# export the COACCH damage functions into a CSV file that can be used by the augmented META model version
write_csv(df_out, file.path("data", "output", "COACCHbetas.csv"))
