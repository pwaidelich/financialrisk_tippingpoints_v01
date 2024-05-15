# clean the environment
rm(list = ls())

# load all required packages
library(PupillometryR)
library(tidyverse)
library(dtplyr)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(readxl)
library(xtable)
library(janitor)
library(waterfalls)

# load helper functions
source("R/utils/summarise_npv_distribution.R")
source("R/utils/clean_value_labels.R")

# select damage function and persistence used for the main results
tdamage_selected_main <- "coacch_central"
persist_selected_main <- "1.0"

# select the AMOC slowdown calibration used for the main results and the ones discarded
amoc_selected_main <- "none"
amoc_unselected <- c("IPSL", "BCM", "Hadley", "HADCM")

# if AMOC user selections are inconsistent, throw an error
if(amoc_selected_main %in% amoc_unselected) stop("amoc_selected_main and amoc_unselected are inconsistent")

# define ggplot2 theme
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

# set the theme for all charts in this script
theme_set(mytheme)

# load total index size for translating relative PV loss in absolute values
# NOTE: underlying assumption: current value = expected PV
df_calibration_yield <- read_excel(file.path("data", "Calibration.xlsx"),
                                   sheet = "Index size")

# read in portfolio weights calculated via MSCI index data
df_portfolioweights <- read_excel(file.path("data", "Calibration.xlsx"),
                                  sheet = "Portfolio",
                                  n_max = 195) %>%
  
  # discard the portfolio based on World Bank market cap data, which is not used in the paper
  select(-contains("wbmarketcap")) %>%
  
  # discount helper columns (marked by "_raw" suffix)
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

# read in the portfolio weights for the different stock indices
df_calibration <- read_excel("data/Calibration.xlsx",
                             sheet = "Portfolio",
                             n_max = 195)

# create a top 10 list of countries for each stock index
df_weights_top10 <- df_weights_long %>%
  
  # for each stock index (identified by the 'portfolio' column), extract the top 10 countries by weight
  group_by(portfolio) %>% slice_max(weight, n = 10) %>% ungroup() %>%
  
  # add a column that features the market category as per MSCI based on index membership
  mutate(status = case_when(portfolio == "msciworld" ~ "Developed market",
                            portfolio == "msciem" ~ "Emerging market",
                            portfolio == "mscifm" ~ "Frontier emerging market",
                            TRUE ~ NA_character_))


################################################################################
############################### FIGURE 1 #######################################
################################################################################

# load reductions in GDP ('lossfactor') from the META runs for RCP4.5 and RCP8.5 with/without TPs
df_lossfactor_tipnone_rcp45 <- read_csv(file.path("data", "META MC results", "lossfactors", "lossfactor_conspc", paste0("lossfactor_conspc_n2000_RCP45SSP2_persist",
                                                                                                                        persist_selected_main, "_tipnone_tdamage",
                                                                                                                        tdamage_selected_main,  "_omhnone_amocnone.csv")))

df_lossfactor_tipall_rcp45 <-  read_csv(file.path("data", "META MC results", "lossfactors", "lossfactor_conspc", paste0("lossfactor_conspc_n2000_RCP45SSP2_persist",
                                                                                                                        persist_selected_main, "_tipall_tdamage",
                                                                                                                        tdamage_selected_main, "_omhnone_amoc",
                                                                                                                        amoc_selected_main, ".csv")))

df_lossfactor_tipall_rcp85 <-  read_csv(file.path("data", "META MC results", "lossfactors", "lossfactor_conspc", paste0("lossfactor_conspc_n2000_RCP85SSP2_persist",
                                                                                                                        persist_selected_main, "_tipall_tdamage",
                                                                                                                        tdamage_selected_main, "_omhnone_amoc",
                                                                                                                        amoc_selected_main, ".csv")))

# read in global mean surface temperature change under RCP4.5 with/without tipping points (and excluding ocean methane hydrates, see Supplementary Information)
df_globaltemp <- bind_rows(read_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP45SSP2_tipnone.csv")) %>%
                             mutate(tip = "none", rcp = "RCP45"),
                           read_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP45SSP2_tipall.csv")) %>%
                             mutate(tip = "all", rcp = "RCP45"),
                           read_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP45SSP2_tipall_omhnone.csv")) %>%
                             mutate(tip = "all_omhnone", rcp = "RCP45"))

# read in global sea level rise for the same runs
df_globalSLR <- bind_rows(read_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP45SSP2_tipnone.csv")) %>%
                            mutate(tip = "none", rcp = "RCP45"),
                          read_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP45SSP2_tipall.csv")) %>%
                            mutate(tip = "all", rcp = "RCP45"),
                          read_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP45SSP2_tipall_omhnone.csv")) %>%
                            mutate(tip = "all_omhnone", rcp = "RCP45"))

# repeat for GMST change under RCP8.5
df_globaltemp_rcp85 <- bind_rows(read_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP85SSP2_tipnone.csv")) %>%
                                   mutate(tip = "none", rcp = "RCP85"),
                                 read_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP85SSP2_tipall_omhnone.csv")) %>%
                                   mutate(tip = "all", rcp = "RCP85"))

# repeat for GMST change under RCP2.6
df_globaltemp_rcp26 <- bind_rows(read_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP3-PD26SSP2_tipnone.csv")) %>%
                                   mutate(tip = "none", rcp = "RCP3-PD26"),
                                 read_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP3-PD26SSP2_tipall_omhnone.csv")) %>%
                                   mutate(tip = "all", rcp = "RCP3-PD26"))

# inspect temperature shifts for patterns that require trimming of MC runs
df_globaltemp %>% filter(year <= 2100) %>% summary()
df_globaltemp %>% filter(year <= 2100) %>% arrange(desc(T_AT))
df_globaltemp_rcp85 %>% filter(year <= 2100) %>% arrange(desc(T_AT))
df_globaltemp %>% filter(year <= 2100) %>% arrange(T_AT)
df_globaltemp %>% filter(year <= 2100) %>% filter(T_AT < 0)
df_globalSLR %>% filter(year <= 2100) %>% arrange(desc(SLR))
df_globaltemp %>% filter(year <= 2100) %>% filter(is.na(T_AT)) %>% count(mc_run)
# -> all look plausible except for runs with NAs

# we discard runs that produce NAs in the model
# NOTE: for RCP8.5, we do not vary the investment horizon, so runs that produce NAs only in the 22nd century are not an issue
mc_runs_screenedout <- c(df_globaltemp %>% filter(is.na(T_AT)) %>% pull(mc_run) %>% unique(),
                         df_globaltemp_rcp85 %>% filter(is.na(T_AT), year <= 2100) %>% pull(mc_run) %>% unique()
                         ) %>% unique()

## export mean GMST change and SLR across MC runs under RCP4.5 to facilitate back-of-an-envelope calculations

# GMST change
df_globaltemp %>%
  
  # trim problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # calculate mean GMST change across MC runs
  group_by(year, tip, rcp) %>%
  summarise(T_AT_mean = mean(T_AT), .groups = "drop") %>%
  
  # convert to a wide format and export to a CSV file
  pivot_wider(names_from = "tip", values_from = "T_AT_mean", names_prefix = "T_AT_MCmean_") %>%
  write_csv(file.path("data", "META MC results", "T_AT", "T_AT_n2000_RCP45SSP2_MCmean.csv"))


# SLR (same steps as above)
df_globalSLR %>%
  
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  group_by(year, tip, rcp) %>%
  
  summarise(SLR_mean = mean(SLR), .groups = "drop") %>%
  
  pivot_wider(names_from = "tip", values_from = "SLR_mean", names_prefix = "SLR_MCmean_") %>%
  
  write_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP45SSP2_MCmean.csv"))


# Figure1a: plot GMST over time for RCP4.5
figure1a <- df_globaltemp %>%
  
  # subset to 21st century
  filter(year %in% 2011:2100) %>%
  
  # exclude runs that use ocean methane hydrates (tip = "all" instead of tip = "all_omhnone")
  filter(tip != "all") %>%
  
  # overwrite the 'tip' column by replacing 'all_omhnone' with 'all'
  # NOTE: this is only necessary because the temperature and sea level rise files still use an outdated terminology
  mutate(tip = ifelse(tip == "all_omhnone", "all", tip)) %>%
  
  # trim problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # calculate MC mean and percentiles by year
  group_by(year, tip, rcp) %>% 
  summarise_at(vars(T_AT), list(mean = mean, perc025 = ~ quantile(.x, 0.025),
                                perc975 = ~ quantile(.x, 0.975))) %>%
  
  # clean up the labels in the 'tip' column
  mutate(label = case_when(tip == "all" ~ "Incl. climate tipping points",
                           tip == "none" ~ "Excl. climate tipping points")) %>%
  
  # initiate plot with year on the x-axis and a line chart for the MC mean
  ggplot(aes(year)) + geom_line(aes(y = mean, colour = label)) +
  
  # add shaded area for 2.5th-97.5th percentiles
  geom_ribbon(aes(ymin = perc025, ymax = perc975, fill = label), alpha = 0.1) +
  
  # set color and fill scales, y-axis labels
  scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
  scale_y_continuous(labels = ~ paste0(.x, "\u00B0C")) +
  
  # force the y-axis to start at 0C of global warming (even though first year's value is much higher)
  coord_cartesian(ylim = c(0, NA)) +
  
  # label axes, supress legend titles and position the legend
  labs(y = "Global warming\nabove pre-industrial levels", x = NULL, fill = NULL, colour = NULL) +
  theme(legend.position = "bottom")


# Figure 1b: global SLR over time for RCP4.5 (same steps as above)
figure1b <- df_globalSLR %>%
  
  filter(year %in% 2011:2100) %>%
  
  filter(tip != "all") %>%
  
  mutate(tip = ifelse(tip == "all_omhnone", "all", tip)) %>%
  
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  group_by(year, tip, rcp) %>% 
  
  summarise_at(vars(SLR), list(mean = mean, perc025 = ~ quantile(.x, 0.025),
                               perc975 = ~ quantile(.x, 0.975))) %>%
  
  mutate(label = case_when(tip == "all" ~ "Incl. climate tipping points",
                           tip == "none" ~ "Excl. climate tipping points")) %>%
  
  ggplot(aes(year)) + geom_line(aes(y = mean, colour = label)) +
  
  geom_ribbon(aes(ymin = perc025, ymax = perc975, fill = label), alpha = 0.1) +
  
  scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
  
  scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
  
  scale_y_continuous(labels = ~ paste0(.x, "m")) +
  
  coord_cartesian(ylim = c(0, NA)) +
  
  labs(y = "Global sea level rise\nabove 2000 levels", x = NULL, fill = NULL, colour = NULL) +
  
  theme(legend.position = "bottom")



## Figure 1c: GDP reduction due to climate change damages for the US over time

# create the data set for the figure by combining lossfactors with and without tipping point damages
# NOTE: in the lossfactor files, 'tipall' corresponds to all tipping points excl. ocean methane hydrates
df_figure1c <- bind_rows(df_lossfactor_tipnone_rcp45,
                         df_lossfactor_tipall_rcp45) %>%
  
  # select variables of interest
  # NOTE: lossfactor files have one column per country named after the ISO3 code. Therefore, we extract the 'USA' column
  select(rcp, ssp, tdamage, tp_used, persist, year, USA, mc_run) %>%
  
  # lossfactors in the files are given as remaining GDP (e.g., 0.9 for a 10% loss), so we convert to losses by subtracting them from 1
  mutate(lossfactor = 1 - USA) %>%
  
  # filter to time horizon under consideration
  filter(year %in% 2011:2100) %>%
  
  # trip problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%

  # calculate mean and percentiles of lossfactors by year, tipping point scenario, and RCP
  group_by(year, tp_used, rcp) %>% 
  summarise_at(vars(lossfactor), list(mean = mean, perc025 = ~ quantile(.x, 0.025),
                                      perc975 = ~ quantile(.x, 0.975))) %>%
  
  # clean up the labels in the 'tp_used' column
  mutate(label = case_when(tp_used == "all" ~ "Incl. climate tipping points",
                           tp_used == "none" ~ "Excl. climate tipping points"))


# create Figure1c following the same steps as above  
figure1c <- df_figure1c %>%
  
  ggplot(aes(year)) +
  
  # add a horizontal dashed line at 0 to indicate loss/gains
  geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "grey", linewidth = 0.3) +
  
  # add a line chart for the MC mean (NOTE: we flip the sign such that a 10% loss is shown as -10%)
  geom_line(aes(y = -mean, colour = label)) +
  
  # remaining steps (as described above), again with flipped signs to show losses as negative values
  geom_ribbon(aes(ymin = -perc025, ymax = -perc975, fill = label), alpha = 0.1) +
  scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
  scale_y_continuous(labels = ~ paste0("-", scales::percent(.x))) +
  labs(y = "US dividend reduction\n due to climate damages", x = NULL, fill = NULL, colour = NULL) +
  theme(legend.position = "bottom")



### load equity return lossfactor (for total PV) by market in the main model speification under RCP4.5

# with climate tipping points
df_npv_bymarket_example_tipall <- read_csv(file.path("data", "portfolio_simulation", "df_npv_bymarket",
                                                     paste0("df_npv_bymarket_n2000_RCP45SSP2_persist",
                                                            persist_selected_main, "_tipall_tdamage", 
                                                            tdamage_selected_main, "_omhnone_amoc",
                                                            amoc_selected_main, "_base2024_end2100.csv")),
                                           show_col_types = F)

# without climate tipping points
df_npv_bymarket_example_tipnone <- read_csv(file.path("data", "portfolio_simulation", "df_npv_bymarket",
                                                      paste0("df_npv_bymarket_n2000_RCP45SSP2_persist",
                                                             persist_selected_main, "_tipnone_tdamage", 
                                                             tdamage_selected_main, "_base2024_end2100.csv")),
                                            show_col_types = F)


# load the PV time series created in a previous script
df_pvtimeseries_us <- readRDS(file.path("data", "weights", "pvtimeseries_2024_2100.rds")) %>%
  
  # subset to SSP2
  filter(ssp == "SSP2") %>%
  
  # subset to the US and the MSCI World
  filter(iso3 %in% c("USA") & portfolio == "msciworld") %>%
  
  # subset to variables of interest
  select(ssp, portfolio, name, year, currentequity, initialequityshare, discount_factor_damodaran_cum,
         discount_factor_damodaran_nocrp_cum) %>%
  
  # norm the current values such that initial equity share in the base year (2024) equals 1
  mutate(currentequity = currentequity / initialequityshare) %>%
  
  # calculate present values based on current values and discount factor
  mutate(pvequity = currentequity * discount_factor_damodaran_cum,
         
         # reduction in PV due to CRP premium (= zero for the US, so irrelevant)
         pvequity_crp_reduction = currentequity * (discount_factor_damodaran_nocrp_cum - discount_factor_damodaran_cum),
         
         # reduction in PV due to general discounting
         pvequity_dr_reduction = currentequity * ( 1- discount_factor_damodaran_nocrp_cum)) %>%
  
  # convert to LONG format
  pivot_longer(cols = starts_with("pvequity"), names_to = "pv_component", values_to = "pv") %>%
  
  # apply labels to the different components of the PV
  mutate(pv_component = case_when(pv_component == "pvequity" ~ "Present value",
                                  pv_component == "pvequity_dr_reduction" ~ "Discounted current value")) %>%
  
  # merge in the CRP premium
  left_join(df_calibration %>% select(name, crp_damodaran), by = "name") %>%
  
  # create a label for the country with the CRP premium
  mutate(country_label = paste0(shorten_country_names(name), " \n(Risk premium: ", 100*round(crp_damodaran, 3), "%)"))


# plot the present value of the US market
# NOTE: since GDP reductions are relatively small (just a few %) and hence barely visible, we magnify them merely for visual purposes
#       Therefore, we create the chart via a function to facilitate its reproduction using different extents to which damages are magnified
make_figure1d <- function(magnify_factor_climatelosses = 5, magnify_factor_tplosses = 50) {
  
  figure1d <- df_pvtimeseries_us %>%
    
    # subset to MSCI World
    filter(portfolio == "msciworld") %>%
    
    # subset to variables of interest and remove redundant rows
    select(ssp, portfolio, name, year, currentequity, discount_factor_damodaran_cum) %>% distinct() %>%
    
    # calculate PV of current equity 
    mutate(presentequity = currentequity * discount_factor_damodaran_cum) %>%
    
    # merge in the loss factors for the US market used in Figure 1c
    left_join(df_figure1c %>% filter(tp_used %in% c("all", "none")) %>% ungroup() %>%
                select(year, tp_used, lossfactor_mean = "mean"), by = "year") %>%
    
    # calculate losses due to climate damages in current value and present value
    mutate(currentequity_netoflosses = currentequity*(1-lossfactor_mean)) %>%
    mutate(presentequity_netoflosses = presentequity*(1-lossfactor_mean)) %>%
    
    # data features rows per different tipping point setting (all, none), so we collapse
    group_by(ssp, portfolio, name, year) %>%
    summarise(currentequity = head(currentequity, 1), # NOTE: current and present values before climate damages are same for tipping point settings, so we just extract the 1st value
              presentequity = head(presentequity, 1),
              presentequity_netoflosses_tp_none = presentequity_netoflosses[tp_used == "none"],
              presentequity_netoflosses_tp_all = presentequity_netoflosses[tp_used == "all"]
              ) %>%
    
    # calculate the different components that add up to the current value
    mutate(discountedvalue = currentequity - presentequity,
           tp_loss = presentequity_netoflosses_tp_none - presentequity_netoflosses_tp_all,
           loss = presentequity - presentequity_netoflosses_tp_none) %>%
    
    # convert to LONG format (one row per year x current value component)
    pivot_longer(cols = c(discountedvalue, tp_loss, presentequity_netoflosses_tp_all, loss),
                 names_to = "component", values_to = "value") %>%
    
    # magnify the climate damages for visual purposes
    mutate(value = case_when(component == "loss" ~ value*magnify_factor_climatelosses,
                             component == "tp_loss" ~ value*magnify_factor_tplosses,
                             TRUE ~ value)) %>%
    
    # apply labels to the different components of the PV
    # NOTE: we include a catch ('INSPECT XX') if none of the case_when() conditions hold to ensure that the issue is flagged
    mutate(component = factor(case_when(component == "discountedvalue" ~ "Discounted value",
                                        component == "loss" ~ "Climate damages (excl. TPs)",
                                        component == "tp_loss" ~ "Additional climate damages due to TPs",
                                        component == "presentequity_netoflosses_tp_all" ~ "Present value (net of all climate damages)",
                                        TRUE ~ "INSPECT XX"),
                              levels = c("Discounted value",
                                         "Climate damages (excl. TPs)",
                                         "Additional climate damages due to TPs",
                                         "Present value (net of all climate damages)"))) %>%
    
    # create a stacked bar chart with year on the x-axis and different colors & transparencies for current value components
    ggplot(aes(year, value)) +
    geom_col(aes(fill = component, alpha = component)) +
    
    # set color and transparency scales manually
    scale_fill_manual(values = c("lightgrey", "#00BFC4", "#F8766D", "#5a5a5a"),
                       breaks = c("Discounted value",
                                  "Climate damages (excl. TPs)",
                                  "Additional climate damages due to TPs",
                                  "Present value (net of all climate damages)")) +
    scale_alpha_manual(values = c(0.25, 0.35, 0.35, 0.7),
                       breaks = c("Discounted value",
                                  "Climate damages (excl. TPs)",
                                  "Additional climate damages due to TPs",
                                  "Present value (net of all climate damages)")) +

    ## annotate the labels for the different current value components manually
    
    # discounted part of CV
    annotate("text", x = 2080, y = 1.75, label = "Current values\n(not discounted)",
             alpha = 1, colour = "darkgrey", hjust = 0.5, fontface = "bold",
             size = 5/.pt) +
    
    # climate losses w/o TPs
    annotate("text", x = 2045, y = 1, label = "Climate losses excl.\nclimate tipping points",
             alpha = 1, colour = "#00BFC4", hjust = 0.5, fontface = "bold",
             size = 5/.pt) +
    
    # climate losses w/ TPs
    annotate("text", x = 2075, y = 0.5, label = "Additional losses due to\nclimate tipping points",
             alpha = 1, colour = "#F8766D", hjust = 0.5, fontface = "bold",
             size = 5/.pt) +
    
    # net PV
    annotate("text", x = 2030, y = -0.1,
             label = paste0("Remaining present value (sum = NPV)"),
             alpha = 1, colour = "#414141", fontface = "bold",
             hjust = 0,
             size = 5/.pt) +
    
    # reverse legend orders
    guides(fill = guide_legend(reverse = T), alpha = guide_legend(reverse = T)) +
    
    # label y-axis and drop legend titles
    labs(y = "US dividend projections\n(2024 value = 1)", fill = NULL, alpha = NULL, x = NULL) +
    
    # drop legend
    theme(legend.position = "none",
          legend.direction  = "vertical") +
    
    # specify years on x-ais manually
    scale_x_continuous(breaks = c(2025, 2050, 2075, 2100))
  
}

# create Figure 1d (= magnifying climate losses)
figure1d <- make_figure1d()

# create a version w/o magnifying for the SI and save out
figure1d_no_rescaling <- make_figure1d(1, 1)

ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure1d_methodology_", tdamage_selected_main, "_amoc", amoc_selected_main, "_norescaling.pdf")),
       plot = figure1d_no_rescaling,
       width = 7.5, height = 6, unit = "cm")


# combine all panels for Figure 1
figure1 <- ggarrange(figure1a, figure1b, figure1c, figure1d,
                     # 2x2 layout with aligned axes
                     nrow = 2, ncol = 2, align = "hv",
                     # use a common legend at the bottom
                     common.legend = T, legend = "bottom",
                     # label charts and specify font size of labels
                     labels = "auto", font.label=list(color="black",size=7))

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " Figure1_methodology_", tdamage_selected_main, "_amoc", amoc_selected_main, ".pdf")),
       plot = figure1,
       width = 15, height = 12, unit = "cm")

# clean up the environment
rm(df_pvtimeseries_us)
gc()


################################################################################
############################# FIGURE 2 #########################################
################################################################################

# plot top10 markets for World, EM & FEM
df_figure2a <-  bind_rows(df_npv_bymarket_example_tipall, df_npv_bymarket_example_tipnone) %>%
  
  # trim problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # select variables of interest
  select(ssp, rcp, portfolio, iso3, mc_run, lossfactor_pv_damodaran, tip) %>%
  
  # convert to WIDE format (different columns for with/without TPs)
  pivot_wider(names_from = "tip", values_from = "lossfactor_pv_damodaran") %>%
  
  # merge in factor identifying top10 markets per index as well as their status label (e.g., "Developed Market")
  left_join(df_weights_top10 %>% select(portfolio, iso3, status) %>% mutate(is_top10 = T),
            by = c("portfolio", "iso3")) %>%
  
  # subset to the three main indices under consideration and their respective top10 markets
  filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  filter(is_top10) %>%
  
  # calculate expected loss and 95% VaR
  group_by(iso3, ssp, rcp, portfolio, status) %>%
  summarise_at(vars(all, none), .f = list(perc95 = ~ quantile(.x, 0.95),
                                          mean = mean)) %>% ungroup() %>%
  
  # calculate the difference in expected loss between w/ and w/o tipping points
  mutate(incl_tipping_adder_mean = all_mean - none_mean) %>%
  
  # merge in country names and shorten them using a user-written utils function (defined under R/utils)
  left_join(df_calibration %>% select(iso3, name) %>% distinct(), by = "iso3") %>%
  mutate(name = shorten_country_names(name)) %>%
  
  # merge in weights of each country in the respective stock index
  left_join(df_weights_top10 %>%
              filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
              select(iso3, weight),
            by = c("iso3")) %>%
  
  # add the weight as a clean % character in parentheses behind the country name
  mutate(name = paste0(name, " (", 100*round(weight, 2), "%)")) %>%
  
  # specify the status (which is used for facetting in Figure 2a)
  mutate(status = factor(case_when(status == "Developed market" ~ "Developed markets\n(in MSCI World, % = weight)",
                                   status == "Emerging market" ~ "Emerging markets\n(in MSCI EM, % = weight)",
                                   status == "Frontier emerging market" ~ "Frontier emerging markets\n(in MSCI FEM, % = weight)"),
                         levels = c("Developed markets\n(in MSCI World, % = weight)",
                                    "Emerging markets\n(in MSCI EM, % = weight)",
                                    "Frontier emerging markets\n(in MSCI FEM, % = weight)")))


# Figure2a: expected loss (bars) and 95% VaR (triangles) for top10 markets in MSCI World, EM, FEM under SSP2-4.5
figure2a <- df_figure2a %>%
  
  # convert to LONG format (one row for expected loss w/o tipping points and for adder due to tipping points)
  pivot_longer(cols = c("none_mean", "incl_tipping_adder_mean"), names_to = "label", values_to = "lossfactor_pv_damodaran") %>%
  
  # clean up the tipping point label and convert to a factor
  mutate(label = case_when(label == "none_mean" ~ "Excl. climate tipping points",
                           label == "incl_tipping_adder_mean" ~ "Incl. climate tipping points")) %>%
  mutate(label = factor(label, levels = c("Incl. climate tipping points", "Excl. climate tipping points"))) %>%
  
  # plot country labels on x-axis ordered in descending order by their index weight
  ggplot(aes(reorder(name, weight), lossfactor_pv_damodaran)) +
  
  # add stacked transparent bars for expected loss (in different colors for w/o TPs and TP adder)
  geom_col(aes(fill = label), alpha = 0.1) +
  
  # add points for expected loss
  geom_point(data = df_figure2a %>%
                      pivot_longer(cols = c("all_mean", "none_mean"), names_to = "label", values_to = "mean") %>%
                      mutate(label = case_when(label == "none_mean" ~ "Excl. climate tipping points",
                                        label == "all_mean" ~ "Incl. climate tipping points")) %>%
                      mutate(label = factor(label, levels = c("Incl. climate tipping points", "Excl. climate tipping points"))),
             aes(y = mean, colour = label, shape = "Expected loss"),
             size = 1) +
  
  # add differently shaped points for 95% VaR
  geom_point(data = df_figure2a %>%
               pivot_longer(cols = c("all_perc95", "none_perc95"), names_to = "label", values_to = "perc95") %>%
               mutate(label = case_when(label == "none_perc95" ~ "Excl. climate tipping points",
                                        label == "all_perc95" ~ "Incl. climate tipping points")) %>%
               mutate(label = factor(label, levels = c("Incl. climate tipping points", "Excl. climate tipping points"))),
             aes(y = perc95, colour = label, shape = "95% VaR (95th percentile)"),
             size = 1) +
  
  # add a horizontal line at 0% losss
  geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "grey", linewidth = 0.3) +
  
  # label y-axis values as %
  scale_y_continuous(labels = ~ paste0("-", scales::percent(.x))) +
  
  # set y-axis label and drop legend titles
  labs(y = "Loss in % of present value", fill = NULL, x = NULL, shape = NULL) +
  
  # facet by market category (developed/emerging/frontier emerging)
  facet_wrap(~ status, scales = "free_y") +
  
  # set shapes for expected loss and VaR
  scale_shape_manual(values = c("Expected loss" = 15, "95% VaR (95th percentile)" = 17, "99% VaR (99th percentile)" = 10)) +
  
  # reverse legend order for fill and drop legend for color (as it is redundant to the fill legend)
  guides(fill = guide_legend(rev = T), color = "none") +
  
  # flip axis and position the legend
  coord_flip() +
  theme(legend.position = "bottom")


# create a data frame hosting all PV loss distributions for RCP2.6, RCP4.5 and RCP8.5 w/ and w/o tipping points
# NOTE: the 'persist' columns in the PV loss files can be a numeric or a string, so we convert to ensure it is a string
df_npv <- bind_rows(file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP3-PD26SSP2_persist",
                                     persist_selected_main, "_tipnone_tdamage", 
                                     tdamage_selected_main, "_base2024_end2100.csv")) %>%
                      read_csv() %>% mutate(persist = as.character(persist)),
                    
                    file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP3-PD26SSP2_persist",
                                     persist_selected_main, "_tipall_tdamage",
                                     tdamage_selected_main, "_omhnone_amoc",
                                     amoc_selected_main, "_base2024_end2100.csv")) %>%
                     read_csv() %>% mutate(persist = as.character(persist)),
                    
                    file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist",
                                     persist_selected_main, "_tipnone_tdamage",
                                     tdamage_selected_main, "_base2024_end2100.csv")) %>%
                      read_csv() %>% mutate(persist = as.character(persist)),
                    
                    file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist",
                                     persist_selected_main, "_tipall_tdamage", 
                                     tdamage_selected_main, "_omhnone_amoc",
                                     amoc_selected_main, "_base2024_end2100.csv")) %>%
                      read_csv() %>% mutate(persist = as.character(persist)),
                    
                    file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP85SSP2_persist",
                                     persist_selected_main, "_tipnone_tdamage",
                                     tdamage_selected_main, "_base2024_end2100.csv")) %>%
                      read_csv() %>% mutate(persist = as.character(persist)),
                    
                    file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP85SSP2_persist",
                                     persist_selected_main, "_tipall_tdamage",
                                     tdamage_selected_main, "_omhnone_amoc",
                                     amoc_selected_main, "_base2024_end2100.csv")) %>%
                      read_csv() %>%mutate(persist = as.character(persist))
                    )


# calculate summary statistics using a user-written helper function (defined in R/utils/)
df_npv_summarystat <- df_npv %>%
  
  # trim the problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # calculate summary statistics
  summarise_npv_distribution()


# print the summary stats into an SI table in TEX format using the xtable package
df_npv_summarystat %>%
  
  # filter to the discount rate with CRPs ('damodaran') and the three indices of interest
  filter(discountrate == "damodaran",
         portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  
  # sort by stock index
  arrange(portfolio) %>%
  
  # drop columns that are not needed in the table
  select(-c(persist, tdamage, base_year, end_year, discountrate, mc_samplesize, iso3,
            perc25, perc75, min, max, perc10, perc90)) %>%
  
  # rename the columns of interest
  select(Portfolio = "portfolio", RCP = "rcp", SSP = "ssp",
         TippingPoints = "tip",
         Mean = "mean",
         Median = "median",
         "1st perc." = "perc01",
         "5th perc." = "perc05",
         "95th perc." = "perc95",
         "99th perc." = "perc99") %>%
  
  # format the numeric columns as rounded percentages
  mutate_if(is.numeric, ~ paste0(100*round(.x, 4), "%")) %>%
  
  # clean the stock index label
  mutate(Portfolio = clean_portfolio_labels_short(Portfolio)) %>%
  
  # sort by stock index, scenario, with or without TPs, SSP
  arrange(Portfolio, RCP, TippingPoints, SSP) %>%
  
  # create the table
  xtable(digits = 4,
         label = "tab:si_resultsoverview",
         caption = "Summary statistics of the loss distribution for the key MSCI indices using SSP2 and the COACCH damage specification (main results)") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " SummaryStatsSI.tex")),
        size="\\fontsize{9pt}{10pt}\\selectfont")

# calculate tipping point-related adder in expected loss
df_figure2 <- df_npv_summarystat %>%
  
  # filter to time horizon of interest and discount rate regime used
  filter(base_year == 2024, end_year == 2100,
         discountrate == "damodaran") %>%
  
  # calculate differences in expected loss, standard deviation and 95% VaR due to tipping points
  group_by(rcp, ssp, portfolio, persist, tdamage, base_year, end_year) %>%
  mutate(mean_adder_tp = mean[tip == "all"] - mean[tip == "none"],
         sd_adder_tp = sd[tip == "all"] - sd[tip == "none"],
         perc95_adder_tp = perc95[tip == "all"] - perc95[tip == "none"],
         # count rows to ensure that we have exactly two rows, one for w/ or w/o TPs
         n_control = n()) %>% ungroup()

# error check
if(mean(df_figure2$n_control == 2) != 1) stop("There are more/less than two rows used for the previous step. Inspect")

# plot mean equity return loss by index
df_figure2b <- df_figure2 %>%
  
  # select to indexes of interest
  filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  
  # grab values excl. TPs as well as TP adder
  filter(tip == "none") %>% select(ssp, portfolio, rcp, persist, tdamage, mean, mean_adder_tp) %>%
  
  # for each portfolio, calculate the sum of value excl. TP & adder (= expected loss w/ TPs)
  group_by(portfolio) %>% mutate(mean_total = mean + mean_adder_tp) %>% ungroup() %>%
  
  # convert to long (one row for excl. TP/TP adder)
  pivot_longer(cols = c(mean, mean_adder_tp), names_to = "tp_label", values_to = "mean") %>%
  
  # merge in the 95th percentile incl. tipping points
  left_join(df_figure2 %>% mutate(tp_label = case_when(tip == "all" ~ "mean_adder_tp",
                                                       tip == "none" ~ "mean")) %>%
              select(rcp, ssp, portfolio, tdamage, tp_label, perc95),
            by = c("rcp", "ssp", "portfolio", "tdamage", "tp_label")) %>%
  
  # clean up the label for the TPs and convert to a factor to determine the order
  mutate(tp_label = if_else(str_detect(tp_label, "adder_tp"), "Incl. climate tipping points", "Excl. climate tipping points")) %>%
  mutate(tp_label = factor(tp_label, levels = c("Incl. climate tipping points", "Excl. climate tipping points"))) %>%
  
  # merge in current index size to express as absolute number
  left_join(df_calibration_yield %>% select(portfolio, index_size), by = "portfolio") %>%
  
  # calculate expected loss and VaR in absolute terms (dividing by 10^9 to convert from USD to USD billion)
  mutate(mean_udsbn = mean * index_size *10^(-9),
         mean_total_udsbn = mean_total * index_size *10^(-9),
         perc95_udsbn = perc95 * index_size *10^(-9)) %>%
  
  # paste the rounded index size in parentheses into the stock index label
  mutate(portfolio = factor(paste0(clean_portfolio_labels(portfolio), "\n(market cap: $", round(index_size/10^12, 1), "tn)"),
                            levels = c("MSCI World\n(market cap: $57.2tn)",
                                       "MSCI Emerging Markets\n(market cap: $6.6tn)",
                                       "MSCI Frontier Emerging Markets\n(market cap: $0.2tn)")))
  
# determine the highest VaR in the figure (MSCI FEM, RCP8.5)
var_max_mscifm_relative <- df_figure2b %>%
  
  filter(str_detect(portfolio, "MSCI Frontier"), rcp == "RCP85", tp_label == "Incl. climate tipping points") %>%
  
  pull(perc95)


# Figure 2b: Expected loss (bar) and 95th percentile (triangle) for MSCI World, EM & FEM indices under SSP2-4.5
figure2b <- df_figure2b %>% 
  
  # clean RCP labels on the x-axis
  ggplot(aes(clean_rcp_labels(rcp))) +
  
  # stacked bars for expected loss w/ and w/o TPs
  geom_col(aes(y = mean, fill = tp_label), alpha = 0.1, width = 0.5) +
  
  # add expected loss w/ and w/o tipping points as differently colored points
  geom_point(aes(y = ifelse(str_detect(tp_label, "^Incl"), mean_total, mean), colour = tp_label, shape = "Expected loss"),
             size = 1) +
  
  # do the same for VaR using a different point shape
  geom_point(aes(y = perc95, colour = tp_label, shape = "95% VaR (95th percentile)"),
                         size = 1) +
  
  # add annotated value labels for expected loss
  geom_text(aes(y = mean_total*1.2, label = paste0("-", 100*round(mean_total, 3), "%")),
            size = 5/.pt) +
  
  # add annotated value labels for expected loss (but only for data points incl. TPs)
  geom_text(aes(y = perc95*1.1, label = ifelse(str_detect(tp_label, "^Incl"),
                                              paste0("-", 100*round(perc95, 3), "%"),
                                              NA_character_)),
            size = 5/.pt) +
  
  # add invisible label for MSCI EM to determine the y-axis range
  # value labels for 95% VaR
  geom_text(aes(y = var_max_mscifm_relative*1.1,
                label = "TEST"),
            size = 5/.pt, alpha = 0) +
  
  # facet by stock index
  facet_wrap(~ portfolio, axes = "all") +
  
  # scale y-axis to percentage
  scale_y_continuous(labels = scales::percent) +
  
  # set point shapes used for expected loss and VaR
  scale_shape_manual(values = c("Expected loss" = 15, "95% VaR (95th percentile)" = 17)) +
  
  # set legend orders and override some of their aesthetics manually
  guides(fill = guide_legend(reverse = T, order = 1, override.aes = list(alpha = 1)),
         shape = guide_legend(reverse = T, order = 2, override.aes = list(color = "grey")),
         color = "none") +

  # label axes and drop legend titles
  labs(x = NULL, y = "Loss in % of present value", fill = NULL, shape = NULL) +
  
  # position the legend
  theme(legend.position = "bottom",
        legend.box = "vertical")


# combine figures
figure2 <- ggarrange(figure2a + theme(legend.position = "none"), figure2b,
                     labels = "auto", nrow = 2, ncol = 1,
                     font.label = list(size = 7, color = "black", face = "bold"),
                     heights = c(1, 1.3))

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " Figure2_main_results_", tdamage_selected_main, "_amoc", amoc_selected_main, ".pdf")),
       plot = figure2, width = 15, height = 10, unit = "cm")



################################################################################
########################## FIGURE 4 ############################################
################################################################################

# calculated the expected loss w/ TPs and its increase due to TPs
df_map <- bind_rows(df_npv_bymarket_example_tipall, df_npv_bymarket_example_tipnone) %>%
  
  # trim problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # select variables of interest
  select(ssp, rcp, portfolio, iso3, mc_run, lossfactor_pv_damodaran, tip) %>%
  
  # convert to WIDE format (different columns for w/ and w/o TPs)
  # NOTE: this creates columns named 'all' w/ TPs and 'none' w/o TPs
  pivot_wider(names_from = "tip", values_from = "lossfactor_pv_damodaran") %>% 
  
  # NOTE: the data contains multiple rows per country for different stock indices
  #       therefore, we first discard all with expected loss of exactly zero (= country not featured in the respective index)
  #       and then we remove all redundant rows (rounded to 10th digit to avoid precision issues)
  filter(none != 0) %>% select(-portfolio) %>%
  mutate_if(is.numeric, ~ round(.x, 10)) %>% distinct() %>%
  
  # calculate the expected loss with and without TPs and the VaR w/
  group_by(rcp, ssp, iso3) %>%
  summarise(perc95_all = quantile(all, 0.95),
            all = mean(all),
            none = mean(none),
            n_control = n(),
            ) %>% ungroup() %>%
  
  # calculate the relative and absolute increase in expected loss due to TPs
  mutate(tp_adder_relative = (all - none)/abs(none),
         tp_adder_percpoints = all - none)

# load country-level shapefiles from the rnaturalearth package
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  
  # subset to variables of interest
  select(sovereignt, type, admin, iso3 = "iso_a3", abbrev, 
         pop_est, pop_year, lastcensus, gdp_md_est, gdp_year, 
         economy, income_grp, continent, region_un, subregion, region_wb, geometry)

# determine the lowest expected loss in the data
limit_panel_map_totalloss <- min(df_map$all)

# Figure 4a: plot the expected loss w/ TPs by country
panel_map_totalloss <- world %>%
  
  # discard Antarctica (not featuring in our data) for visual purposes
  filter(!str_detect(admin, "Antarctica")) %>%
  
  # merge the expected loss data into the shapefill
  left_join(df_map, by = "iso3") %>%
  
  # create the map and reduce the width of country boundaries
  ggplot() + geom_sf(aes(fill = all), lwd = 0.05) +
  
  # set the color scale (and set its lower limit down to zero if the lowest loss is above 0)
  scale_fill_gradient2(high = "darkred",
                       low = "darkblue",
                       na.value = "#ededed",
                       n.breaks = 3,
                       limits = c(ifelse(limit_panel_map_totalloss > 0, 0, limit_panel_map_totalloss),
                                  NA),
                       labels = ~ paste0("-", scales::percent(.x))) +
  
  # set the legend title and position
  labs(fill = "Expected loss incl. climate tipping points") +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

# determine the lowest change in the expected loss due to TPs in the data
limit_panel_map <- min(df_map$tp_adder_percpoints)

# Figure 4b: plot the increase in expected loss due to TPs by country (following same steps as above)
panel_map <- world %>%
  
  filter(!str_detect(admin, "Antarctica")) %>%
  
  left_join(df_map, by = "iso3") %>%
  
  ggplot() + geom_sf(aes(fill = tp_adder_percpoints), lwd = 0.05) +
  
  scale_fill_gradient2(high = "darkred",
                       low = "darkblue",
                       labels = ~ paste0(100*round(.x, 3)),
                       na.value = "#ededed",
                       limits = c(ifelse(limit_panel_map > 0, 0, limit_panel_map),
                                  NA),
                       n.breaks = 3) +
  
  labs(fill = "Increase of expected loss\ndue to climate tipping points (%-pts)") +
  
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")



### Figure 4c: Panel on individual tipping point contributions

# identify the files for the individual tipping point runs (= only one TP activated)
files_individual_tps <- file.path("data", "portfolio_simulation", "df_npv_fullportfolio") %>%
  list.files(full.names = T) %>%
  
  # discard model specifications using all tipping points
  str_subset("tipall", negate = T) %>%
  
  # subset to specifications using the main damage function & persistence
  str_subset(tdamage_selected_main) %>%
  str_subset(paste0("persist", persist_selected_main)) %>%
  
  # discard model specifications using no TPs
  str_subset("tipnone", negate = T) %>%
  
  # subset to the main MC sample size 
  str_subset("n2000.+\\.csv$") %>%
  
  # subset to model specifications excluding ocean methane hydrates
  str_subset("omhnone") 

# subset further to AMOC variant selected by removing all non-selected AMOC specs from the vector
for(amoc_jj in amoc_unselected) files_individual_tps <- files_individual_tps %>% str_subset(paste0("_amoc", amoc_jj), negate = T)

# add the files for the default runs w/ and w/o TPs
files_added <- file.path("data", "portfolio_simulation", "df_npv_fullportfolio") %>%
  list.files(full.names = T) %>% str_subset("n2000") %>%
  
  # subset to RCP4.5 and SSP2 and respective tipping point identifiers
  str_subset("RCP45SSP2") %>% str_subset("tipall|tipnone") %>%
  
  # subset to the main damage function and persistence
  str_subset(paste0("persist", persist_selected_main)) %>%
  str_subset(tdamage_selected_main) %>%
  
  # subset to time horizons ending in 2100 and exclude alternative equity risk premiums (identified via the 'erp' suffix)
  str_subset("end2100") %>%
  str_subset("erp0", negate = T)
  

# again, remove non-selected AMOC variants from the list
for(amoc_jj in amoc_unselected) files_added <- files_added %>% str_subset(amoc_jj, negate = T)

# NOTE: we expect 8 files in total if 'amoc_selected_main' is set to 'none' and 9 files if a specific AMOC variant is selected; otherwise, throw an error
if(length(c(files_individual_tps, files_added)) != 9 & (length(c(files_individual_tps, files_added)) != 8 & amoc_selected_main == "none")) stop("Expected 9 files (7 individual TPs plus one all and one none file), or 8 files if amoc_selected_main is none, but got different number. Inspect")

# read in the respective PV loss distributions and trim problematic MC runs
df_individual_tps <- c(files_individual_tps, files_added) %>%
  
  map_dfr(read_csv) %>%
  
  filter(!mc_run %in% mc_runs_screenedout)


# calculate summary statistics
df_individual_tps_sumstat <- summarise_npv_distribution(df_individual_tps) %>%
  
  # remove catastrophic AMOC collapse
  filter(amoc != "Cai" | is.na(amoc)) %>%
  
  # remove OMH
  filter(tip != "OMH")


# create a function to create a waterfall chart for TP contributions to the expected loss
make_waterfall_increase <- function(data = NULL, # summary statistics of the loss distribution for the different specifications using individual tipping points
                                    portfolio_selected = "msciworld", # the stock index of interest
                                    aggregate_tip = F, # whether or not to aggregate the contribution of some tipping points into one waterfall bar (e.g., the two ice sheets)
                                    rect_text_size_selected = 0.5 # the size of the text in the rectangles
                                    ) {
  
  df_chart <- data %>%
    
    # subset to index selected and discount rate incl. CRPs ('damodaran')
    filter(portfolio == portfolio_selected, discountrate == "damodaran") %>%
    
    # calculate increase in expected loss over no tipping points
    mutate(mean_increase_relative = mean/mean[tip=="none"] - 1)
  
  
  # add an interaction term calculated as a residual
  df_chart <- bind_rows(df_chart,
                        tibble(tip = "interact",
                               mean_increase_relative =  df_chart$mean_increase_relative[df_chart$tip == "all"] - (df_chart %>% filter(tip != "all") %>% pull(mean_increase_relative) %>% sum())))
  
  
  # aggregate tipping points up if this is specified by the user
  # NOTE: as of May 2024, this is not used anywhere in the paper
  if(aggregate_tip) {
    
    df_chart <- df_chart %>%
      
      mutate(tip = case_when(tip %in% c("GIS", "WAIS") ~ "Disintegration\nof ice sheets",
                             tip %in% c("OMH", "PCF") ~ "Methane hydrates &\npermafrost thawing",
                             TRUE ~ tip)) %>%
      
      group_by(tip) %>%
      
      summarise(mean_increase_relative = sum(mean_increase_relative),
                .groups = "drop")
  }
  
  # specify the order of the tipping points and select variables of interest
  df_chart <- df_chart %>%
    
    # first comes none, then individual tipping points, then interaction, then the total
    mutate(order = case_when(tip == "none" ~ 1,
                           tip == "interact" ~ 3,
                           tip == "all" ~ 4,
                           TRUE ~ 2)) %>%
    
    # arrange by order and individual tipping points by their contribution
    arrange(order, mean_increase_relative) %>%
    
    # subset to tipping point label and expected loss contribution
    select(tip, mean_increase_relative)
  
  
  # calculate the label used for the total bar in the waterfall chart
  label_total <- paste0(100*round(df_chart$mean_increase_relative[df_chart$tip == "all"], 3),
                        "%")
  
  # discard the entries with all and no TPs and create clean labels for the waterfall bars
  df_chart <- df_chart %>%
    
    filter(tip != "all" & tip != "none") %>%
    
    mutate(rect_labels = paste0(ifelse(mean_increase_relative >= 0, "+", ""),
                                       100*round(mean_increase_relative, 3), "%") %>%
                        # NOTE: if R rounds to 0%, we ensure that the 1st digit is still displayed manually: 
                        str_replace("^\\+0%", "+0.0%"))
  
  
  # create the waterfall chart
  waterfalls::waterfall(values = df_chart$mean_increase_relative,
                        labels = clean_individual_tip_labels(df_chart$tip),
                        rect_text_labels = df_chart$rect_labels,
                        rect_text_size = rect_text_size_selected,
                        calc_total = T,
                        total_axis_text = "All tipping points\n(combined)",
                        total_rect_text = label_total,
                        fill_by_sign = F,
                        rect_border = NA,
                        total_rect_border_color = NA,
                        draw_lines = F,
                        linetype = "dashed",
                        draw_axis.x = "none",
                        total_rect_color = "darkred",
                        total_rect_text_color = "white",
                        fill_colours = rep("grey", 10))

}

# make the chart for the MSCI World
panel_waterfall <- make_waterfall_increase(df_individual_tps_sumstat, "msciworld") +
  
  # add horizontal line at zero
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey", linewidth = 0.2) +
  
  # set the y-axis labels to percentages
  scale_y_continuous(labels = ~ paste0(ifelse(.x > 0, "+", ""), scales::percent(.x))) +
  
  # label the axes
  labs(x = "Increase of expected loss\ndue to climate tipping points",
       y = NULL) +
  
  # flip the axes and facet by the stock index (using clean labels)
  coord_flip() +
  facet_wrap(~ clean_portfolio_labels("msciworld"))


# repeat for the MSCI EM
panel_waterfall_em <- make_waterfall_increase(df_individual_tps_sumstat, "msciem",
                                           rect_text_size_selected = 1.25) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey", linewidth = 0.3) +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(y = "Increase of expected loss\ndue to climate tipping points",
       x = NULL) +
  
  # use the classic theme here instead
  theme_classic() +
  
  coord_flip() +
  
  facet_wrap(~ clean_portfolio_labels("msciem"))


# repeat for the MSCI FEM
panel_waterfall_fm <- make_waterfall_increase(df_individual_tps_sumstat, "mscifm",
                                              rect_text_size_selected = 1.25) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey", linewidth =  0.3) +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(y = "Increase of expected loss\ndue to climate tipping points",
       x = NULL) +
  
  theme_classic() +
  
  coord_flip()  +
  
  facet_wrap(~ clean_portfolio_labels("mscifm"))

# combine the panels for the MSCI EM and FEM and save out for the SI
ggarrange(panel_waterfall_em, panel_waterfall_fm,
          nrow = 1, align = "hv", labels = "auto", font.label = list(size = 7, color = "black", face = "bold"))

ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure_Waterfall_EM_FEM", tdamage_selected_main, "_amoc", amoc_selected_main, ".pdf")),
       width = 10, height = 4.5)


#### Figure 4e: Panel on investor horizons

# identify the three relevant files using the 2024-2100, the 2034-2110, and the 2044-2120 horizon for the default specification
files_timehorizon <- file.path("data", "portfolio_simulation", "df_npv_fullportfolio") %>%
  list.files(full.names = T) %>%
  
  # subset to the main specification
  str_subset("RCP45SSP2") %>%
  str_subset(paste0("persist", persist_selected_main)) %>%
  str_subset("tipall|tipnone") %>%
  str_subset(tdamage_selected_main) %>%
  
  # subset to the horizons of interest
  str_subset("base2024_end2100|base2034_end2110|base2044_end2120") %>%
  
  # discard specifications that use alternative equity risk premiums (identified via the 'erp' suffix)
  str_subset("_erp0", negate = T) %>%
  
  # discard specifications that use ocean methane hydrates or catastrophic AMOC collapse
  str_subset("omhdefault", negate = T) %>%
  str_subset("amocCai", negate = T)


# further discard all non-selected AMOC specifications
for(amoc_jj in amoc_unselected) {
  files_timehorizon <- files_timehorizon %>%
    str_subset(paste0("tipall_.+_amoc", amoc_jj), negate = T)
}

# we expect exactly six files (3 horizons x with or without TPs). Throw an error if this is not the case
if(length(files_timehorizon) != 6) stop("Expected 6 files (3 horizons x 2 switches for TPs (all or none))")

# read in the respective loss distributions
df_timehorizon <- files_timehorizon %>% map_dfr(read_csv)

# calculate the summary statistics
df_timehorizon_sumstat <- df_timehorizon %>%
  
  # trim the problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # calculate summary stats
  summarise_npv_distribution() %>%
  
  # subset to the default discount rate regime (i.e., including country risk premiums)
  filter(discountrate == "damodaran")


# define a function to create the investment horizon chart
make_panel_horizon_chart <- function(data = df_timehorizon_sumstat, # data with the loss distribution summary statistics
                                     portfolio_selected = "msciworld", # stock index of interest
                                     index_label_subtitle = T # whether or not to add the index name as a plot subtitle
                                     ) {

  df_chart <- data %>%
    
    # convert to WIDE format by saving expected loss & PV w/ and w/o TPs into separate columns
    group_by(portfolio, base_year, end_year) %>%
    summarise(mean_all = mean[tip == "all"],
              mean_none = mean[tip == "none"],
              perc95_all = perc95[tip == "all"],
              perc95_none = perc95[tip == "none"]) %>% ungroup() %>%
    
    # calculate the increase in the expected loss due to TPs
    mutate(tp_adder = mean_all - mean_none) %>%
    
    # convert back to LONG format 
    pivot_longer(cols = c(mean_none, tp_adder, perc95_all, perc95_none), values_to = "value", names_to = "label") %>%
    
    # subset to the index of interest
    filter(portfolio == portfolio_selected) %>%
    
    # add a column to distinguish between the mean and the 95th percentile
    mutate(summarystat = if_else(str_detect(label, "perc95"), "perc95", "mean")) %>%
    
    # add a clean label for the investment horizon based on the 'base_year' and 'end_year' columns and clean up the TP label
    mutate(horizon = paste0(base_year, "-", end_year),
           label_clean = factor(case_when(label %in% c("mean_none", "perc95_none") ~ "Excl. climate tipping points",
                             TRUE ~ "Incl. climate tipping points"),
                          levels = c("Incl. climate tipping points", "Excl. climate tipping points"))) %>%
    
    # calculate the expected loss used for positioning
    group_by(portfolio, base_year, end_year) %>%
    mutate(mean_total = if_else(summarystat == "mean" & str_detect(label, "none"), value, sum(value[summarystat == "mean"]))) %>%
    ungroup()
  
  
  # make the plot (time horizon on the x-axis, loss/VaR on the y-axis)
  df_chart %>% ggplot(aes(horizon, value)) +
    
  # semi-transparent bars for expected loss
  geom_col(aes(y = ifelse(summarystat == "mean", value, NA), fill = label_clean), alpha = 0.1, width = 0.5) +
    
  # squares for expected loss
  geom_point(aes(y = ifelse(summarystat == "mean", mean_total, NA), colour = label_clean, shape = clean_summarystat_labels("mean")),
               size = 1) +
  
  # triangles for 95% VaR
  geom_point(aes(y = ifelse(summarystat == "perc95", value, NA), colour = label_clean, shape = clean_summarystat_labels("perc95")),
               size = 1) +
    
  # value labels for expected loss
  geom_text(aes(y = ifelse(summarystat == "mean" & label == "tp_adder", mean_all*1.2, NA), label = paste0("-", 100*round(mean_all, 3), "%")), size = 5/.pt) +
    
  # value labels for 95% VaR
  geom_text(aes(y = ifelse(summarystat == "perc95" & label == "perc95_all", value*1.1, NA), label = paste0("-", 100*round(value, 3), "%")), size = 5/.pt) +  
    
  # set y-axis values to percentages
  scale_y_continuous(labels = ~ paste0("-", scales::percent(.x))) +
  
  # select shapes for expected loss and 95% VaR  
  scale_shape_manual(values = c("Expected loss" = 15, "95% VaR (95th percentile)" = 17)) +
  
  # set order and aesthetics of different legends  
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 1),
         shape = guide_legend(order = 2, override.aes = list(color = "grey"))) +
    
  # set axis titles
  labs(x = "Investor horizon", y = "Loss in % of present value", fill = NULL, shape = NULL,
       color = NULL) +
    
  # if user sets 'index_label_subtitle' to TRUE, facet wrap by stock index using clean labels
  {if(index_label_subtitle) facet_wrap(~ clean_portfolio_labels(portfolio_selected))} +  
    
  # position legend
  theme(legend.position = "bottom",
        legend.direction = "vertical")

}

# make the chart for the MSCI World
panel_horizon <- make_panel_horizon_chart()


### Figure 4d: Panel on relationship with ERP

# this can require a lot of memory, so first we clean up the environment a bit by discarding objects that are no longer needed
rm(df_figure1c, df_figure2, df_figure2c, 
   df_timehorizon)

# as usual, we identify the files or interest - this time those with the 'erp' suffix
files_erp <- file.path("data", "portfolio_simulation", "df_npv_fullportfolio") %>%
  list.files(full.names = T) %>%
  str_subset(tdamage_selected_main) %>%
  str_subset("_erp0\\.[:digit:]+\\.csv") %>%
  str_subset("_omhdefault", negate = T) %>%
  
  # unselect files with ERP higher than 0.15 as they are not used in the paper
  str_subset("erp0\\.0[0-9]+|erp0\\.1[1-5]+|erp0\\.1\\.csv")

# also discard non-selected AMOC specifications
for(amoc_jj in amoc_unselected) {
  files_erp <- files_erp %>%
    str_subset(paste0("amoc", amoc_jj), negate = T)
}

# we expect exactly 30 files (15 ERP values x w/ or w/o tipping points). Throw an error otherwise
if(length(files_erp) != 30) stop("Expected 30 files but got a different number")

# read in the files and calculate summary statistics of the respective loss distributions
df_erp_sumstat <- map_dfr(files_erp, ~ read_csv(.x) %>%
                            
                            # extract the ERP value used
                            mutate(erp = str_extract(.x, "(?<=\\_erp)0\\.[:digit:]+")) %>%
                            
                            # trim problematic MC runs
                            filter(!mc_run %in% mc_runs_screenedout) %>%
                            
                            # convert to LONG format
                            pivot_longer(cols = contains("lossfactor_pv"), names_to = "discountrate", values_to = "lossfactor_pv") %>%
                            
                            # clean up the 'discountrate' column
                            mutate(discountrate = str_remove(discountrate, "^lossfactor_pv_")) %>%
                            
                            # calculate summary statistics
                            group_by(rcp, ssp, portfolio, persist, mc_samplesize, tip, tdamage, base_year, end_year, discountrate, erp) %>%
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
                            ungroup()
                          )

  

# write a function to create the chart
make_panel_erp_chart <- function(data = df_erp_sumstat, # data with summary stats for different ERP values
                                 portfolio_selected = "msciworld", # stock index of interest
                                 index_label_subtitle = T # whether or not to add a subtitle with the stock index name
                                 ) {
  
  # extract the index size from df_calibration_yield for the index in question and convert from USD to USD trillion and billion
  index_size_tn <- df_calibration_yield$index_size[df_calibration_yield$portfolio == portfolio_selected]/10^12
  index_size_bn <- index_size_tn*10^3
  
  # create the chart
  data %>%
    
    # filter for the stock index of interest and the Damodaran discount rate incl. country risk premiums
    filter(portfolio == portfolio_selected, discountrate == "damodaran") %>%
    
    # convert the 'erp' column to numeric
    mutate(erp = as.numeric(erp)) %>%
    
    # initiate the plot with ERP values on the x-axis
    ggplot(aes(erp)) +
  
    # vertical line at the default value of 5%
    geom_vline(aes(xintercept = 0.05), linetype = "dashed", color = "grey", linewidth = 0.2) +
  
    # line & point chart for expected loss expressed in USD billion 
    geom_line(aes(y = mean*index_size_bn , colour = clean_tip_category_labels(tip)), alpha = 0.5, linewidth = 0.2) +
    geom_point(aes(y = mean*index_size_bn , colour = clean_tip_category_labels(tip), shape = clean_summarystat_labels("mean")),
             size = 0.5) +
  
    # repeat for the 95% VaR
    geom_line(aes(y = perc95*index_size_bn , colour = clean_tip_category_labels(tip)), alpha = 0.5, linewidth = 0.2) +
    geom_point(aes(y = perc95*index_size_bn , colour = clean_tip_category_labels(tip), shape = clean_summarystat_labels("perc95")),
               size = 0.5) +
  
    # errorbar for increases in expected loss due to tipping points  
    geom_errorbar(data = df_erp_sumstat %>%
                    
                    # subset to the stock index of interest and the Damodaran discount rate incl. country risk premiums
                    filter(portfolio == portfolio_selected, discountrate == "damodaran") %>%
                    
                    # calculate the increase in the expected loss due to TPs
                    group_by(erp, portfolio) %>%
                    
                    mutate(tp_adder = mean[tip == "all"] - mean[tip == "none"],
                           mean_total = mean + tp_adder,
                           erp = as.numeric(erp)) %>%
                    
                    # filter for the case without tipping points (to avoid having the error bar twice) and to selected ERP example values
                    filter(tip == "none",
                           erp %in% c(0.05, 0.1, 0.15, 0.2)),
                  
                  # draw error bar from the expected loss w/o TPs to the expected loss with TPs
                  aes(ymin = mean*index_size_bn, ymax = mean_total*index_size_bn ),
                  
                  # reduce width of the error bar and set color & linewidth
                  width = 0.005,
                  colour = "darkred",
                  linewidth = 0.3) +
  
    # errorbar for increases in 95% VaR due to tipping points (same steps as above) 
    geom_errorbar(data = df_erp_sumstat %>%
                    
                    filter(portfolio == portfolio_selected, discountrate == "damodaran") %>%
                    
                    group_by(erp, portfolio) %>%
                    
                    mutate(tp_adder = perc95[tip == "all"] - perc95[tip == "none"],
                           perc95_total = perc95 + tp_adder,
                           erp = as.numeric(erp)) %>%
                    
                    filter(tip == "none",
                           erp %in% c(0.05, 0.1, 0.15, 0.2)),
                  aes(ymin = perc95*index_size_bn, ymax = perc95_total*index_size_bn ),
                  width = 0.005,
                  colour = "darkred",
                  linewidth = 0.3) +  
    
    # add text labels for the increase in the expected loss due to tipping points
    geom_text(data = df_erp_sumstat %>%
                  
                  filter(portfolio  == portfolio_selected, discountrate == "damodaran") %>%
                  
                  group_by(erp, portfolio) %>%
                  
                  mutate(tp_adder = mean[tip == "all"] - mean[tip == "none"],
                         mean_total = mean + tp_adder,
                         erp = as.numeric(erp)) %>%
                  
                  filter(tip == "none",
                         erp %in% c(0.05, 0.1, 0.15, 0.2)),
                
              aes(x = erp, y = (0.5*mean + 0.5*mean_total)*index_size_bn*1.35,
                  label = paste0("+", 100*round(tp_adder/mean, 3), "%")),
            colour = "darkred", size = 5/.pt) +
    
    # repeat the same for the 95% VaR
    geom_text(data = df_erp_sumstat %>%
                
                filter(portfolio  == portfolio_selected, discountrate == "damodaran") %>%
                
                group_by(erp, portfolio) %>%
                
                mutate(tp_adder = perc95[tip == "all"] - perc95[tip == "none"],
                       perc95_total = perc95 + tp_adder,
                       erp = as.numeric(erp)) %>%
                
                filter(tip == "none",
                       erp %in% c(0.05, 0.1, 0.15, 0.2)),
              aes(x = erp, y = (0.5*perc95 + 0.5*perc95_total)*index_size_bn*1.25,
                  label = paste0("+", 100*round(tp_adder/perc95, 3), "%")),
              colour = "darkred", size = 5/.pt) +
  
    # if the stock index is the MSCI World (= used for the main chart), add a text label for the default value of 5%
    {if(portfolio_selected == "msciworld") annotate("text", x = 0.055, y = 2700, label = "Default value",
           color = "darkgrey", size = 5/.pt, fontface = "bold", hjust = 0)} +
  
    # format x and x axis labels  
    scale_y_continuous(labels = ~ paste0("-$", .x, "bn")) +
    scale_x_continuous(labels = scales::percent,
                       breaks = c(0.01, 0.05, 0.1, 0.15)) +
      
    # specify the shapes for expected loss and VaR
    scale_shape_manual(values = c("Expected loss" = 15, "95% VaR (95th percentile)" = 17)) +
    
    # set axis labels and drop legend titles
    labs(x = "Equity risk premium for discounting",
         y = "Loss",
         colour = NULL,
         shape = NULL,
         linetype = NULL) +
    
    # ensure that the y-axis starts at 0 and that we stop at 15% ERP
    coord_cartesian(ylim = c(0, NA), xlim = c(NA, 0.15)) +
    
    # set order and aesthetics of the legends
    guides(color = guide_legend(order = 1),
           shape = guide_legend(order = 2, override.aes = list(color = "grey")),
           linetype = guide_legend(order = 2)) +
    
    # position the legend
    theme(legend.position = "bottom",
          legend.direction = "vertical") +
    
    # if required by the user, facet the chart by the stock index label
    {if(index_label_subtitle) facet_wrap(~ clean_portfolio_labels(portfolio_selected))}
  
}

# create the chart for the MSCI World
panel_erp <- make_panel_erp_chart()


### combine panels

figure3 <- ggarrange(# first we combine Panels a and b (= the maps) horizontally as a 1x2
                     ggarrange(panel_map_totalloss, panel_map, nrow = 1, ncol = 2,
                               labels = "auto",
                               font.label = list(size = 7, color = "black", face = "bold")), 
                     # then we combine the other three panels horizontally as a 1x3
                     ggarrange(panel_waterfall, panel_erp, panel_horizon,
                               nrow = 1, ncol = 3, align = "hv",
                               labels = c("c", "d", "e"),
                               common.legend = F, #legend = "bottom",
                               font.label = list(size = 7, color = "black", face = "bold")),
                     # then we combine those two vertically
                     nrow = 2, ncol = 1,
                     labels = c("", ""),
                     font.label = list(size = 7, color = "black", face = "bold"),
                     heights = c(1.2, 1))

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " Figure3_", tdamage_selected_main, "_amoc", amoc_selected_main, ".pdf")),
       plot = figure3, width = 17, height = 12, unit = "cm")

# pending modifications in Illustrator
# - add x axis label to waterfall panel which is omitted for some reason; reposition value labels if required

# make SI version of horizon & ERP panels for MSCI EM & FEM
si_erp_horizon_em_fm <- ggarrange(make_panel_horizon_chart(portfolio_selected = "msciem", index_label_subtitle = T),
                                  make_panel_horizon_chart(portfolio_selected = "mscifm", index_label_subtitle = T),
                                  make_panel_erp_chart(portfolio_selected = "msciem", index_label_subtitle = T),
                                  make_panel_erp_chart(portfolio_selected = "mscifm", index_label_subtitle = T),
                                  nrow = 2, ncol = 2, labels = "auto",
                                  font.label = list(size = 7, color = "black", face = "bold"),
                                  common.legend = T, legend = "bottom", align = "hv")

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure3_horizon_erp_EM_FEM_", tdamage_selected_main, "_amoc", amoc_selected_main, ".pdf")),
       plot = si_erp_horizon_em_fm,
       width = 12, height = 12, unit = "cm")


################################################################################
########################### FIGURE 3 ###########################################
################################################################################

# load the loss distributions for the different damage function specifications (COACCH vs BHM, w/ or w/o persistence)
df_figure4a <- c(### BHM
                 paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persistDistribution_tipall_tdamagebhm_distribution_omhnone_amoc", amoc_selected_main, "_base2024_end2100.csv"),
                 paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persistDistribution_tipnone_tdamagebhm_distribution_base2024_end2100.csv"),
                 paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist0.0_tipall_tdamagebhm_distribution_omhnone_amoc", amoc_selected_main, "_base2024_end2100.csv"),
                 paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist0.0_tipnone_tdamagebhm_distribution_base2024_end2100.csv"),
                 ### COACCH
                 paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist1.0_tipall_tdamagecoacch_central_omhnone_amoc", amoc_selected_main, "_base2024_end2100.csv"),
                 paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist1.0_tipnone_tdamagecoacch_central_base2024_end2100.csv")
                 ) %>%
  
  # read in the files and ensure that the 'persist' column is always a character
  map_dfr(~ read_csv(file.path("data", "portfolio_simulation", "df_npv_fullportfolio", .x),
                     show_col_types = F) %>% mutate(persist = as.character(persist))) %>%
  
  # convert the persistence parameter values (1 = no persistence) into clean labels for the chart, convert to a factor to fix order
  mutate(persist = factor(case_when(persist == "1" ~ "No persistence",
                                    persist == "0" ~ "Full persistence",
                                    persist == "Distribution" ~ "Uniform distribution of\n 0-100% persistence"),
                          levels = c("No persistence", "Uniform distribution of\n 0-100% persistence", "Full persistence"
                          ))) %>%
  
  # do the same for the 'tdamage' column indicating the damage function used
  mutate(tdamage = factor(case_when(tdamage == "bhm_distribution" ~ "Burke et al. (2015)",
                                    tdamage == "coacch_central" ~ "Van der Wijst et al. (2023): COACCH"),
                          levels = c("Burke et al. (2015)", "Van der Wijst et al. (2023): COACCH"))) %>%
  
  # clean up the tipping point label
  mutate(tip = if_else(tip == "all", "Incl. climate\n tipping points", "Excl. climate\n tipping points")) %>%
  
  # trim the problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout)


# calculate the summary statistics for the loss distributions and subset to the discount rate with CRPs
df_figure4a_summarystat <- df_figure4a %>% summarise_npv_distribution() %>% filter(discountrate == "damodaran")

### calculate the maximum and mimum loss across specifications
# maximum
max_all <- df_figure4a_summarystat %>% filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  pull(max) %>% max()
# minimum
min_all <- df_figure4a_summarystat %>% filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  pull(min) %>% min()

# create a chart to create raincloud plots for the loss distribution of a given damage function specification
make_persist_chart <- function(persist_selected = NULL, # persistence selected
                               tdamage_selected = NULL, # damage function selected
                               ylim_selected = c(NA, NA), # limits for the y-axis
                               portfolio_selected = "msciworld", # stock index of interest
                               min_label_adjustmentfactor = -0.07 # by how much the annotation labels for expected loss and 95% VaR should be adjusted vertically
                               ) {
  
  # fix random seed for geom_jitter; otherwise the chart would look differently every time we create it
  set.seed(1)
  
  # subset the data to the damage function specification and stock index of interest
  df_figure4a %>%
    filter(portfolio == portfolio_selected,
           persist == persist_selected,
           tdamage == tdamage_selected) %>%
    
    # group by w/ or w/o tipping points (required for the geom_flat_violin function used below)
    group_by(tip) %>%
    
    ggplot(aes(tip)) +
    
    # horizontal line at zero
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey", linewidth = 0.2) +
    
    # add each data point as jitter point
    geom_point(aes(y = -lossfactor_pv_damodaran, colour = tip),
               position = position_jitter(w = .15),
               size = 0.5,
               alpha = 0.05) +
    
    # one-sided violin chart for the density
    geom_flat_violin(aes(y = -lossfactor_pv_damodaran,
                         fill = tip),
                     position = position_nudge(x = .2),
                     alpha = 0.9,
                     adjust = 0.5, 
                     width = 0.5, size = 0.1) +
    
    # boxplots based on the distribution summary stats
    geom_boxplot(data = df_figure4a_summarystat %>% filter(persist == persist_selected,
                                                           portfolio == portfolio_selected,
                                                           tdamage == tdamage_selected),
                 stat = "identity",
                 # we use quartiles for the box, and 5% and 95% VaR for the whiskers
                 aes(lower = -perc25, upper = -perc75,
                     middle = -median, ymin = -perc05, ymax = -perc95),
                 fill = NA, outlier.shape = NA, width = 0.15, colour = "black", linewidth = 0.2) +
    
    # add the expected loss and 95% VaR as text labels
    geom_text(data = df_figure4a_summarystat %>% filter(persist == persist_selected,
                                                        portfolio == portfolio_selected,
                                                        tdamage == tdamage_selected),
              aes(y = -0.37, label = paste0("Mean: ", 100*round(-mean, 3), "%",
                                           "\n95% VaR: ", 100*round(-perc95, 3), "%")),
              size = 5/.pt) +
    
    # label y-axis and drop other axis and legend titles
    labs(y = "Loss in % of present value\n(positive values = gains)", x = NULL,
         fill = NULL, colour = NULL) +

    # facet by the stock index & damage function (NOTE: we subset to one index & function only, so this simply creates a clean-looking subtitle)
    facet_grid(~ paste0(clean_portfolio_labels_short(portfolio_selected), "\n", tdamage, "\n", persist)) +
    
    # set color scales
    scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
    scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
    
    # set y-axis values to percentage
    scale_y_continuous(labels = scales::percent) +
    
    # drop the legend and set the y-axis limits to what the user specified
    theme(legend.position = "none") +
    coord_cartesian(ylim = ylim_selected) +
    
    # remove the strip background from the facets
    theme(strip.background = element_blank())
}


# Figure 3a: MSCI World & COACCH
figure4a <- make_persist_chart(persist_selected = "No persistence", tdamage_selected = "Van der Wijst et al. (2023): COACCH",
                               ylim_selected = c(min_all - 0.1, max_all), min_label_adjustmentfactor = -0.1) +
            # make the subtitle bold to indicate that this is the default
            theme(strip.text = element_text(size = 5, face = "bold"))

# Figure 3b: MSCI World & Burke et al. (2015), 0-100% persistence
figure4b <- make_persist_chart(persist_selected = "Uniform distribution of\n 0-100% persistence", tdamage_selected = "Burke et al. (2015)",
                   ylim_selected = c(min_all - 0.1, max_all))

# Figure 3c: MSCI World & Burke et al. (2015), 100% persistence
figure4c <- make_persist_chart(persist_selected = "Full persistence", tdamage_selected = "Burke et al. (2015)",
                   ylim_selected = c(min_all - 0.1, max_all))


# repeat the same for the MSCI EM 
figure4d <- make_persist_chart(persist_selected = "No persistence", tdamage_selected = "Van der Wijst et al. (2023): COACCH", portfolio_selected = "msciem",
                               ylim_selected = c(min_all - 0.1, max_all), min_label_adjustmentfactor = -0.1) +
            theme(strip.text = element_text(size = 5, face = "bold"))

figure4e <- make_persist_chart(persist_selected = "Uniform distribution of\n 0-100% persistence", tdamage_selected = "Burke et al. (2015)", portfolio_selected = "msciem",
                               ylim_selected = c(min_all - 0.1, max_all))
figure4f <- make_persist_chart(persist_selected = "Full persistence", tdamage_selected = "Burke et al. (2015)", portfolio_selected = "msciem",
                               ylim_selected = c(min_all - 0.1, max_all))


# combine the panels into one 2x3 grid chart
figure4 <- ggarrange(figure4a, figure4b, figure4c, figure4d, figure4e, figure4f,
                     ncol = 3, nrow = 2, align = "hv", labels = "auto",
                     font.label = list(size = 7, color = "black", face = "bold"))

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " Figure4_", tdamage_selected_main, "_amoc", amoc_selected_main, ".pdf")),
       plot = figure4, width = 15, height = 10, unit = "cm")


################################################################################
############################### FIGURE 5 #######################################
################################################################################

# set the labels for our main specification w/o AMOC collapse and the specification with AMOC collapse
figure5_label_tipall_amocnone <- "Incl. climate tipping points\n(without AMOC collapse)"
figure5_label_tipall_amocCai <- "+ AMOC collapse"

# select the three specifications of interest (default values w/o TPs, w TPs, and w/ TPs incl. AMOC collapse)
df_figure5_distr <- c("df_npv_fullportfolio_n2000_RCP45SSP2_persist1.0_tipall_tdamagecoacch_central_omhnone_amocCai_base2024_end2100.csv",
                "df_npv_fullportfolio_n2000_RCP45SSP2_persist1.0_tipall_tdamagecoacch_central_omhnone_amocnone_base2024_end2100.csv",
                "df_npv_fullportfolio_n2000_RCP45SSP2_persist1.0_tipnone_tdamagecoacch_central_base2024_end2100.csv") %>%
  
  # load the loss distributions and ensure 'persist' column is a character
  map_dfr(~ read_csv(file.path("data", "portfolio_simulation", "df_npv_fullportfolio", .x),
                     show_col_types = F) %>% mutate(persist = as.character(persist))) %>%
  
  # trim problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # clean up the tipping point column labels and convert to a factor to set the order
  mutate(case = factor(case_when(tip == "none" ~ "Excl. climate tipping points",
                                 tip == "all" & amoc == "none" ~ figure5_label_tipall_amocnone,
                                 tip == "all" & amoc == "Cai" ~ figure5_label_tipall_amocCai,
                                 TRUE ~ "INSPECT XX"),
                       levels = c(figure5_label_tipall_amocCai,
                                  figure5_label_tipall_amocnone,
                                  "Excl. climate tipping points"))) %>%
  
  # calculate VaRs of interest
  group_by(ssp, portfolio, rcp, persist, tdamage, omh, amoc, base_year, end_year, case, tip, mc_samplesize) %>%
  mutate(lossfactor_pv_damodaran_perc90 = quantile(lossfactor_pv_damodaran, 0.9),
         lossfactor_pv_damodaran_perc95 = quantile(lossfactor_pv_damodaran, 0.95),
         lossfactor_pv_damodaran_perc99 = quantile(lossfactor_pv_damodaran, 0.99)
         ) %>% ungroup()


# calculate summary stats for the different distributions
df_figure5_sumstat <- df_figure5_distr %>% summarise_npv_distribution() %>%
  
  # subset to the discount rate using CRPs and the three main stock indices
  filter(discountrate == "damodaran", portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  
  # clean up the tipping point column labels and convert to a factor to set the order
  mutate(case = factor(case_when(tip == "none" ~ "Excl. climate tipping points",
                                 tip == "all" & amoc == "none" ~ figure5_label_tipall_amocnone,
                                 tip == "all" & amoc == "Cai" ~ figure5_label_tipall_amocCai,
                                 TRUE ~ "INSPECT XX"),
                       levels = c(figure5_label_tipall_amocCai,
                                  figure5_label_tipall_amocnone,
                                  "Excl. climate tipping points")))


# Figure 5a: Loss factor distributions for the three main stock indices w/ TPs and w/ TPs incl. AMOC collapse
figure5a <- df_figure5_distr %>%
  
  # subset to the three main stock indices
  filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  
  # remove the distribution w/o TPs
  filter(case != "Excl. climate tipping points") %>%
  
  # group by TP specification (required for geom_flat_violin() below)
  group_by(case) %>%
  
  ggplot(aes(case)) +
  
  # horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey", linewidth = 0.2) +
  
  # jitter points for the individual distribution
  geom_point(aes(y = lossfactor_pv_damodaran, colour = case),
             position = position_jitter(w = .15),
             size = 0.5,
             alpha = 0.05) +
  
  # one-sided violin chart for the distribution
  geom_flat_violin(aes(y = lossfactor_pv_damodaran,
                       fill = case),
                   position = position_nudge(x = .2),
                   alpha = 0.9,
                   adjust = 0.5, 
                   width = 0.5, size = 0.1) +
  
  # add MC as a point
  geom_point(data = df_figure5_sumstat %>% filter(case != "Excl. climate tipping points"),
             aes(y = mean, shape = clean_summarystat_labels("mean")), color = "darkgrey", size = 1.5) +
  
  # add 95% VaR
  geom_point(data = df_figure5_sumstat %>% filter(case != "Excl. climate tipping points"),
             aes(y = perc95, shape = clean_summarystat_labels("perc95")), color = "darkgrey", size = 1.5) +
  
  # add 99% VaR
  geom_point(data = df_figure5_sumstat %>% filter(case != "Excl. climate tipping points"),
             aes(y = perc99, color = case, shape = clean_summarystat_labels("perc99")), size = 1.5,
             stroke = 0.1) +
  
  # add boxplot based on summary stats
  geom_boxplot(data = df_figure5_sumstat %>% filter(case != "Excl. climate tipping points"),
               stat = "identity",
               # we use quartiles for the box and 5% and 95% VaR for whiskers
               aes(lower = perc25, upper = perc75,
                   middle = median, ymin = perc05, ymax = perc95),
               fill = NA, outlier.shape = NA, width = 0.15, colour = "black", linewidth = 0.2) +
  
  # add the 99% VaR as a text label
  geom_text(data = df_figure5_sumstat %>% filter(case != "Excl. climate tipping points"),
            aes(y = perc99, label = paste0("99% VaR:\n-", 100*round(perc99, 3), "%")),
            size = 5/.pt, nudge_x = -0.35) +
  
  # set y-axis title and drop other titles of axes and legends
  labs(y = "Loss in % of present value", x = NULL,
       fill = NULL, colour = NULL, shape = NULL) +
  
  # set shapes for expected loss and VaRs
  scale_shape_manual(values = c("Expected loss" = 15, "95% VaR (95th percentile)" = 17, "99% VaR (99th percentile)" = 10)) +
  
  # set y-axis labels to percentages and determine axis breaks
  scale_y_continuous(labels = ~ paste0("-", scales::percent(.x)),
                     breaks = c(0, 0.05, 0.1)) +
  
  # facet by stock index using clean labels
  facet_wrap(~ clean_portfolio_labels(portfolio)) +
  
  # drop some legends entirely
  guides(color = "none",
         fill = "none") +
  
  # specify the colors manually and override the size of the legend keys
  scale_fill_manual(values = c(#"#00BFC4",
    "#582A27", "#F8766D"), 
                    guide = guide_legend(override.aes=list(size = 0.0001))) +
  scale_color_manual(values = c(#"#00BFC4",
    "#582A27", "#F8766D"),
                     guide = guide_legend(override.aes=list(size = 0.0001))) +
  
  # flip x- and y-axis
  coord_flip() +
  
  # set the legend key size
  theme(legend.key.size = unit(0.1, "cm"))



### Figure 5b: Loss factor percentiles by year for the US

## load the loss factors for the three main markets per index
# NOTE: as of May 2024, we only use the data for the US as effect on EM and FEM is limited

# RCP4.5 - no AMOC collapse
df_lossfactor_conspc_rcp45_amocnone <- file.path("data", "META MC results", "lossfactors", "lossfactor_conspc", "lossfactor_conspc_n2000_RCP45SSP2_persist1.0_tipall_tdamagecoacch_central_omhnone_amocnone.csv") %>%
  read_csv() %>%
  select(mc_run, year, USA, CHN, PHL)

# RCP4.5 - with AMOC collapse
df_lossfactor_conspc_rcp45_amocCai <- file.path("data", "META MC results", "lossfactors", "lossfactor_conspc", "lossfactor_conspc_n2000_RCP45SSP2_persist1.0_tipall_tdamagecoacch_central_omhnone_amocCai.csv") %>%
  read_csv() %>%
  select(mc_run, year, USA, CHN, PHL)

# RCP8.5 - no AMOC collapse
df_lossfactor_conspc_rcp85_amocnone <- file.path("data", "META MC results", "lossfactors", "lossfactor_conspc", "lossfactor_conspc_n2000_RCP85SSP2_persist1.0_tipall_tdamagecoacch_central_omhnone_amocnone.csv") %>%
  read_csv() %>%
  select(mc_run, year, USA, CHN, PHL)

# RCP8.5 - with AMOC collapse
df_lossfactor_conspc_rcp85_amocCai <- file.path("data", "META MC results", "lossfactors", "lossfactor_conspc", "lossfactor_conspc_n2000_RCP85SSP2_persist1.0_tipall_tdamagecoacch_central_omhnone_amocCai.csv") %>%
  read_csv() %>%
  select(mc_run, year, USA, CHN, PHL)

# combine the MC percentile runs into one data frame
df_figure5b <- bind_rows(df_lossfactor_conspc_rcp45_amocnone %>% 
                           mutate(rcp = "RCP4.5", amoc = figure5_label_tipall_amocnone),
                         
                          df_lossfactor_conspc_rcp45_amocCai %>% 
                           mutate(rcp = "RCP4.5", amoc = figure5_label_tipall_amocCai),
                         
                         df_lossfactor_conspc_rcp85_amocnone %>% 
                           mutate(rcp = "RCP8.5", amoc = figure5_label_tipall_amocnone),
                         
                         df_lossfactor_conspc_rcp85_amocCai %>% 
                           mutate(rcp = "RCP8.5", amoc = figure5_label_tipall_amocCai)
                        ) %>%
  
  # initiate dtplyr to speed up calculations
  data.table::as.data.table() %>% dtplyr::lazy_dt() %>%
  
  # select variables of interest
  select(mc_run, year, USA, CHN, PHL, rcp, amoc) %>%
  
  # discard years after 2100
  filter(year <= 2100) %>%
  
  # trim the problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # convert to LONG format (one row per country)
  pivot_longer(cols = c(USA, CHN, PHL), names_to = "iso3", values_to = "lossfactor") %>%
  
  # calculate mean and percentiles of dividend losses by country and year
  group_by(year, rcp, amoc, iso3) %>%
  summarise(mean = mean(lossfactor),
            # NOTE: we take lower percentiles here because lossfactor are reversed (e.g. 0.9 for 10% loss)
            perc95 = quantile(lossfactor, 0.05),
            perc99 = quantile(lossfactor, 0.01),
            .groups = "drop") %>%
  
  # convert dtplyr results to a tibble
  as_tibble() %>%
  
  # convert lossfactor from multiplicative factor to loss in %
  mutate_at(vars(mean, perc95, perc99), ~ 1 - .x) %>%
  
  # subset to years starting in 2025
  filter(year >= 2025) %>%
  
  # merge in country names, clean them up and set the order
  left_join(df_calibration %>% select(iso3, name), by = "iso3") %>%
  mutate(name = factor(shorten_country_names(name),
                       levels = c("USA", "China", "Philippines")))


# plot the loss factor means and percentiles for the US under RCP4.5 and RCP8.5
figure5b <- df_figure5b %>%
  
  # subset to the US
  filter(iso3 == "USA") %>%
  
  # convert to LONG format (one row per year x summary stat)
  pivot_longer(cols = c(mean, perc95, perc99), names_to = "summarystat", values_to = "lossfactor") %>%
  
  # line chart with loss factor over years - color by AMOC specification (w/ or w/o collapse), linetype by RCP scenario
  ggplot(aes(year)) + geom_line(aes(y = lossfactor, colour = amoc, linetype = rcp), linewidth = 0.3) +

  # facet by summary stats using clean labels  
  facet_wrap(~ clean_summarystat_labels(summarystat)) +
  
  # set color scale manually
  scale_colour_manual(values = c("#582A27", "#F8766D" )) +
  scale_fill_manual(values = c("#582A27", "#F8766D" )) +
  
  # set y-axis values to negative percentages
  scale_y_continuous(labels = ~ paste0("-", scales::percent(.x))) +
  
  # set legend order
  guides(linetype = guide_legend(order = 1),
         colour = guide_legend(order = 2, reverse = T)) +
  
  # set y-axis title and drop x-axis and legend titles
  labs(y = "US dividend reduction\ndue to climate damages", x = NULL, fill = NULL, colour = NULL,
       linetype = NULL) +
  
  # position legend
  theme(legend.position = "bottom")


# combine panels for Figure 5 in a 2x1 grid
figure5 <- ggarrange(figure5a, figure5b, nrow = 2, align = "hv", labels = "auto",
                     font.label = list(size = 7, color = "black", face = "bold"))

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " Figure5_", tdamage_selected_main, "_amoc", amoc_selected_main, ".pdf")),
       plot = figure5, width = 14, height = 12, unit = "cm")


################################################################################
########################### SI FIGURES #########################################
################################################################################


### a) climatic outcomes in META for different RCPs considered

### read in global sea level rise
# NOTE: no need for an amoc selector because the AMOC module does not affect SLR in META

# RCP8.5
df_globalSLR_rcp85 <- bind_rows(read_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP85SSP2_tipnone.csv")) %>%
                                  mutate(tip = "none", rcp = "RCP85"),
                                read_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP85SSP2_tipall_omhnone.csv")) %>%
                                  mutate(tip = "all", rcp = "RCP85"))

# RCP2.6
df_globalSLR_rcp26 <- bind_rows(read_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP3-PD26SSP2_tipnone.csv")) %>%
                                  mutate(tip = "none", rcp = "RCP3-PD26"),
                                read_csv(file.path("data", "META MC results", "SLR", "SLR_n2000_RCP3-PD26SSP2_tipall_omhnone.csv")) %>%
                                  mutate(tip = "all", rcp = "RCP3-PD26"))


# GMST over time for different RCPs
# NOTE: we use the tipping point specification that includes ocean methane hydrates (= main specification)
figure1a_siversion <- bind_rows(df_globaltemp_rcp26, df_globaltemp %>% filter(tip %in% c("all_omhnone", "none")), df_globaltemp_rcp85) %>%
  
  # subset to 21st century
  filter(year %in% 2010:2100) %>%
  
  # trim problematic MC runs
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # calculate MC mean and percentiles by year
  group_by(year, tip, rcp) %>% 
  summarise_at(vars(T_AT), list(mean = mean,
                                perc025 = ~ quantile(.x, 0.025),
                                perc975 = ~ quantile(.x, 0.975))) %>%
  
  # overwrite the 'all_omhnone' label in the tip column with 'all', then use clean labels 
  mutate(tip = ifelse(tip == "all_omhnone", "all", tip)) %>%
  mutate(label = case_when(tip == "all" ~ "Incl. climate tipping points",
                           tip == "none" ~ "Excl. climate tipping points")) %>%
  
  # line chart over time with color by tipping point specification (w/ or w/o)
  ggplot(aes(year)) + geom_line(aes(y = mean, colour = label)) +
  
  # shade between 2.5th and 97.5th percentiles
  geom_ribbon(aes(ymin = perc025, ymax = perc975, fill = label), alpha = 0.1) +
  
  # set color scale manually
  scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
  
  # add degC symbol to y-axis values
  scale_y_continuous(labels = ~ paste0(.x, "\u00B0C")) +
  
  # set titles, ensure that y-axis starts at 0, facet by RCP and position legend
  labs(y = "Global warming\n above pre-industrial levels", x = NULL, fill = NULL, colour = NULL) +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(~ clean_rcp_labels(rcp)) +
  theme(legend.position = "bottom")

# global SLR over time for different RCPs (same steps as above)
figure1b_siversion <- bind_rows(df_globalSLR_rcp26, df_globalSLR %>% filter(tip %in% c("all_omhnone", "none")), df_globalSLR_rcp85) %>%
  
  filter(year %in% 2010:2100) %>%
  
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  group_by(year, tip, rcp) %>% 
  summarise_at(vars(SLR), list(mean = mean, perc025 = ~ quantile(.x, 0.025),
                               perc975 = ~ quantile(.x, 0.975))) %>%
  
  mutate(tip = ifelse(tip == "all_omhnone", "all", tip)) %>%
  
  mutate(label = case_when(tip == "all" ~ "Incl. climate tipping points",
                           tip == "none" ~ "Excl. climate tipping points")) %>%
  
  ggplot(aes(year)) + geom_line(aes(y = mean, colour = label)) +
  
  geom_ribbon(aes(ymin = perc025, ymax = perc975, fill = label), alpha = 0.1) +
  
  scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
  
  scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
  
  scale_y_continuous(labels = ~ paste0(.x, "m")) +
  
  labs(y = "Global sea level rise\nsince 2000", x = NULL, fill = NULL, colour = NULL) +
  
  facet_wrap(~ clean_rcp_labels(rcp)) +
  
  theme(legend.position = "bottom")


# combine the GMST and the SLR chart into a 2x1 grid for the SI
ggarrange(figure1a_siversion, figure1b_siversion,
          nrow = 2, align = "hv", labels = "auto",
          common.legend = T, legend = "bottom",
          font.label=list(color="black",size=7))

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure_temperatureSLR_by_rcp.pdf")),
       width = 5, height = 4)

# clean up the environment
rm(df_globalSLR_rcp26, df_globalSLR_rcp85,
   df_globaltemp_rcp26, df_globaltemp_rcp85)
gc()


### b) temperature evolution for individual TPs

# NOTE: we only have three tipping points affecting GMST: ocean methane hydrates (OMH), permafrost carbon feedback (PCF), and the Amazon rainforest (AMAZ)

# apply the same function to create GMST charts to the different tipping point labels
tempchart_individual_TPs <- c("all_omhnone", "OMH", "PCF", "AMAZ") %>%
  
  map(.f = function(tip_selected) {

    # read in the GMST data for the tipping point specification in question
    read_csv(file.path("data", "META MC results", "T_AT", paste0("T_AT_n2000_RCP45SSP2_tip", tip_selected, ".csv"))) %>%
                                       mutate(tip = tip_selected, rcp = "RCP45") %>%
      
      # combine with GMST for no TPs
      bind_rows(df_globaltemp %>% filter(tip == "none")) %>%
      
      # subset to period of interest
      filter(year %in% 2011:2100) %>%
      
      # trim problematic MC runs
      filter(!mc_run %in% mc_runs_screenedout) %>%
      
      # calculate MC mean and percentiles per year and TP specification
      group_by(year, tip, rcp) %>% 
      summarise_at(vars(T_AT), list(mean = mean,
                                    perc025 = ~ quantile(.x, 0.025),
                                    perc975 = ~ quantile(.x, 0.975))) %>%
    
      # clean up labels
      mutate(label = case_when(tip != "none" ~ "Incl. individual tipping point",
                               tip == "none" ~ "Excl. climate tipping points")) %>%
      
      # save the TP specification selected into a column
      mutate(facet_label = tip_selected) %>%
      
      # plot GMST over time (same steps as above)
      ggplot(aes(year)) + geom_line(aes(y = mean, colour = label)) +
      geom_ribbon(aes(ymin = perc025, ymax = perc975, fill = label), alpha = 0.1) +
      scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
      scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
      scale_y_continuous(labels = ~ paste0(.x, "\u00B0C")) +
      labs(y = "Global warming\n above pre-industrial levels", x = NULL, fill = NULL, colour = NULL) +
      coord_cartesian(ylim = c(0, 3)) +
      facet_wrap(~ clean_individual_tip_labels(facet_label)) +
      theme(legend.position = "bottom")
    
  })

# subset to tipping points actually affecting GMST and save out
ggarrange(plotlist = tempchart_individual_TPs,
          nrow = 1, ncol = 4, align = "hv", legend = "bottom", common.legend = T,
          font.label=list(color="black",size=7))
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure_temperature_by_tip.pdf")),
       width = 6, height = 2)


# repeat the same steps for all tipping points affecting SLR directly (= ice sheets) or indirectly (= through GMST)
slrchart_individual_TPs <- c("all_omhnone", "OMH", "PCF", "GIS", "WAIS", "AMAZ") %>%
  
  map(.f = function(tip_selected) {
    
    read_csv(file.path("data", "META MC results", "SLR", paste0("SLR_n2000_RCP45SSP2_tip", tip_selected, ".csv"))) %>%
      
      mutate(tip = tip_selected, rcp = "RCP45") %>%
      
      bind_rows(df_globalSLR %>% filter(tip == "none")) %>%
      
      filter(year %in% 2011:2100) %>%
      
      filter(!mc_run %in% mc_runs_screenedout) %>%
      
      group_by(year, tip, rcp) %>% 
      
      summarise_at(vars(SLR), list(mean = mean, perc025 = ~ quantile(.x, 0.025),
                                   perc975 = ~ quantile(.x, 0.975))) %>%
      
      mutate(label = case_when(tip != "none" ~ "Incl. climate tipping points",
                               tip == "none" ~ "Excl. climate tipping points")) %>%
      
      mutate(facet_label = tip_selected) %>%
      
      ggplot(aes(year)) + geom_line(aes(y = mean, colour = label)) +
      
      geom_ribbon(aes(ymin = perc025, ymax = perc975, fill = label), alpha = 0.1) +
      
      scale_colour_manual(values = c("#00BFC4", "#F8766D" )) +
      
      scale_fill_manual(values = c("#00BFC4", "#F8766D" )) +
      
      scale_y_continuous(labels = ~ paste0(.x, "m")) +
      
      labs(y = "Global sea level rise\nsince 2000", x = NULL, fill = NULL, colour = NULL) +
      
      facet_wrap(~ clean_individual_tip_labels(facet_label)) +
      
      coord_cartesian(ylim = c(NA, 0.5)) +
      
      theme(legend.position = "bottom")
    
  })

# subset to tipping points actually affecting SLR and save out
ggarrange(plotlist = slrchart_individual_TPs,
          nrow = 2, ncol = 3, align = "hv", legend = "bottom", common.legend = T,
          font.label=list(color="black",size=7))
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure_sealevelrise_by_tip.pdf")),
       width = 5, height = 3)


### c) country weights in different indexes

# map the country weights
figure_si_countryweights <- df_weights_long %>%
  
  # subset to the stock indices of interest
  filter(portfolio %in% c("msciacwi", "msciworld", "msciem", "mscifm")) %>%
  
  # merge in the shapefile and convert to an sf object (= required for creating maps)
  left_join(world %>% select(iso3, geometry), by = "iso3") %>% st_as_sf() %>%
  
  # drop all entires with a zero weight
  filter(weight > 0) %>%
  
  # group the weights into categories using the cut() function
  mutate(share_grouped = cut(weight, breaks = c(0, 0.002, 0.005, 0.01, 0.03, 0.05, 0.1, 0.2, 0.3, max(weight)),
                             include.lowest = TRUE, 
                             labels = c("<0.2%", "0.2-0.5%", "0.5-1%", "1-3%", "3-5%", "5-10%", "10-20%", "20-30%", ">30%"))) %>%
  
  # create a map with no filling for all countries except Antarctica (excluded for visual purposes)
  ggplot() + geom_sf(data = world %>% filter(admin != "Antarctica")) +
  
  # overlay this with country weight (= only for countries with a weight > 0)
  geom_sf(aes(fill = share_grouped)) +
  
  # facet by stock index using clean labels
  facet_wrap(~ clean_portfolio_labels(portfolio), nrow = 2, ncol = 2) +
  
  # set color scale, theme, and legend title
  scale_fill_brewer() +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "Country share in index market valuation")


# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure_indexweights_pv.pdf")),
       plot = figure_si_countryweights, width = 6, height = 4.5)



### c) country risk premiums

df_portfolioweights %>%
  
  # subset to countries that have any weight in the portfolios considered here
  filter(has_any_weight) %>%
  
  # subset to columns of interest and rename them
  select(Name = "name", crp_damodaran) %>%
  
  # drop countries with no CRP
  filter(crp_damodaran != 0) %>%
  
  # order countries by CRP
  arrange(desc(crp_damodaran)) %>%
  
  # round CRP and convert to percentage, clean up country names
  mutate(crp_damodaran = paste0(100*round(crp_damodaran, 3), "%"),
         Name = clean_country_names(Name)) %>%
  
  # clean up the column names
  setNames(names(.) %>% str_replace("^crp_damodaran$", "Country Risk Premium")) %>%
  
  # export as .tex table
  xtable(label = "tab:si_crp_damodaran",
         caption = "Country risk premium values used for the investor discount rate taken from the Damodaran database. Countries from Table S3 not listed here have a country risk premium of zero") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " crp_damodaran_inputvalues.tex")),
        size="\\fontsize{9pt}{10pt}\\selectfont")
  


### d) SSP5 table

# read in the PV loss distributions based on SSP5
df_npv_ssp5 <- bind_rows(# RCP4.5 - no TPs
                         file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP45SSP5_persist",
                                     persist_selected_main, "_tipnone_tdamage",
                                     tdamage_selected_main, "_base2024_end2100.csv")) %>%
                         read_csv(),
                    
                         # RCP4.5 - all TPs
                         file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP45SSP5_persist",
                                     persist_selected_main, "_tipall_tdamage", 
                                     tdamage_selected_main, "_omhnone_amoc", amoc_selected_main, "_base2024_end2100.csv")) %>%
                         read_csv(),
                    
                         
                         # RCP8.5 - no TPs
                         file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP85SSP5_persist",
                                     persist_selected_main, "_tipnone_tdamage", 
                                     tdamage_selected_main, "_base2024_end2100.csv")) %>%
                         read_csv,
                    
                         
                         # RCP8.5 - all TPs
                         file.path("data", "portfolio_simulation", "df_npv_fullportfolio",
                              paste0("df_npv_fullportfolio_n2000_RCP85SSP5_persist",
                                     persist_selected_main, "_tipall_tdamage", 
                                     tdamage_selected_main, "_omhnone_amoc", amoc_selected_main, "_base2024_end2100.csv")) %>%
                         read_csv())

# calculate summary stats
df_npv_summarystat_ssp5 <- df_npv_ssp5 %>% 
  
  # exclude the problematic run
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # summarise, subset to discount rate of interest and ensure 'persist' column is a character
  summarise_npv_distribution() %>%
  filter(discountrate == "damodaran") %>%
  mutate(persist = as.character(persist))



# write them out as a .text table
df_npv_summarystat_ssp5 %>% filter(portfolio %in% c("msciworld", "msciem", "mscifm"),
                                   rcp %in% c("RCP45", "RCP85")) %>%
  
  # rename the columns
  rename(TippingPoints = "tip",
         RCP = "rcp",
         SSP="ssp",
         Portfolio="portfolio",
         Mean = "mean",
         Median = "median",
         "1st perc." = "perc01",
         "5th perc." = "perc05",
         "95th perc." = "perc95",
         "99th perc." = "perc99") %>%
  
  # select to columns of interest
  select(Portfolio, RCP, SSP, TippingPoints, Mean, Median, contains("perc.")) %>%
  
  # clean up the stock index labels
  mutate(Portfolio = clean_portfolio_labels_short(Portfolio)) %>%
  
  # round and convert numeric columns to percentages
  mutate_if(is.numeric, ~ paste0(100*round(.x, 4), "%")) %>%
  
  # sort by index, RCP, w/ or w/o TPs
  arrange(Portfolio, RCP, TippingPoints) %>%
  
  # create .tex table
  xtable(digits = 4,
         label = "tab:si_results_ssp5",
         caption = "Summary statistics of the loss distribution for the key MSCI indices using SSP5 and the COACCH damage specification") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " SummaryStatsSI_SSP5.tex")),
        size="\\fontsize{9pt}{10pt}\\selectfont")

# clean up
rm(df_npv_ssp5, df_npv_summarystat_ssp5)
gc()


### e) GDP allocation vs market cap

# load World Bank WDI data on real GDP by country
# NOTE: we stop at the 217th entry (= Zimbabwe) because the subsequent rows in the raw data file are country groups as aggregates
df_gdp <- read_csv("data/230503 World Bank GDP_2015USD.csv", n_max = 217) %>%
  
  # clean column names
  clean_names() %>%
  
  # convert GDP values (in columns starting with the respective year) to numeric
  mutate_at(vars(starts_with("x20")), as.numeric) %>%
  
  # imputate 2021 with 2020 if necessary
  mutate(x2021_yr2021 = if_else(is.na(x2021_yr2021), x2020_yr2020, x2021_yr2021)) %>%
  
  # subset to columns of interest and rename them
  select(iso3 = "country_code",
         gdp = "x2021_yr2021")
  
# check NAs in gdp
df_gdp %>% filter(is.na(gdp))

# merge together with portfolio weights in the MSCI All Country World Index and print as LaTeX table
df_portfolioweights %>%
  
  # subset to countries with non-zero weight
  filter(weight_msciacwi > 0) %>%
  
  # select columns of interest
  select(iso2, iso3, name, weight_msciacwi) %>%
  
  # merge in GDP
  left_join(df_gdp, by = c("iso3" = "iso3")) %>%
  
  # filter out countries with no GDP data
  filter(!is.na(gdp)) %>%
  
  # convert from USD to trillion USD
  mutate(gdp = gdp * 1e-12) %>%
  
  # calculate share of countries in the group of ACWI members
  mutate(gdp_share = gdp/sum(gdp, na.rm = T)) %>%
  
  # calculate difference between index weight and GDP weight
  mutate(diff_shares = weight_msciacwi - gdp_share) %>%
  
  # order by diff_shares
  arrange(desc(diff_shares)) %>%
  
  # clean up column names for final table
  rename("Country" = "name",
         "MSCI ACWI share" = "weight_msciacwi",
         "GDP 2021 (in tn 2015 USD)" = "gdp",
         "GDP share" = "gdp_share",
         "Difference" = "diff_shares") %>%
  
  # select columns of interest
  select("Country", "MSCI ACWI share", "GDP 2021 (in tn 2015 USD)", "GDP share", "Difference") %>%
  
  # show shares as %
  mutate_at(vars(contains("share"), Difference), ~ paste0(round(.x, 3) * 100, "%")) %>%
  
  # round gdp
  mutate_at(vars("GDP 2021 (in tn 2015 USD)"), ~ round(.x, 2)) %>%
  
  # clean up country names
  mutate(Country = clean_country_names(Country)) %>%
  
  # create .tex table
  xtable(label = "tab:si_gdpallocation",
         caption = "Comparison of countries' weight in the MSCI All Country World Index (ACWI) and their share in the joint GDP of all MSCI ACWI countries. GDP data are taken from the Word Bank's World Development Indicators database, with the missing 2021 GDP value for Kuwait imputed with the country's 2020 value. Taiwan is omitted as it does not feature in the World Bank's GDP data. Singapore is omitted as it is not included in the META model and hence in our analysis") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " GDPAllocation.tex")),
        size="\\fontsize{9pt}{10pt}\\selectfont")


# create a table with all values used in Fig. 4a and 4b (country-level expected loss and VaR)
df_map %>%
  
  # merge in country names
  left_join(df_portfolioweights %>% select(iso3, name), by = "iso3") %>%
  
  # order by expected loss incl. TPs (column 'all') in descending order
  arrange(desc(all)) %>%
  
  # clean up country names
  mutate(name = clean_country_names(name)) %>%
  
  # convert numeric results to % and round
  mutate_at(vars(all, perc95_all, none), ~ paste0(100*round(.x, 3), "%")) %>%
  mutate_at(vars(tp_adder_percpoints), ~ paste0(100*round(.x, 3))) %>%
  mutate_at(vars(tp_adder_relative), ~ paste0(ifelse(.x > 0, "+", ""), 100*round(.x, 3), "%")) %>%
  
  # select and rename columns of interest
  select("Country" = "name",
         "Expected loss\n(incl. TPs)" = "all",
         "5% VAR\n(incl. TPs)" = "perc95_all",
         "Expected loss\n(excl. TPs)" = "none",
         "Increase of expected\nloss due to TPs (pp)" = "tp_adder_percpoints",
         "Relative\nincrease" = "tp_adder_relative") %>%
  
  # create .tex table
  xtable(label = "tab:si_countrylevelresults_rcp45",
         caption = "Country-level results under SSP2-4.5 using the COACCH damage specification (used in Fig. 4) for all 68 countries that feature in any of the three stock indices considered in our analysis") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " countrylevel_ssp245.tex")),
        size="\\fontsize{7pt}{8pt}\\selectfont")


### f) use all tipping points as per Dietz et al 2021 main specification

# load the loss distribution for the full portfolio
df_npv_dietz <- bind_rows(# RCP4.5 - w/o TPs
                          file.path("data", "portfolio_simulation", "df_npv_fullportfolio", "alternative_tipping_points",
                                   paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist",
                                          persist_selected_main, "_tipnone_tdamage",
                                          tdamage_selected_main, "_omhnone_amocnone_base2024_end2100.csv")) %>%
                           read_csv(),
                          # RCP4.5 - w/ TPs
                          file.path("data", "portfolio_simulation", "df_npv_fullportfolio", "alternative_tipping_points",
                                   paste0("df_npv_fullportfolio_n2000_RCP45SSP2_persist",
                                          persist_selected_main, "_tipall_tdamage", 
                                          tdamage_selected_main, "_omhdefault_amocIPSL_base2024_end2100.csv")) %>%
                           read_csv())


# calculate summary statistics
df_npv_summarystat_dietz <- df_npv_dietz %>% 
  
  # exclude the problematic run
  filter(!mc_run %in% mc_runs_screenedout) %>%
  
  # calculate summary statistics and subset to discount rate incl. CRPs
  summarise_npv_distribution() %>%
  filter(discountrate == "damodaran")


# plot expected loss and 5% VAR for the alternative specifications under SSP2-4.5 for all three main portfolios
figure_si_dietztippingpoints <- df_npv_summarystat_dietz %>%
  
  # subset to stock indices of interest
  filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  
  # for each specification, calculate the adder in the expected loss due to TPs
  group_by(rcp, ssp, portfolio, persist, tdamage, base_year, end_year, discountrate, iso3, mc_samplesize) %>%
  mutate(tip_adder_mean = mean[tip == "all"] - mean[tip == "none"],
         tip_adder_mean_relative = tip_adder_mean/mean[tip == "none"]) %>%
  
  # create components of the expected loss bar plot (= expected loss if no TPs, expected loss adder if TPs included)
  mutate(mean_component_bars = if_else(tip == "all", tip_adder_mean, mean)) %>%
  
  # create a stacked bar chart with the expected loss components, using clean labels
  ggplot(aes(clean_portfolio_labels_short(portfolio))) +
  geom_col(aes(y = mean_component_bars, fill = clean_tip_category_labels(tip)),
           width = 0.5) +
  
  # add the increase in the expected loss due to TPs as text
  geom_text(aes(y = mean, label = ifelse(tip == "all", paste0("+", 100*round(tip_adder_mean_relative, 3), "%"), NA_character_)),
            nudge_y = 0.0025, color = "darkred", size = 5/.pt) +
  
  # percentile triangles for the 95% VaR
  geom_point(aes(y = perc95, color = clean_tip_category_labels(tip), shape = clean_tip_category_labels(tip)), size = 2, alpha = 0.5) +
  
  # set legend and axes titles
  labs(fill = "Expected loss", color = "95% VaR (95th percentile)", shape = "95% VaR (95th percentile)",
       x = NULL, y = "Loss in % of present value under SSP2-4.5") +
  
  # select shape of the 95% VaR points
  scale_shape_manual(values = c(17, 17)) +
  
  # set y-axis values to percentages
  scale_y_continuous(labels = scales::percent) +
  
  # position legend
  theme(legend.direction = "vertical")


# export as .pdf chart
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_Figure_tipping_points_dietz2021.pdf")),
       plot = figure_si_dietztippingpoints, width = 3, height = 3)

# inspect some summary stats cited in the Supplementary Note
df_npv_summarystat_dietz


### g) print index weights into table

# clean up
df_weights_sitable <- df_weights_long %>%
  
  # subset to stock indices of interest
  filter(portfolio %in% c("msciworld", "msciem", "mscifm")) %>%
  
  # merge in country names
  left_join(df_portfolioweights %>% select(iso3, name), by = "iso3") %>%
  
  # sort by stock index and weight
  arrange(portfolio, desc(weight)) %>%
  
  # convert weight to rounded percentages
  mutate(weight = paste0(100*round(weight, 3), "%")) %>%
  
  # clean up country names
  mutate(name = clean_country_names(name)) %>%
  
  # select and rename columns of interest
  select(Country = "name", "Weight in index" = "weight", Index = "portfolio")


# print as table for the MSCI World
df_weights_sitable %>%
  
  # subset to MSCI World and drop the 'Index' column
  filter(Index == "msciworld") %>% select(-Index) %>%
  
  # create .tex table
  xtable(label = "tab:si_indexweights_msciworld",
         caption = "Country weights in the MSCI World used in our analysis (rounded to first digits). Singapore (weight: 0.4\\%) is omitted from our analysis as it is not included in the META model") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " indexweights_msciworld.tex")),
        size="\\fontsize{7pt}{8pt}\\selectfont")

# repeat for the MSCI EM
df_weights_sitable %>%
  
  filter(Index == "msciem") %>%select(-Index) %>%
  
  xtable(label = "tab:si_indexweights_msciem",
         caption = "Country weights in the MSCI Emerging Markets used in our analysis (rounded to first digits)") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " indexweights_msciem.tex")),
        size="\\fontsize{7pt}{8pt}\\selectfont")

# repeat for the MSCI Frontier Emerging Markets
df_weights_sitable %>%
  
  filter(Index == "mscifm") %>% select(-Index) %>%
  
  xtable(label = "tab:si_indexweights_mscifem",
         caption = "Country weights in the MSCI Frontier Emerging Markets used in our analysis (rounded to first digits)") %>%
  print(file = file.path("tables", paste0(Sys.Date(), " indexweights_mscifem.tex")),
        size="\\fontsize{7pt}{8pt}\\selectfont")




################################################################################
########## SUMMARY STATS CITED IN THE MAIN #####################################
################################################################################

# NOTE: this code section only covers numbers that are *not* covered by a main figure already

# write a helper function to get clean summary results by stock index
inspect_results_by_index <- function(data = df_npv_summarystat, # data with PV loss distribution summary stats
                                     data_index_size = df_calibration_yield, # data containing the index market cap
                                     scenario = "RCP45", # RCP of interest
                                     index = "msciworld", # stock index of interest
                                     discountrate_used = "damodaran" # discount rate regime of interest (default = including country risk premiums)
                                     ) {
  
  data %>%
    
    # subset to index, RCP and discount rate of interest
    filter(portfolio == index, rcp == scenario,
           discountrate == discountrate_used) %>%
    
    # extract the index value and convert from USD to USD trillion
    mutate(index_value = data_index_size$index_size[data_index_size$portfolio == index]*10^(-12)) %>%
  
    # calculate absolute values for expected loss and 95% VaR
    mutate(mean_absolute = mean * index_value,
           perc95_absolute = perc95 * index_value) %>%
    
    # reorder columns
    select(tip, mean, mean_absolute, perc95, perc95_absolute, everything())

}



### Implications for diversified stock indices

# country-level results for example markets
df_figure2a %>% filter(portfolio == "msciworld") %>% arrange(desc(all_mean))
df_figure2a %>% filter(iso3 %in% c("USA", "CHN", "PHL")) %>% mutate_if(is.numeric, ~ round(.x, 3))

# RCP 2.6 - MSCI World
inspect_results_by_index(scenario = "RCP3-PD26")

# RCP4.5 - MSCI World
inspect_results_by_index(scenario = "RCP45") %>% mutate_at(vars(ends_with("absolute")), ~round(.x, 1))

# RCP 8.5 - MSCI World
inspect_results_by_index(scenario = "RCP85")

# RCP 4.5 - emerging markets
inspect_results_by_index(scenario = "RCP45", index = "msciem")

# RCP 4.5 - frontier markets
inspect_results_by_index(scenario = "RCP45", index = "mscifm")


### damage function & persistence

# increase of expected loss and 95% VaR due to increasing tipping points
df_figure4a_summarystat %>%
  
  # subset to MSCI World and EM
  filter(portfolio %in% c("msciworld", "msciem")) %>%
  
  # calculate relative increase of expected loss and 95% VaR by damage function & persistence
  group_by(rcp, ssp, portfolio, persist, amoc, mc_samplesize, tdamage) %>%
  summarise(increase_mean = mean[str_detect(tip, "^Incl")]/mean[str_detect(tip, "^Excl")],
            increase_perc95 = perc95[str_detect(tip, "^Incl")]/perc95[str_detect(tip, "^Excl")])


### risk increases due to tipping points

# expected loss and 95% VaR for India
df_figure2a %>% filter(iso3 == "IND")

# other countries with high 95% VaR
df_map %>% arrange(desc(perc95_all))

# relative increase of expected loss due to TPs by country
df_figure2a %>% ungroup() %>%
  
  # calculate relative increase of expected loss due to tipping points
  mutate(increase_relative = round(incl_tipping_adder_mean/none_mean, 2)) %>%
  
  # sort countries by relative increase
  arrange(desc(increase_relative)) %>%
  
  # reorder columns and view
  select(iso3, increase_relative, everything()) %>% view()


# tippint point-related VaR increases
df_map %>%
  
  # subset to countries mentioned
  filter(iso3 %in% c("IND", "IDN", "BRA", "SAU", "CHN", "USA", "FRA", "CHE", "CIV", "SEN", "NLD", "JPN", "AUS")) %>%
  
  # round numbers and sort countries by them
  mutate_at(vars(all, perc95_all), ~ round(.x, 3)) %>%
  mutate_at(vars(tp_adder_relative), ~ round(.x, 2)) %>%
  arrange(desc(tp_adder_relative))


### AMOC collapse

# expected loss and VaRs of the MSCI World
df_figure5_sumstat %>%
  
  filter(portfolio == "msciworld") %>%
  
  select(tip, amoc, mean, perc95, perc99, max, everything()) %>%
  
  mutate_if(is.numeric, ~ round(.x, 3))  


# increase of expected loss due to AMOC collapse inclusion for MSCI World
df_figure5_sumstat %>%
  
  filter(portfolio == "msciworld") %>%
  
  summarise(increase_mean = mean[amoc == "Cai"]/mean[amoc == "none" & tip == "all"])

