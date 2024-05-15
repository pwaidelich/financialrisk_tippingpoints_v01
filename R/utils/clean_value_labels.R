clean_portfolio_labels <- function(x) {
  
  out <- x %>% str_replace("^msciacwi$", "MSCI All Country World Index") %>%
    
    str_replace("^msciacwiiminous$", "MSCI ACWI excl. US") %>%
    
    str_replace("^msciem$", "MSCI Emerging Markets") %>%
    
    str_replace("^mscifm$", "MSCI Frontier Emerging Markets") %>%
    
    str_replace("^msciworld$", "MSCI World") %>%
    
    str_replace("^wbmarketcap$", "WB market cap shares")
  
  out <- factor(out, levels = c("MSCI All Country World Index", "MSCI World", "MSCI Emerging Markets", "MSCI Frontier Emerging Markets", "MSCI ACWI excl. US"))

}


clean_portfolio_labels_short <- function(x) {
  
  out <- x %>% str_replace("^msciacwi$", "MSCI ACWI") %>%
    
    str_replace("^msciem$", "MSCI EM") %>%
    
    str_replace("^mscifm$", "MSCI FEM") %>%
    
    str_replace("^msciworld$", "MSCI World") %>%
    
    str_replace("^wbmarketcap$", "WB market cap")
  
  out <- factor(out, levels = c("MSCI ACWI", "MSCI World", "MSCI EM", "MSCI FEM", "WB market cap"))

}


shorten_country_names <- function(x) {
  
  x %>% str_replace("China, Taiwan Province of", "Taiwan") %>%
    
    str_replace("United States of America", "USA") %>%
    
    str_replace("United Kingdom", "UK") %>%
    
    str_replace("Republic of Korea", "Korea") %>%
    
    str_replace("United Arab Emirates", "UAE")

}


clean_individual_tip_labels <- function(x) {
  x %>% str_replace("^ISM$", "Indian summer\nmonsoon") %>%
    
    str_replace("^AMOC$", "AMOC\nslowdown") %>%
    
    str_replace("^SAF$", "Artic sea\nice loss") %>%
    
    str_replace("^PCF$", "Permafrost\ncarbon feedback") %>%
    
    str_replace("^OMH$", "Ocean methane\nhydrate dissociation") %>%
    
    str_replace("^AMAZ$", "Amazon\ndieback") %>%
    
    str_replace("^GIS$", "Greenland ice\nsheet disintegration") %>%
    
    str_replace("^WAIS$", "West-Antarctic ice\n sheet disintegration") %>%
    
    str_replace("^none$", "No tipping\npoints") %>%
    
    str_replace("^interact$", "Interaction of\ntipping points") %>%
    
    str_replace("^all$", "All tipping\npoints") %>%
    
    str_replace("^all_omhnone$", "All tipping points\n (excl. OMH)")

}


clean_tip_category_labels <- function(x) {
  
  factor(x %>% str_replace("^all$", "Incl. climate tipping points") %>%
    
           str_replace("^none$", "Excl. climate tipping points"),
    
         levels = c("Incl. climate tipping points", "Excl. climate tipping points"))
}


clean_summarystat_labels <- function(x) {
  
  factor(x %>% str_replace("^mean$", "Expected loss") %>%
    
           str_replace("^perc95$", "95% VaR (95th percentile)") %>%
    
           str_replace("^perc99$", "99% VaR (99th percentile)"),
    levels = c("Expected loss", "95% VaR (95th percentile)", "99% VaR (99th percentile)"))

}


clean_country_names <- function(x) {
  
  x %>% str_replace("^T.+rkiye$", "Turkey") %>%
    
    str_replace("^C.+te d'Ivoire$", "Cote d'Ivoire")
}


clean_rcp_labels <- function(x) {
  
  factor(x %>% str_replace("^RCP45$", "RCP4.5") %>%
    
           str_replace("^RCP85$", "RCP8.5") %>%
    
           str_replace("^RCP3-PD26$", "RCP3-PD/2.6"),
    levels = c("RCP3-PD/2.6", "RCP4.5", "RCP8.5"))

}
