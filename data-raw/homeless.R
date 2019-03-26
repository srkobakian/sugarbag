# download homelessness data

#Table 5.3 ALL HOMELESS PERSONS, by place of enumeration, Statistical Area Level 2, 2016

# https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&20490do005_2016.xls&2049.0&Data%20Cubes&D33F662E34986740CA25824F001762EA&0&2016&14.03.2018&Latest

# read in xls file
library(readxl)
homeless <- read_excel("data-raw/20490do005_2016.xls", 
  sheet = "Table_5.3", skip = 5) %>% 
  filter(!is.na(SA2)) %>% 
  dplyr::select(homeless = `no.`,
    SA2_NAME16 = SA2)

# usethis::use_data(homeless, overwrite = TRUE)
