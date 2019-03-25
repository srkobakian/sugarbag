# download homelessness data
# http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&attk1xaa.xls&2049.0&Data%20Cubes&72F94B5D83ACB5D8CA25824F0017631D&0&2016&14.03.2018&Latest

# read in xls file
library(readxl)
homeless <- read_excel("data-raw/attk1xaa.xls", 
  sheet = "Table_6.1", skip = 4) %>% 
  filter(!is.na(LGA)) %>% 
  dplyr::select(homeless = `All homeless persons`,
    LGA_NAME16 = LGA)

# usethis::use_data(homeless)
