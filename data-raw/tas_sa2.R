
###########################################################################
# Download zip of 2011 sa2 shape file

# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202011?OpenDocument

# zip file
# www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa2_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&7130A5514535C5FCCA257801000D3FBD&0&July%202011&23.12.2010&Latest

# unzip to a folder:
file_path <- "data/lga_2011.rda"

# Load sugaRbag library
library(sugaRbag)

# read in the shape file
sa2_2011 <- read_shape(shp_path = file_path, simplify = 0.1)

# to subset for Tasmania
tas_sa2 <- filter(sa2_2011, sa2_2011$STE_NAME11 == "Tasmania")
