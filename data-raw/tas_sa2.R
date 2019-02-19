
###########################################################################
# Download zip of 2011 sa2 shape file

# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202011?OpenDocument

# zip file

# unzip to a folder:
file_path <- ""

# Load sugaRbag library
library(sugaRbag)

# read in the shape file
sa2_2016 <- read_shape(shp_path = file_path, simplify = 0.1)

# to subset for Tasmania
tas_sa2 <- dplyr::filter(sa2_2016, sa2_2016$STE_NAME16 == "Tasmania")
