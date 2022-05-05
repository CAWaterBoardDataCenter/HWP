#
# Combine EROM extension table data from VPUs 15, 16, 17, 18
# needed to calculate & map dam storage ratio for bordering CA COMIDs
#

# load libraries ----
library(here)
library(tidyverse)
library(foreign)

# load raw data ----
Streamflow15 <- read.dbf(here("UpdatedData", "NHDPlusCO", "NHDPlus15", "EROMExtension", "EROM_MA0001.DBF"))
Streamflow16 <- read.dbf(here("UpdatedData", "NHDPlusGB", "NHDPlus16", "EROMExtension", "EROM_MA0001.DBF"))
Streamflow17 <- read.dbf(here("UpdatedData", "NHDPlusPN", "NHDPlus17", "EROMExtension", "EROM_MA0001.DBF"))
Streamflow18 <- read.dbf(here("UpdatedData", "NHDPlusCA", "NHDPlus18", "EROMExtension", "EROM_MA0001.DBF")) 
NLCDcatch <- read_csv("./UpdatedData/NLCD2016_CA.csv") ## Streamcat NLCD data
COMIDs <- as.vector(NLCDcatch$COMID)

# combine data & filter ----
StreamflowCA <- bind_rows(Streamflow15, Streamflow16, Streamflow17, Streamflow18) %>%
  dplyr::filter(ComID %in% COMIDs)
## note that obs in StreamflowCA = 140494 and total obs in COMIDs = 140710; some COMIDs may not have streamflow data. 

# save ----
here()
write.csv(StreamflowCA, here("UpdatedData", "CA_EROM_compiled.csv"))
