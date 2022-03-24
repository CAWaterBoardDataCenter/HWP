#################################
#################################
## Playing with nhdplustools
## Started: 07 Oct 2021
## Edited: 08 Oct 2021
#################################
#################################

library(nhdplusTools)
library(here)
library(tidyverse)
library(sf)
library(mapview)
library(tigris)
library(tmaptools)

here()

nhdplus_path(file.path(here("UpdatedData", "NHDPlusV21_natseamless"), "natseamless.gdb"))
nhdplus_path()

## run the below once per local machine just to subset the CA catchments. 
# staged_data <- stage_national_data(output_path = here("UpdatedData"))
# catch_all <- readRDS(staged_data$catchment)
# catch_all_sf <- catch_all %>%
#   as_tibble() %>%
#   st_as_sf()
# 
# rgdal::ogrListLayers(here("UpdatedData", "NHDPlusV21_natseamless" ,"natseamless.gdb"))
# 
# ## reduce
# NLCDcatch <- read_csv("./UpdatedData/NLCD2016_CA.csv") %>% ## Streamcat NLCD data
#   select(COMID, CatAreaSqKm, PctOw2016Cat, PctIce2016Cat, PctBl2016Cat, PctDecid2016Cat, PctConif2016Cat, PctMxFst2016Cat, PctShrb2016Cat, PctGrs2016Cat, PctWdWet2016Cat, PctHbWet2016Cat, PctUrbOp2016Cat, PctUrbLo2016Cat, PctUrbMd2016Cat, PctUrbHi2016Cat, PctHay2016Cat, PctCrop2016Cat)
# COMIDs <- as.vector(NLCDcatch$COMID)
# 
# catch_ca <- subset(catch_all, FEATUREID %in% COMIDs)
# write_rds(catch_ca, file = here("UpdatedData", "nhdplus_catchment_ca.rds"))

## load CA catchments, subset from the national seamless database
catch_ca <- readRDS(here("UpdatedData", "nhdplus_catchment_ca.rds")) %>%
  arrange(FEATUREID)
st_crs(catch_ca)
st_crs(catch_ca)$epsg # should be 4269

plot(catch_ca$Shape) #takes a long time
plot(catch_ca$Shape[1:10]) #less time, but catchments aren't next to each other
catchsub <- catch_ca[1:10,]
class(catch_ca)
mapview(catchsub)
# catch_all <- rgdal::readOGR(here("UpdatedData", "NHDPlusV21_natseamless" ,"natseamless.gdb"),"Catchment")


## grab CA state polygon from tigris package & crop catch_ca to state boundaries
## both tigris & natseamless use NAD83 as datum with geographic coordinates
states_detail <- states(cb = FALSE) # get states
CA_polygon <- states_detail[states_detail$NAME == "California",] 
mapview(CA_polygon) # includes coastal waters

# testCrop <- st_crop(catch_ca, CA_polygon) ## bad vertices, indicates bad shapefile?
# testCrop <- crop_shape(catch_all_sf, CA_polygon)
