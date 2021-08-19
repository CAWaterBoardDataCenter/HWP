#################################
#################################
## Reading in Watershed Condition files for HWP re-assessment
## Started: 17 Aug 2021
## Edited: 19 Aug 2021
#################################
#################################


## libraries
library(readr)
library(tidyverse)
library(nhdplusTools) ## requires download first using devtools::install_github("usgs-r/nhdplusTools")


## read in files
NLCDcatch <- read_csv("./UpdatedData/NLCD2016_CA.csv") %>% ## Streamcat NLCD data
  select(COMID, CatAreaSqKm, PctOw2016Cat, PctIce2016Cat, PctBl2016Cat, PctDecid2016Cat, PctConif2016Cat, PctMxFst2016Cat, PctShrb2016Cat, PctGrs2016Cat, PctWdWet2016Cat, PctHbWet2016Cat, PctUrbOp2016Cat, PctUrbLo2016Cat, PctUrbMd2016Cat, PctUrbHi2016Cat, PctHay2016Cat, PctCrop2016Cat)
COMIDs <- as.vector(NLCDcatch$COMID)
Kffact <- read_csv("./UpdatedData/Kffact_CA.csv") %>% ## Streamcat Kffact data
  select(COMID, CatAreaSqKm, KffactCat)
CatRdx <- read_csv("./UpdatedData/RoadStreamCrossings_CA.csv") %>% ## Streamcat Road Crossings data
  dplyr::select(COMID, RdCrsSlpWtdCat, RdCrsCat) %>%
  mutate(CatSlopePct = round((RdCrsSlpWtdCat/RdCrsCat)*100, 3)) ## calculate percent slope

### test whether values are similar between Streamcat and NLCD datasets (some are not)
# NLCD2016test <- read_delim("./UpdatedData/NLCD16_CAT_CONUS.txt", delim = ",") %>%
#   select(COMID, CAT_NLCD16_11, CAT_NLCD16_12, CAT_NLCD16_31, CAT_NLCD16_41, CAT_NLCD16_42, CAT_NLCD16_43, CAT_NLCD16_52, CAT_NLCD16_71, CAT_NLCD16_90, CAT_NLCD16_95) %>%
# left_join(NLCDcatch, ., by = "COMID") %>%
#   select(COMID, PctOw2016Cat, CAT_NLCD16_11) %>%
#   dplyr::mutate(PctOw2016Cat = round(PctOw2016Cat, 2))
# all(NLCD2016test$PctOw2016Cat == NLCD2016test$CAT_NLCD16_11) ## should be true
# which(NLCD2016test$PctOw2016Cat != NLCD2016test$CAT_NLCD16_11) ## returns row numbers where values are not equal
# notsame <- NLCD2016test[which(NLCD2016test$PctOw2016Cat != NLCD2016test$CAT_NLCD16_11),] ## subsets rows where values are not equal

## LS: mean catchment slope. Calculated as CatSlopePct in CatRdx, but could check against NHDPlusV2 data
## load RoadStreamCrossings_CA.csv; select RdCrsSlpWtdCat & divide by RdCrsCat; Load NHDPlusV2, BASIN_CHAR_CAT_CONUS.TXT; select CAT_BASIN_SLOPE; join & compare as above.
# NLCD2016TestSlope <- read_delim("./UpdatedData/BASIN_CHAR_CAT_CONUS.txt", delim = ",") %>%
#   select(COMID, CAT_BASIN_SLOPE) %>%
#   mutate(CAT_BASIN_SLOPE_PCT = CAT_BASIN_SLOPE)
# left_join(CatRdx, ., by = "COMID") %>%
#   select(COMID, CatSlopePct, CAT_BASIN_SLOPE) ## are not similar...at all. Why are some CAT_BASIN_SLOPEs >100 if a percentage?

colnames(NLCDcatch)
str(NLCDcatch)


### Percent natural land cover
NLCD.df <- NLCDcatch %>%
  dplyr::select(COMID:PctHbWet2016Cat) %>%
  rowwise(.) %>%
  mutate(PctNatCover = round(sum(c_across(starts_with("Pct")), na.rm=TRUE),2)) %>%
  dplyr::select(COMID, PctNatCover) %>%
  data.frame(.)


### ARA -- need ArcGIS to complete


### Sedimentation Risk

## looking for R factor: looks like was pre-calculated in the obs variables from 2013.
data2013 <- read_csv("./CA_HWI_FinalData_101813/Deliverables/CAdata_obs.csv")
colnames(data2013) ## has Kfact and sedrisk, but no Rfact?
# would like to bring in Rfactor from NHDPlusV2 data but given discrepancies I've found between the NHDPlusV2 and StreamCat datasets I'm wary.

## K: KffactCat in Streamcat
sedrisk.K <- Kffact %>%
  dplyr::select(COMID, KffactCat)

## C: CatAreaSqKm in Streamcat; Multiple colnames in Streamcat from NLCD2016_CA; re-create land cover factor values
sedrisk.C <- NLCDcatch %>%
  mutate(across(PctBl2016Cat:PctCrop2016Cat, ~ (.x / 100)*CatAreaSqKm)) %>% ## divide percent by 100 to get proportions, then total area
  mutate(C_OpenSpace = PctUrbOp2016Cat*0.003,
         C_DevLoInt = PctUrbLo2016Cat*0.001,
         C_DevMdInt = PctUrbMd2016Cat*0.001,
         C_DevHiInt = PctUrbHi2016Cat*0.001,
         C_BarrenLand = PctBl2016Cat*0.001,
         C_DecidForest = PctDecid2016Cat*0.003,
         C_EverForest = PctConif2016Cat*0.003,
         C_MixedForest = PctMxFst2016Cat*0.003,
         C_Shrub = PctShrb2016Cat*0.02,
         C_GrassHerbs = PctGrs2016Cat*0.02,
         C_PastHay = PctHay2016Cat*0.02,
         C_CultCrops = PctCrop2016Cat*0.2,
         C_WoodyWet = PctWdWet2016Cat*0.013,
         C_HerbWet = PctHbWet2016Cat*0.003) %>%
  dplyr::select(COMID, CatAreaSqKm, C_OpenSpace:C_HerbWet) %>%
  rowwise(.) %>%
  mutate(C_total = round(sum(c_across(starts_with("C_")), na.rm=TRUE),3)) %>%
  dplyr::select(COMID, C_total)
 
## LS: requires mean catchment slope (theta, CatSlopePct) and m (based on mean slope)
# calculated CatSlopePct for theta; mean catchment slope (issues with NHDPlusV2 vs Streamcat)
sedrisk.LS <- CatRdx %>%
  mutate(m = case_when(
    CatSlopePct >= 5 ~ 0.5,
    CatSlopePct >= 3.5 ~ 0.4,
    CatSlopePct >= 1 ~ 0.3,
    TRUE ~ 0.2
  ),
  CatSlopePct = replace_na(CatSlopePct, 0),
  LS = round(1^m * ((65.41*(sin(CatSlopePct)^2))+(4.56*sin(CatSlopePct))+0.065),3))

## create sed.risk df

### Percent artificial drainage area


### Dam storage ratio


### Road crossing density: RdCrsCat in CatRdx
Rdx.df <- CatRdx %>%
  dplyr::select(COMID, RdCrsCat)
