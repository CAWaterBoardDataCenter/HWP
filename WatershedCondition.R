
# Watershed Condition.R ----
## Started: 17 Aug 2021
## Edited: 5 May 2022
## Developed by: Corey Clatterbuck


##  read in libraries ----
library(readr)
library(tidyverse)
library(nhdplusTools) ## requires download first using devtools::install_github("usgs-r/nhdplusTools")
library(here)
library(scales)
library(foreign)


## read in data files ----
catch_ca <- readRDS(here("UpdatedData", "nhdplus_catchment_ca_cropped.rds"))
NLCDcatch <- read_csv("./UpdatedData/NLCD2016_CA.csv") %>% ## Streamcat NLCD data
  select(COMID, CatAreaSqKm, PctOw2016Cat, PctIce2016Cat, PctBl2016Cat, PctDecid2016Cat, PctConif2016Cat, PctMxFst2016Cat, PctShrb2016Cat, PctGrs2016Cat, PctWdWet2016Cat, PctHbWet2016Cat, PctUrbOp2016Cat, PctUrbLo2016Cat, PctUrbMd2016Cat, PctUrbHi2016Cat, PctHay2016Cat, PctCrop2016Cat)
COMIDs <- as.vector(NLCDcatch$COMID)
Soil <- read.table(here("UpdatedData" , "STATSGO_LAYER_CAT_CONUS.txt"), 
                   header = TRUE, sep = ",") %>% 
  select(COMID, CAT_RFACT, CAT_KFACT, NODATA) %>%
  dplyr::filter(COMID %in% COMIDs) ## NHDPlusV2 STATSGO data
Slope <- read.table(here("UpdatedData" , "BASIN_CHAR_CAT_CONUS.txt"), 
                    header = TRUE, sep = ",") %>% 
  dplyr::filter(COMID %in% COMIDs) ## NHDPlusV2 basin slope
AgDitches92 <- read.table(here("UpdatedData" , "DITCHES92_CONUS.txt"), 
                          header = TRUE, sep = ",") %>%
  dplyr::filter(COMID %in% COMIDs)
AgTiles92 <- read.table(here("UpdatedData" , "TILES92_CONUS.txt"), 
                        header = TRUE, sep = ",") %>%
  dplyr::filter(COMID %in% COMIDs)
AgTilesNakagaki <- read.table(here("UpdatedData" , "TILES_Nakagaki_CONUS.txt"), 
                              header = TRUE, sep = ",") %>%
  dplyr::filter(COMID %in% COMIDs)
Dams <- read_csv(here("UpdatedData", "Dams_CA.csv"))
Streamflow <- read.csv(here("UpdatedData", "CA_EROM_compiled.csv")) %>%
  dplyr::filter(ComID %in% COMIDs) ## need to determine where missing ComIDs are
# Streamflow <- read.dbf(here("UpdatedData", "NHDPlusCA", "NHDPlus18", "EROMExtension", "EROM_MA0001.DBF")) %>%
#   dplyr::filter(ComID %in% COMIDs) ## CA VPU 18 only, created DSRatio..._pre figures
CatRdx <- read_csv("./UpdatedData/RoadStreamCrossings_CA.csv") %>% ## Streamcat Road Crossings data
  dplyr::select(COMID, RdCrsCat)

## read in functions ----
source(here("functions", "normalrank.R"))

## test whether values are similar between Streamcat and NLCD datasets (some are not)
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

## indicator calculations ----

### Percent natural land cover ----
NLCD.df <- NLCDcatch[,c(1, 3:12)]
NLCD.df[!complete.cases(NLCD.df), ] ## has 1 COMID without any land cover data
NLCD.df$PctNatCover <- round(rowSums(NLCD.df[2:11]),2)
NLCD.df$nrank_PctNatCover <- normalrank(NLCD.df$PctNatCover)
NLCD.df <- select(NLCD.df, COMID, nrank_PctNatCover)
NLCD.df[!complete.cases(NLCD.df), ] ## same COMID as above was not ranked

### ARA: need ArcGIS to complete ----


### Sedimentation Risk ----

## looking for R factor: looks like was pre-calculated in the obs variables from 2013.
# data2013 <- read_csv("./CA_HWI_FinalData_101813/Deliverables/CAdata_obs.csv")
# colnames(data2013) ## has Kfact and sedrisk, but no Rfact?
# would like to bring in Rfactor from NHDPlusV2 data but given discrepancies I've found between the NHDPlusV2 and StreamCat datasets I'm wary.

## R & K factors: in NHDPlusV2 STATSGO table; "Soil" here

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
 
## LS: requires mean catchment slope (theta, CAT_BASIN_SLOPE) and m (based on mean slope)
# Uses basin slope from NHDPlusV2; could not reconcile back-calculating basin slope correctly from Streamcat's RdCrsSlpWtdCat variable.
# calculated CatSlopePct for theta; mean catchment slope (issues with NHDPlusV2 vs Streamcat)
range(Slope$CAT_BASIN_SLOPE) ## no NA or no data values
sedrisk.LS <- Slope %>%
  mutate(m = case_when(
    CAT_BASIN_SLOPE >= 5 ~ 0.5,
    CAT_BASIN_SLOPE >= 3.5 ~ 0.4,
    CAT_BASIN_SLOPE >= 1 ~ 0.3,
    TRUE ~ 0.2
  ),
  LS = round(1^m * ((65.41*(sin(CAT_BASIN_SLOPE)^2))+(4.56*sin(CAT_BASIN_SLOPE))+0.065),3)) %>%
  dplyr::select(COMID, CAT_BASIN_SLOPE, m, LS)

## create sedrisk.df
# sedrisk.df <- left_join(sedrisk.R, sedrisk.K, by = "COMID") %>%
sedrisk.df <- dplyr::select(Soil, COMID, CAT_RFACT, CAT_KFACT) %>%
  left_join(., sedrisk.LS, by = "COMID") %>%
  left_join(., sedrisk.C, by = "COMID")

## calculate sedrisk & rank-normalize
# sedrisk.df$sedrisk <- sedrisk.df$CAT_RF7100*sedrisk.df$KffactCat*sedrisk.df$C_total*sedrisk.df$LS
sedrisk.df$sedrisk <- sedrisk.df$CAT_RFACT*sedrisk.df$CAT_KFACT*sedrisk.df$C_total*sedrisk.df$LS
head(sedrisk.df)
sedrisk.df$nrank_sedrisk <- normalrank(-sedrisk.df$sedrisk)
Sedrisk.df <- select(sedrisk.df, COMID, sedrisk, nrank_sedrisk)


### Percent artificial drainage area ----
#### compare AgTiles92 to AgTilesNakagaki
filterT1 <- AgTiles92 %>%
  dplyr::filter(CAT_TILES92 > 0) %>%
  dplyr::select(COMID, CAT_TILES92, CAT_NODATA) ## why does this dataset have more obs?
filterT2 <- AgTilesNakagaki %>%
  dplyr::filter(CAT_TILES_Early90s > 0) %>%
  dplyr::select(COMID, CAT_TILES_Early90s, CAT_NODATA)

#### get tiles and ditches in same dataset & sum percentages
AgCombo <- AgDitches92 %>%
  dplyr::select(COMID, CAT_DITCHES92) %>%
  left_join(AgTiles92, by = "COMID") %>%
  dplyr::select(COMID, CAT_DITCHES92, CAT_TILES92) %>%
  left_join(AgTilesNakagaki, by = "COMID") %>%
  dplyr::select(COMID, CAT_DITCHES92, CAT_TILES92, CAT_TILES_Early90s) %>%
  dplyr::mutate(across(CAT_DITCHES92:CAT_TILES_Early90s, ~na_if(.,-9999))) %>% ## make no data to na
  rowwise() %>%
  mutate(Drainage92 = round(sum(CAT_DITCHES92,CAT_TILES92,na.rm=TRUE),2),
         Drainage90s = round(sum(CAT_DITCHES92,CAT_TILES_Early90s,na.rm=TRUE),2))

#### rank-normalize & create .df
AgCombo$nrank_Drainage92 <- normalrank(-AgCombo$Drainage92)
AgCombo$nrank_Drainage90s <- normalrank(-AgCombo$Drainage90s)
Ag.df <- dplyr::select(AgCombo, COMID, nrank_Drainage92, nrank_Drainage90s)


### Dam storage ratio ----

#### explore data 
hist(Dams$DamNrmStorCat)
hist(Dams$DamNrmStorWs)
sub1 <- dplyr::filter(Dams, DamNrmStorCat > 0) ## 1272 obs
sub2 <- dplyr::filter(Dams, DamNrmStorWs > 0) ## 19267 obs...interesting, makes sense
which(is.na(Dams$DamNrmStorCat)) # none
which(is.na(Dams$DamNrmStorWs)) # none
which(is.na(Streamflow$V0001C)) # none

sub3 <- dplyr::filter(Dams, DamNIDStorCat > 0) ## ~25 dams in CA that do not store water
NoNrmStorageCat <- anti_join(sub3, sub1, by = "COMID") ## COMIDs with potential storage according to NID but no normal (average) storage
sub4 <- dplyr::filter(Dams, DamNIDStorWs > 0) ## ~180 downstream segments could have water
NoNrmStorageWs <- anti_join(sub4, sub2, by = "COMID") ## COMIDs with potential storage in watershed according to NID but no normal (average) storage
## I think using normal storage is fine


#### calculate storage in cat & ws

##### replace DamNrm with DamNID where DamNrm == 0 & DamNID > 0 & calculate
StorageCombo <- Dams %>%
  mutate(DamNrmStorCat2 = case_when(DamNrmStorCat == 0 & DamNIDStorCat > 0 ~ DamNIDStorCat,
                          TRUE ~ DamNrmStorCat),
         DamNrmStorWs2 = case_when(DamNrmStorWs == 0 & DamNIDStorWs > 0 ~ DamNIDStorWs,
                                    TRUE ~ DamNrmStorWs),
         StorageCatCM = DamNrmStorCat2 / CatAreaSqKm, # cubic meters
         StorageCatCF = StorageCatCM*35.3147, # cubic feet
         StorageCatAF = StorageCatCF / 43560, # acre feet
         StorageWsCM = DamNrmStorWs2 / WsAreaSqKm, # cubic meters, watershed
         StorageWsCF = StorageWsCM*35.3147, # cubic feet, watershed
         StorageWsAF = StorageWsCF / 43560) %>% # acre feet, watershed
  dplyr::select(COMID, CatAreaSqKm, DamNrmStorCat2, StorageCatCM, StorageCatCF, StorageCatAF, WsAreaSqKm, DamNrmStorWs2, StorageWsCM, StorageWsCF, StorageWsAF)

##### without replacing DamNrm with DamNID where DamNrm == 0 & DamNID > 0
# StorageCombo <- Dams %>%
#   mutate(StorageCatCM = DamNrmStorCat / CatAreaSqKm, # cubic meters
#          StorageCatCF = StorageCatCM*35.3147, # cubic feet
#          StorageCatAF = StorageCatCF / 43560, # acre feet
#          StorageWsCM = DamNrmStorWs / WsAreaSqKm, # cubic meters, watershed
#          StorageWsCF = StorageWsCM*35.3147, # cubic feet, watershed
#          StorageWsAF = StorageWsCF / 43560) %>% # acre feet, watershed
#   dplyr::select(COMID, CatAreaSqKm, DamNrmStorCat, StorageCatCM, StorageCatCF, StorageCatAF, WsAreaSqKm, DamNrmStorWs, StorageWsCM, StorageWsCF, StorageWsAF)


#### calculate flow in acft/yr 
FlowCombo <- Streamflow %>%
  dplyr::select(ComID, Q0001A) %>%
  dplyr::rename(COMID = ComID,
                MeanFlow_cfs = Q0001A) %>%
  dplyr::mutate(Flow_AFyear = MeanFlow_cfs * 723.96695) ## convert cfs to acre-feet/yr

#### join combo frames & calculate ratio 
RatioCombo <- left_join(StorageCombo, FlowCombo, by = "COMID") %>%
  dplyr::mutate(DSRatioCat = StorageCatAF / Flow_AFyear,
                DSRatioWs = StorageWsAF / Flow_AFyear)

#### which COMIDs did not have mean flows available?
test3 <- dplyr::filter(RatioCombo, is.na(MeanFlow_cfs)) ## 216 obs

#### rank-normalize and create .df 
DSRatio.df <- RatioCombo %>%
  dplyr::mutate(nrank_DSRatioCat = normalrank(-DSRatioCat),
                nrank_DSRatioWs = normalrank(-DSRatioWs)) %>%
  dplyr::select(COMID, DSRatioCat, nrank_DSRatioCat, DSRatioWs, nrank_DSRatioWs)


### Road crossing density: RdCrsCat in CatRdx ----
Rdx.df <- CatRdx
Rdx.df$nrank_RdCrsCat <- normalrank(-Rdx.df$RdCrsCat)
Rdx.df <- select(Rdx.df, COMID, RdCrsCat, nrank_RdCrsCat)



## join dfs together, add to catch_ca ----
catch_ca_plot <- catch_ca %>%
  left_join(NLCD.df, by = c("FEATUREID" = "COMID")) %>%
  left_join(Rdx.df, by = c("FEATUREID" = "COMID")) %>%
  left_join(Sedrisk.df, by = c("FEATUREID" = "COMID")) %>%
  left_join(Ag.df, by = c("FEATUREID" = "COMID")) %>%
  left_join(DSRatio.df, by = c("FEATUREID" = "COMID"))


## Try some mapping ----
library(sf)
library(viridisLite)
str(catch_ca)

### set figure parameters ----
breaks <- seq(0,1,.1)
# theme(legend.position = c(0.25, 0.08),
#       legend.direction = "horizontal") ## legend position in lower left

### map whole state ----
# png(here("figures", "PctNatCover.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = catch_ca_plot, mapping = aes(fill = nrank_PctNatCover), colour = NA) +
  scale_fill_binned(breaks = breaks, labels = NULL, direction = -1, type = "viridis") +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.95), legend.direction = "horizontal",
        legend.margin=margin(t=0, r=0, b=-0.2, l=0, unit="in")) +
  ggtitle("Percent natural cover, rank-normalized")
dev.off()

# png(here("figures", "sedrisk.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = catch_ca_plot, mapping = aes(fill = nrank_sedrisk), colour = NA) +
  scale_fill_binned(breaks = breaks, labels = NULL, type = "viridis") +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.95), legend.direction = "horizontal",
        legend.margin=margin(t=0, r=0, b=-0.2, l=0, unit="in")) +
  ggtitle("Sedimentation risk, rank-normalized")
dev.off()

# png(here("figures", "ArtificialDrainage92.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = catch_ca_plot, mapping = aes(fill = nrank_Drainage92), colour = NA) +
  scale_fill_binned(breaks = breaks, labels = NULL, type = "viridis") +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.95), legend.direction = "horizontal",
        legend.margin=margin(t=0, r=0, b=-0.2, l=0, unit="in")) +
  ggtitle("% artificial drainage area (1992), rank-normalized")
dev.off()

# png(here("figures", "ArtificialDrainage90s.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = catch_ca_plot, mapping = aes(fill = nrank_Drainage90s), colour = NA) +
  scale_fill_binned(breaks = breaks, labels = NULL, direction = -1, type = "viridis") +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.95), legend.direction = "horizontal",
        legend.margin=margin(t=0, r=0, b=-0.2, l=0, unit="in")) +
  ggtitle("% artificial drainage area (1990s), rank-normalized")
dev.off()

# png(here("figures", "DSRatioCat_post.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = catch_ca_plot, mapping = aes(fill = nrank_DSRatioCat), colour = NA) +
  scale_fill_binned(breaks = breaks, labels = NULL, direction = -1, type = "viridis") +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.95), legend.direction = "horizontal",
        legend.margin=margin(t=0, r=0, b=-0.2, l=0, unit="in")) +
  ggtitle("Dam Storage Ratio (Cat), rank-normalized")
dev.off()

# png(here("figures", "DSRatioWs_post.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = catch_ca_plot, mapping = aes(fill = nrank_DSRatioWs), colour = NA) +
  scale_fill_binned(breaks = breaks, labels = NULL, direction = -1, type = "viridis") +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.95), legend.direction = "horizontal",
        legend.margin=margin(t=0, r=0, b=-0.2, l=0, unit="in")) +
  ggtitle("Dam Storage Ratio (Ws), rank-normalized")
dev.off()


# png(here("figures", "Rdx.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = catch_ca_plot, mapping = aes(fill = nrank_RdCrsCat), colour = NA) +
  scale_fill_binned(breaks = breaks, labels = NULL, direction = -1, type = "viridis") +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.95), legend.direction = "horizontal",
        legend.margin=margin(t=0, r=0, b=-0.2, l=0, unit="in")) +
  ggtitle("Road-stream crossings, rank-normalized")
dev.off()


### try a subset to map ----
catch_ca[which.max(catch_ca$s_RdCrsCat),]
ggplot() + geom_sf(data = catch_ca, mapping = aes(fill = nrank_RdCrsCat), colour = NA) + 
  coord_sf(xlim = c(-122.2, -122.4),
           ylim = c(39.9, 40)) +
  scale_fill_viridis_c() +
  theme_bw()