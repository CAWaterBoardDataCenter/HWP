## Testing whether NHDPlusV2 NLCD and Streamcat NLCD are similar


## libraries
library(readr)
library(tidyverse)
library(nhdplusTools) ## requires download first using devtools::install_github("usgs-r/nhdplusTools")
library(here)
library(scales)


## load data
Streamcat <- read_csv(here("UpdatedData", "NLCD2016_CA.csv")) %>% ## Streamcat NLCD data
  select(COMID, PctOw2016Cat, PctIce2016Cat, PctBl2016Cat, PctDecid2016Cat, PctConif2016Cat, PctMxFst2016Cat, PctShrb2016Cat, PctGrs2016Cat, PctWdWet2016Cat, PctHbWet2016Cat)
COMIDs <- Streamcat$COMID
NHD <- read_delim((here("UpdatedData", "NLCD16_CAT_CONUS.txt")), delim = ",") %>%
  dplyr::filter(COMID %in% COMIDs) %>%
  dplyr::select(COMID, CAT_NLCD16_11, CAT_NLCD16_12, CAT_NLCD16_31, CAT_NLCD16_41, CAT_NLCD16_42, CAT_NLCD16_43, CAT_NLCD16_52, CAT_NLCD16_71, CAT_NLCD16_90, CAT_NLCD16_95)
catch_ca <- readRDS(here("UpdatedData", "nhdplus_catchment_ca.rds"))


## calculate % natural land cover per COMID in each dataset
Streamcat$PctNatCover <- round(rowSums(Streamcat[2:11]),2)
Streamcat.df <- Streamcat %>%
  dplyr::select(COMID, PctNatCover)
# Streamcat.df <- Streamcat %>%
#   dplyr::select(COMID:PctHbWet2016Cat) %>%
#   rowwise(.) %>%
#   mutate(PctNatCover = round(sum(c_across(starts_with("Pct")), na.rm=TRUE),2)) %>%
#   dplyr::select(COMID, PctNatCover) %>%
#   data.frame(.)
Streamcat.df$rank_PctNatCover <- rank(Streamcat.df$PctNatCover)
Streamcat.df$nrank_PctNatCover <- (Streamcat.df$rank_PctNatCover - 1)/ (max(Streamcat.df$rank_PctNatCover) - 1)

NHD$PctNatCover <- round(rowSums(NHD[2:11]),2)
NHD.df <- NHD %>%
  dplyr::select(COMID, PctNatCover)
# NHD.df <- NHD %>%
#   rowwise(.) %>%
#   mutate(PctNatCover = round(sum(c_across(starts_with("CAT")), na.rm=TRUE),2)) %>%
#   dplyr::select(COMID, PctNatCover) %>%
#   data.frame(.)
NHD.df$rank_PctNatCover <- rank(NHD.df$PctNatCover)
NHD.df$nrank_PctNatCover <- (NHD.df$rank_PctNatCover - 1)/ (max(NHD.df$rank_PctNatCover) - 1)


## compare by % nat land cover
landcover <- left_join(Streamcat.df, NHD.df, by = "COMID") %>%
  mutate(absNLCD = abs(PctNatCover.x - PctNatCover.y)) %>%
  left_join(., catch_ca, by = c("COMID" = "FEATUREID"))


## Try some mapping
library(sf)
library(viridisLite)

png(here("figures", "landcoverdiff.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = landcover, mapping = aes(fill = absNLCD, geometry = Shape), colour = NA) +
  scale_fill_viridis_c(option = "magma", breaks = c(10, 50, 90), labels = c("10% difference", "50% different", "90% different"))+
  labs(fill = NULL) +
  theme_bw() +
  ggtitle("NLCD nat land cover, % differences between streamcat & NHD")
dev.off()


landcover_bad <- landcover %>%
  dplyr::filter(absNLCD > 5)

png(here("figures", "landcoverdiff_zoom.png"), width = 6, height = 5, units = "in", res = 300)
ggplot() +
  geom_sf(data = landcover_bad, mapping = aes(fill = absNLCD, geometry = Shape), colour = NA) +
  scale_fill_viridis_c(option = "magma", breaks = c(10, 50, 90), labels = c("10% difference", "50% different", "90% different"))+
  labs(fill = NULL) +
  theme_bw()
  # ggtitle("NLCD nat land cover, % differences between streamcat & NHD")
dev.off()

COMIDs_bad <- landcover_bad$COMID
NHD_bad <- NHD %>%
  filter(COMID %in% COMIDs_bad) %>%
  arrange(COMID)
Streamcat_bad <- Streamcat %>%
  filter(COMID %in% COMIDs_bad) %>%
  arrange(COMID)
