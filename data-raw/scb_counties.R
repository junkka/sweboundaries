# scb_counties.R
# projection epsg:3021

library(sp)
library(rgdal)
library(maptools)
library(dplyr)

sp07 <- readOGR(dsn = "data-raw/.temp/", layer = "Sweden_county07", encoding = "latin1")
sp03 <- readOGR(dsn = "data-raw/.temp/", layer = "Sweden_county", encoding = "latin1")

proj4string(sp03) <- proj4string(sp07)

# Add uppsala and vastmanland from 1998-2007
slot(sp07, "data") <- slot(sp07, "data") %>% 
  select(LNKOD:BEF96, KOD97:URNAMN) %>% 
  mutate(tom = 9999, from = ifelse(LNKOD %in% c("03", "19"), 2007, 1998))
slot(sp03, "data") <- slot(sp03, "data") %>% 
  mutate(BEF05 = NA, tom = 2006, from = 1998) %>% 
  select(-RO)

sp_diff <- subset(sp03, LNKOD %in% c("03", "19"))
sp_diff <- spChFIDs(sp_diff, paste0(sp_diff$LNKOD, sp_diff$tom))
sp07    <- spChFIDs(sp07, paste0(sp07$LNKOD, sp07$tom))
swe_county <- spRbind(sp_diff, sp07)
colnames(swe_county@data) <- tolower(colnames(swe_county@data))
swe_county@data$id <- rownames(swe_county@data)

use_data(swe_county, overwrite = TRUE)