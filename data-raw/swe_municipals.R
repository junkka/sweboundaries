# swe_municipality.R
# every municipal boundry has a geom with geomid
# each version of a municipal has a id, from, tom and geomid

library(sp)
library(stringr)
library(rgdal)
library(maptools)
library(tidyr)
library(dplyr)
library(assertthat)

sp <- readOGR(dsn = "data-raw/.temp/", layer = "Sweden_municipality07", encoding = "latin1")
sp1 <- sp
slot(sp, "polygons") <- sapply(slot(sp, "polygons"), checkPolygonsHoles)

colnames(sp@data) <- tolower(colnames(sp@data))

# # Add new id to all codes independent of changing codes
slot(sp, "data") <- slot(sp, "data") %>% 
  group_by(kod98_03) %>% 
  mutate(code98_03 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod04_06) %>% 
  mutate(code04_06 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod97_97) %>% 
  mutate(code97_97 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod97_97) %>% 
  mutate(code97_97 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod95_96) %>% 
  mutate(code95_96 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod92_94) %>% 
  mutate(code92_94 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod83_91) %>% 
  mutate(code83_91 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod80_82) %>% 
  mutate(code80_82 = knkod[row_number() == 1]) %>% 
  ungroup %>% group_by(kod77_79) %>% 
  mutate(code77_79 = knkod[row_number() == 1]) %>% 
  ungroup %>% as.data.frame()

slot(sp, "data")  <- slot(sp, "data") %>% 
  mutate(from = 1974, tom = 9999, id = paste0(knkod, tom))
sp <- spChFIDs(sp, sp@data$id)

century <- function(x) x = ifelse(x < 20, x + 2000, x + 1900)
# make geoms, with geom and id = knkod + tom
combind_sp <- function(cname) {

  from_y <- str_extract(cname, "code[0-9]{2}") %>% 
    str_extract("[0-9]{2}$") %>% as.integer() %>% 
    century()
  
  to_y <- str_extract(cname, "[0-9]{2}$") %>% 
    as.integer() %>% century() 

  # change if more than one id from previous
  filt <- sprintf("!is.na(%s)", cname)
  change03 <- slot(sp, "data") %>% 
    filter_(filt) %>% 
    select_("id", cname) %>% distinct() %>% 
    group_by_(cname) %>% 
    filter(n() > 1)
  
  if (nrow(change03) == 0)
    return(nrow(change03))

  ch03 <- subset(sp, id %in% change03$id)
  new  <- unionSpatialPolygons(ch03, ch03@data[ ,cname])

  slot(sp, "data") <- slot(sp, "data") %>% 
    mutate(
      from      = ifelse(id %in% change03$id, (to_y + 1), from),
      code98_03 = ifelse(id %in% change03$id, NA, as.character(code98_03)),
      code97_97 = ifelse(id %in% change03$id, NA, as.character(code97_97)),
      code95_96 = ifelse(id %in% change03$id, NA, as.character(code95_96)),
      code92_94 = ifelse(id %in% change03$id, NA, as.character(code92_94)),
      code83_91 = ifelse(id %in% change03$id, NA, as.character(code83_91)),
      code80_82 = ifelse(id %in% change03$id, NA, as.character(code80_82)),
      code77_79 = ifelse(id %in% change03$id, NA, as.character(code77_79))
    )

  filt <- sprintf('as.character(%s) == as.character(knkod)', cname)
  dat2 <- slot(ch03, "data") %>% 
    filter_(filt) %>% 
    mutate(
      landareakm = NA, 
      tom = to_y, 
      from = 1977, 
      id = paste0(knkod, tom), 
      knnamn = stringr::str_extract(urnamn, "[[:alpha:]][[:alpha:]]{3,10}")
    )

  new <- spChFIDs(new, dat2$id)
  rownames(dat2) <- dat2$id
  new <- SpatialPolygonsDataFrame(new, data = dat2)
  old_sp    <- spChFIDs(sp, sp@data$id)
  sp <<- spRbind(old_sp, new)
  return(nrow(sp@data))
}

sapply(c("code98_03", "code97_97", "code95_96", "code92_94", "code83_91", "code80_82", "code77_79"), combind_sp)

d <- tbl_df(slot(sp, "data")) %>% 
  select(id, from, tom, knkod, knnamn, kod04_06:kod77_79,bef2004, knbef96) 
d2 <- d %>% 
  gather(period, code, kod04_06:kod77_79) %>% 
  mutate(
    from_p = str_extract(as.character(period), "kod[0-9]{2}") %>% 
      str_extract("[0-9]{2}$") %>% as.integer() %>% 
      century(),
    tom_p = str_extract(as.character(period), "[0-9]{2}$") %>% 
      as.integer() %>% century(),
      code = as.factor(code)
  ) %>% 
  group_by(knnamn, code, id) %>% 
  summarise(
    from = min(from_p),
    tom = max(tom_p),
    bef2004 = first(bef2004),
    knbef96 = first(knbef96)
  ) %>% 
  select(knkod = code, knnamn, from, tom, geomid = id, bef2004, knbef96) %>% 
  as.data.frame()

pop1996 <- d2 %>% filter(from <= 1996, tom >= 1996) %>% 
  select(knkod, pop = knbef96) %>% 
  mutate(year = 1996)

pop2004 <- d2 %>% filter(from <= 2004, tom >= 2004) %>% 
  select(knkod, pop = knbef96) %>% 
  mutate(year = 2004)

geo_pop <- rbind(pop1996, pop2004)

municipal_meta <- d2 %>% select(-bef2004, -knbef96)

slot(sp, "data") <- slot(sp, "data") %>% 
  select(geomid = id, from, tom)

area <- data.frame(
    geomid = sapply(swe_municipality@polygons, function(x) x@ID),
    area = sapply(swe_municipality@polygons, function(x) x@area)
  )

slot(sp, "data") <- slot(sp, "data") %>% 
  left_join(area, by = "geomid")

swe_municipality <- sp

# for each year test
plyr::l_ply(c(1979, 1982, 1991, 1994, 1996, 1997, 2003, 2006), function(a){
  subd <- swe_municipality@data %>% 
    filter(from <= a, tom >= a) 
  test <- municipal_meta %>% 
    filter(from <= a, tom >= a) %>% 
    left_join(subd, ., by = "geomid")

  assert_that(nrow(test) == nrow(subd)) %>% message()
  assert_that(nrow(test[is.na(test$knkod), ]) == 0)  %>% message()
  
})


save(geo_pop, file = "data/geo_pop.rda")
save(municipal_meta, file = "data/municipal_meta.rda")
save(swe_municipality, file = "data/swe_municipality.rda")