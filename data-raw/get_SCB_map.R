# get_SCB_map.R

filename <- "SCB_PXmap2007.zip"
temp_dir <- "data-raw/.temp/"
dir.create(temp_dir)
url <- file.path("http://www.gis.scb.se/how/PXmap1", filename)
temppath <- file.path(temp_dir, filename)
download.file(url, temppath)

unzip(temppath, exdir = temp_dir)