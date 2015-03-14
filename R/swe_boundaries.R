# swe_boundries.R

#' Swe Boundaries
#'
#' Get swedish administrative boundaries for a specified year.
#'
#' @param date a date or a year
#' @param type type of unit, "county" or "municipal"
#' @param format format of return object, "df" for data.frame and "sp" for 
#'   SpatialPolygonsDataFrame
#' @export
#' @import dplyr
#' @import sp
#' 

swe_boundaries <- function(date, 
    type = c("county", "municipal"), 
    format = c("sp", "df")) {
  x <- get_year(date)
  
  # env <- environment()
  type <- match.arg(type)
  format <- match.arg(format)

  if (type == "county"){
    if (x > 2010 || x < 1998)
      stop("For county, date must be a date between 1998-01-01 and 2010-12-31")
    res <- subset(swe_counties, from <= x & tom >= x)
  }
  if (type == "municipal"){
    if (x > 2010 || x < 1977)
      stop("For municipal, date must be a date between 1977-01-01 and 2010-12-31")
    meta <- municipal_meta %>% 
      filter(from <= x, tom >= x)
    res <- subset(swe_municipality, geomid %in% meta$geomid)
    slot(res, "data") <- slot(res, "data") %>% 
      select(geomid, area) %>% 
      left_join(meta, by = "geomid")
  }

  return(switch(format,
    sp = res,
    df = sp_to_ggplot(res)
  ))
}

#' Create ggplot map data.frame from NAD sp
#'
#' @param sp a SparialPolygonsDataFrame object
#' @importFrom ggplot2 fortify
#' @export

sp_to_ggplot <- function(sp){
  
  sp@data$id = rownames(sp@data)
  ret_d = ggplot2::fortify(sp, region="id")
  ret_d = left_join(ret_d, sp@data, by="id")
  # save(ret_d, file='data/nad_ggolot.RData')
  return(ret_d)
}
