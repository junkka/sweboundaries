#' Get year
#'
#' Transforms a atomic vector to a integer of year
#'
#' @param x An oabject to be converted
#' @param ... other paramaters for methods
#' @export

get_year <- function(x, ...) UseMethod("get_year")

get_year.integer <- function(x, ...) {
  if(x < 2020) return(x)

  as.integer(substr(as.character(x), 1, 4))
}

get_year.numeric <- function(x, ...) {
  if(x < 2020) 
    return(as.integer(x))

  as.integer(substr(as.character(x), 1, 4))
}

get_year.character <- function(x, ...) {
  if (nchar(x) <= 4)
    return(as.integer(x))

  y <- as.integer(lubridate::year(lubridate::ymd(x, quiet = TRUE, ...)))
  if (!is.na(y))
    return(y)
  else
    as.integer(stringr::str_extract(x, "^[0-9]{1,4}"))
}

get_year.default <- function(x, ...) {
  as.integer(lubridate::year(x))
}