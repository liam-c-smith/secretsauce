#' Convert dates to fiscal years
#'
#' Convert a vector of dates or monthly data to the enclosing fiscal year. There are
#' options to change the start of the fiscal year and choose from either numeric or label
#' output.
#'
#' @param x A date vector or a vector of class \code{"yearmon"} from the \code{zoo} package.
#' @param start An integer between 1 and 12 representing the month on which the fiscal
#'   begins. The default gives April.
#' @param label Should labels be returned instead of numeric years? If \code{label = TRUE}
#'   then the out will be a character vector of labels for the appropriate fiscal years,
#'   e.g. "2017/18".
#'
#' @return By default, a numeric vector of fiscal years or, if \code{label = TRUE}, a
#'   character vector of fiscal years.
#'
#' @examples
#' x <- seq.Date(as.Date("2012-01-01"), as.Date("2017-01-01"), by = "month")
#'
#' fiscal_year(x)             # April is the default starting month
#' fiscal_year(x, start = 7)  # But this can be changed
#'
#' fiscal_year(x, label = TRUE)  # Label output may be attractive when publishing results
#'
#' @export
fiscal_year <- function(x, start = 4, label = FALSE){

  if(start < 1 | start > 12){
    stop("`start` must be between 1 and 12.")
  }

  if (label) {
    ifelse(
      lubridate::month(x) < start,
      paste0(lubridate::year(x) - 1, "/", substr(lubridate::year(x),     3, 4)),
      paste0(lubridate::year(x),     "/", substr(lubridate::year(x) + 1, 3, 4))
    )
  } else {
    ifelse(lubridate::month(x) < start, lubridate::year(x), lubridate::year(x) + 1)
  }
}
