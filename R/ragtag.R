# Finally a more friendly NOT IN, friendlier than !(x %in% y) at least
`%not_in%` <- Negate(`%in%`)


geomean <- function(x, na.rm = FALSE) {
	if (any(x == 0)) {
		warning("Input vector contains zeros, therefore the result is zero. The geometric mean is only valid for positive values.")
	}
	prod(x, na.rm = na.rm)^(1 / sum(!is.na(x)))
}

fiscal_year <- function(x, start = 4, char = FALSE){
	if (char) {
		ifelse(
			lubridate::month(x) < start,
			paste0(lubridate::year(x) - 1, "/", substr(lubridate::year(x),     3, 4)),
			paste0(lubridate::year(x),     "/", substr(lubridate::year(x) + 1, 3, 4))
		)
	} else {
		ifelse(lubridate::month(x) < start, lubridate::year(x), lubridate::year(x) + 1)
	}
}
