#' A binary operator for NOT IN
#'
#' Test whether the elements of \code{x} are contained within \code{y}. It is simply a
#' wrapper around \code{!(x \%in\% y)} but intended as a more readable alternative.
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return A logical vector of the corresponding matches.
#'
#' @export
`%not_in%` <- function(x, y){
  !(x %in% y)
}
