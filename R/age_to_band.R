#' Convert ages to age-bands
#'
#' Use \code{age_to_band()} to bin ages (measured in years) into regularly spaced
#' intervals with options for grouping values above or below given thresholds. Intervals
#' are closed on the right and open on the left.
#'
#' @param x A numeric vector of non-negative values representing ages.
#' @param width An integer specifying the size of the interval for binning. Must be
#'   greater than or equal to 2 for meaningful age bands.
#' @param style A string specifying the format of labels assigned to the bins. \code{style
#'   = "bracket"} provides mathmatical interval notation, e.g. "[5, 10)", whereas
#'   \code{style = "hyphen"} provides integer limits separated by a hyphen, e.g. "5-9".
#'   While the hyphen-style format is less precise mathmatically, it may be more intuitive
#'   for audiences without a mathmatical background.
#' @param group_below,group_above An integer specifying end group thresholds. Must be a
#'   multiple of \code{width}. This is sometimes helpful when younger or older ages should
#'   be collected into wider intervals groups groups. E.g. \code{width = 5} and
#'   \code{group_below = 15} puts every age below 15 into a wider bin of size [0, 15) then
#'   5-year bands proceed from there: [15, 20), [20, 25), [25, 30), etc..
#'
#' @return An ordered factor with labels determined by \code{style}. Bands that don't
#'   appear in the data are still included as levels in the resulting factor.
#'
#' @examples
#' # Generate non-negative data
#' x <- runif(20, 0, 100)
#' age_to_band(x)              # 5-year age bands (the default)
#' age_to_band(x, width = 20)  # 20-year age bands
#'
#' # The ordered factor output ensures the bands are
#' # appropriately ordered and that bands not present
#' # in the data are still included. This might otherwise lead to
#' # undesireable plot results.
#' y <- c(runif(10, 0, 20), runif(10, 30, 50))
#' plot(table(age_to_band(y)))
#'
#' # `style = "bracket"` and `style = "hyphen"` have
#' # different labels when grouping the ends
#' z <- c(5, 23, 70)
#' age_to_band(z, group_below = 15, group_above = 65, style = "bracket")
#' age_to_band(z, group_below = 15, group_above = 65, style = "hyphen")
#'
#' @export
age_to_band <- function(x, width = 5, style = "bracket",
                        group_below = NULL, group_above = NULL){

  # Check errors
  if (width < 2){
    stop("`width` < 2, please specify a `width` of at least 2.")
  }

  if (width %% 1 != 0){
    stop("`width` is not an integer")
  }

  group_args <- c(group_below, group_above)

  if (any(group_args %% width != 0)){
    stop("`group_below` and/or `group_above` are not multiples of `width`. Please use other means for irregularly spaced intervals.")
  }

  if (length(group_args) == 2){
    if (group_above < group_below){
      stop("`group_above` is less than `group_below`.")
    }
  }

  if (!(style %in% c("bracket", "hyphen"))){
    stop("`style = '", style, "'` is not recognised. Please use either 'bracket' or 'hyphen'.")
  }

  # Warn of peculiarities
  if (any(is.na(x))){
    warning("Missing values detected.")
  }

  x_rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

  if (x_rng < width){
    warning("The range of x (", round(x_rng, 2), ") is less than the interval width (",
            width, "). Consider `width` < ", round(x_rng, 2), " for more useful bands.")
  }

  # Bin values
  binned <- x %/% width * width

  # Apply end groupings (if needed)
  binned[binned <  group_below] <- 0
  binned[binned >= group_above] <- group_above

  # Create labels for the bands
  lvls <- seq(max(min(binned, na.rm = TRUE), group_below, na.rm = TRUE),
              max(binned, na.rm = TRUE), width)

  if (!is.null(group_below)){
    lvls <- c(0, lvls)
  }

  if (style == "bracket"){
    lbls <- paste0("[", lvls, ", ", lvls + width, ")")
    if (!is.null(group_below)){
      lbls[1] <- paste0("[0, ", group_below, ")")
    }
    if (!is.null(group_above)){
      lbls[length(lvls)] <- paste0("[", tail(lvls, 1), ", Inf)")
    }
  } else if (style == "hyphen"){
    lbls <- paste0(lvls, "-", lvls + width - 1)
    if (!is.null(group_below)){
      lbls[1] <- paste0("<", group_below)
    }
    if (!is.null(group_above)){
      lbls[length(lvls)] <- paste0(tail(lvls, 1), "+")
    }
  }

  factor(binned, lvls, lbls, ordered = TRUE)

}
