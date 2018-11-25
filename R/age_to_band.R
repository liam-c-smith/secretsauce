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
    stop("`style = \"", style, \"` is not recognised. Please use either \"bracket\" or \"hyphen\".")
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




