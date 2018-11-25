net_promoter <- function(x, scale = 100, include_na = FALSE, interval = FALSE,
						 method = "wald", alpha = 0.05, boot_r = 2000, boot_type = "bca"){

  # Check errors
  if (any(x %% 1 != 0, na.rm = TRUE)){
    stop("x contains non-integer values.")
  }

  if (any(x < 0 | x > 10, na.rm = TRUE)){
    stop("x contains values outside 0:10.")
  }

  if (!(method %in% c("wald", "boot"))) {
  	stop("`method = '", method, "'` is not recognised. Please use either 'wald' or 'boot'.")
  }

  if (alpha < 0 | alpha > 1){
  	stop("`alpha` is outside [0, 1].")
  }

  if (!(boot_type %in% c("norm", "basic", "perc", "bca"))){
  	stop("`boot_type = ", boot_type, "` is not implemented. Please use one of 'norm', 'basic', 'perc' or 'bca'.")
  }

  # Warn of peculiarities
  if (any(is.na(x))){
    if (include_na){
      warning("Missing values detected. As `include_na = TRUE`, these have been inluded in the denominator.")
    } else {
      warning("Missing values detected.")
    }
  }

  # Transform Likert data
  x[x < 7] <- -1
  x[x > 8] <-  1
  x[x > 1] <-  0
  if (include_na){
    x[is.na(x)] <- 0
  }

  # Caclulate the score (and interval if requested)
  if (!interval){

  	mean(x, na.rm = TRUE) * scale

  } else if (method == "wald"){

  	prop_p <- mean(x ==  1, na.rm = TRUE)
  	prop_d <- mean(x == -1, na.rm = TRUE)
  	n <- sum(!is.na(x))

  	variance <- (prop_p * (1 - prop_p) + prop_d * (1 - prop_d) + 2 * prop_p * prop_d) / n
  	std_error <- sqrt(variance)
  	z_quantile <- -qnorm((alpha / 2))

  	estimate <- mean(x, na.rm = TRUE)
  	lower <- estimate - z_quantile * std_error
  	upper <- estimate + z_quantile * std_error

  	c(estimate = estimate, lower = lower, upper = upper) * scale

  } else if (method == "boot"){

  	boot_estimator <- function(x, i){
  		z <- x[i] # `boot()` requires a function with an indexing variable
  		mean(z, na.rm = TRUE) * scale
  	}

  	boot_dist <- boot::boot(x, boot_estimator, boot_r)

  	boot_ci <- boot::boot.ci(boot_dist, conf = 1 - alpha, type = boot_type)

  	boot_ci_ints <- tail(boot_ci[[4]][1,], 2)  # using `tail()` in case `boot_type = norm`

  	c(estimate = boot_dist$t0, lower = boot_ci_ints[1], upper = boot_ci_ints[2])
  }
}