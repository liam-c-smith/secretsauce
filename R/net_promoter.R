#' Calculate a 'Net Promoter Score' from rating scale data
#'
#' \code{net_promoter()} is a summary function which takes a vector of 0 to 10 rating
#' scale data and returns a 'Net Promoter Score'. There are options to and calculate
#' confidence intervals and treat missing values as 'Neutral' ratings.
#'
#' The 'Net Promoter Score' is commonly used to summarise responses to 'likelihood to
#' recommend' questions obtained in customer surveys. The rating scale ranges from 0 to 10
#' but this is collapsed into 3 bins. 0 to 6 are considered 'Detractors', 7 to 8 are
#' 'Neutral' and 9 to 10 are 'Promoters'. The score is then the difference between the
#' proportion of 'Promoter' responses and the proportion of 'Detractor' responses.
#'
#' As this score is a difference between two proportions in the same sample it takes
#' discrete values between -1 and 1. However, when reported the score is often multiplied
#' by 100 and treated much like a percentage. Of course, this scaling is completely
#' arbitrary so the function provides an argument to change the scale but the default is
#' set to 100.
#'
#' Confidence Intervals:
#'
#' By default, the function will return the score alone but when \code{interval = TRUE} a
#' confidence interval will also be calculated. Either a Wald-type or bootstrapping
#' approach will be used depending on the value of \code{method}.
#'
#' For \code{method = #' "wald"} the score is assumed to be normally distributed with the
#' variance and derived from the properties of a multinomial random variable.
#'
#' For \code{method = "boot"}, bootstrapping will be employed using functions from the
#' \code{boot} package. The default approach for calculating the interval the adjusted
#' bootstrap percentile method but other \code{boot::boot.ci} interval types can be
#' specified.
#'
#' The Wald-type intervals should be a good approximation in large samples but when the
#' sample size is small or the score is near -1 or +1 bootstrapping is recommended.
#'
#' @param x A numeric vector of integers between 0 and 10.
#' @param scale A numeric scaling factor to apply to the score once it has been
#'   calculated.
#' @param include_na Should missing values be treated as 'Neutral' responses?
#' @param interval Should a confidence interval, as well as the score, be returned?
#' @param method A string specifying the type of method used in calculating the confidence
#'   interval, see details. Must be either "wald" or "boot".
#' @param level Desired confidence level.
#' @param boot_r An integer specifying the number of bootstrap replications used when
#'   \code{method = "boot"}.
#' @param boot_type A string specifying the type of calculation used to find the
#'   confidence limits when \code{method = "boot"}. These are the same as in
#'   \code{boot::boot.ci} but only \code{c("norm", "basic", "perc", "bca")} are
#'   implemented.
#'
#' @return If \code{interval = FALSE} then only the score is returned as a numeric value.
#'   Otherwise both the score and limits of the confidence interval will be returned in a
#'   length 3 numeric vector of the form \code{c(score, lower, upper)}.
#'
#' @examples
#' # Generate rating scale data
#' p <- 0.5 # probability of 'Promoter'
#' d <- 0.3 # probability of 'Detractor
#' prob <- c(rep(d, 7) / 7, rep(1 - p - d, 2) / 2, rep(p, 2) / 2)
#' ratings <- sample(0:10, 30, TRUE, prob)
#'
#' # Calculate NPS
#' net_promoter(ratings)                   # default action
#' net_promoter(ratings, scale = 1)        # unadulterated score
#' net_promoter(ratings, interval = TRUE)  # get a confidence interval
#'
#' # Wald-type confidence interval can lead to impossible limits when the score is near the
#' # extremes. Its better to use a percentile bootsrapping method in this case.
#' p <- 0.05
#' d <- 0.9
#' prob <- c(rep(d, 7) / 7, rep(1 - p - d, 2) / 2, rep(p, 2) / 2)
#' set.seed(1234)
#' ratings <- sample(0:10, 30, TRUE, prob)
#'
#' net_promoter(ratings, interval = TRUE, method = "wald")
#' net_promoter(ratings, interval = TRUE, method = "boot")
#'
#' @export
net_promoter <- function(x, scale = 100, include_na = FALSE, interval = FALSE,
                         method = "wald", level = 0.95, boot_r = 2000, boot_type = "bca"){

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

  if (level < 0 | level > 1){
  	stop("`level` is outside (0, 1).")
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
  	z_quantile <- -qnorm(((1 - level) / 2))

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

  	boot_ci <- boot::boot.ci(boot_dist, conf = level, type = boot_type)

  	boot_ci_ints <- tail(boot_ci[[4]][1,], 2)  # using `tail()` in case `boot_type = norm`

  	c(estimate = boot_dist$t0, lower = boot_ci_ints[1], upper = boot_ci_ints[2])
  }
}
