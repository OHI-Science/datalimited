#' Summarize B/B_MSY values
#'
#' @param bbmsy A numeric matrix of B/B_MSY values. Columns should represent
#'   years and rows should represent samples of B/B_BMSY.
#' @param probs A numeric vector of quantile probabilities.
#' @param log Logical: should the mean and standard deviation be calculated on
#'   the log scale and then exponentiated at the end?
#' @param ... Other parameters to pass to \code{quantile}, \code{sd}, and
#'   \code{mean}. For example, \code{na.rm = TRUE}.
#' @return A data frame: rows are years and columns represent quantile, mean,
#'   and standard deviation values of B/B_MSY.
#' @export
summarize_bbmsy <- function(bbmsy, probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
  log = FALSE, ...) {

  #adding in a rolling mean every 5 years. since we take the rolling mean of mean_bbmsy, we also need the rolling
  # mean for the 2.5 and 97.5 quantiles to more accurately calculate low and high score estimates. this was not part of
  # the original code from the datalimited summarize_bbmsy function.
  roll <- apply(bbmsy,1,function(x) zoo::rollmean(x,5,align="right", fill=NA))

  q <- apply(roll, 1, quantile, probs = probs,na.rm=T, ...)
  q <- as.data.frame(t(q))
  names(q) <- paste0("bbmsy_q", names(q))
  names(q) <- gsub("%", "", names(q))
  if (log) {
    x_sd <- exp(apply(log(bbmsy), 2, sd, ...))
    x_mean <- exp(apply(log(bbmsy), 2, mean, ...))
  } else {
    x_sd <- apply(bbmsy, 2, sd, ...)
    x_mean <- apply(bbmsy, 2, mean, ...)
  }
  data.frame(q, bbmsy_sd = x_sd, bbmsy_mean = x_mean)
}
