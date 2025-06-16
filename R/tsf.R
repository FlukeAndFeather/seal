#' Extract features from an interval
#'
#' @param t Time vector
#' @param y Series vector
#' @param start_idx Starting index of the interval
#' @param end_idx Ending index of the interval
#'
#' @returns A named vector containing the mean, standard deviation, and slope of
#'   the interval.
#' @export
#'
#' @examples
#' set.seed(123)
#' t <- 1:30
#' y <- cumsum(rnorm(30, 0.2, 0.3))
#' extract_interval_features(t, y, 15, 25)
extract_interval_features <- function(t, y, start_idx, end_idx) {
  t_int <- t[start_idx:end_idx]
  y_int <- y[start_idx:end_idx]
  # Slope via linear regression
  c(mean = mean(y_int),
    sd = sd(y_int),
    slope = slope(t_int, y_int))
}

#' Calculate the slope of a linear regression line
#'
#' @param x Vector of x-coordinates
#' @param y Vector of y-coordinates
#'
#' @returns The slope of the linear regression line fitted to the points (x, y).
#'
#' @examples
#' set.seed(123)
#' x <- 1:10
#' y <- 1 + 2 * x + rnorm(10, 0, 0.2)
#' slope(x, y)
slope <- function(x, y) {
  sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
}


sample_intervals <- function(m) {
  intervals <- matrix(NA, nrow = m, ncol = 2)
  # Window sizes
  w <- sample(1:(m - 2), sqrt(m))
  # Interval start and end locations
  interval_list <- lapply(w, \(.w) {
    start <- sample(1:(m - .w - 1), floor(sqrt(m - .w + 1)))
    end <- start + .w
    cbind(start, end)
  })
  do.call(rbind, interval_list)
}

#' Build TSF features
#'
#' @param X Matrix of time series with series in rows and time points in
#'   columns.
#' @param n_intervals Number of intervals to sample from each time series. By
#'   default, square root of the number of points per time series.
#' @param seed Random seed for reproducibility.
#'
#' @returns
#' @export
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(30 * 10), nrow = 30, ncol = 10)
#' feat <- build_tsf_features(X, n_intervals = 5)
#' feat$features
#' feat$intervals
build_tsf_features <- function(X, n_intervals = sqrt(ncol(X)), seed = 123) {
  # Initialize
  set.seed(seed)
  n_samples <- nrow(X)
  series_length <- ncol(X)

  # Randomly select intervals
  interval_start <- sample(1:(series_length - 2), n_intervals, replace = TRUE)
  interval_length <- runif(n_intervals, 0, 1)
  interval_end <- ceiling(interval_start + interval_length * (series_length - interval_start))
  intervals <- t(mapply(interval_start, interval_end, FUN = c))

  # Create feature matrix
  feature_matrix <- t(apply(X, MARGIN = 1, FUN = function(series) {
    apply(intervals, MARGIN = 1, FUN = function(interval) {
      extract_interval_features(1:series_length, series, interval[1], interval[2])
    })
  }))
  colnames(feature_matrix) <- unlist(lapply(1:n_intervals, function(i) {
    c(paste0("mean_", i), paste0("sd_", i), paste0("slope_", i))
  }))

  # Return the features and intervals
  list(features = as.data.frame(feature_matrix), intervals = intervals)
}
