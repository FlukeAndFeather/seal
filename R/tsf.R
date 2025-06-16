#' Create Time Series Forest Classifier
#'
#' Create a Time Series Forest Classifier using the sktime package.
#'
#' @param min_interval Minimum interval length
#' @param n_estimators Number of estimators
#' @param n_jobs Number of jobs to run in parallel
#' @param random_state Random state for reproducibility
#'
#' @returns A TimeSeriesForestClassifier object from the sktime package.
#' @export
#'
#' @examples
#' \dontrun{
#'  tsf_model <- tsf(min_interval = 3, n_estimators = 200, n_jobs = 1, random_state = 42)
#'  # Add a test example
#' }
tsf <- function(min_interval = 3, n_estimators = 200, n_jobs = 1, random_state = NULL) {
  sktime$classification$interval_based$TimeSeriesForestClassifier(
    min_interval = as.integer(min_interval),
    n_estimators = as.integer(n_estimators),
    inner_series_length = NULL,
    n_jobs = as.integer(n_jobs),
    random_state = as.integer(random_state)
  )
}

#' Fit Time Series Forest Classifier
#'
#' Fits a Time Series Forest Classifier to the provided time series data and
#' labels.
#'
#' @param tsf A TimeSeriesForestClassifier object created using the `tsf`
#'   function.
#' @param X A data frame or matrix containing the time series data.
#' @param y A vector containing the labels for the time series data.
#'
#' @returns The fitted TimeSeriesForestClassifier object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create X, y
#'   tsf_model <- tsf(min_interval = 3, n_estimators = 200, n_jobs = 1, random_state = 42)
#'   tsf_fit(tsf_model, X, y)
#' }
tsf_fit <- function(tsf, X, y) {
  stopifnot(inherits(tsf, "sktime.classification.interval_based._tsf.TimeSeriesForestClassifier"))
  tsf2 <- tsf
  tsf2$fit(X, y)
  tsf
}

#' Predict with Time Series Forest Classifier
#'
#' Predicts the class probabilities for the provided time series data using a
#' fitted TimeSeriesForestClassifier.
#'
#' @param tsf A fitted TimeSeriesForestClassifier object created using
#'   `tsf_fit()`.
#' @param X A data frame or matrix containing the time series data to predict
#'   on.
#'
#' @returns A matrix of class probabilities for each time series in `X`.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Do something
#' }
tsf_predict <- function(tsf, X) {
  stopifnot(inherits(tsf, "sktime.classification.interval_based._tsf.TimeSeriesForestClassifier"))
  tsf$predict_proba(X)
}
