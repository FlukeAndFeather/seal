#' Assign a dive identifier
#'
#' Group depth records together into dives. Dives are defined by excursions
#' deeper than 1 meter exceeding 30 s duration and 15 m depth.
#'
#' @param t Date and time of the depth record (POSIXct)
#' @param d Depth record in meters (numeric)
#'
#' @returns A numeric vector the same length as t and d, with numbers for dive
#'   identifiers and NAs indicating non-dive periods.
#' @export
#'
#' @examples
#' assign_dive_id(seal3d$time, seal3d$depth)
assign_dive_id <- function(t, d) {
  # Set the surface threshold
  surface <- 1
  # Identify the start of excursions
  is_dive <- d > surface
  prev_dive <- c(FALSE, is_dive[-length(is_dive)])
  new_dive <- is_dive & !prev_dive
  # Assign initial dive identifiers
  dive_id0 <- ifelse(is_dive, cumsum(new_dive), NA)
  # Reassign dive identifiers based on duration and depth thresholds
  min_dur_s <- 30
  min_depth_m <- 15
  ## Summarize initial dives and assess validity
  corrected_dives <- tibble::tibble(t, d, dive_id0) %>%
    tidyr::drop_na(dive_id0) %>%
    dplyr::group_by(dive_id0) %>%
    dplyr::summarize(dur_s = as.numeric(max(t) - min(t), unit = "secs"),
                     depth_m = max(d)) %>%
    dplyr::mutate(valid_dive = dur_s >= min_dur_s & depth_m >= min_depth_m) %>%
    dplyr::mutate(dive_id = ifelse(valid_dive, cumsum(valid_dive), NA))
  ## Return corrected dive identifiers
  tibble::tibble(dive_id0) %>%
    dplyr::left_join(dplyr::select(corrected_dives, dive_id0, dive_id),
                     by = "dive_id0") %>%
    dplyr::pull(dive_id)
}

#' Preprocess dives for sktime
#'
#' @param dive_df A data frame containing dive data with columns `dive_id`,
#'   `time`, and `depth`.
#'
#' @returns A sktime-friendly data frame with dives interpolated to a fixed
#'   length.
#' @export
#'
#' @examples
#' preprocess_dives(seal3d)
preprocess_dives <- function(dive_df) {
  # Summarize dives
  dive_summary <- dive_df %>%
    tidyr::drop_na(dive_id) %>%
    dplyr::group_by(dive_id) %>%
    dplyr::summarize(n = dplyr::n(),
                     depth = max(depth))
  max_n <- max(dive_summary$n)

  # Interpolate dives to a fixed length
  dive_standard <- dive_df %>%
    tidyr::drop_na(dive_id) %>%
    dplyr::group_by(dive_id) %>%
    dplyr::group_modify(\(rows, groups) {
      tibble::tibble(
        time = seq(max_n),
        depth = approx(seq_along(rows$depth), rows$depth, n = max_n)$y
      )
    }) %>%
    dplyr::ungroup()

  # Convert to sktime-friendly format
  dive_py <- reticulate::r_to_py(dive_standard)
  dive_py$set_index(c("dive_id", "time"))
}

#' Slice a dive, preserving multi-index
#'
#' @param dives A preprocessed dives data frame created by `preprocess_dives()`.
#' @param dive_id A dive identifier (numeric) to slice the dives data frame.
#'
#' @returns A sliced data frame containing only the records for the specified
#'   dive (with multi-index)
#' @export
slice_dive <- function(dives, dive_id) {
  # I can't believe this is necessary to preserve the multi-index
  dives$loc[reticulate::tuple(dive_id, py_slice(NULL)),
            py_slice(NULL)]
}

add_dive <- function(dives, dive_id) {
  py_run_string("x = 10")
}
