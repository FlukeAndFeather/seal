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
