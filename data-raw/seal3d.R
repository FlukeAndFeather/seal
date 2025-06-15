## code to prepare `seal3d` dataset goes here

# Open connection to NetCDF file for one deployment
dive_nc <- ncdf4::nc_open(here::here("data-raw/large-data/2017_RawCurated/2017007_TrackTDR_RawCurated.nc"))

# Clean locations with aniMotum (note: there's a weird installation process)
## Prepare data
loc_df <- tibble::tibble(
  id = paste(ncdf4::ncatt_get(dive_nc, 0, "Animal_ID")$value,
             ncdf4::ncatt_get(dive_nc, 0, "Deployment_ID")$value,
             sep = "_"),
  date = as.POSIXct(ncdf4::ncvar_get(dive_nc, "RAW_ARGOS/DATE"), tz = "UTC"),
  lc = ncdf4::ncvar_get(dive_nc, "RAW_ARGOS/CLASS"),
  lon = ncdf4::ncvar_get(dive_nc, "RAW_ARGOS/LON1"),
  lat = ncdf4::ncvar_get(dive_nc, "RAW_ARGOS/LAT1"),
  smaj = ncdf4::ncvar_get(dive_nc, "RAW_ARGOS/SEMIMAJOR"),
  smin = ncdf4::ncvar_get(dive_nc, "RAW_ARGOS/SEMIMINOR"),
  eor = ncdf4::ncvar_get(dive_nc, "RAW_ARGOS/EOR")
)
## Fit movement persistence model
loc_fit <- aniMotum::fit_ssm(loc_df,
                             model = "mp",
                             time.step = 24)
loc_fitted <- aniMotum::grab(loc_fit, what = "fitted")

# Extract dive data

## Assign a dive identifier
### Minimum 30 s duration, 15 m depth
assign_dive_id <- function(t, d) {
  surface <- 1
  is_dive <- d > surface
  prev_dive <- c(FALSE, is_dive[-length(is_dive)])
  new_dive <- is_dive & !prev_dive
  dive_id0 <- ifelse(is_dive, cumsum(new_dive), NA)
  min_dur_s <- 30
  min_depth_m <- 15
  corrected_dives <- tibble::tibble(t, d, dive_id0) %>%
    tidyr::drop_na(dive_id0) %>%
    dplyr::group_by(dive_id0) %>%
    dplyr::summarize(dur_s = as.numeric(max(t) - min(t), unit = "secs"),
                     depth_m = max(d)) %>%
    dplyr::mutate(valid_dive = dur_s >= min_dur_s & depth_m >= min_depth_m) %>%
    dplyr::mutate(dive_id = ifelse(valid_dive, cumsum(valid_dive), NA))
  tibble::tibble(dive_id0) %>%
    dplyr::left_join(dplyr::select(corrected_dives, dive_id0, dive_id),
                     by = "dive_id0") %>%
    dplyr::pull(dive_id)
}

seal3d <- tibble::tibble(
  time = as.POSIXct((ncdf4::ncvar_get(dive_nc, "CLEAN_ZOC_TDR1/DATE") - 719529) * 86400,
                    tz = "UTC"),
  depth = as.vector(ncdf4::ncvar_get(dive_nc, "CLEAN_ZOC_TDR1/CORR_DEPTH")),
  # Interpolate lon, lat from fitted movement persistence model
  lon = approx(loc_fitted$date, loc_fitted$lon, xout = time)$y,
  lat = approx(loc_fitted$date, loc_fitted$lat, xout = time)$y,
  # Assign dive id
  dive_id = assign_dive_id(time, depth)
)

usethis::use_data(seal3d, overwrite = TRUE)
