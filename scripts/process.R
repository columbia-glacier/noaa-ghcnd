# ---- Install missing dependencies ----

packages <- c("rnoaa")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

# ---- Load functions ----

ghcnd_stations <- function() {
  "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt" %>%
    read.fwf(
      widths = c(11, 9, 11, 7, 2, 31, 5, 10),
      col.names = c("id", "latitude", "longitude", "elevation", "state", "name", "gsn_flag", "wmo_id"),
      header = FALSE, strip.white = TRUE, comment.char = "",
      stringsAsFactors = FALSE)
}

parse_noaa_ghcnd <- function(df) {
  with(df, data.frame(
    stringsAsFactors = FALSE,
    id = id,
    t = {
      date %>%
        format("%Y-%m-%d")
    },
    # Precipitation (tenths of mm)
    precipitation = {
      prcp %>%
        divide_by(10) %>%
        convert_units(from = mm, to = m)
    },
    # Snowfall (mm)
    snowfall = {
      snow %>%
        convert_units(from = mm, to = m)
    },
    # # Water equivalent of snowfall (tenths of mm)
    # snowfall_water = {
    #   wesf %>%
    #     divide_by(10) %>%
    #     convert_units(from = mm, to = m)
    # },
    # Snow depth (mm)
    snow_depth = {
      snwd %>%
        convert_units(from = mm, to = m)
    },
    # Water equivalent of snow on the ground (tenths of mm)
    snow_depth_water = {
      wesd %>%
        divide_by(10) %>%
        convert_units(from = mm, to = m)
    },
    # Average temperature (tenths of degrees C)
    air_temperature_avg = {
      tavg %>%
        divide_by(10)
    },
    # Minimum temperature (tenths of degrees C)
    air_temperature_min = {
      tmin %>%
        divide_by(10)
    },
    # Maximum temperature (tenths of degrees C)
    air_temperature_max = {
      tmax %>%
        divide_by(10)
    }
    # # Daily total sunshine (minutes)
    # sunshine = {
    #   tsun %>%
    #     convert_units(from = min, to = s)
    # }
  ))
}

# ---- Process station data ----

station_ids <- c(
  "USW00026442", # Valdez Weather Service Office
  "USC00509685", # Valdez Municipal Airport
  "USC00509687", # Valdez
  "USC00501240", # Cannery Creek
  "USS0046M04S" # Sugarloaf Mountain
)

# Get metadata
stations <- ghcnd_stations()
# Write metadata to file
stations %>%
  subset(id %in% station_ids, c("id", "name", "longitude", "latitude", "elevation")) %>%
  write.csv(file.path("data", "stations.csv"), quote = FALSE, na = "", row.names = FALSE)
# Get data
meteo <- rnoaa::meteo_pull_monitors(monitors = station_ids) %>%
  parse_noaa_ghcnd() %>%
  remove_empty(ignore = c("id", "t"))
# Write data to file
meteo %>%
  write.csv(file.path("data", "data.csv"), quote = FALSE, na = "", row.names = FALSE)
