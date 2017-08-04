library(magrittr)

# ---- Load functions ----

ghcnd_stations <- function() {
  "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt" %>%
    read.fwf(
      widths = c(11, 9, 11, 7, 2, 31, 5, 10),
      col.names = c("id", "latitude", "longitude", "elevation", "state", "name", "gsn_flag", "wmo_id"),
      header = FALSE, strip.white = TRUE, comment.char = "",
      stringsAsFactors = FALSE)
}

# ---- Load parsers ----

station_parsers <- list(
  id = function(id) {
    id
  },
  name = function(name) {
    name
  },
  longitude = function(longitude) {
    longitude %>%
      units2::as_units("°") %>%
      dpkg::set_field(description = "Longitude (unknown datum)")
  },
  latitude = function(latitude) {
    latitude %>%
      units2::as_units("°") %>%
      dpkg::set_field(description = "Latitude (unknown datum)")
  },
  elevation = function(elevation) {
    elevation %>%
      units2::as_units("m") %>%
      dpkg::set_field(description = "Elevation (unknown datum)")
  }
)

data_parsers <- list(
  station_id = function(id) {
    id %>%
      dpkg::set_field(description = "Station identifier")
  },
  t = function(date) {
    date %>%
      dpkg::set_field(description = "Date (AKST:UTC-9)", format = "%Y-%m-%d")
  },
  # Precipitation (tenths of mm)
  precipitation = function(prcp) {
    prcp %>%
      divide_by(10) %>%
      units2::convert_units("mm", "m") %>%
      dpkg::set_field(description = "")
  },
  # Snowfall (mm)
  snowfall = function(snow) {
    snow %>%
      units2::convert_units("mm", "m")
  },
  # Water equivalent of snowfall (tenths of mm)
  snowfall_lwe = function(wesf) {
    wesf %>%
      divide_by(10) %>%
      units2::convert_units("mm", "m")
  },
  # Snow depth (mm)
  snow_thickness = function(snwd) {
    snwd %>%
      units2::convert_units("mm", "m")
  },
  # Water equivalent of snow on the ground (tenths of mm)
  snow_thickness_lwe = function(wesd) {
    wesd %>%
      divide_by(10) %>%
      units2::convert_units("mm", "m")
  },
  # Average temperature (tenths of degrees C)
  air_temperature_avg = function(tavg) {
    tavg %>%
      divide_by(10) %>%
      units2::as_units("°C")
  },
  # Minimum temperature (tenths of degrees C)
  air_temperature_min = function(tmin) {
    tmin %>%
      divide_by(10) %>%
      units2::as_units("°C")
  },
  # Maximum temperature (tenths of degrees C)
  air_temperature_max = function(tmax) {
    tmax %>%
      divide_by(10) %>%
      units2::as_units("°C")
  },
  # Daily total sunshine (minutes)
  sunshine_duration = function(tsun) {
    tsun %>%
      units2::convert_units("min", "s")
  }
)

# ---- Get data ----

station_ids <- c(
  "USW00026442", # Valdez Weather Service Office
  "USC00509685", # Valdez Municipal Airport
  "USC00509687", # Valdez
  "USC00501240", # Cannery Creek
  "USS0046M04S" # Sugarloaf Mountain
)

# Get station metadata
stations <- ghcnd_stations() %>%
  subset(id %in% station_ids)

# Get station data
data <- rnoaa::meteo_pull_monitors(monitors = station_ids)

# ---- Build data package ----

dp <- list(
  stations = {
    stations %>%
      cgr::parse_table(station_parsers) %>%
      dpkg::set_resource(
        title = "Station metadata",
        path = "data/stations.csv"
      )
  },
  data = {
    data %>%
      cgr::parse_table(data_parsers) %>%
      cgr::remove_empty_dimensions(ignore = c("station_id", "t")) %>%
      dpkg::set_resource(
        title = "Station data",
        path = "data/data.csv",
        schema = dpkg::schema(
          foreignKeys = list(
            dpkg::foreignKey("station_id", "stations", "id")
          )
        )
      )
  }
) %>%
  dpkg::set_package(
    name = "noaa-ghcnd",
    title = "NOAA Daily Global Historical Climatology Network (GHCN-DAILY)",
    description = "Meteorological observations from nearby land surface weather stations.",
    version = "0.1.0",
    contributors = list(
      dpkg::contributor("Ethan Welty", email = "ethan.welty@gmail.com", role = "author")
    ),
    sources = list(
      dpkg::source("NOAA GHCN-DAILY Data File Access", path = "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/")
    )
  )

# ---- Write data package ----

dp %>%
  dpkg::write_package()