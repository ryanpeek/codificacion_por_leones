# reading in telemetry data from csv
# need to fix datetimes with two different formats

# to make a section header: Ctrl + Shift + R

# Load Libraries ----------------------------------------------------------

library(tidyverse) # cleaning and plotting data
library(sf) # spatial package
library(janitor) # to clean names
library(lubridate) # for working with datetimes


# Import Data -------------------------------------------------------------

# read in one of the files
dinga <- read_csv("data/Argos_Dinga.csv")

# access a column with spaces or weird characters with back ticks ``
dinga$`Error radius`

# we can rename to clean names to make things easier with janitor package
# you can access a function within a package by using the "::" like 'package::function()'
dinga_clean <- dinga %>% janitor::clean_names()
str(dinga_clean)


# Clean the Datetimes -----------------------------------------------------

# format one of the datetime types with lubridate function dmy / mdy / ymd
dinga_dates <- dinga_clean %>%
  # note handy ".after" function which adds the new column right after date (instead of at end)
  mutate(datetime1 = lubridate::dmy_hm(date), .after=date) %>%
  separate(col=date, into = c("HMS", "date2"), sep = " ", remove = FALSE)

# the Warning message is ok! it says 121 rows didn't work, so those must be the other datetime format.

# Now fix the other date time format:
# separate and switch the date and times (so date is first and time is second)
dinga_dates <- dinga_dates %>%
  # case_when() is like an ifelse statement, give it a condition (or multiple conditions)
  # and then what you want value to be after the "~"
  mutate(datetime2 = case_when(
    is.na(datetime1) ~ paste0(date2, " ", HMS),
    TRUE ~ NA_character_ # must match the same class of data, which is character here
    ), .after=datetime1)


# Now Merge the Columns back together -------------------------------------

# format and merge the datetimes
dinga_datetimes <- dinga_dates %>%
  # first format the second datetime column properly (as POSIX)
  mutate(datetime2= lubridate::dmy_hms(datetime2),
  # now use case when to say if NA in one col, use the other
         datetime = case_when(
           is.na(datetime1) ~ datetime2,
           is.na(datetime2) ~ datetime1
         ), .after=date) %>%
  # drop the old temporary columns
  select(-c(datetime2, datetime1, HMS, date2))


# Make Data Spatial -------------------------------------------------------

# convert our dataframe to an "sf" object so we can use it for mapping
dinga_sf <- dinga_datetimes %>%
  # pull a subset of columns of interest
  select(deploy_id, ptt, datetime, type, quality, latitude, longitude) %>%
  # convert to sf
  sf::st_as_sf(coords=c("longitude", "latitude"), remove=FALSE, crs=4326) %>%
  # add day for plotting purpose:
  mutate(days=yday(datetime))

# simple static plot
plot(dinga_sf$geometry)


# Make a dynamic Map with Mapview -----------------------------------------

library(mapview)
mapviewOptions(fgb = FALSE) # need this so it will display in browser...weird bug
mapview(dinga_sf, zcol="days") # color by day of observation
mapview(dinga_sf, zcol="quality") # color by quality


# Make a static map: first get boundary data ------------------------------------

## Boundaries: http://www.soest.hawaii.edu/pwessel/gshhg/

# I had to install these all this way
## devtools::install_github("ropensci/rnaturalearth")
## devtools::install_github("ropensci/rnaturalearthdata")
## devtools::install_github("ropensci/rnaturalearthhires")

# these packages are helpful for boundaries
library(rnaturalearth)
library(USAboundaries)

# get mexico boundary:
mex_bound_sf <- ne_countries(scale = 50, country="Mexico", returnclass = "sf")

# get mexico states (requires hires package)
mex_sf <- ne_states(country="Mexico", returnclass = "sf")

# now get boundaries for CA, Arizona, and NV
ca_az_sf <- USAboundaries::us_boundaries(type = "state", resolution = "high", states = c("california", "arizona", "nevada", "utah"))

# this pulls coastline info for whole globe
coast_sf <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf")

# combine mexico with the states:
boundaries <- st_union(mex_bound_sf, ca_az_sf)

# simple plot
plot(dinga_sf$geometry, bg=alpha("orange",0.5), pch=21, bgc=alpha("steelblue",0.5))
plot(boundaries$geometry, border=alpha("darkgreen", alpha=0.9), col="gray90", lwd=1, add=T)

# Make Static Plots -------------------------------------------------------

library(cartography)

# get background:
mex.osm <- getTiles(
  x = boundaries,
  type = "OpenStreetMap",
  zoom = 5, # the bigger this number the longer it takes!
  crop = TRUE
)

# warnings ok

# set the map parameters (so small margins)
opar <- par(mar = c(0,0,1.2,0))

# plot osm tiles
tilesLayer(x = mex.osm)

# Plot the dinga data
plot(boundaries$geometry, border="darkseagreen4",
     bg = NULL, lwd = 1.5, add=T)
plot(mex_sf$geometry, border="darkgreen", bg=NULL, lwd=0.25, add=T)
plot(dinga_sf$geometry, bg="orange", col="gray20", pch=21, add=T)

# Plot a layout
layoutLayer(title = "Dinga",
            sources = "Sources: majo, 2018\n© OpenStreetMap contributors.\nTiles style under CC BY-SA, www.openstreetmap.org/copyright.",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, scale = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "topleft", x = dinga_sf)

# this doesn't work because data are in degrees currently...need to transform
#barscale(pos="bottomright", style = "pretty", size = .01)

# restore graphics parameters
par(opar)
