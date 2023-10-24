library(sf)
# From https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-nebraska-current-county-subdivision-state-based
# Zipped version is available at srvanderplas.github.io/unl-stat850-slides/deps/spatial/tl_2016_31_cousub.zip
dat <- read_sf("deps/spatial/tl_2016_31_cousub.shp")

head(dat)

skimr::skim(dat)

library(ggplot2)
ggplot(data = dat) + geom_sf()


library(tidyverse)

# Aggregate to form counties (rather than sub-county spatial units)
counties <- dat %>%
  group_by(STATEFP, COUNTYFP) %>%
  summarize(geometry = st_union(geometry))

# plot
counties %>%
  ggplot() + geom_sf()

# Underneath, this is just a list of lat/long points
counties[11,]
counties$geometry[[11]][[1]] %>% plot()


# Question: Why are the points spaced differently?
# Answer: ??? Not entirely sure - could be how they measured/surveyed
# We can use st_simplify to remove unnecessary points, but ...
dat %>%
  group_by(COUNTYFP) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(geometry2 = st_simplify(geometry))

# From stackoverflow: https://stackoverflow.com/questions/60008135/st-simplify-dtolerence-with-decimal-degree

proj1 <- "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
proj2 <- st_crs(counties$geometry)
dat2 <- counties %>%
  ungroup() %>%
  mutate(geometry = geometry %>% st_transform(proj1) %>% st_simplify(dTolerance = 1) %>% st_transform(proj2))

dim(counties$geometry[[11]][[1]])
dim(dat2$geometry[[11]][[1]])
counties$geometry[[11]][[1]] %>% plot()
dat2$geometry[[11]][[1]] %>% points(col = "red")
# So we've reduced the number of points quite a bit...


# We can combine multiple shapefiles to get cool results

# From https://catalog.data.gov/dataset/tiger-line-shapefile-2018-state-nebraska-primary-and-secondary-roads-state-based-shapefile
# Zipped version is available at srvanderplas.github.io/unl-stat850-slides/deps/spatial/tl_2018_prisecroads.zip
roads <- read_sf("deps/spatial/tl_2018_31_prisecroads.shp")
head(roads)

ggplot() +
  geom_sf(data = counties, fill = "white") +
  geom_sf(data = roads, aes(color = RTTYP)) +
  scale_color_discrete("Road Type") +
  theme_bw()

# We can also merge in new data (with or without shape info) using e.g. FIPS codes to indicate state/counties

# From https://coronavirus-resources.esri.com/datasets/628578697fb24d8ea4c32fa0c5ae1843_0
# Downloaded version is available at srvanderplas.github.io/unl-stat850-slides/deps/spatial/COVID-19_Cases_US.csv
covid <- read_csv("deps/spatial/COVID-19_Cases_US.csv") %>%
  # Fips codes
  mutate(STATEFP = stringr::str_sub(FIPS, 1, 2),
         COUNTYFP = stringr::str_sub(FIPS, 3, 5))

# Check what's there - any missing counties?
counties %>%
  anti_join(covid, by = c("STATEFP", "COUNTYFP"))

# get population
# From https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html
# Downloaded version is available at srvanderplas.github.io/unl-stat850-slides/deps/spatial/co-est2019-annres-31.xlsx
pop <- readxl::read_xlsx("deps/spatial/co-est2019-annres-31.xlsx", skip = 5,
                         col_names = c("county", "pop"), col_types = c("text", "numeric", rep("skip", 11))) %>%
  mutate(county = str_remove(county, "^\\.") %>% str_remove(., " County") %>% paste0(., ", US"))
# Here, we have to merge by county name, where the field is structured County, State, US

# Check for missing counties
pop %>%
  anti_join(covid, by = c("county" = "Combined_Key"))
# The only things missing are some non-county rows at the end of the spreadsheet

# Join 'em together
covid <- covid %>%
  left_join(pop, by = c("Combined_Key" = "county"))


# Create a rate statistic and plot using fill
counties %>%
  left_join(covid, by = c("STATEFP", "COUNTYFP")) %>%
  mutate(rate = Confirmed/pop) %>%
  ggplot() +
  geom_sf(aes(fill = rate))

# Add in county names
counties %>%
  left_join(covid, by = c("STATEFP", "COUNTYFP")) %>%
  mutate(rate = Confirmed/pop,
         county_name = str_remove(Combined_Key, ", Nebraska, US")) %>%
  ggplot() +
  geom_sf(aes(fill = rate)) +
  geom_text(aes(x = Long_, y = Lat, label = county_name), color = "white") +
  scale_fill_viridis_c()

