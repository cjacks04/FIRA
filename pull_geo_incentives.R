library(stringr)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidycensus)
library(tigris)
library(mapview) #devtools::install_github("r-spatial/mapview")
# Geocode id for Google
register_google(key = 'AIzaSyBqJnGUAYDNRszPjaOfySDBgfwV-Uucb3o') 

## Import dataset
interventions <- read_csv("~/Library/CloudStorage/Box-Box/FIRA Writing/Interventions/Encouragement-Strategies-1222022.csv")

### Clean dataset
interventions <- interventions[,c(1:24)]
interventions$Address <- str_squish((str_replace_all(interventions$Address, regex("\\W+"), " "))) # remove non asiacc characters 
interventions$address_combined <- str_c(interventions$Address,", ",interventions$City,", ",interventions$State)
interventions_original <- interventions
interventions <- interventions[which(!is.na(interventions$address_combined)),]# remove interventions with no address to lookup

### Loop throughout data and produce lat/lon information
for(i in 1:nrow(interventions))
  {
    # Print("Working...")
    result <- geocode(interventions$address_combined[i], output = "latlona", source = "google")
    interventions$lon[i] <- as.numeric(result[1])
    interventions$lat[i] <- as.numeric(result[2])
    interventions$geoAddress[i] <- as.character(result[3])
}


# Create a map of locations with county boundaries
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

# get lat/lon estimates
sites <- data.frame(longitude = interventions$lon, latitude = interventions$lat)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("wisconsin", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Wisconsin Map
wisconsin_interventions <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_point(data = sites, aes(x= longitude,y=latitude)) +
  coord_sf(xlim = c(-86.5, -93.75), ylim = c(42.5, 47), expand = FALSE)

# Milwaukee Map
milwaikee_interventions <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_point(data = sites, aes(x= longitude,y=latitude)) +
  coord_sf(xlim = c(-87.7, -88.2), ylim = c(42.8, 43.2), expand = FALSE)

# Census tract information
#https://walker-data.com/census-r/mapping-census-data-with-r.html 

la_tracts <- tracts("WI", "Milwaukee")
plot(la_tracts$geometry)
dc_block_groups <- block_groups("WI")
plot(dc_block_groups$geometry)

ggplot(la_tracts) + 
  geom_sf() + 
  theme_void() + 
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_point(data = sites, aes(x= longitude,y=latitude)) +
  coord_sf(xlim = c(-87.7, -88.2), ylim = c(42.8, 43.2), expand = FALSE)

