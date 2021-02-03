# From r-gallery

load_package = function(x){
        if(!require(x,character.only = T)) {
                install.packages(x)
                library(x,character.only = T)
        } else {
                library(x,character.only = T)
        }
}

invisible(sapply(c('rnaturalearth','broom','tidyverse'),load_package))

# Consider taking the 'sf' from rnaturalearth::ne_countries (better quality)
world_sf = ne_countries(scale = 'medium',returnclass = 'sf')
ggplot(world_sf) + geom_sf()

# Reading shapefile with rgdal package
# my_spdf = rgdal::readOGR(dsn = 'D:/Tidy Tuesday/World Borders/TM_WORLD_BORDERS_SIMPL-0.3',
#                          layer = 'TM_WORLD_BORDERS_SIMPL-0.3',verbose = T)

ggplot(countries_sf) + geom_sf(aes(fill = AREA))

# Only india
india_sf = ne_countries(scale = 'medium',country = 'India',returnclass = 'sf')

ggplot(india_sf) + geom_sf()

# Using the downloaded large shapefiles (India)
# Has better details
my_spdf = rgdal::readOGR(dsn = 'D:/Tidy Tuesday/World Borders/TM_WORLD_BORDERS-0.3',
                         layer = 'TM_WORLD_BORDERS-0.3',verbose = T)
ggplot(sf::st_as_sf(my_spdf)) + geom_sf()


india_spdf = my_spdf[my_spdf@data$NAME == 'India',] %>% sf::st_as_sf()
ggplot(india_spdf) + geom_sf()

# India state level
# Best quality
india_state = raster::getData('GADM',country = 'India',download = F,level = 1)

ggplot(sf::st_as_sf(india_state)) + geom_sf()
