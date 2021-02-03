load_package = function(x){
        if(!require(x,character.only = T)) {
                install.packages(x)
                library(x,character.only = T)
        } else {
                library(x,character.only = T)
        }
}

invisible(sapply(c('raster','sf','rnaturalearth','rnaturalearthdata','rgeos','maps',
                   'tidyverse'),load_package))

# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
world = rnaturalearth::ne_countries(scale = 'medium',returnclass = 'sf')
sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005,26.83))
sites <- st_as_sf(sites,coords = c("longitude", "latitude"),crs = 4326,agr = "constant")
ggplot(data = world) + geom_sf(color = 'black') + theme_bw() +
        #scale_fill_viridis_c(option = 'plasma',trans = 'sqrt') +
        geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
        coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = F)

india = rnaturalearth::ne_countries(scale = 'medium',country = 'india',returnclass = 'sf')
india_cities = maps::world.cities %>% filter(country.etc == 'India') %>%
        st_as_sf(coords = c('long','lat'))
ggplot(data = india) + theme_bw() +
        geom_sf() +
        geom_sf(data = india_cities,size = 5,shape = 20,fill = 'red')

# https://stackoverflow.com/questions/30706542/how-to-map-an-indian-state-with-districts-in-r
# https://stackoverflow.com/questions/28322866/mapping-just-one-state-of-india-and-writing-its-name-inside-the-state-boundary
# For india states data
india = raster::getData('GADM',country = 'India',level = 1)
# Choose MP
mp = india %>% subset(NAME_1 == 'Madhya Pradesh') %>% fortify()

ggplot(data = mp) +
        geom_map(map = mp,aes(x = long,y = lat,map_id = id,group = group))
