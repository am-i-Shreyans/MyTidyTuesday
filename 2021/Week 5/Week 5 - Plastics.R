# https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-26

# Tidy Tuesday: 26-Jan

library(tidyverse)
library(tidytuesdayR)

tuesdata = tt_load(2021,5)

plastics = tuesdata$plastics

# Define the not-in operator
`%nin%` = function(x,y) !(x %in% y)

# Top 5 sources of total plastics count per volunteer in India in 2019
top_5_2019 = plastics %>% filter(country == 'India',year == 2019) %>%
        group_by(parent_company = str_to_upper(parent_company)) %>%
        summarise(Total_2019 = sum(grand_total,na.rm = T)/volunteers) %>% arrange(desc(Total_2019)) %>%
        filter(parent_company %nin% c('GRAND TOTAL','UNBRANDED','NULL')) #%>% top_n(10,Total_plastic)

top_5_2020 = plastics %>% filter(country == 'India',year == 2020) %>%
        group_by(parent_company = str_to_upper(parent_company)) %>%
        summarise(Total_2020 = sum(grand_total,na.rm = T)/volunteers) %>% arrange(desc(Total_2020)) %>%
        filter(parent_company %nin% c('GRAND TOTAL','UNBRANDED','NULL')) #%>% top_n(10,Total_plastic)

top_both_year = inner_join(top_5_2019,top_5_2020,by = 'parent_company') %>% ungroup() %>%
        top_n(5,Total_2019)
# Take top of 2019

## Checking the contrast between plastic count of 2019 and 2020 of each plastic variety from the top sources
# Problem in data. No data available for 2020 count of top 5 2019
plastics_india = plastics %>% filter(country == 'India',
                                     str_to_upper(parent_company) %in% top_both_year$parent_company) %>%
        select(-c(empty,grand_total:volunteers)) %>%
        pivot_longer(cols = hdpe:pvc,names_to = 'plastic_type',values_to = 'count')

ggplot(plastics_india,aes(x = plastic_type,y = count)) + geom_bar(stat = 'identity') +
        facet_grid(parent_company~year)

## Total plastic per volunteer for each country in 2019
country_total_2019 = plastics %>% filter(!grepl('total',parent_company,T),!is.na(year)) %>%
        group_by(country,year) %>% summarise(Total = sum(grand_total/volunteers,na.rm = T)) #%>% unique()

world_geo = rgdal::readOGR(dsn = 'D:/Tidy Tuesday/World Borders/TM_WORLD_BORDERS-0.3',
                           layer = 'TM_WORLD_BORDERS-0.3',verbose = T) %>%
        sf::st_as_sf()

plot_data = left_join(world_geo,country_total_2019,by = c('NAME' = 'country'))

ggplot(plot_data %>% filter(!is.na(year)),aes(fill = log2(Total),geometry = geometry)) +
        geom_sf(data = plot_data %>% select(-year),color = NA,fill = 'light grey') +
        geom_sf(color = NA) +
        scale_fill_viridis_c(option = 'plasma') + facet_wrap(year~.,nrow = 2,strip.position = 'left') + theme_void() +
        labs(title = 'Total plastic collected per volunteer in 2019 v/s 2020',
             subtitle = 'Comparison between years of plastic collected per volunteer in different countries',
             fill = expression(log[2]('Count per volunteer')),
             tag = 'Tidy Tuesday/2021/Week-5',
             caption = '@Am_I_Shreyans') +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5,size = 20),
              plot.subtitle = element_text(face = 'italic',hjust = 0.5,size = 15),
              legend.title = element_text(size = 13),
              legend.box.spacing = unit(5,'cm'),
              strip.text.y = element_text(size = 10),
              strip.background.y = element_rect(fill = 'light grey',colour = 'black',size = 0.1),
              plot.caption = element_text(colour = '#1DA1F2',face = 'italic',hjust = 1,vjust = 1),
              plot.tag = element_text(face = 'italic'))

ggsave('D:/Tidy Tuesday/2021/Week-5 Plastics ggsave.png',width = 1524/72,height = 858/72,dpi = 72)

# ggplot(mtcars,aes(x = disp,y = mpg)) + geom_point(aes(fill = cyl)) + labs(fill = expression(log[2]('cyl')))
