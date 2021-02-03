library(tidyverse)
library(tidytuesdayR)
if(!require('rKenyaCensus',character.only = T)){
        devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
        library('rKenyaCensus',character.only = T)
} else {
        library('rKenyaCensus',character.only = T)
}

tuesdata = tidytuesdayR::tt_load(2021,4)

gender = tuesdata$gender
household = tuesdata$households
crops = tuesdata$crops

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-19/readme.md
rKenyaCensus::DataCatalogue %>% knitr::kable()

data_summ = function(x){
        x %>% as_tibble() %>% print()
        x %>% str()
}
data_summ(V4_T2.2)
V4_T2.2 %>% filter(SubCounty == 'KENYA')
x = KenyaCounties_SHP
