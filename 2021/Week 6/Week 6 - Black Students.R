# TidyTuesday: 2021/Week-6
# https://github.com/am-i-Shreyans/tidytuesday/tree/master/data/2021/2021-02-02

library(tidytuesdayR)
library(tidyverse)

tuesdata = tt_load('2021-02-02')

# Each column in representation of the % of total people > 25 age who are bachelors graduate
bach_students = tuesdata$bach_students %>%
        # Removing the SE columns
        select(!contains('error')) %>%
        # Selecting only the useful columns and renaming them
        select(Year = Total,Total = `Total, percent of all persons age 25 and over`,
               White = White1,Black = Black1,Hispanic,
               Asian_PcfInd = `Total - Asian/Pacific Islander`,
               AmIndia_Alaskan = `American Indian/\r\nAlaska Native`,
               Two_or_more = `Two or more race`) %>%
        # Converting the non-numeric columns to numeric type
        mutate_if(~!is.numeric(.),as.numeric) %>%
        # Adding a column to check if there is any NA in the row
        mutate(NAs = is.na(rowSums(across(White:Two_or_more)))) %>%
        # Removing all the rows that contain NA
        filter(!NAs) %>% select(-NAs) %>%
        pivot_longer(Total:Two_or_more,names_to = 'Race',values_to = 'Percent')

# Time series plot
ggplot(bach_students,aes(x = Year,y = Percent)) + geom_line(aes(color = Race,group = Race),size = 1) +
        scale_x_continuous(breaks = bach_students$Year) +
        scale_colour_viridis_d(option = 'C',labels = c('American Indian/Alaskan',
                                                       'Asian/Pacific Indian','Black','Hispanic','Total',
                                                       'Two or more races','White')) +
        labs(title = 'Percent of people age >= 25 yrs. that had attained bachelors degree',
             tag = 'TidyTuesday/2021/Week-6',
             caption = '@Am_i_Shreyans') +
        theme_minimal() + theme(panel.grid.minor = element_blank(),
                                legend.title = element_text(hjust = 0.5),
                                plot.title = element_text(hjust = 0.5),
                                plot.tag.position = 'topright')

hbcu_al = tuesdata$hbcu_all
