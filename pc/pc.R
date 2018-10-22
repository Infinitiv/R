library(tidyverse)

df <- read.csv('~/R/data/pc/pc.csv')
names(df) <- c('number', 'title', 'type', 'date', 'year')
d.desc <- df %>% group_by(year) %>% summarise(year_count = n()) %>% arrange(desc(year))%>% mutate(year_sum = cumsum(year_count))
d.asc <- df %>% group_by(year) %>% summarise(year_count = n()) %>% arrange(year)%>% mutate(year_sum = cumsum(year_count))
