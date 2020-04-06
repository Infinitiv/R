library(tidyverse)

df <- read.csv('~/R/data/pc/pc2019.csv')
names(df) <- c('code', 'year', 'date', 'name', 'count', 'division', 'type')
df$age <- 'старый'
df$age[df$year >= 2014] <- 'новый'
df$age[df$year < 2007] <- 'очень старый'
d.desc <- df %>% group_by(year) %>% summarise(year_count = n()) %>% arrange(desc(year))%>% mutate(year_sum = cumsum(year_count))
d.asc <- df %>% group_by(year) %>% summarise(year_count = n()) %>% arrange(year)%>% mutate(year_sum = cumsum(year_count))
d.divisions <- df %>% group_by(division, type, age) %>% summarise(count = n()) %>% arrange(division, desc(age))
write_csv(d.divisions, '~/R/data/pc/pc2019_export.csv')
