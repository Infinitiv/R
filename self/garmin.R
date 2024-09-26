library(tidyverse)
library(jsonlite)

df <- fromJSON('/home/markovnin/weight_.json')

df$muscleMass <- df$muscleMass/1000
df$weight <- df$weight/1000
df$date <- as.Date(df$date)
df <- df %>% mutate(fatMass = weight*bodyFat/100, month = as.factor(lubridate::month(date)), year = as.factor(lubridate::year(date)), round_weight = round(weight, 1)) %>% arrange(date)
df <- df %>% arrange(round_weight, date) %>% filter(!is.na(bmi))
ggplot(filter(df, !is.na(fatMass))) + geom_point(aes(x = bodyFat, y = muscleMass/fatMass), position = 'jitter') + geom_point(aes(x = bodyFat, y = muscleMass/10), position = 'jitter')
ggplot(filter(df, !is.na(fatMass))) + geom_point(aes(x = date, y = muscleMass/weight)) + geom_smooth(aes(x = date, y = muscleMass/weight))
ggplot(filter(df, !is.na(fatMass))) + geom_point(aes(x = bodyFat, y = boneMass))
ggplot(filter(df, !is.na(fatMass))) + geom_point(aes(x = date, y = muscleMass)) + geom_smooth(aes(x = date, y = muscleMass), method = 'lm') + geom_smooth(aes(x = date, y = bodyFat), method = 'lm')

d <- df %>% group_by(year, month) %>% summarise(mean_weight = mean(weight))
ggplot(d, aes(x = paste(year, sprintf("%02d", month), sep = '-'), y = mean_weight, group = 1)) + 
  geom_point(aes(color = year)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab('Дата, мес.') +
  ylab('Вес, кг')
ggplot(df, aes(x = year, y = weight))   + geom_boxplot()
d <- df %>% filter(!is.na(bmi)) %>% group_by(year, month) %>% summarise(meanmuscleMass = mean(muscleMass), meanweight = mean(weight)) %>% mutate(yearmonth = paste(year, month, sep = '-'))
ggplot(d, aes(x = yearmonth, y = meanmuscleMass/meanweight)) + geom_point()

activities <- fromJSON('/home/markovnin/activities.json')
activities <- as.data.frame(activities$activityList)
