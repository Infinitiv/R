library(tidyverse)

df <- read.csv('/home/markovnin/R/data/g-point/g-point.csv')
df <- df %>% arrange(date, desc(summa), desc(stage.6), desc(stage.5), desc(stage.4), desc(stage.3), desc(stage.2), desc(stage.1))
n <- length(unique(df$date))
for(i in 1:n){
  df$game_rank[df$date == unique(df$date)[i]] <- seq(1, length(df$date[df$date == unique(df$date)[i]]))
}
report <- df %>% group_by(title) %>% 
  summarise(season_sum = ifelse(n() < n, sum(summa), sum(summa) - min(summa)), rank_sum = sum(game_rank)) %>% 
  arrange(desc(season_sum), rank_sum)
report$season_rank <- seq(1, length(report$title))
names(report) <- c('Место в сезоне', 'Название команды', 'Сумма баллов', 'Сумма мест в турах')
write.csv(select(report, season_rank, title, season_sum), file = "/home/markovnin/R/data/g-point/report.csv")
                                                                                                 