library(tidyverse)

a <- 0.95

df <- read.csv('/home/markovnin/R/data/gpmu/2018.csv')
df$Team <- as.factor(df$Team)
result <- df %>% group_by(Stage) %>% mutate(Rank = min_rank(desc(Mark))) %>% arrange(Team)
ggplot(filter(result, Stage == 1 || Stage == 3 || Stage == 4), aes(x = Stage, y = Rank, color = Team)) + geom_point() + geom_line()

cor.test(filter(result, Stage == 4)$Mark, filter(result, Stage == 3)$Mark, method = 'pearson')
