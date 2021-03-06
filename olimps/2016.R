library('ggplot2')
d <- read.csv('2016.csv')
d$Номер.ситуации <- as.factor(d$Номер.ситуации)
d$Команда.этап <- paste(d$Команда, d$Этап.оказания.помощи, sep = " - ")
teams <- levels(d$Команда)
stages <- levels(d$Этап.оказания.помощи)
summa <- tapply(d$Оценка.команды, d$Команда.этап, sum)
df <- data.frame(teams, stages, summa)
ggplot(df, aes(y = summa, x = teams, fill = stages)) + geom_point()
