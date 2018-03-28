library(tidyverse)
library(ROCR)
# устанавливаем уровень достоверности
a <- 0.05

# читаем данные
df <- read.csv("~/R/data/korobov/korobov.csv", check.names = F)
cor(select(df, Тропонин, Креатинин, СКФ, ЛПНП, Лейкоциты), use = 'pairwise.complete.obs')
df_select <- select(df, -`№ ИБ`)
fit <- glm(Группа ~ Тропонин + ЛПНП + Лейкоциты, df_select, family = 'binomial')
df$predict <- predict(fit, newdata = df_select, type = 'response')
write.csv(df, file = '~/R/stud_stat/korobov/roc.csv')
pred_fit <- prediction(df$predict, df$Группа)
perf_fit <- performance(pred_fit, 'tpr', 'fpr')
plot(perf_fit, colorize = T, print.cutoffs.at = seq(0, 1, by = 0.1))