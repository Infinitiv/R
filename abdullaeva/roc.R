library(tidyverse)
library(ROCR)
a <- 0.05
df <- read.csv("../data/abdullaeva/roc1.csv", check.names = F)
df <- select(df, - patient)
nums <- sapply(df, is.numeric)
cor <- cor(df[, nums], method = 'spearman', use = 'complete.obs')
diag(cor) <- 0
cor > 0.7
cor < -0.7
tapply(df, df$group, shapiro.test)
cor.test(df$`CD20+`, df$`CD19+`, method = 'spearman')
cor_names <- c('group')
for(i in 2:24){
  w <- wilcox.test(df[, i] ~ df$group)$p.value
  if(w < 0.05){
    print(paste(names(df)[i], round(w, 6), sep = ' '))
    cor_names <- c(cor_names, names(df)[i])
  }
}
df_select <- select(df, cor_names)
cor <- cor(df_select, method = 'spearman', use = 'complete.obs')
fit <- glm(group ~ `CD27+` + `IL-10+`, df, family = 'binomial')
summary(fit)
df_select_filter <- filter(df_select, !is.na(`CD27+`), !is.na(`IL-10+`))
df_select_filter$predicted <- predict(fit, type = 'response')
pred_fit <- prediction(df_select_filter$predicted, df_select_filter$group)
perf_fit <- performance(pred_fit, 'tpr', 'fpr')
plot(perf_fit, colorize = T, print.cutoffs.at = seq(0, 1, by = 0.1))
write.csv(df_select_filter, file = 'roc/roc.csv')
