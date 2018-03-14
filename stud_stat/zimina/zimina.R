library(tidyverse)
a <- 0.05
df <- read.csv("~/R/data/zimina/zimina.csv", check.names = F)
df$group <- ifelse(df$Группа == 1 | df$Группа == 2, 'Астма + поражение ВОПТ', 'Изолированная астма')
df$salbutamol_test <- ifelse(abs(df$ОФВ1 - df$`ОФВ1 с Sal`) > 12, 'положительный', 'отрицательный')
df$control <- ifelse(df$ОФВ1 > 80, 'контроль', 'нет контроля')
df$treatment_scale <- df$`Баллы терапии`
df$reflux <- ifelse(df$Рефлюкс == 0, 'рефлюкса нет', 'рефлюкс есть')
df_select <- select(df, group, salbutamol_test, control, treatment_scale, reflux)
wilcox_group <- wilcox.test(treatment_scale ~ group, df_select)
means_group <- tapply(df_select$treatment_scale, df_select$group, function(x) mean(x, na.rm = T))
mean_se_group <- tapply(df_select$treatment_scale, df_select$group, function(x) mean_se(x, mult = 1.96))
wilcox_reflux <- wilcox.test(treatment_scale ~ reflux, df_select)
means_reflux <- tapply(df_select$treatment_scale, df_select$reflux, function(x) mean(x, na.rm = T))
png(filename = "~/R/stud_stat/zimina/output/Сравнение групп по баллам базисной терапии.png", width = 800, height = 800)
ggplot(df_select, aes(x = group, y = treatment_scale, color = group)) + 
  stat_summary(fun.data = mean_se_group) + 
  ylab('Балл терапии') + xlab('Группа') + 
  ggtitle(paste('Сравнение групп по баллам базисной терапии', 'p =', round(wilcox_group$p.value, 6), sep = ' ')) +
  theme_bw() +
  theme(legend.position = 'none')
dev.off()

