library(tidyverse)
# устанавливаем уровенть достоверности
a <- 0.05

# читаем данные
df <- read.csv("~/R/data/zimina/zimina.csv", check.names = F)

# создаем пользовательские переменные
df$group <- ifelse(df$Группа == 1 | df$Группа == 2, 'Астма + поражение ВОПТ', 'Изолированная астма')
df$sex <- ifelse(df$Пол == 'м', 'мужской', 'женский')
df$age <- df$Возраст
df$salbutamol_test <- ifelse(abs(df$ОФВ1 - df$`ОФВ1 с Sal`) > 12, 'положительный', 'отрицательный')
df$control <- ifelse(df$ОФВ1 > 80, 'контроль', 'нет контроля')
df$treatment_scale <- df$`Баллы терапии`
df$reflux <- ifelse(df$Рефлюкс == 0, 'рефлюкса нет', 'рефлюкс есть')

# выбираем переменные для анализа
df_select <- filter(select(df, group, sex, age, salbutamol_test, control, treatment_scale, reflux), age < 17, treatment_scale > 0)
# считаем достоверность различия баллов терапии по группа исследования
wilcox_group <- wilcox.test(treatment_scale ~ group, df_select)

# описательная статистика
cor <- cor.test(df_select$age, df_select$treatment_scale, method = 'spearman')
ggplot(df_select) + 
  geom_point(aes(x = age, y = treatment_scale, color = sex), position = 'jitter') +
  geom_smooth(aes(x = age, y = treatment_scale), method = 'lm') +
  theme_bw() +
  ylab('Балл терапии') + 
  xlab('Возраст') +
  theme(legend.title = element_blank()) +
  ggtitle(paste('Корелляция между возрастом и баллами терапии - ', 'rho = ', round(cor$estimate, 4), ' (p = ', round(cor$p.value, 6), ')', sep = ''))

# считаем достоверность различия баллов терапии в зависимости от наличия рефлюкса
df_select_reflux <- filter(df_select, group == 'Астма + поражение ВОПТ')
wilcox_reflux <- wilcox.test(treatment_scale ~ reflux, df_select)


png(filename = "~/R/stud_stat/zimina/output/Сравнение групп по баллам базисной терапии.png", width = 800, height = 800)
means_group <- tapply(df_select$treatment_scale, df_select$group, function(x) mean(x, na.rm = T))
mean_se_group <- tapply(df_select$treatment_scale, df_select$group, function(x) mean_se(x, mult = 1.96))
means_reflux <- tapply(df_select$treatment_scale, df_select$reflux, function(x) mean(x, na.rm = T))
ggplot(df_select, aes(x = group, y = treatment_scale, color = group)) + 
  stat_summary(fun.data = mean_se_group) + 
  ylab('Балл терапии') + xlab('Группа') + 
  ggtitle(paste('Сравнение групп по баллам базисной терапии', 'p =', round(wilcox_group$p.value, 6), sep = ' ')) +
  theme_bw() +
  theme(legend.position = 'none')
dev.off()

