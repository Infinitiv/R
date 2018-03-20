library(tidyverse)
# устанавливаем уровень достоверности
a <- 0.05

# читаем данные
df <- read.csv("~/R/data/zimina/zimina.csv", check.names = F)

# создаем пользовательские переменные
df$group <- ifelse(df$Группа == 1 | df$Группа == 2, 'Астма + поражение ВОПТ', 'Изолированная астма')
df$sex <- factor(ifelse(df$Пол == 'м', 'мужской', 'женский'))
df$age <- df$Возраст
df$salbutamol_test <- ifelse(abs(df$ОФВ1 - df$`ОФВ1 с Sal`) > 12, 'положительный', 'отрицательный')
df$control <- ifelse(df$ОФВ1 > 80, 'контроль', 'нет контроля')
df$treatment_scale <- df$`Баллы терапии`
df$reflux <- ifelse(df$Рефлюкс == 0, 'рефлюкса нет', 'рефлюкс есть')
df$level <- df$`Тяжесть БА`

# выбираем переменные для анализа
df_select <- filter(select(df, group, sex, age, salbutamol_test, control, treatment_scale, reflux, level), !(age > 16 & sex == 'мужской'))

# описательная статистика
chisq.sex <- chisq.test(table(df_select$sex))
chisq.sex.in.groups <- chisq.test(df_select$sex, df_select$group)
wilcox.age.in.groups <- wilcox.test(age ~ group, df_select)
table.group <- table(df_select$group)
cor <- cor.test(df_select$age, df_select$level, method = 'spearman')

# считаем достоверность различия баллов терапии в зависимости от наличия рефлюкса
df_select_reflux <- filter(df_select, group == 'Астма + поражение ВОПТ')
wilcox_reflux <- wilcox.test(level ~ reflux, df_select)
tapply(df_select_reflux$treatment_scale, df_select_reflux$reflux, mean_se)

# считаем достоверность различия баллов терапии по группа исследования
wilcox_group <- wilcox.test(level ~ group, df_select)
wilcox.test(level ~ group, df_select)
cor.test(df_select$level, df_select$treatment_scale)
fisher.test(df_select$group, df_select$level)
ggplot(df_select) + geom_point(aes(x = treatment_scale, y = level, color = sex), position = 'jitter') + geom_smooth(aes(x = treatment_scale, y = level), method = 'lm')

# отчет
sink('~/R/stud_stat/zimina/output/zimina.txt')
cat(paste('Сравнение групп по полу, p = ', round(chisq.sex.in.groups$p.value, 4), ' (Метод - ', chisq.sex.in.groups$method, ')', sep = ''))
cat("\r\n")
cat(paste('Сравнение групп по возрасту, p = ', round(wilcox.age.in.groups$p.value, 4), ' (Метод - ', wilcox.age.in.groups$method, ')', sep = ''))
cat("\r\n")
cat(paste('Сравнение групп по баллам терапии, p = ', round(wilcox_group$p.value, 4), ' (Метод - ', wilcox_group$method, ')', sep = ''))
cat("\r\n")
cat(paste('Сравнение баллов терапии по наличию рефлюкса, p = ', round(wilcox_reflux$p.value, 4), ' (Метод - ', wilcox_reflux$method, ')', sep = ''))
sink()

# графики
# диаграмма распределение групп по возрасту
png(filename = "~/R/stud_stat/zimina/output/Сравнение групп по возрасту.png", width = 600, height = 480)
ggplot(df_select, aes(x = group, y = age, color = group)) + 
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96)) + 
  ylab('Возраст') + xlab('Группа') + 
  ggtitle(paste('Сравнение групп по возрасту', 'p =', round(wilcox.age.in.groups$p.value, 6), sep = ' ')) +
  theme_bw() +
  theme(legend.position = 'none')
dev.off()

# диаграмма распределения баллов терапии по возрасту
png(filename = "~/R/stud_stat/zimina/output/Корелляция между возрастом и баллами терапии.png", width = 600, height = 480)
ggplot(df_select) + 
  geom_point(aes(x = age, y = level, color = sex), position = 'jitter') +
  geom_smooth(aes(x = age, y = level), method = 'lm') +
  theme_bw() +
  ylab('Балл терапии') + 
  xlab('Возраст') +
  theme(legend.title = element_blank()) +
  ggtitle(paste('Корелляция между возрастом и баллами терапии - ', 'rho = ', round(cor$estimate, 4), ' (p = ', round(cor$p.value, 6), ')', sep = ''))
dev.off()

# диаграмма возрастно-половой структуры
png(filename = "~/R/stud_stat/zimina/output/Восзрастно-половая структура.png", width = 600, height = 480)
ggplot(df_select) +
  geom_histogram(aes(x = age, fill = sex), position = 'fill', binwidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 18, 1)) +
  ylab('Доля') + 
  xlab('Возраст') +
  theme(legend.title = element_blank()) +
  ggtitle(paste('Возрастно-половой состав - ', 'p = ', round(chisq.sex.in.groups$p.value, 4), sep = ''))
dev.off()

# сравнение групп по баллам базисной терапии
png(filename = "~/R/stud_stat/zimina/output/Сравнение групп по баллам базисной терапии.png", width = 600, height = 480)
ggplot(df_select, aes(x = group, y = level, color = group)) + 
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96)) + 
  ylab('Балл терапии') + xlab('Группа') + 
  ggtitle(paste('Сравнение групп по баллам базисной терапии', 'p =', round(wilcox_group$p.value, 6), sep = ' ')) +
  theme_bw() +
  theme(legend.position = 'none')
dev.off()

# сравнение баллов базисной терапии по наличию рефлюкса
png(filename = "~/R/stud_stat/zimina/output/Сравнение баллов базисной терапии по наличию рефлюкса.png", width = 600, height = 480)
ggplot(df_select_reflux, aes(x = reflux, y = level, color = reflux)) + 
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96)) + 
  ylab('Балл терапии') + xlab('Рефлюкс') + 
  ggtitle(paste('Сравнение баллов базисной терапии по наличию рефлюкса', 'p =', round(wilcox_reflux$p.value, 6), sep = ' ')) +
  theme_bw() +
  theme(legend.position = 'none')
dev.off()

