library(tidyverse)

df <- read.csv('/home/markovnin/R/data/vahromeev/vahromeev.csv')
ggplot(df, aes(x = group, y = frequency, fill = group)) + 
  geom_bar(stat = 'identity') + 
  facet_grid(cols = vars(class)) + 
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      legend.position = 'bottom',
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      strip.text.x = element_text(size = 16, face = 'bold'),
      axis.title.y = element_text(size = 16),
      axis.text.y = element_text(size = 14),
      plot.title = element_text(family = 'Times New Roman', face = 'bold', size = 20, hjust = 0.5, color = 'red')) +
  ylab('Частота, %') +
  ggtitle('Структура и уровень перенесенной и имеющей место на момент обследования экстрагенитальной патологии женщин (по МКБ 10)') +
  geom_text(aes(label = paste(frequency, '%', sep = ''), y = frequency), stat= "identity", vjust = - .5, size = 5) +
  geom_label(aes(label = ifelse(!is.na(p), paste('p = ', format(p, digits = 2, scientific = F), sep = ''), '-'), y = - 5), stat = 'identity', size = 5, fill = 'white')
ggsave('/home/markovnin/R/vahromeev/Диаграмма 4.png', height = 7, width = 20)


df <- read.csv('/home/markovnin/R/data/vahromeev/biochem.csv', check.names = F)
one_control <- subset(df, Группа == 'I группа' | Группа == 'Контрольная группа')
one_control_wilcox_test <- wilcox.test(one_control$`Уровень N-терминального пропептида проколлагена III типа в сыворотке крови (пг/мл)` ~ one_control$Группа)
medians <- tapply(one_control$`Уровень N-терминального пропептида проколлагена III типа в сыворотке крови (пг/мл)`, one_control$Группа, median)
ggplot(one_control, aes(x = Группа, y = `Уровень N-терминального пропептида проколлагена III типа в сыворотке крови (пг/мл)`, fill = Группа)) + 
  geom_boxplot(varwidth = T) +
  ggtitle('Содержание N-терминального пропептида проколлагена III типа в сыворотке крови у беременных в сроках гестации 22-36 недель') +
  annotate('label', 
           label = paste(one_control_wilcox_test$method, ', p = ', format(one_control_wilcox_test$p.value, digits = 2, scientific = F), sep = ''), 
           x = 1.5, 
           y = 0,
           size = 5,
           family = 'Times New Roman') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(family = 'Times New Roman', face = 'bold', size = 11, hjust = 0.5, color = 'red'))
ggsave('/home/markovnin/R/vahromeev/Boxplot.png', height = 10, width = 10)
