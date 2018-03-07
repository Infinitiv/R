library('ggplot2')
library('scales')
library('Hmisc')
df <- read.csv("../data/oxalat/oxalat.csv")
df$start.ege <- ifelse(df$возраст.начала.заболевания < median(df$возраст.начала.заболевания, na.rm = T), 'младше 7 лет', '7 лет и старше')
population.region3_15 <- 124056
population.town3_15 <- 101869
population.village3_15 <- 22187
population.ivanovo1_15 <- 53800
population.ivanovo1_18 <- 63700
population.zone1 <- 48740
population.zone2 <- 15000
pallete <- c('#ef5350', '#2196f3')

sex.count <- as.data.frame(round(table(df$Пол)/length(df$Пол)*100, 2))
prop.test.sex <- prop.test(table(df$Пол))
if(prop.test.sex$p.value < 0.001){
  s <- 'Одновыборочный тест пропорций - p < 0.001'
} else if(prop.test.sex$p.value < 0.05){
  s <- 'Одновыборочный тест пропорций - p < 0.05'
} else{
  s <- paste('Одновыборочный тест пропорций - p = ', prop.test.sex$p.value, sep = '')
}
ggplot(df, aes(Пол)) + geom_bar(aes(y=..count../sum(..count..), fill = ЛПУ)) + scale_y_continuous(name = 'Частота', labels=percent_format(), breaks = seq(0, 1, 0.25)) + ggtitle('Распределение наблюдений по полу', subtitle = s) + theme_classic() + scale_fill_manual(values = pallete)
ggsave(filename = 'распределение наблюдений по полу.png', path = 'output', height = 5, width = 5)

means <- tapply(df$доля.от.нормы, list(df$start.ege, df$ЛПУ), function(x) mean(x, na.rm = T))
ymin <- tapply(df$доля.от.нормы, list(df$start.ege, df$ЛПУ), function(x) mean(x, na.rm = T) - 1.96*sd(x)/sqrt(length(x)))
ymax <- tapply(df$доля.от.нормы, list(df$start.ege, df$ЛПУ), function(x) mean(x, na.rm = T) + 1.96*sd(x)/sqrt(length(x)))
d <- data.frame(means = c(means), lower = c(ymin), upper = c(ymax), hospital = c('1 ГКБ', '1 ГКБ', 'ОКБ', 'ОКБ'), age = c('7 лет и старше', 'младше 7 лет'))
ggplot(d, aes(age, means, col = hospital)) + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + geom_point() + labs(x = 'Возраст', y = 'Превышение нормального уровня экскреции, разы', col = 'ЛПУ') + ggtitle('Уровень экскреции оксалатов \nв зависимости от возраста начала заболевания') + theme_classic() + scale_color_manual(values = pallete)
ggsave(filename = 'экскреция оксалатов в зависимости от возраста.png', path = 'output', height = 5, width = 5)

wilcox.age.place <- wilcox.test(df$возраст.начала.заболевания ~ df$город.село)
if(wilcox.age.place$p.value < 0.001){
  s <- 'Критерий Манна-Уитни - p < 0.001'
} else if(wilcox.age.place$p.value < 0.05){
  s <- 'Критерий Манна-Уитни - p < 0.05'
} else{
  s <- paste('Критерий Манна-Уитни - p = ', prop.test.sex$p.value, sep = '')
}
means <- tapply(df$возраст.начала.заболевания, df$город.село, function(x) mean(x, na.rm = T))
ymin <- tapply(df$возраст.начала.заболевания, df$город.село, function(x) mean(x, na.rm = T) - 1.96*sd(x, na.rm = T)/sqrt(length(x)))
ymax <- tapply(df$возраст.начала.заболевания, df$город.село, function(x) mean(x, na.rm = T) + 1.96*sd(x, na.rm = T)/sqrt(length(x)))
d <- data.frame(means = c(means), lower = c(ymin), upper = c(ymax), place = c('город', 'село'))
ggplot(d, aes(place, means, col = place)) + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + geom_point() + labs(x = 'Место жительства', y = 'Возраст начала заболевания, лет', col = 'Место жительства') + ggtitle('Возраст начала заболевания \nв зависимости от места проживания', s) + guides(col = F) + theme_classic() + scale_color_manual(values = pallete)
ggsave(filename = 'возраст начала заболевания в зависимости от места проживания.png', path = 'output', height = 5, width = 5)

ggplot(df, aes(стаж.заболевания, доля.от.нормы, col = ЛПУ)) + geom_point()

sink("output/заболеваемость ДОН на 1000 детского населения.txt")
cat("Заболеваемость дисметаболической нефропатией в Ивановской области по обращаемости в ОКБ")
cat("\n")
cat(paste(round(length(d[,1])/population.region3_15*1000, 2), " на 1000 детей в возрасте от 3 до 15 лет", sep = ''))
cat("\n\n")
cat(paste(round(length(d$город.село[d$город.село == "город"])/population.town3_15*1000, 2), " на 1000 детей городского населения в возрасте от 3 до 15 лет", sep = ''))
cat("\n\n")
cat(paste(round(length(d$город.село[d$город.село == "село"])/population.village3_15*1000, 2), " на 1000 детей сельского населения в возрасте от 3 до 15 лет", sep = ''))
cat("\n\n")
cat(paste(round(length(d$город.область[d$город.область == "город"])/population.ivanovo1_15*1000, 2), " на 1000 детей г. Иваново в возрасте от 0 до 15 лет", sep = ''))
cat("\n\n")
cat(paste(round(length(d$город.область[d$город.область == "город"])/population.ivanovo1_18*1000, 2), " на 1000 детей г. Иваново в возрасте от 0 до 18 лет", sep = ''))
cat("\n\n")
cat(paste(round(length(d$Зона.водоснабжения[d$Зона.водоснабжения == "ОНВС-1"])/population.zone1*1000, 2), " на 1000 детей первой зоны водоснабжения ОНВС-1 г. Иваново в возрасте от 0 до 18 лет", sep = ''))
cat("\n\n")
cat(paste(round(length(d$Зона.водоснабжения[d$Зона.водоснабжения == "ОНВС-2"])/population.zone2*1000, 2), " на 1000 детей второй зоны водоснабжения ОНВС-2 г. Иваново в возрасте от 0 до 18 лет", sep = ''))
cat("\n\n")
chisq.test.town.village <- chisq.test(matrix(c(length(d$город.село[d$город.село == "город"]), population.town3_15, length(d$город.село[d$город.село == "село"]), population.village3_15), ncol = 2))
cat(paste("Различие частоты заболеваемости городского и сельского детского населения\n", chisq.test.town.village$method), chisq.test.town.village$p.value, sep = " ")
cat("\n\n")
chisq.test.zone <- chisq.test(matrix(c(length(d$Зона.водоснабжения[d$Зона.водоснабжения == "ОНВС-1"]), population.zone1, length(d$Зона.водоснабжения[d$Зона.водоснабжения == "ОНВС-2"]), population.zone2), ncol = 2))
cat(paste("Различие частоты заболеваемости детей г. Иваново в зависимости от источника водоснабжения\n", chisq.test.zone$method), chisq.test.zone$p.value, sep = " ")
sink()

wilcox.test.sex <- wilcox.test(d$доля.от.нормы ~ d$Пол)
png(filename = "output/превышение нормы оксалатов в зависимости от пола.png")
  means <- tapply(d$доля.от.нормы, d$Пол, function(x) mean(x, na.rm = T))
  boxplot(d$доля.от.нормы ~ d$Пол, ylab = "Степень превышения нормального уровня экскреции оксалатов", xlab = "Пол", main = paste(wilcox.test.sex$method, "\n", ifelse(wilcox.test.sex$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.sex$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.sex$p.value, 4), sep = "")))))
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  text(1:length(means), means, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 1.2, col = "red")
dev.off()

wilcox.test.city.village <- wilcox.test(d$доля.от.нормы[d$город.село == "город"], d$доля.от.нормы[d$город.село == "село"])
png(filename = "output/превышение нормы оксалатов в зависимости от проживания в городе или на селе.png")
  means <- c(mean(d$доля.от.нормы[d$город.село == "город"], na.rm = T), mean(d$доля.от.нормы[d$город.село == "село"], na.rm = T))
  boxplot(d$доля.от.нормы[d$город.село == "город"], d$доля.от.нормы[d$город.село == "село"], ylab = "Степень превышения нормального уровня экскреции оксалатов", xlab = "Место проживания", names = c("город", "село"), main = paste(wilcox.test.city.village$method, "\n", ifelse(wilcox.test.city.village$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.city.village$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.city.village$p.value, 4), sep = "")))))
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  text(1:length(means), means, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 0.9, col = "red")
dev.off()

wilcox.test.aqua.zone <- wilcox.test(d$доля.от.нормы[d$Зона.водоснабжения == "ОНВС-1"], d$доля.от.нормы[d$Зона.водоснабжения == "ОНВС-2"])
png(filename = "output/превышение нормы оксалатов в зависимости от зоны водоснабжения.png")
  means <- c(mean(d$доля.от.нормы[d$Зона.водоснабжения == "ОНВС-1"], na.rm = T), mean(d$доля.от.нормы[d$Зона.водоснабжения == "ОНВС-2"], na.rm = T))
  boxplot(d$доля.от.нормы[d$Зона.водоснабжения == "ОНВС-1"], d$доля.от.нормы[d$Зона.водоснабжения == "ОНВС-2"], ylab = "Степень превышения нормального уровня экскреции оксалатов", xlab = "Источник водоснабжения", names = c("ОНВС-1", "ОНВС-2"), main = paste(wilcox.test.aqua.zone$method, "\n", ifelse(wilcox.test.aqua.zone$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.aqua.zone$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.aqua.zone$p.value, 4), sep = "")))))
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  text(1:length(means), means, labels = formatC(means, format = "f", digits = 2), pos = 3, cex = 0.9, col = "red")
dev.off()

wilcox.test.start.aqua.zone <- wilcox.test(d$возраст.начала.заболевания[d$Зона.водоснабжения == "ОНВС-1"], d$возраст.начала.заболевания[d$Зона.водоснабжения == "ОНВС-2"])
png(filename = "output/возраст начала заболевания в зависимости от зоны водоснабжения.png")
  means <- c(mean(d$возраст.начала.заболевания[d$Зона.водоснабжения == "ОНВС-1"], na.rm = T), mean(d$возраст.начала.заболевания[d$Зона.водоснабжения == "ОНВС-2"], na.rm = T))
  boxplot(d$возраст.начала.заболевания[d$Зона.водоснабжения == "ОНВС-1"], d$возраст.начала.заболевания[d$Зона.водоснабжения == "ОНВС-2"], ylab = "Степень превышения нормального уровня экскреции оксалатов", xlab = "Источник водоснабжения", names = c("ОНВС-1", "ОНВС-2"), main = paste(wilcox.test.start.aqua.zone$method, "\n", ifelse(wilcox.test.start.aqua.zone$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.start.aqua.zone$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.start.aqua.zone$p.value, 4), sep = "")))))
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  text(1:length(means), means, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 0.9, col = "red")
dev.off()

wilcox.test.start.city.village <- wilcox.test(d$возраст.начала.заболевания[d$город.село == "город"], d$возраст.начала.заболевания[d$город.село == "село"])
png(filename = "output/возраст начала заболевания в зависимости от проживания в городе или на селе.png")
  means <- c(mean(d$возраст.начала.заболевания[d$город.село == "город"], na.rm = T), mean(d$возраст.начала.заболевания[d$город.село == "село"], na.rm = T))
  boxplot(d$возраст.начала.заболевания[d$город.село == "город"], d$возраст.начала.заболевания[d$город.село == "село"], ylab = "Возраст начала заболевания", xlab = "Место проживания", names = c("город", "село"), main = paste(wilcox.test.start.city.village$method, "\n", ifelse(wilcox.test.start.city.village$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.start.city.village$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.start.city.village$p.value, 4), sep = "")))))
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  text(1:length(means), means, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 0.9, col = "red")
dev.off()

wilcox.test.city.region <- wilcox.test(d$доля.от.нормы[d$город.область == "город"], d$доля.от.нормы[d$город.область == "область"])
png(filename = "output/превышение нормы оксалатов в зависимости от проживания в городе Иваново или в Ивановской области.png")
  means <- c(mean(d$доля.от.нормы[d$город.область == "город"], na.rm = T), mean(d$доля.от.нормы[d$город.область == "область"], na.rm = T))
  boxplot(d$доля.от.нормы[d$город.область == "город"], d$доля.от.нормы[d$город.область == "область"], ylab = "Степень превышения нормального уровня экскреции оксалатов", xlab = "Место проживания", names = c("г. Иваново", "Ивановская область"), main = paste(wilcox.test.city.region$method, "\n", ifelse(wilcox.test.city.region$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.city.region$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.city.region$p.value, 4), sep = "")))))
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  text(1:length(means), means, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 0.9, col = "red")
dev.off()

wilcox.test.start <- wilcox.test(d$доля.от.нормы[d$возраст.начала.заболевания < median(d$возраст.начала.заболевания, na.rm = T)], d$доля.от.нормы[d$возраст.начала.заболевания >= median(d$возраст.начала.заболевания, na.rm = T)])
png(filename = "output/превышение нормы оксалатов в зависимости от возраста начала заболевания.png")
  means <- c(mean(d$доля.от.нормы[d$возраст.начала.заболевания < median(d$возраст.начала.заболевания, na.rm = T)], na.rm = T), mean(d$доля.от.нормы[d$возраст.начала.заболевания >= median(d$возраст.начала.заболевания, na.rm = T)], na.rm = T))
  boxplot(d$доля.от.нормы[d$возраст.начала.заболевания < median(d$возраст.начала.заболевания, na.rm = T)], d$доля.от.нормы[d$возраст.начала.заболевания >= median(d$возраст.начала.заболевания, na.rm = T)], ylab = "Степень превышения нормального уровня экскреции оксалатов", xlab = "Возраст начала заболевания", names = c("младше 7 лет", "7 лет и старше"), main = paste(wilcox.test.start$method, "\n", ifelse(wilcox.test.start$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.start$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.start$p.value, 4), sep = "")))))
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  text(1:length(means), means, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 0.9, col = "red")
dev.off()

cor.test.start <- cor.test(d$доля.от.нормы, d$возраст.начала.заболевания, method = "spearman")
cor.test.long <- cor.test(d$доля.от.нормы, d$стаж.заболевания, method = "spearman")
sink("output/корреляционные связи.txt")
cat("Корреляция между степенью превышения нормального уровня экскреции оксалатов и возрастом начала заболевания")
cat("\n")
cat(paste(cor.test.start$method, "=", round(cor.test.start$estimate, 2), ifelse(cor.test.start$p.value < 0.001, "p < 0.001", ifelse(cor.test.start$p.value < 0.05, "p < 0.05", paste("p = ", round(cor.test.start$p.value, 4), sep = ""))), sep = " "))
cat("\n\n")
cat("Корреляция между степенью превышения нормального уровня экскреции оксалатов и стажем заболевания")
cat("\n")
cat(paste(cor.test.long$method, " = ", round(cor.test.long$estimate, 2), ifelse(cor.test.long$p.value < 0.001, "p < 0.001", ifelse(cor.test.long$p.value < 0.05, "p < 0.05", paste("p = ", round(cor.test.long$p.value, 4), sep = ""))), sep = " "))
sink()

sink("output/средние значения с ошибками.txt")
cat("Средние значени числовых показателей")
cat("\n\n")
cat("Степень превышения экскреции оксалатов у детей с началом заболевания ранее 7-летнего возраста")
cat("\n")
x <- d$доля.от.нормы[d$возраст.начала.заболевания < median(d$возраст.начала.заболевания, na.rm = T)]
cat(paste(round(mean(x, na.rm = T), 2), "±", round(sd(x, na.rm=T)/sqrt(length(x)), 2), sep = ""))
cat("\n\n")
cat("Степень превышения экскреции оксалатов у детей с началом заболевания позднее 7-летнего возраста")
cat("\n")
x <- d$доля.от.нормы[d$возраст.начала.заболевания >= median(d$возраст.начала.заболевания, na.rm = T)]
cat(paste(round(mean(x, na.rm = T), 2), "±", round(sd(x, na.rm=T)/sqrt(length(x)), 2), sep = ""))
cat("\n\n")
cat("Возраст начала заболевания у детей, проживающих в городе")
cat("\n")
x <- d$возраст.начала.заболевания[d$город.село == "город"]
cat(paste(round(mean(x, na.rm = T), 2), "±", round(sd(x, na.rm=T)/sqrt(length(x)), 2), sep = ""))
cat("\n\n")
cat("Возраст начала заболевания у детей, проживающих на селе")
cat("\n")
x <- d$возраст.начала.заболевания[d$город.село == "село"]
cat(paste(round(mean(x, na.rm = T), 2), "±", round(sd(x, na.rm=T)/sqrt(length(x)), 2), sep = ""))
sink()
