library('ggplot2')
system("mkdir -p ./output")
y2015 <- read.csv('./data/2015.csv')
y2016 <- read.csv('./data/2016.csv')
for(i in 1:length(y2015)){
  y2015[i][y2015[i] == ""] <- NaN
}
for(i in 1:length(y2016)){
  y2016[i][y2016[i] == ""] <- NaN
}
y2015$summa <- y2015$chemistry + y2015$biology + y2015$russian
y2016$summa <- y2016$chemistry + y2016$biology + y2016$russian
y2015_competitiion <- subset(y2015, y2015$chemistry > 36 & y2015$biology > 36 & y2015$russian > 36 & y2015$status_id == 4)
y2016_competitiion <- subset(y2016, y2016$chemistry > 37 & y2016$biology > 37 & y2016$russian > 37 & y2016$status_id == 4)

png(filename = "output/оценки по химии.png", width = 800, height = 800)
means <- c(mean(y2015_competitiion$chemistry), mean(y2016_competitiion$chemistry))
se <- c(sd(y2015_competitiion$chemistry)/sqrt(length(y2015_competitiion$chemistry)), sd(y2016_competitiion$chemistry)/sqrt(length(y2016_competitiion$chemistry)))
years <- c("2015", "2016")
wilcox.test.chemistry <- wilcox.test(y2015_competitiion$chemistry, y2016_competitiion$chemistry)
df <- data.frame(means = means, years = years, se = se)
ggplot(df, aes(y = means, x = years, color = years)) + geom_errorbar(aes(ymin = means - 1.96*se, ymax = means + 1.96*se), width = 0.2, size = 2) + geom_point(size=5, shape=21, fill="white") + xlab("Годы") + ylab("Балл по химии\n") + theme_bw() + theme(legend.position="none", text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + geom_text(label = paste("  ", round(means, 3), sep = ""), hjust = 0, color = "black") + ggtitle(paste(wilcox.test.chemistry$method, "\n", ifelse(wilcox.test.chemistry$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.chemistry$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.chemistry$p.value, 4), sep = "")))))
dev.off()

png(filename = "output/оценки по биологии.png", width = 800, height = 800)
means <- c(mean(y2015_competitiion$biology), mean(y2016_competitiion$biology))
se <- c(sd(y2015_competitiion$biology)/sqrt(length(y2015_competitiion$biology)), sd(y2016_competitiion$biology)/sqrt(length(y2016_competitiion$biology)))
years <- c("2015", "2016")
wilcox.test.biology <- wilcox.test(y2015_competitiion$biology, y2016_competitiion$biology)
df <- data.frame(means = means, years = years, se = se)
ggplot(df, aes(y = means, x = years, color = years)) + geom_errorbar(aes(ymin = means - 1.96*se, ymax = means + 1.96*se), width = 0.2, size = 2) + geom_point(size=5, shape=21, fill="white") + xlab("Годы") + ylab("Балл по химии\n") + theme_bw() + theme(legend.position="none", text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + geom_text(label = paste("  ", round(means, 3), sep = ""), hjust = 0, color = "black") + ggtitle(paste(wilcox.test.biology$method, "\n", ifelse(wilcox.test.biology$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.biology$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.biology$p.value, 4), sep = "")))))
dev.off()

png(filename = "output/оценки по русскому языку.png", width = 800, height = 800)
means <- c(mean(y2015_competitiion$russian), mean(y2016_competitiion$russian))
se <- c(sd(y2015_competitiion$russian)/sqrt(length(y2015_competitiion$russian)), sd(y2016_competitiion$russian)/sqrt(length(y2016_competitiion$russian)))
years <- c("2015", "2016")
wilcox.test.russian <- wilcox.test(y2015_competitiion$russian, y2016_competitiion$russian)
df <- data.frame(means = means, years = years, se = se)
ggplot(df, aes(y = means, x = years, color = years)) + geom_errorbar(aes(ymin = means - 1.96*se, ymax = means + 1.96*se), width = 0.2, size = 2) + geom_point(size=5, shape=21, fill="white") + xlab("Годы") + ylab("Балл по химии\n") + theme_bw() + theme(legend.position="none", text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + geom_text(label = paste("  ", round(means, 3), sep = ""), hjust = 0, color = "black") + ggtitle(paste(wilcox.test.russian$method, "\n", ifelse(wilcox.test.russian$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.russian$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.russian$p.value, 4), sep = "")))))
dev.off()

png(filename = "output/сумма баллов.png", width = 800, height = 800)
means <- c(mean(y2015_competitiion$summa, na.rm = T), mean(y2016_competitiion$summa, na.rm = T))
se <- c(sd(y2015_competitiion$summa, na.rm = T)/sqrt(length(y2015_competitiion$summa)), sd(y2016_competitiion$summa, na.rm = T)/sqrt(length(y2016_competitiion$summa)))
years <- c("2015", "2016")
wilcox.test.summa <- wilcox.test(y2015_competitiion$summa, y2016_competitiion$summa)
df <- data.frame(means = means, years = years, se = se)
ggplot(df, aes(y = means, x = years, color = years)) + geom_errorbar(aes(ymin = means - 1.96*se, ymax = means + 1.96*se), width = 0.2, size = 2) + geom_point(size=5, shape=21, fill="white") + xlab("Годы") + ylab("Сумма баллов\n") + theme_bw() + theme(legend.position="none", text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + geom_text(label = paste("  ", round(means, 3), sep = ""), hjust = 0, color = "black") + ggtitle(paste(wilcox.test.summa$method, "\n", ifelse(wilcox.test.summa$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.summa$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.summa$p.value, 4), sep = "")))))
dev.off()

png(filename = "output/сумма баллов абитуриентов с аттестатами с отличием.png", width = 800, height = 800)
means <- c(mean(y2015_competitiion$summa[y2015_competitiion$achievement]), mean(y2016_competitiion$summa[y2016_competitiion$achievement_att == 1]))
se <- c(sd(y2015_competitiion$summa, na.rm = T)/sqrt(length(y2015_competitiion$summa)), sd(y2016_competitiion$summa, na.rm = T)/sqrt(length(y2016_competitiion$summa)))
years <- c("2015", "2016")
wilcox.test.summa <- wilcox.test(y2015_competitiion$summa[y2015_competitiion$achievement], y2016_competitiion$summa[y2016_competitiion$achievement_att == 1])
df <- data.frame(means = means, years = years, se = se)
ggplot(df, aes(y = means, x = years, color = years)) + geom_errorbar(aes(ymin = means - 1.96*se, ymax = means + 1.96*se), width = 0.2, size = 2) + geom_point(size=5, shape=21, fill="white") + xlab("Годы") + ylab("Сумма баллов\n") + theme_bw() + theme(legend.position="none", text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + geom_text(label = paste("  ", round(means, 3), sep = ""), hjust = 0, color = "black") + ggtitle(paste(wilcox.test.summa$method, "\n", ifelse(wilcox.test.summa$p.value < 0.001, "p < 0.001", ifelse(wilcox.test.summa$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox.test.summa$p.value, 4), sep = "")))))
dev.off()
