library('ggplot2')
d <- read.csv("guseva.csv")

chart <- function(i){
  means <- tapply(d[,i], d$Группа, mean)
  se <- tapply(d[,i], d$Группа, function(x) sd(x)/sqrt(length(x)))
  df <- data.frame(means = means, sex = levels(d$Группа), se = se)
  png(filename = paste("output/", names(d)[i], " (ggplot2).png", sep = ''), width = 800, height = 800)
    ggplot(df, aes(y = means, x = sex, color = sex)) + geom_errorbar(aes(ymin = means - 1.96*se, ymax = means + 1.96*se), width = 0.2, size = 2) + geom_point(size=5, shape=21, fill="white") + xlab("Группа") + ylab(names(d)[i]) + theme_bw() + theme(legend.position="none", text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + geom_text(label = paste("  ", round(means, 3), sep = ""), hjust = 0, color = "black") + ggtitle(paste(wilcox$method, "\n", ifelse(wilcox$p.value < 0.001, "p < 0.001", ifelse(wilcox$p.value < 0.05, "p < 0.05", paste("p = ", round(wilcox$p.value, 4), sep = "")))))
  dev.off()
}

d.cardio <- subset(d, d$Группа == "кардио")
d.gastro <- subset(d, d$Группа == "гастро")
age.wilcox <- wilcox.test(d$Возраст..лет ~ d$Группа)

png(filename = "output/возраст по группам (ggplot2).png", width = 800, height = 800)
  means <- tapply(d$Возраст..лет, d$Группа, mean)
  se <- tapply(d$Возраст..лет, d$Группа, function(x) sd(x)/sqrt(length(x)))
  df <- data.frame(means = means, sex = levels(d$Группа), se = se)
  ggplot(df, aes(y = means, x = sex, color = sex)) + geom_errorbar(aes(ymin = means - 1.96*se, ymax = means + 1.96*se), width = 0.2, size = 2) + geom_point(size=5, shape=21, fill="white") + xlab("Группа") + ylab("Возраст\n") + theme_bw() + theme(legend.position="none", text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + geom_text(label = paste("  ", round(means, 3), sep = ""), hjust = 0, color = "black") + ggtitle(paste(age.wilcox$method, "\n", ifelse(age.wilcox$p.value < 0.001, "p < 0.001", ifelse(age.wilcox$p.value < 0.05, "p < 0.05", paste("p = ", round(age.wilcox$p.value, 4), sep = "")))))
dev.off()

sink("output/средние значения.txt")
for(i in 4:length(names(d))){
  value.cardio <- paste(round(mean(d.cardio[,i], na.rm=T), 4), "±", round(sd(d.cardio[,i], na.rm=T)/sqrt(length(d.cardio[,i][is.na(d.cardio[,i])==F])), 4), sep='')
  value.gastro <- paste(round(mean(d.gastro[,i], na.rm=T), 4), "±", round(sd(d.gastro[,i], na.rm=T)/sqrt(length(d.gastro[,i][is.na(d.gastro[,i])==F])), 4), sep='')
  wilcox <- wilcox.test(d[,i] ~ d$Группа)
  if(wilcox$p.value < 0.05){
    chart(i)
  }
  cat(names(d)[i])
  cat("\r\n")
  cat(paste("кардио = ", value.cardio, ", гастро = ", value.gastro, ", p = ", round(wilcox$p.value, 7), sep =""))
  cat("\r\n")
  cat("\r\n")
}
sink()
