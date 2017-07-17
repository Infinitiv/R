a <- 0.05
d <- read.csv("../data/abdullaeva/blood.csv", check.names = F)

sink("blood/проверка на нормальность.txt")
for(i in 4:length(names(d))){
  n <- tapply(d[,i], d$Группа, shapiro.test)
  medians <- tapply(d[,i], d$Группа, function(x) paste(round(median(x, na.rm=T), 4), " (", quantile(x, 0.25, na.rm = T), "-", quantile(x, 0.75, na.rm = T),")", sep=''))
  means <- tapply(d[,i], d$Группа, function(x) paste(round(mean(x, na.rm=T), 4), " (", round(mean(x, na.rm=T) - sd(x, na.rm = T)/length(x[!is.na(x)])*1.96, 4), "-", round(mean(x, na.rm=T) + sd(x, na.rm = T)/length(x[!is.na(x)])*1.96, 4), ")", sep=''))
  for(j in 1:length(n)){
    cat(paste(names(d)[i], " ", "Группа ", levels(d$Группа)[j], " распределение - ", ifelse(n[[j]]$p.value < a, "не нормальное", "нормальное"), sep =''))
    cat("\r\n")
    cat(paste("Медиана и квартили = ", medians[j], sep = ''))
    cat("\r\n")
    cat(paste("Среднее и 0,95 доверительный интервал = ", means[j], sep = ''))
    cat("\r\n")
  }
  cat("\r\n")
}
sink()

d.first <- subset(d, d$group == 1)
d.second <- subset(d, d$group == 2)

summary.count.first <- lapply(d.first[3:15], function(x) paste(round(median(x, na.rm=T), 4), " (", quantile(x, 0.25, na.rm = T), "-", quantile(x, 0.75, na.rm = T),")", sep=''))
summary.count.second <- lapply(d.second[3:15], function(x) paste(round(median(x, na.rm=T), 4), " (", quantile(x, 0.25, na.rm = T), "-", quantile(x, 0.75, na.rm = T),")", sep=''))

sink("lakk/сравнение показателей.txt")
for(i in 3:15){
  title <- names(d)[i]
  normality <- ifelse(shapiro.test(d.first[,i])$p.value < a , FALSE, TRUE) && ifelse(shapiro.test(d.second[,i])$p.value < a , FALSE, TRUE)
  if (normality){
    comparsion <- t.test(d[,i] ~ d$group)
  }
  else {
    comparsion <- wilcox.test(d[,i] ~ d$group, paired = F)
  }

    cat(paste("Показатель", title, sep = ' - '))
    cat("\r\n")
    cat(paste("Нормальность распределения", ifelse(normality, "да", "нет"),sep = " - "))
    cat("\r\n")
    cat(paste("Метод", comparsion$method, sep = ' - '))
    cat("\r\n")
    cat(paste("Медиана и 25%-75% квартили в группе 1", summary.count.first[title], sep = " = "))
    cat("\r\n")
    cat(paste("Медиана и 25%-75% квартили в группе 2", summary.count.second[title], sep = " = "))
    cat("\r\n")
    cat(paste("Значение p", round(comparsion$p.value, 6), sep = ' = '))
    cat("\r\n")
    cat("\r\n")
}
sink()