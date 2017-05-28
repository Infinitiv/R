a <- 0.05
d <- read.csv("lakk.csv", check.names = F) 
d.first <- subset(d, d$group == 1)
d.second <- subset(d, d$group == 2)

summary.count.first <- lapply(d.first[3:15], function(x) paste(round(mean(x, na.rm=T), 4), "±", round(sd(x, na.rm=T)/sqrt(length(x[is.na(x)==F])), 4), sep=''))
summary.count.second <- lapply(d.second[3:15], function(x) paste(round(mean(x, na.rm=T), 4), "±", round(sd(x, na.rm=T)/sqrt(length(x[is.na(x)==F])), 4), sep=''))

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
    cat(paste("Среднее значение и ошибка в группе 1", summary.count.first[title], sep = " = "))
    cat("\r\n")
    cat(paste("Среднее значение и ошибка в группе 2", summary.count.second[title], sep = " = "))
    cat("\r\n")
    cat(paste("Значение p", round(comparsion$p.value, 6), sep = ' = '))
    cat("\r\n")
    cat("\r\n")
}
sink()