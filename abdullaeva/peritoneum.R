a <- 0.05
d <- read.csv("../data/abdullaeva/peritoneum.csv", check.names = F)

sink("peritoneum/перитонеальная жидкость.txt")
for(i in 4:length(names(d))){
  n <- tapply(d[,i], d$Группа, shapiro.test)
  medians <- tapply(d[,i], d$Группа, function(x) paste(round(median(x, na.rm=T), 4), " (", quantile(x, 0.25, na.rm = T), "-", quantile(x, 0.75, na.rm = T),")", sep=''))
  means <- tapply(d[,i], d$Группа, function(x) paste(round(mean(x, na.rm=T), 4), " (", round(mean(x, na.rm=T) - sd(x, na.rm = T)/length(x[!is.na(x)])*1.96, 4), "-", round(mean(x, na.rm=T) + sd(x, na.rm = T)/length(x[!is.na(x)])*1.96, 4), ")", sep=''))
  oneway <- oneway.test(d[, i] ~ d$Группа)
  kruskal <- kruskal.test(d[, i] ~ d$Группа)
  all_n <- T
  for(j in 1:length(n)){
    if(n[[j]]$p.value < a){
      all_n <- F
    }
    cat(paste(names(d)[i], " ", "Группа ", levels(d$Группа)[j], " распределение - ", ifelse(n[[j]]$p.value < a, "не нормальное", "нормальное"), sep =''))
    cat("\r\n")
    cat(paste("Медиана и 25%-75% квартили = ", medians[j], sep = ''))
    cat("\r\n")
    cat(paste("Среднее и 0,95 доверительный интервал = ", means[j], sep = ''))
    cat("\r\n")
  }
  cat(ifelse(all_n, paste("Однофакторный дисперсионный анализ: ", "p = ", round(oneway$p.value, 6), sep = ''), paste("Критерий Краскела-Уоллиса: ", "p = ", round(kruskal$p.value, 6), sep = '')))
  cat("\r\n")
  cat("\r\n")
}
sink()