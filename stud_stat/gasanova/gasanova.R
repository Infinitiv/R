d <- read.csv("gasanova.csv", check.names = F)
for(i in 4:10){
  d[,i] <- as.factor(d[,i])
}
d.vjk <- subset(d, d$Группа == "с вжк")
d.novjk <- subset(d, d$Группа == "без вжк")

sink("output/fisher.txt")
cat("Используется точный критерий Фишера")
cat("\r\n")
for(i in 5:10){
  fisher <- fisher.test(d[,i], d$Группа)
  value.vjk <- length(d.vjk[d.vjk[,i] == "есть"])*100/length(d.vjk[,1])
  value.novjk <- length(d.novjk[,i] == "есть")*100/length(d.novjk[,1])
  cat(names(d)[i])
  cat("\r\n")
  cat(paste("с вжк = ", value.vjk, ", без вжк = ", value.novjk, ", p = ", round(fisher$p.value, 6), sep = ''))
  cat("\r\n")
  cat("\r\n") 
}
sink()

sink("output/wilcox.txt")
cat("Используется критерий Манна-Уитни")
cat("\r\n")
for(i in 11:12){
  wilcox <- wilcox.test(d[,i] ~ d$Группа)
  value.novjk <- paste(round(mean(d.novjk[,i], na.rm=T), 4), "±", round(sd(d.novjk[,i], na.rm=T)/sqrt(length(d.novjk[,i][is.na(d.novjk[,i])==F])), 4), sep='')
  value.vjk <- paste(round(mean(d.vjk[,i], na.rm=T), 4), "±", round(sd(d.vjk[,i], na.rm=T)/sqrt(length(d.vjk[,i][is.na(d.vjk[,i])==F])), 4), sep='')
  cat(names(d)[i])
  cat("\r\n")
  cat(paste("с вжк = ", value.vjk, ", без вжк = ", value.novjk, ", p = ", round(wilcox$p.value, 7), sep =""))
  cat("\r\n")
  cat("\r\n") 
}
sink()
