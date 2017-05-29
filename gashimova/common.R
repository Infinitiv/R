a <- 0.05
d <- read.csv("../data/gashimova/common.csv")
for(i in 1:33){
  d[,i] <- as.factor(d[,i])
}
d.first <- subset(d, d$group == 1)
d.second <- subset(d, d$group == 2)

sink("common/сравнение показателей.txt")
for(i in 3:33){
  title <- names(d)[i]
  count.first <- as.list(table(d.first[,i]))
  percent.first <- as.list(round(table(d.first[,i])*100/length(d.first[,1]), 2))
  count.second <- as.list(table(d.second[,i]))
  percent.second <- as.list(round(table(d.second[,i])*100/length(d.second[,1]), 2))
  fisher <- fisher.test(d[,i], d$group)
  
  cat(paste("Показатель", title, sep = ' - '))
  cat("\r\n")
  cat(paste("Метод", fisher$method, sep = ' - '))
  cat("\r\n")
  cat("Количество и процент в группе 1")
  for(j in 1:length(count.first)){
    name <- names(count.first)[i]
    cat(paste("Количество ", name, " - ", count.first[i][[1]]))
    cat("\r\n")
    cat(paste("Процент ", name, " - ", percent.first[i][[1]]))
  }
}
sink()