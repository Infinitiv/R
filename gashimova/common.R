a <- 0.05
d <- read.csv("../data/gashimova/common.csv", check.names = F)
for(i in 1:20){
  d[,i] <- as.factor(d[,i])
}
d.first <- subset(d, d$group == 1)
d.second <- subset(d, d$group == 2)

sink("common/сравнение показателей.txt")
for(i in 3:20){
  title <- names(d)[i]
  count.first <- as.list(table(d.first[,i]))
  percent.first <- as.list(round(table(d.first[,i])*100/length(d.first[,1]), 2))
  count.second <- as.list(table(d.second[,i]))
  percent.second <- as.list(round(table(d.second[,i])*100/length(d.second[,1]), 2))
  fisher <- fisher.test(d[,i], d$group)
  cat(paste(title, " (группа 1/группа 2)", sep = ''))
  cat("\r\n")
  for(j in 1:length(count.first)){
    name <- names(count.first)[j]
    cat(paste(name, " - ", count.first[j][[1]], ' (', percent.first[j][[1]], '%)', sep = ""))
    cat('/')
    cat(paste(count.second[j][[1]], ' (', percent.second[j][[1]], '%)', sep = ""))
    cat("\r\n")
  }
  cat(paste("Точный тест Фишера - p = ", round(fisher$p.value, 6), sep = ""))
  cat("\r\n")
  cat("\r\n")
}
sink()