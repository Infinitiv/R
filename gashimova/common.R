require('epitools')
a <- 0.05
d <- read.csv("../data/gashimova/common.csv", check.names = F)
for(i in 20:28){
  d[,i] <- as.numeric(d[,i])
  median <- median(d[,i])
  d[,i][d[,i] < median] <- paste("до ", median, sep = '')
  d[,i][d[,i] != paste("до ", median, sep = '')] <- paste(median, " или больше", sep = '')
}
for(i in 1:29){
  d[,i] <- as.factor(d[,i])
}
d.first <- subset(d, d$group == 1)
d.second <- subset(d, d$group == 2)

sink("common/сравнение показателей.txt")
for(i in 3:29){
  title <- names(d)[i]
  count.first <- as.list(table(d.first[,i]))
  percent.first <- as.list(round(table(d.first[,i])*100/length(d.first[,1]), 2))
  count.second <- as.list(table(d.second[,i]))
  percent.second <- as.list(round(table(d.second[,i])*100/length(d.second[,1]), 2))
  fisher <- fisher.test(d[,i], d$group)
  ifelse(length(levels(d[,i])) == 2, oddratio <- oddsratio(d[,1], d[,i], method = 'fisher'), oddratio <- "нельзя вычислить")
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
  if(oddratio == "нельзя вычислить"){
    cat("Отношение шансов для данного показателя вычислить нельзя")
  } 
  else {
    cat(paste("Отношение шансов ", round(oddratio$measure[2, 1], 6), ", p = ", round(oddratio$p.value[2, 2], 6), sep = ''))
  }
  cat("\r\n")
  cat("\r\n")
}

sink()