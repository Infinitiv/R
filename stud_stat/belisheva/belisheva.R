library('ggplot2')
d <- read.csv("base.csv")
for(i in 5:80){
  d[,i] <- as.factor(d[,i])
}
sink("output/fisher.txt")
cat("Используется Fisher's Exact Test for Count Data")
cat("\r\n")
for(i in 7:80){
  fisher <- fisher.test(d[,i], d$нет.аномалий.положения.хориона)
  if(fisher$p.value < 0.05){
    cat(names(d)[i])
    cat("\r\n")
    cat(paste("значение p = ", round(fisher$p.value, 6), sep = ''))
    cat("\r\n")
    cat("\r\n") 
  }
}
sink()