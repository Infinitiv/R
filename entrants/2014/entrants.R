entrants <- read.csv('report.csv')
entrants$institute <- factor(entrants$institute)
entrants$spec <- factor(entrants$spec)
for(i in 1:length(levels(entrants$institute))){
  institute <- levels(entrants$institute)[i]
  means <- by(entrants$sum[entrants$institute == institute], entrants$spec[entrants$institute == institute], mean)
  medians <- by(entrants$sum[entrants$institute == institute], entrants$spec[entrants$institute == institute], median)
  mins <- by(entrants$sum[entrants$institute == institute], entrants$spec[entrants$institute == institute], min)
  png(filename = paste(institute, '.png', sep=''))
  par(mar = c(4.1, 5.1, 4.1, 2.1))
  plot(entrants$sum[entrants$institute == institute] ~ entrants$spec[entrants$institute == institute], xlab = "", ylab = "Сумма баллов", main = institute)
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  points(1:length(mins), mins, pch = 1, cex = 0.75, bg = "blue")
  text(1:length(means), medians, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 0.9, col = "red")
  text(1:length(mins), mins, labels = mins, pos = 1, cex = 0.9, col = "blue")
  dev.off()
}
institute <- "ИвГМА"
for(i in 1:length(levels(entrants$spec))){
  spec <- levels(entrants$spec)[i]
  means <- by(entrants$sum[entrants$spec == spec], entrants$institute[entrants$spec == spec], mean)
  medians <- by(entrants$sum[entrants$spec == spec], entrants$institute[entrants$spec == spec], median)
  mins <- by(entrants$sum[entrants$spec == spec], entrants$institute[entrants$spec == spec], min)
  counts <- by(entrants$sum[entrants$spec == spec], entrants$institute[entrants$spec == spec], length)
  png(filename = paste(spec, '.png', sep=''), width = 2000, height = 800)
  par(las = 2, mar = c(12.1, 5.1, 4.1, 2.1))
  min.count <- min(table(entrants$institute[entrants$spec == spec]))
  plot(entrants$sum[entrants$spec == spec] ~ entrants$institute[entrants$spec == spec], xlab = "", ylab = "Сумма баллов", main = spec)
  points(1:length(means), means, pch = 23, cex = 0.75, bg = "red")
  points(1:length(mins), mins, pch = 1, cex = 0.75, bg = "blue")
  text(1:length(means), medians + 2, labels = formatC(means, format = "f", digits = 1), pos = 3, cex = 0.9, col = "red")
  text(1:length(mins), mins, labels = mins, pos = 1, cex = 0.9, col = "blue")
  abline(h = median(entrants$sum[entrants$spec == spec & entrants$institute == institute]), lty = 2)
  abline(h = median(medians, na.rm = T), lty = 2, col = "red")
  dev.off()
  png(filename = paste("Рейтинг вузов по медианам баллов - ", spec, '.png', sep=''), width = 1024, height = 768)
  dotchart(sort(medians), labels = names(sort(medians)), xlab = "Рейтинг вузов по медиане баллов", pch = 1, main = spec)
  dev.off()
  png(filename = paste("Рейтинг вузов по средним баллам - ", spec, '.png', sep=''), width = 1024, height = 768)
  dotchart(sort(means), labels = names(sort(means)), xlab = "Рейтинг вузов по средним баллам", pch = 1, main = spec)
  dev.off()
  png(filename = paste("Рейтинг вузов по проходным баллам - ", spec, '.png', sep=''), width = 1024, height = 768)
  dotchart(sort(mins), labels = names(sort(mins)), xlab = "Рейтинг вузов по проходным баллам", pch = 1, main = spec)
  dev.off()
  png(filename = paste("Зависимость проходного балла от количества мест на общий конкурс - ", spec, '.png', sep = ''))
  plot(mins, counts, xlab = "Проходной балл", ylab = "Количество мест", main = spec)
  dev.off
}
sink('ranks.txt')
inst <- c()
sp <- c()
delta <- c()
for(j in 1:length(levels(entrants$spec[entrants$institute == institute]))){
  spec <- levels(entrants$spec[entrants$institute == institute])[j]
  institute.count <- length(levels(droplevels(entrants$institute[entrants$spec == spec])))
  institute.rank <- institute.count
  institute.equal <- 0
  entrants.institute.sum <- entrants$sum[entrants$institute == institute & entrants$spec == spec]
  inst <- c(inst, institute)
  sp <- c(sp, spec)
  delta <- c(delta, 0)
  entrants.not.institute.names <- droplevels(entrants$institute[entrants$institute != institute & entrants$spec == spec])
  for(i in 1:length(levels(entrants.not.institute.names))){
    entrants.current.institute.sum <- entrants$sum[entrants$institute == levels(entrants.not.institute.names)[i] & entrants$spec == spec]
    ifelse(length(entrants.institute.sum) > length(entrants.current.institute.sum), spec.count <- length(entrants.current.institute.sum), spec.count <- length(entrants.institute.sum))
    ifelse(min(head(entrants.current.institute.sum, spec.count)) <= min(head(entrants.institute.sum, spec.count)), institute.rank <- institute.rank - 1, institute.rank <- institute.rank)
    ifelse(min(head(entrants.current.institute.sum, spec.count)) == min(head(entrants.institute.sum, spec.count)), institute.equal <- institute.equal + 1, institute.equal <- institute.equal)
    inst <- c(inst, levels(entrants.not.institute.names)[i])
    sp <- c(sp, spec)
    delta <- c(delta, min(head(entrants.current.institute.sum, spec.count)) - min(head(entrants.institute.sum, spec.count)))
  }
  ifelse(institute.equal == 0, string <- paste(institute, " ", spec, " - место ", institute.rank, " из ", institute.count, sep = ""), string <- paste(institute, " ", spec, " - место ", institute.rank, "-", institute.rank + institute.equal, " из ", institute.count, sep = ""))
  cat(string)
  cat('\n')
}
institutes.rank <- data.frame(inst = inst, sp = sp, delta = delta)
institutes.rank <- institutes.rank[order(institutes.rank$delta), ]
institutes.rank$color[institutes.rank$sp == "Лечебное дело"] <- 1
institutes.rank$color[institutes.rank$sp == "Педиатрия"] <- 2
institutes.rank$color[institutes.rank$sp == "Стоматология"] <- 3
sink()
for(i in 1:length(levels(institutes.rank$sp))){
  spec <- levels(institutes.rank$sp)[i]
  png(filename = paste("Рейтинг ", spec, '.png', sep=''), width = 1024, height = 768)
  dotchart(institutes.rank$delta[institutes.rank$sp == spec], labels = institutes.rank$inst[institutes.rank$sp == spec], xlab = paste("Отклонение минимального балла от", institute, sep = " "), pch = 1, main = spec)
  abline(v = 0, lty = 2)
  dev.off()
}