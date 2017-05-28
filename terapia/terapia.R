terapia <- read.csv('terapia.csv')
terapia$Группа <- factor(terapia$Группа)
terapia$Гр.здр. <- factor(terapia$Гр.здр.)
terapia$Степень.ОЖ <- factor(terapia$Степень.ОЖ)
terapia$bottlesCount <- rowSums(terapia[8:11])
for(i in 8:15){
  for(j in 2:5){
    output <- by(terapia[,i], terapia[j], function(x) round(table(x)/length(x[!is.na(x)])*100, 4))
    filepath <- paste('output/', names(terapia)[i], ' против ', names(terapia)[j], ' - процентное распределение количества бутылочек.txt', sep = '')
    sink(filepath)
    print(output)
    sink()
    output <- by(terapia[,i], terapia[j], function(x) round(mean(x, na.rm = T), 2))
    filepath <- paste('output/', names(terapia)[i], ' против ', names(terapia)[j], ' - среднее количество бутылочек.txt', sep = '')
    sink(filepath)
    print(output)
    sink()
  }
}