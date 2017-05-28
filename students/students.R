students <- read.csv("students.csv")
students$course <- factor(students$course)
students$course_spec <- paste(students$course, "курс,", students$speciality, sep = " ")
students$course_spec <- factor(students$course_spec)
sink("Проверка гипотезы нормальности распределения выборок.txt")
for(i in 1:length(levels(students$course_spec))){
  tmp <- subset(students, students$course_spec == levels(students$course_spec)[i])
  shapiros <- lapply(tmp[3:5], function(x) paste(ifelse(shapiro.test(x)$p.value > 0.05, "Нормальное распределение", ""), sep=''))
  print(levels(students$course_spec)[i])
  cat('\n')
  print(shapiros)
}
sink()
sink("Описательные статистики.txt")
for(i in 1:length(levels(students$course_spec))){
  tmp <- subset(students, students$course_spec == levels(students$course_spec)[i])
  summary <- lapply(tmp[3:5], summary)
  print(levels(students$course_spec)[i])
  cat('\n')
  print(summary)
}
sink()
sink("Сравнение средних значений.txt")
for(i in 3:5){
  print(names(students)[i])
  print(kruskal.test(students[, i], students$course_spec))
  print(pairwise.wilcox.test(students[, i], students$course_spec))
  cat("\n")
}
sink()
for(i in 1:length(levels(students$course_spec))){
  tmp <- subset(students, students$course_spec == levels(students$course_spec)[i])
  for(j in 3:5){
    png(filename = paste("Гистограмма ", levels(students$course_spec)[i], " ", gsub('\\.', ' ', names(tmp)[j]), '.png', sep=''))
    par(las = 2, mar = c(12.1, 5.1, 4.1, 2.1))
    hist(tmp[,j], ylab = "Частота", xlab = gsub('\\.', ' ', names(tmp)[j]), main = levels(students$course_spec)[i])
    dev.off()
  }
}
for(i in 3:5){
  png(filename = paste("Диаграмма размаха ", gsub('\\.', ' ', names(students)[i]), '.png', sep=''))
  par(las = 2, mar = c(12.1, 5.1, 4.1, 2.1))
  boxplot(students[,i] ~ students$course_spec, ylab = gsub('\\.', ' ', names(students)[i]))
  dev.off()
}
