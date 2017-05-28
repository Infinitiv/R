children <- read.csv('karmanova.csv')
means.sex <- lapply(children[2:5], function(y) tapply(y, children$пол, function(x) paste(round(mean(x, na.rm=T), 4), "±", round(sd(x, na.rm=T)/sqrt(length(x)), 4), sep='')))
means.stigmat <- lapply(children[2:4], function(y) tapply(y, children$степень.стигматизации, function(x) paste(round(mean(x, na.rm=T), 4), "±", round(sd(x, na.rm=T)/sqrt(length(x)), 4), sep='')))
means.stigmat.yes.no <- lapply(children[2:4], function(y) tapply(y, children$наличие.стигматизации, function(x) paste(round(mean(x, na.rm=T), 4), "±", round(sd(x, na.rm=T)/sqrt(length(x)), 4), sep='')))
percents.sex <- lapply(children[9:20], function(y) tapply(y, children$пол, function(x) paste(table(x), " чел. или ", round(100*table(x)/length(x), 2), '%', sep='')))
percents.stigmat <- lapply(children[9:20], function(y) tapply(y, children$степень.стигматизации, function(x) paste(table(x), " чел. или ", round(100*table(x)/length(x), 2), '%', sep='')))
percents.stigmat.yes.no <- lapply(children[9:20], function(y) tapply(y, children$наличие.стигматизации, function(x) paste(table(x), " чел. или ", round(100*table(x)/length(x), 2), '%', sep='')))
wilcox.sex <- lapply(children[2:5], function(x) wilcox.test(x ~ children$пол))
wilcox.stigmat.yes.no <- lapply(children[2:4], function(x) wilcox.test(x ~ children$наличие.стигматизации))
kruskal.stigmat <- lapply(children[2:4], function(x) kruskal.test(x ~ children$степень.стигматизации))
pairwise.wilcox.test.stigmat <- lapply(children[2:4], function(x) pairwise.wilcox.test(x, children$степень.стигматизации))
chisq.sex <- lapply(children[9:20], function(x) ifelse(min(table(x, children$пол)) < 5, paste(fisher.test(x, children$пол)$method, ", p = ", fisher.test(x, children$пол)$p.value), paste(chisq.test(x, children$пол)$method, "p = ", chisq.test(x, children$пол)$p.value)))
chisq.stigmat <- lapply(children[9:20], function(x) ifelse(min(table(x, children$степень.стигматизации)) < 5, paste(fisher.test(x, children$степень.стигматизации)$method, ", p = ", fisher.test(x, children$степень.стигматизации)$p.value), paste(chisq.test(x, children$степень.стигматизации)$method, "p = ", chisq.test(x, children$степень.стигматизации)$p.value)))
chisq.stigmat.yes.no <- lapply(children[9:20], function(x) ifelse(min(table(x, children$наличие.стигматизации)) < 5, paste(fisher.test(x, children$наличие.стигматизации)$method, ", p = ", fisher.test(x, children$наличие.стигматизации)$p.value), paste(chisq.test(x, children$наличие.стигматизации)$method, "p = ", chisq.test(x, children$наличие.стигматизации)$p.value)))
for(i in 2:5) {
  png(filename = paste('karmanova/', gsub('\\.', ' ', names(children)[i]), " в зависимости от пола.png", sep = ""))
  boxplot(children[,i] ~ children$пол, xlab = "Пол", ylab = gsub('\\.', ' ', names(children)[i]))
  dev.off()
}
for(i in 2:4) {
  png(filename = paste('karmanova/', gsub('\\.', ' ', names(children)[i]), " в зависимости от степени стигматизации.png", sep = ""))
  boxplot(children[,i] ~ children$степень.стигматизации, xlab = "Степень стигматизации", ylab = gsub('\\.', ' ', names(children)[i]))
  dev.off()
}
sink("karmanova/средние значения количественный признаков в зависимости от пола.txt")
means.sex
sink()
sink("karmanova/средние значения количественный признаков в зависимости от степени стигматизации.txt")
means.stigmat
sink()
sink("karmanova/доли качественных признаков в зависимости от пола.txt")
percents.sex
sink()
sink("karmanova/доли качественных признаков в зависимости от степени стигматизации.txt")
percents.stigmat
sink()
sink("karmanova/достоверность различий средних значений количественных признаков в зависимости от пола.txt")
wilcox.sex
sink()
sink("karmanova/однофакторный дисперсионный анализ количественных признаков в зависимости от степени стигматизации.txt")
kruskal.stigmat
sink()
sink("karmanova/сравнение средних значений количественных признаков в зависимости от степени стигматизации.txt")
pairwise.wilcox.test.stigmat
sink()
sink("karmanova/достоверность различия частот качественных признаков в зависимости от пола.txt")
chisq.sex
sink()
sink("karmanova/достоверность различия частот качественных признаков в зависимости от степени стигматизации.txt")
chisq.stigmat
sink()
system("soffice --headless --convert-to docx --outdir ./karmanova ./karmanova/*.txt")
system("rm ./karmanova/*.txt")
system("zip -r ./karmanova karmanova")