d <- read.csv('prognoz.csv')
d$киста.прошла <- factor(d$prognoz)
d <- d[order(d$sum), ]
fit.2 <- glm(prognoz ~ sum, data=d, family=binomial)
png(filename = 'prognoz1.png', width = 1024, height = 768)
plot(d$sum, predict(fit.2, type="response"), xlab = "Сумма баллов", ylab = "Вероятность, что киста пройдет")
lines(d$sum, predict(fit.2, type="response"))
dev.off()
d <- d[order(d$sum2), ]
fit.2 <- glm(prognoz ~ sum2, data=d, family=binomial)
png(filename = 'prognoz2.png', width = 1024, height = 768)
plot(d$sum2, predict(fit.2, type="response"), xlab = "Сумма баллов", ylab = "Вероятность, что киста пройдет")
lines(d$sum2, predict(fit.2, type="response"))
dev.off()
d <- d[order(d$sum3), ]
fit.2 <- glm(prognoz ~ sum3, data=d, family=binomial)
png(filename = 'prognoz3.png', width = 1024, height = 768)
plot(d$sum3, predict(fit.2, type="response"), xlab = "Сумма баллов", ylab = "Вероятность, что киста пройдет")
lines(d$sum3, predict(fit.2, type="response"))
dev.off()
d <- d[order(d$sum4), ]
fit.2 <- glm(prognoz ~ sum4, data=d, family=binomial)
png(filename = 'prognoz4.png', width = 1024, height = 768)
par(mar=(c(5,6,5,2)))
plot(d$sum4, predict(fit.2, type="response"), xlab = "Сумма баллов", ylab = "Вероятность регресса фолликулярной кисты", cex.axis = 2, cex.lab = 2)
lines(d$sum4, predict(fit.2, type="response"))
dev.off()