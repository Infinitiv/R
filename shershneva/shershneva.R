 library(tidyverse)
df <- read.csv('~/R/data/shershneva/data.csv', header = T) 

age <- df %>% group_by(группа) %>% summarise(mean = mean_se(возраст)) 
age$mean$y - age$mean$ymin
df$механизм.травмы <- as.factor(df$механизм.травмы)
fisher.test(df$механизм.травмы, df$группа)$p.value

fisher.test(df$давность.травмы, df$группа)$p.value

fisher.test(df$жалобы.боль.в.кистевом.суставе, df$группа)$p.value

fisher.test(df$жалобы.ограничение.движений, df$группа)$p.value

fisher.test(df$жалобы.отек.кисти, df$группа)$p.value

chisq.test(df$ВАШ, df$группа)$p.value

chisq.test(df$условия.возникновения.боли, df$группа)$p.value

fisher.test(df$клиика.отек, df$группа)$p.value

fisher.test(df$клиника.боль.при.пальпации.по.тылу.кистевого.сустава, df$группа)$p.value

fisher.test(df$клиника.ограничение.функции, df$группа)$p.value

fisher.test(df$клиника.деформация.в.кистевом.суставе, df$группа)$p.value

fisher.test(df$клиника.укорочение, df$группа)$p.value

fisher.test(df$клиника.наличие.неврологических.нарушений, df$группа)$p.value

fisher.test(df$клиника.наличие.сосудистых.нарушений, df$группа)$p.value

fisher.test(df$результаты.дополн.обследоваия...SL.диастаз.более.3.мм, df$группа)$p.value

fisher.test(df$результаты.дополн.обследования...SL.угол.более.60., df$группа)$p.value

fisher.test(df$результаты.дополн.обследования...симптом.кольца, df$группа)$p.value

fisher.test(df$рентгенол.прзнаки.отсутствуют..выявлено.на.артрографии, df$группа)$p.value

fisher.test(df$рентгенологически.перелом.лучевой.кости, df$группа)$p.value

fisher.test(df$рентгенологически.перилунарный.вывих, df$группа)$p.value

fisher.test(df$контраст.проникает.в.SL.при.артрографии, df$группа)$p.value

d1 <- matrix(c(49, 62, 145, 22, 19, 30, 15, 18), nrow = 2, ncol = 4)
fisher.test(d1)

fisher.test(df$отдален..ВАШ, df$группа)$p.value
