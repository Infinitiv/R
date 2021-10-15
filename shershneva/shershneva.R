library(tidyverse)
df <- read.csv('~/R/data/shershneva/data.csv', header = T) 

age <- df %>% group_by(группа, пол) %>% summarise(mean = mean(возраст))
age <- df %>% group_by(пол) %>% summarise(mean = mean(возраст))
age <- df %>% group_by(группа) %>% summarise(mean = mean_se(возраст))
age$mean$y - age$mean$ymin
age <- df %>% filter(пол == "жен.") %>% group_by(группа) %>% summarise(mean = mean_se(возраст))
age$mean$y - age$mean$ymin
age <- df %>% filter(пол == "муж.") %>% group_by(группа) %>% summarise(mean = mean_se(возраст))
age$mean$y - age$mean$ymin
mean_age <- df %>% group_by(пол) %>% summarise(mean = mean_se(возраст))
mean_age$mean$y - mean_age$mean$ymin

gender <- df %>% group_by(группа, пол) %>% summarise(n = n())
tapply(df$пол, df$группа, function(x){table(x)*100/length(x)})
table(df$пол)*100/length(df$пол)
table(df$пол)

fisher.test(df$возраст, df$пол)

d <- subset(df, df$группа == "группа 4")
chisq.test(df$возраст, df$пол)

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

d <- matrix(c(49, 62, 145, 22, 19, 30, 15, 18), nrow = 2, ncol = 4)
fisher.test(d)

fisher.test(df$отдален..ВАШ, df$группа)$p.value

chisq.test(df$отдал..Мейо, df$группа)$p.value
chisq.test(df$Ранние.результаты.Мейо, df$группа)$p.value

chisq.test(df$отдал..DASH, df$группа)$p.value
chisq.test(df$Ранние.DASH, df$группа)$p.value

chisq.test(df$Отдален..жалобы.на.отек, df$группа)$p.value
chisq.test(df$отдален..ВАШ, df$группа)$p.value
chisq.test(df$отделен...Жалобы.на.слабость.в.кисти, df$группа)$p.value
chisq.test(df$отделен..Снижение.на.динамоментии..разница., df$группа)$p.value
chisq.test(df$отдален..Количество.пациентов.с.ограничением.движений.в.той.или.иной.плоскости, df$группа)$p.value
chisq.test(df$отдален...Увеличение.SL.щирины.и.угла.на.станд..Рентгенограмме.и.с.нагрузкой, df$группа)$p.value
chisq.test(df$отдал..Уменьшение.SL.дистаза.и.угла.на.стандарт..Рентгенограмме.и.с.нагрузкой, df$группа)$p.value
chisq.test(df$отделн..Уменьшение.SL.диастаза.и.угла.на.станд..Рентгенограмме..но.есть.с.нагрузкой, df$группа)$p.value
chisq.test(df$отдален..Без.динамики, df$группа)$p.value
chisq.test(df$отдален, df$группа)$p.value

chisq.test(df$отдален...остеоартрит, df$группа)$p.value
