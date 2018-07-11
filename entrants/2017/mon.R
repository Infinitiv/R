library(tidyverse)
library(stargazer)

df <- read.csv('~/base_20170808.csv')
df_select <- select(df, application_number, region_id, enrolled, chemistry_form, biology_form, russian_form, chemistry, biology, russian, education_document_type, olympionic)
df_enrolled <- filter(df_select, !is.na(enrolled))
df_enrolled$code[df_enrolled$enrolled > 18 & df_enrolled$enrolled < 23] <- '31.05.01'
df_enrolled$code[df_enrolled$enrolled > 22 & df_enrolled$enrolled < 27] <- '31.05.02'
df_enrolled$code[df_enrolled$enrolled > 26 & df_enrolled$enrolled < 31] <- '31.05.03'
df_enrolled$source <- 'budget'
df_enrolled$source[df_enrolled$enrolled == 20 | df_enrolled$enrolled == 24 | df_enrolled$enrolled == 28] <- 'paid'
df_enrolled$type <- 'common'
df_enrolled$type[df_enrolled$enrolled == 21 | df_enrolled$enrolled == 25 | df_enrolled$enrolled == 29] <- 'benefit'
df_enrolled$type[df_enrolled$enrolled == 22 | df_enrolled$enrolled == 26 | df_enrolled$enrolled == 30] <- 'target'
df_enrolled$region_id <- as.factor(df_enrolled$region_id)
table(filter(df_enrolled, region_id == '37')$code)
table(df_enrolled$education_document_type)
ege <- filter(df_enrolled, type == 'common', chemistry_form == 'ЕГЭ', biology_form == 'ЕГЭ', russian_form == 'ЕГЭ', olympionic != 1)
table(ege$code)
table(filter(ege, source == 'paid')$code)
mean(sapply(select(filter(ege, code == '31.05.01', source == 'budget'), chemistry, biology, russian), min))
mean(sapply(select(filter(ege, code == '31.05.02', source == 'budget'), chemistry, biology, russian), min))
mean(sapply(select(filter(ege, code == '31.05.03', source == 'budget'), chemistry, biology, russian), min))
mean(sapply(select(filter(ege, code == '31.05.01', source == 'paid'), chemistry, biology, russian), min))
mean(sapply(select(filter(ege, code == '31.05.02', source == 'paid'), chemistry, biology, russian), min))
mean(sapply(select(filter(ege, code == '31.05.03', source == 'paid'), chemistry, biology, russian), min))

mean(unlist(select(filter(ege, code == '31.05.01', source == 'budget'), chemistry, biology, russian)))
mean(unlist(select(filter(ege, code == '31.05.02', source == 'budget'), chemistry, biology, russian)))
mean(unlist(select(filter(ege, code == '31.05.03', source == 'budget'), chemistry, biology, russian)))
mean(unlist(select(filter(ege, code == '31.05.01', source == 'paid'), chemistry, biology, russian)))
mean(unlist(select(filter(ege, code == '31.05.02', source == 'paid'), chemistry, biology, russian)))
mean(unlist(select(filter(ege, code == '31.05.03', source == 'paid'), chemistry, biology, russian)))

ege_target <- filter(df_enrolled, type == 'target', chemistry_form == 'ЕГЭ', biology_form == 'ЕГЭ', russian_form == 'ЕГЭ', olympionic != 1)
mean(unlist(select(filter(ege_target, code == '31.05.01'), chemistry, biology, russian)))
mean(unlist(select(filter(ege_target, code == '31.05.02'), chemistry, biology, russian)))
mean(unlist(select(filter(ege_target, code == '31.05.03'), chemistry, biology, russian)))