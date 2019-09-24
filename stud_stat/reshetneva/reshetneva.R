library('tidyverse')
df <- read.csv('~/R/data/reshetneva/reshetneva.csv')
df$количество.очагов <- as.factor(df$количество.очагов)
sink(file = '~/R/stud_stat/reshetneva/output/text.txt')
df %>% group_by(group = группа) %>% summarise(treatment_duration = mean(длительность.терапии..нед., na.rm = T),
                                              age = mean(возраст, na.rm = T),
                                              weight = mean(масса.тела..кг, na.rm = T),
                                              day = mean(день.обращения, na.rm = T))
wilcox.test(df$длительность.терапии..нед. ~ df$группа)
wilcox.test(df$возраст ~ df$группа)
wilcox.test(df$масса.тела..кг ~ df$группа)
wilcox.test(df$день.обращения ~ df$группа)

tapply(df$место.обучения, df$группа, table)
fisher.test(df$место.обучения, df$группа)

tapply(df$место.жительства, df$группа, table)
fisher.test(df$место.жительства, df$группа)

tapply(df$контакт.с.животными, df$группа, table)
fisher.test(df$контакт.с.животными, df$группа)
  
tapply(df$контакт.с.больными.людьми, df$группа, table)
fisher.test(df$контакт.с.больными.людьми, df$группа)

tapply(df$контакт, df$группа, table)
fisher.test(df$контакт, df$группа)

tapply(df$поражение.гладкой.кожи, df$группа, table)
fisher.test(df$поражение.гладкой.кожи, df$группа)

tapply(df$поражение.ногтей, df$группа, table)
fisher.test(df$поражение.ногтей, df$группа)

tapply(df$соблюдение.условий.приема, df$группа, table)
fisher.test(df$соблюдение.условий.приема, df$группа)

tapply(df$комплаенс, df$группа, table)
fisher.test(df$комплаенс, df$группа)

tapply(df$побочные.эффекты, df$группа, table)
fisher.test(df$побочные.эффекты, df$группа)

tapply(df$количество.очагов, df$группа, table)
fisher.test(df$количество.очагов, df$группа)

tapply(df$вид.побочных.эффектов, df$группа, table)
sink()