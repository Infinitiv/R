library(tidyverse)
library(Hmisc)

a <- 0.95
dir.create('~/R/stud_stat/suleymangadzhieva/output', recursive = T)
df <- read.csv('~/R/data/suleymangadzhieva/data.csv')
df$sex <- as.factor(df$sex)
df$group <- as.factor(df$group)
df$diabetes <- as.factor(df$diabetes)
df$smoking <- as.factor(df$smoking)
df$hypertension <- as.factor(df$hypertension)
df$ECGaxis <- as.factor(df$ECGaxis)
df$stent <- as.factor(df$stent)
df$percutaneousCoronaryIntervention <- as.factor(df$percutaneousCoronaryIntervention)

for(i in 2:length(names(df))){
  if(is.numeric(df[, i])){
    ggplot(df, aes(x = df[, i])) +
      geom_histogram(bins = 10) + 
      xlab(label = names(df)[i])
    ggsave(filename = paste('~/R/stud_stat/suleymangadzhieva/output/histogram_', names(df)[i], '.png', sep = ''), width = 20, height = 10)
    ggplot(df, aes(y = df[, i], x = group, color = sex)) +
      geom_boxplot() +
      ylab(label = names(df)[i])
    ggsave(filename = paste('~/R/stud_stat/suleymangadzhieva/output/boxplot_', names(df)[i], '.png', sep = ''), width = 20, height = 10)
  }
}

for(i in 2:length(names(df))){
  if(is.numeric(df[, i])){
    if(all(tapply(df[, i], df$group, function(x){shapiro.test(x)$p.value}) > 0.05)){
      test.name <- 'критерий Стьюдента'
      p <- t.test(df[, i] ~ df$group)$p.value
      if(p < (1 - a)){
        print(paste(names(df)[i], test.name, sep = ' - '))
        print(tapply(df[, i], df$group, function(x){paste('m = ', round(mean(x, na.rm = T), 4),
                                                          '±',
                                                          round(sd(x, na.rm = T), 4),
                                                          sep = '')}))
      }
    }
    else {
      test.name <- 'критерий Уилкоксона'
      p <- wilcox.test(df[, i] ~ df$group)$p.value
      if(p < (1 - a)){
        print(paste(names(df)[i], test.name, sep = ' - '))
        print(tapply(df[, i], df$group, function(x){paste('me = ', median(x, na.rm = T),
                                                          ' (',
                                                          paste(round(quantile(x, probs = c(0.25, 0.75), na.rm = T), 4), collapse = ', '),
                                                          ')',
                                                          sep = '')}))
      }
    }
    if(p < (1 - a)){
      print(paste('p = ', p, sep = ''))
      print('')
    }
  }
  if(is.factor(df[, i])){
    p <- fisher.test(df[, i], df$group)$p.value
    if(p < (1 - a)){
      print(paste(names(df)[i], 'точный критерий Фишера', sep = ' - '))
      print(tapply(df[, i], df$group, function(x){table(x)}))
      print(tapply(df[, i], df$group, function(x){round(table(x)*100/length(x), 2)}))
      print(fisher.test(df[, i], df$group)$p.value)
      print('')
    }
  }
}

df_smoking <- df %>% select(smoking, body.mass.index, systolicBP, diastolicBP, ЗСЛЖ)

for(i in 2:length(names(df_smoking))){
  if(is.numeric(df_smoking[, i])){
    p <- wilcox.test(df_smoking[, i] ~ df_smoking$smoking)$p.value
    if(p <= 1 - a){
      print(paste(names(df_smoking)[i], 'wilcox.test', sep = ' - '))
      print(tapply(df_smoking[, i], df_smoking$smoking, function(x){mean(x, na.rm = T)}))
      print(paste('p = ', wilcox.test(df_smoking[, i] ~ df_smoking$smoking)$p.value, sep = ''))
      print('')
    }
  }
  if(is.factor(df_smoking[, i])){
    p <- fisher.test(df_smoking[, i], df_smoking$smoking)$p.value
    if(p <= 1 - a){
      print(paste(names(df_smoking)[i], 'fisher.test', sep = ' - '))
      print(tapply(df_smoking[, i], df_smoking$smoking, function(x){table(x)*100/length(x)}))
      print(fisher.test(df_smoking[, i], df_smoking$smoking)$p.value)
      print('')
    }
  }
}

df_diabetes <- df %>% select(diabetes, body.mass.index, systolicBP, diastolicBP, ЗСЛЖ, LDL, HDL)

for(i in 2:length(names(df_diabetes))){
  if(is.numeric(df_diabetes[, i])){
    p <- wilcox.test(df_diabetes[, i] ~ df_diabetes$diabetes)$p.value
    if(p <= 1 - a){
      print(paste(names(df_diabetes)[i], 'wilcox.test', sep = ' - '))
      print(tapply(df_diabetes[, i], df_diabetes$diabetes, function(x){mean(x, na.rm = T)}))
      print(paste('p = ', wilcox.test(df_diabetes[, i] ~ df_diabetes$diabetes)$p.value, sep = ''))
      print('')
    }
  }
  if(is.factor(df_diabetes[, i])){
    p <- fisher.test(df_diabetes[, i], df_diabetes$diabetes)$p.value
    if(p <= 1 - a){
      print(paste(names(df_diabetes)[i], 'fisher.test', sep = ' - '))
      print(tapply(df_diabetes[, i], df_diabetes$diabetes, function(x){table(x)*100/length(x)}))
      print(fisher.test(df_diabetes[, i], df_diabetes$diabetes)$p.value)
      print('')
    }
  }
}

cor.test(df$body.mass.index, df$systolicBP, method = 'kendall')

shapiro.test(rnorm(1000, mean = 5, sd = 3))

wilcox.test(df$Left.vetricular.miocardial.mass ~ df$hypertension)
                   
