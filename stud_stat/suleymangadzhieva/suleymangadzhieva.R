library(tidyverse)

a <- 0.95
dir.create('~/R/stud_stat/suleymangadzhieva/output', recursive = T)
df <- read.csv('~/R/data/suleymangadzhieva/suleymangadzhieva.csv')
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
    p <- wilcox.test(df[, i] ~ df$group)$p.value
    if(p <= 1 - a){
      print(paste(names(df)[i], 'wilcox.test', sep = ' - '))
      print(tapply(df[, i], df$group, function(x){mean(x, na.rm = T)}))
      print(paste('p = ', wilcox.test(df[, i] ~ df$group)$p.value, sep = ''))
      print('')
    }
  }
  if(is.factor(df[, i])){
    p <- fisher.test(df[, i], df$group)$p.value
    if(p <= 1 - a){
      print(paste(names(df)[i], 'fisher.test', sep = ' - '))
      print(tapply(df[, i], df$group, function(x){table(x)*100/length(x)}))
      print(fisher.test(df[, i], df$group)$p.value)
      print('')
    }
  }
}

wilcox.test(df$HDL ~ df$smoking)
                   