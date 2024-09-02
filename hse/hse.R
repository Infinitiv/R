library(tidyverse)
library(rvest)
library(urltools)
base_url <- 'https://ege.hse.ru'
parsed_base_url <- url_parse(base_url)
base_html <- read_html(base_url)
urls <- base_html %>% html_nodes('.navigation_dpo') %>% html_nodes('a') %>% html_attr('href')
df1 <- data.frame(matrix(ncol = 4, nrow = 0))
df2 <- data.frame(matrix(ncol = 5, nrow = 0))
for(i in 1:length(urls)){
  current_url <- urls[i]
  parsed_current_url <- url_parse(current_url)
  if(grepl('rating', parsed_current_url$path)){
    parsed_current_url$scheme <- parsed_base_url$scheme
    parsed_current_url$domain <- parsed_base_url$domain
    current_url <- url_compose(parsed_current_url)
    current_html <- read_html(current_url)
    file_name <- '~/R/data/hse/data.csv'
    name <- current_html %>% html_nodes('.titla') %>% html_text()
    form <- if_else(grepl("Платн|платн", name), 'внебюджет', 'бюджет')
    year <- as.numeric(str_extract(name, "[:digit:][:digit:][:digit:][:digit:]"))
    names <- trimws(current_html %>% html_nodes('.transparence') %>% html_nodes('th') %>% html_text())
    d <- matrix(trimws(current_html %>% html_nodes('.transparence') %>% html_nodes('td') %>% html_text()), ncol = length(names), byrow = T)
    d <- as.data.frame(d)
    if(!is.na(year) & year > 2014){
      if(grepl('группа', names[1])){
        d <- d %>% select(c(1:3)) %>% mutate(Форма = form, Год = year)
        df2 <- rbind(df2, d)
      }
      else{
        d <- d %>% select(c(1:2)) %>% mutate(Форма = form, Год = year)
        df1 <- rbind(df1, d)
      }
    }
  }
}
names(df1) <- c('Вуз', 'ЕГЭ', 'Форма', 'Год')
df1$ЕГЭ <- as.numeric(as.character(df1$ЕГЭ))
df1$Год <- as.factor(df1$Год)
df1 <- df1 %>% arrange(Форма, Год, desc(ЕГЭ))
names(df2) <- c('Группа', 'Вуз', 'ЕГЭ', 'Форма', 'Год')
df2$ЕГЭ <- as.numeric(df2$ЕГЭ)
df2$Год <- as.factor(df2$Год)
df2 <- df2 %>% arrange(Форма, Год, desc(ЕГЭ))
write.csv(df1, file = '~/R/data/hse/data1.csv', row.names = F)
write.csv(df2, file = '~/R/data/hse/data2.csv', row.names = F)

ranks <- data.frame(matrix(ncol = 5, nrow = 0))
names(ranks) <- c('Год', 'Тип', 'Форма', 'Количество', 'Место')

for(i in 1:length(levels(df1$Год))){
  year <- levels(df1$Год)[i]
  # общий бюджет
  tmp <- subset(df1, Год == year & Форма == 'бюджет')
  n_inst <- tmp %>% nrow()
  rank <- which(tmp$Вуз == 'Ивановская гос. медицинская академия')
  ranks <- rbind(ranks, data.frame(Год = year, Тип = 'общий', Форма = 'бюджет', Количество = n_inst, Место = rank))
  # общий внебюджет
  tmp <- subset(df1, Год == year & Форма == 'внебюджет')
  n_inst <- tmp %>% nrow()
  rank <- which(tmp$Вуз == 'Ивановская гос. медицинская академия')
  ranks <- rbind(ranks, data.frame(Год = year, Тип = 'общий', Форма = 'внебюджет', Количество = n_inst, Место = rank))
  # общий бюджет Иваново
  tmp <- subset(df1, Год == year & Форма == 'бюджет' & grepl('Ивановск', Вуз))
  n_inst <- tmp %>% nrow()
  rank <- which(tmp$Вуз == 'Ивановская гос. медицинская академия')
  ranks <- rbind(ranks, data.frame(Год = year, Тип = 'региональный', Форма = 'бюджет', Количество = n_inst, Место = rank))
  # общий внебюджет Иваново
  tmp <- subset(df1, Год == year & Форма == 'внебюджет' & grepl('Ивановск', Вуз))
  n_inst <- tmp %>% nrow()
  rank <- which(tmp$Вуз == 'Ивановская гос. медицинская академия')
  ranks <- rbind(ranks, data.frame(Год = year, Тип = 'региональный', Форма = 'внебюджет', Количество = n_inst, Место = rank))
  # по направлению бюджет
  tmp <- subset(df2, Год == year & Форма == 'бюджет' & Группа == 'Здравоохранение')
  n_inst <- tmp %>% nrow()
  rank <- which(tmp$Вуз == 'Ивановская гос. медицинская академия')
  ranks <- rbind(ranks, data.frame(Год = year, Тип = 'здравоохранение', Форма = 'бюджет', Количество = n_inst, Место = rank))
  # по направлению внебюджет
  tmp <- subset(df2, Год == year & Форма == 'внебюджет'  & Группа == 'Здравоохранение')
  n_inst <- tmp %>% nrow()
  rank <- which(tmp$Вуз == 'Ивановская гос. медицинская академия')
  ranks <- rbind(ranks, data.frame(Год = year, Тип = 'здравоохранение', Форма = 'внебюджет', Количество = n_inst, Место = rank))
}

ranks <- ranks %>% mutate(Процентиль = round(Место*100/Количество))
ranks_med <- ranks %>% filter(Тип == 'здравоохранение')

ggplot(ranks, aes(y = Процентиль, x = Год, group = Форма, col = Форма)) + 
  geom_point() +
  geom_line() +
  scale_y_reverse() +
  facet_wrap(~Тип) +
  theme_minimal()

write.csv(ranks, file = '~/R/data/hse/ranks.csv', row.names = F)
