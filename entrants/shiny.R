library(shiny)
library(tidyverse)
library(jsonlite)
library
ui <- fluidPage(
  titlePanel('Статистика приемной кампании'),
  mainPanel(
    plotOutput(outputId = 'entrantsPlot'),
    plotOutput(outputId = 'entrantsCumPlot'),
    plotOutput(outputId = 'marksPlot')
  )
)

server <- function(input, output){
  api_url <- 'http://priem.me/api/stats/'
  campaigns = data.frame(fromJSON(paste(api_url, 'campaigns', sep = '/')))
  names(campaigns) <- c('id', 'name', 'year')
  
  df <- data.frame(row.names = c('year', 'subject', 'value'))
  for(i in 1:length(campaigns)){
    year <- campaigns$year[i]
    name <- paste(campaigns$name[i], year, sep = ' ')
    url <- paste(api_url, year, 'marks', sep = '/')
    data <- data.frame(fromJSON(url))
    names(data) <- c('subject', 'value')
    data$value <- as.numeric(as.character(data$value))
    data$name <- name
    df <- rbind(df, data)
  }
  
  df1 <- data.frame(row.names = c('day', 'entrants', 'name'))
  for(i in 1:length(campaigns)){
    year <- campaigns$year[i]
    name <- paste(campaigns$name[i], year, sep = ' ')
    url <- paste(api_url, year, 'entrants', sep = '/')
    data <- data.frame(fromJSON(url))
    data <- as.data.frame(table(data))
    names(data) <- c('day', 'entrants')
    data$day <- seq(1:length(data$day))
    data$name <- name
    df1 <- rbind(df1, data)
  }
  
  df1 <- df1 %>% group_by(name) %>% mutate(cum_sum = cumsum(entrants))
  
  output$entrantsPlot <- renderPlot({
    ggplot(df1, aes(x = day, y = entrants, col = name)) + 
      geom_point() + 
      geom_line() + 
      labs(x = 'День подачи документов', y = 'Количество поступающих', col = 'Приёмная кампания') +
      ggtitle('Количество подавших документы по дням приема')
  })
  output$entrantsCumPlot <- renderPlot({
    ggplot(df1, aes(x = day, y = cum_sum, col = name)) + 
      geom_line() + 
      labs(x = 'День подачи документов', y = 'Общее количество поступающих', col = 'Приёмная кампания') +
      ggtitle('Количество подавших документы по дням приема с накоплением')
  })
  output$marksPlot <- renderPlot({
    ggplot(df, aes(x = subject, y = value, col = name)) + 
      stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', position = 'dodge', width = 0.3) + 
      labs(x = 'Предмет', y = 'Оценка', col = 'Приёмная кампания') + 
      ggtitle('Средний балл поступающих по ЕГЭ')
  })
}

shinyApp(ui = ui, server = server)