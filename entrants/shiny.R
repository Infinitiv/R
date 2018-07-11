library(shiny)
library(tidyverse)
ui <- fluidPage(
  titlePanel('Динамика подачи заявлений'),
  mainPanel(
    plotOutput(outputId = 'entrantsPlot')
  )
)

server <- function(input, output){
  df <- data.frame(row.names = c('day', 'entrants', 'name'))
  campaigns = data.frame(fromJSON('http://priem.me/api/stats/campaigns'))
  names(campaigns) <- c('id', 'name', 'year')
  for(i in 1:length(campaigns)){
    year <- campaigns$year[i]
    name <- paste(campaigns$name[i], year, sep = ' ')
    url <- paste('http://priem.me/api/stats/', year, '/entrants', sep = '')
    data <- data.frame(fromJSON(url))
    data <- as.data.frame(table(data))
    names(data) <- c('day', 'entrants')
    data$day <- seq(1:length(data$day))
    data$name <- name
    df <- rbind(df, data)
  }
  df$year <- as.factor(df$year)
  df <- df %>% group_by(year) %>% mutate(cum_sum = cumsum(entrants))
  output$entrantsPlot <- renderPlot({
    ggplot(df, aes(x = day, y = entrants, col = year)) + geom_point() + geom_line()
  })
}

shinyApp(ui = ui, server = server)