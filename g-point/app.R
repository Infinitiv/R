library(shiny)
library(tidyverse)

library
ui <- fluidPage(
  titlePanel('Рейтинг G-point'),
  fluidRow(
    column(12,
           dataTableOutput('table')
    )
  )
)

server <- function(input, output){
  df <- read.csv('/home/markovnin/R/data/g-point/g-point.csv')
  n <- length(unique(df$date))
  report <- df %>% group_by(title) %>% summarise(season_sum = ifelse(n() < n, sum(summa), sum(summa) - min(summa))) %>% arrange(desc(season_sum))
  output$table <- renderDataTable(report,
                                  options = list(
                                    pageLength = 50
                                  )
  )
}

shinyApp(ui = ui, server = server)