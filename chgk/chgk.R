library(tidyverse)
library(jsonlite)
url_api <- 'https://rating.chgk.info/api'
town_url <- paste(url_api, 'teams.json/search?town=Иваново', sep = '/')
teams <- fromJSON(town_url, simplifyDataFrame = T)$items
df <- data.frame(team_id = integer(), team_name = character(), tournament_name = character(), tournament_year = integer())
for(i in 1:length(teams$idteam)){
  team_id <- teams$idteam[i]
  if(as.numeric(teams$tournaments_total[i]) > 0){
    team_tournaments_url <- paste(url_api, 'teams', team_id, 'tournaments', sep = '/')
    team_tourmametns <- fromJSON(team_tournaments_url)
    for(j in 1:length(team_tourmametns)){
      tournaments <- as.character(unlist(as.data.frame(team_tourmametns[j])[3]))
      for(n in 1:length(tournaments)){
        tournament_url <- paste(url_api, 'tournaments', tournaments[n], sep = '/')
        tournament <- fromJSON(tournament_url, simplifyDataFrame = T)
        tournament_year <- format(as.Date(tournament$date_start), '%Y')
        new_row <- data.frame(as.numeric(team_id), teams$name[i], tournament$name ,as.numeric(tournament_year))
        df <- rbind(df, new_row)
      }
    }
  }
}
names(df) <- c('team_id', 'team_name', 'tournament_name', 'tournament_year')
d <- df %>% group_by(team_id, tournament_year) %>% summarise(tournaments_count = n()) %>% group_by(tournament_year) %>% summarise(count = n(), means = round(mean(tournaments_count)))
ggplot(d, aes(x = tournament_year, y = count)) + 
  geom_line() + geom_label(aes(label = count)) + 
  scale_x_continuous(breaks = seq(min(d$tournament_year), max(d$tournament_year), 1)) +
  xlab(label = 'год') +
  ylab(label = 'количество команд') +
  theme_bw()
ggsave(filename = 'Динамика количества команда по рейтингу МАК.png', path = 'chgk/output', height = 5, width = 10)

       