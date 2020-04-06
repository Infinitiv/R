library(tidyverse)
library(jsonlite)
url_api <- 'https://rating.chgk.info/api'
town_url <- paste(url_api, 'teams.json/search?town=Иваново', sep = '/')
teams <- fromJSON(town_url, simplifyDataFrame = T)$items
tournaments <- data.frame(team_id = integer(), team_name = character(), tournament_name = character(), tournament_date = character(), tournament_id = integer(), tournament_questions = integer())
for(i in 1:length(teams$idteam)){
  team_id <- teams$idteam[i]
  if(as.numeric(teams$tournaments_total[i]) > 0){
    team_tournaments_url <- paste(url_api, 'teams', team_id, 'tournaments', sep = '/')
    team_tourmaments <- fromJSON(team_tournaments_url)
    for(j in 1:length(team_tourmaments)){
      tournaments_list <- as.character(unlist(as.data.frame(team_tourmaments[j])[3]))
      for(n in 1:length(tournaments_list)){
        tournament_url <- paste(url_api, 'tournaments', tournaments_list[n], sep = '/')
        tournament <- fromJSON(tournament_url, simplifyDataFrame = T)
        tournament_date <- tournament$date_start
        new_row <- data.frame(as.numeric(team_id), teams$name[i], tournament$name, as.Date(tournament_date), as.numeric(tournament$idtournament), as.numeric(tournament$questions_total))
        tournaments <- rbind(tournaments, new_row)
      }
    }
  }
}
names(tournaments) <- c('team_id', 'team_name', 'tournament_name', 'tournament_date', 'tournament_id', 'tournament_questions')
tournaments <- tournaments %>% 
  mutate(tournament_year = format(as.Date(tournament_date), '%Y')) %>% 
  filter(tournament_year == 2019) %>% group_by(tournament_id) %>% summarise(n = n(), questions_count = mean(tournament_questions))
rating <- data.frame(team_id = integer(),
                     team_name = character(),
                     tournament_current_name = character(),
                     tournament_base_name = character(),
                     tournament_questions_count = integer(),
                     tournament_date = character(),
                     questions_total = integer(),
                     tournametn_long_name = character())
for(i in 1:length(tournaments$tournament_id)){
  tournament_id <- tournaments$tournament_id[i]
  city_id <- 114
  tournament_url <- paste(url_api, 'tournaments', tournament_id, sep = '/')
  tournament <- fromJSON(tournament_url, simplifyDataFrame = T)
  tournament_teams_url <- paste(url_api, 'tournaments', tournament_id, 'list', 'town', city_id, sep = '/')
  tournament_teams <- fromJSON(tournament_teams_url, simplifyDataFrame = T)
  tournament_teams <- tournament_teams %>% select(idteam, current_name, base_name, questions_total) %>% mutate(tournament_date = tournament$date_start, tournament_questions_count = tournament$questions_total, tournament_long_name = tournament$long_name)
  rating <- rbind(rating, tournament_teams)
}
write.csv(rating, file = '/home/markovnin/R/chgk/output/rating.csv')


rating_2019 <- read_csv('/home/markovnin/R/chgk/output/rating_2019.csv')
rt <- 20
rating_compare <- data.frame(base_name = character(), sum = numeric())
# старая схема (20 игр в зачет, коэффициент 1,5 на сложные турниры)
default <- rating_2019  %>% 
  mutate(points = questions_total * coef) %>% 
  group_by(base_name) %>%
  top_n(rt, points) %>%
  summarise(sum = sum(head(points, rt))) %>%
  arrange(desc(sum))
write.csv(default, file = '/home/markovnin/R/chgk/output/default.csv')
rating_compare <- default

# простая сумма всех турниров за год
simple_sum <- rating_2019  %>% 
  mutate(points = questions_total * 1) %>% 
  group_by(base_name) %>% 
  summarise(sum = sum(points)) %>% 
  arrange(desc(sum))
write.csv(simple_sum, file = '/home/markovnin/R/chgk/output/simple_sumt.csv')
rating_compare <- cbind(rating_compare, simple_sum)

# учет коэффициента сложности турнира
dl <- rating_2019  %>% 
  mutate(points = questions_total * difficulty) %>% 
  group_by(base_name) %>% 
  top_n(rt, points) %>% 
  summarise(sum = sum(head(points, rt))) %>% 
  arrange(desc(sum))
write.csv(dl, file = '/home/markovnin/R/chgk/output/dl.csv')
rating_compare <- cbind(rating_compare, dl)

# учет коэффициента сложности турнира и количества вопросов в турнире
dl_q <- rating_2019  %>% 
  mutate(points = (questions_total * difficulty)/tournament_questions_count) %>% 
  group_by(base_name) %>% 
  top_n(rt, points) %>% 
  summarise(sum = sum(head(points, rt))) %>% 
  arrange(desc(sum))
write.csv(dl_q, file = '/home/markovnin/R/chgk/output/dl_q.csv')
rating_compare <- cbind(rating_compare, dl_q)

st <- 7
season_top <- rating_2019 %>%
  mutate(points = questions_total * 1) %>%
  group_by(tournament_long_name) %>%
  mutate(rank = rank(points)) %>%
  group_by(season, base_name) %>%
  top_n(st, rank) %>%
  summarise(sum = sum(head(rank, st))) %>%
  arrange(season, desc(sum)) %>%
  group_by(base_name) %>%
  summarise(sum = sum(sum)) %>%
  arrange(desc(sum))
write.csv(season_top, file = '/home/markovnin/R/chgk/output/season_top.csv')
rating_compare <- cbind(rating_compare, season_top)