library(tidyverse)
library(ggplot2)

target <- read.csv('~/R/data/entrants/target.csv')

target <- target %>%
  mutate(exeptioned_denied = enrolled + exeptioned)

numbers_by_year <- target %>%
  group_by(year) %>%
  summarise(numbers_sum = sum(number, na.rm = T), 
            applications_sum = sum(applications, na.rm = T), 
            enrolled_sum = sum(enrolled, na.rm = T), 
            exeptioned_sum = sum(exeptioned, na.rm = T),
            enrolled_percent = round(enrolled_sum/numbers_sum, 2),
            exeptioned_denied_sum = sum(exeptioned_denied, na.rm = T),
            exeptioned_denied_percent = round(exeptioned_denied_sum/numbers_sum, 2)
            )
ggplot(numbers_by_year, aes(x = year)) +
  geom_line(aes(y = enrolled_percent, color = "Enrolled Percent")) +
  geom_line(aes(y = exeptioned_denied_percent, color = "Exeptioned Denied Percent")) +
  labs(x = "Year", y = "Percent", color = "Legend") +
  ggtitle("Enrolled and Exeptioned Denied Percent Over the Years") +
  theme_minimal()

numbers_by_year_and_speciality <- target %>%
  group_by(year, speciality) %>%
  summarise(numbers_sum = sum(number, na.rm = T), 
            applications_sum = sum(applications, na.rm = T), 
            enrolled_sum = sum(enrolled, na.rm = T), 
            exeptioned_sum = sum(exeptioned, na.rm = T),
            enrolled_percent = round(enrolled_sum/numbers_sum, 2),
            exeptioned_denied_sum = sum(exeptioned_denied, na.rm = T),
            exeptioned_denied_percent = round(exeptioned_denied_sum/numbers_sum, 2)
  )

year_to_plot <- 2023

ggplot(numbers_by_year_and_speciality, aes(x = speciality, y = numbers_sum)) +
  geom_bar(stat = "identity") +
  labs(x = "Speciality", y = "Total Numbers") +
  ggtitle(paste("Total Numbers by Speciality in", year_to_plot)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability
numbers_by_year_ivanovo <- target %>%
  filter(organization == 'Департамент здравоохранения Ивановской области') %>%
  group_by(year) %>%
  summarise(numbers_sum = sum(number, na.rm = T), 
            applications_sum = sum(applications, na.rm = T), 
            enrolled_sum = sum(enrolled, na.rm = T), 
            exeptioned_sum = sum(exeptioned, na.rm = T),
            enrolled_percent = round(enrolled_sum/numbers_sum, 2),
            exeptioned_denied_sum = sum(exeptioned_denied, na.rm = T),
            exeptioned_denied_percent = round(exeptioned_denied_sum/numbers_sum, 2)
  )

numbers_by_year_and_speciality_ivanovo <- target %>%
  filter(organization == 'Департамент здравоохранения Ивановской области') %>%
  group_by(year, speciality) %>%
  summarise(numbers_sum = sum(number, na.rm = T), 
            applications_sum = sum(applications, na.rm = T), 
            enrolled_sum = sum(enrolled, na.rm = T), 
            exeptioned_sum = sum(exeptioned, na.rm = T),
            enrolled_percent = round(enrolled_sum/numbers_sum, 2),
            exeptioned_denied_sum = sum(exeptioned_denied, na.rm = T),
            exeptioned_denied_percent = round(exeptioned_denied_sum/numbers_sum, 2)
  )

write_csv(numbers_by_year, '~/R/entrants/numbers_by_year.csv')
  