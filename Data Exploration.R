library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(tidytext)

nov2017 <- read_csv("Parking_Violations_Issued_in_November_2017.csv")
dec2017 <- read_csv("Parking_Violations_Issued_in_December_2017.csv")
jan2018 <- read_csv("Parking_Violations_Issued_in_January_2018.csv")
feb2018 <- read_csv("Parking_Violations_Issued_in_February_2018.csv")

data <- bind_rows(nov2017, dec2017, jan2018, feb2018)

data %>% 
  group_by(VIOLATION_CODE) %>% 
  tally() %>% arrange(desc(n))

data %>% 
  group_by(VIOLATION_DESCRIPTION) %>%
  tally() %>% 
  arrange(desc(n))

data %>% 
  filter(is.na(VIOLATION_DESCRIPTION)) %>% View()

data %>% 
  group_by(RP_PLATE_STATE) %>% 
  tally() %>% 
  arrange(desc(n))

data <- data %>% 
  mutate("day_of_week" = case_when(wday(TICKET_ISSUE_DATE) == 1 ~ "Sunday",
                                   wday(TICKET_ISSUE_DATE) == 2 ~ "Monday",
                                   wday(TICKET_ISSUE_DATE) == 3 ~ "Tuesday",
                                   wday(TICKET_ISSUE_DATE) == 4 ~ "Wednesday",
                                   wday(TICKET_ISSUE_DATE) == 5 ~ "Thursday",
                                   wday(TICKET_ISSUE_DATE) == 6 ~ "Friday",
                                   wday(TICKET_ISSUE_DATE) == 7 ~ "Saturday"))

data %>% 
  group_by(day_of_week) %>% 
  tally()


data %>% 
  filter(!RP_PLATE_STATE %in% c("MD","DC","VA")) %>% 
  ggplot(aes(x=RP_PLATE_STATE)) +
  geom_bar() +
  coord_flip()

data %>% 
  filter(RP_PLATE_STATE %in% c("MD","DC","VA")) %>% 
  ggplot(aes(x=X, y=Y, color = factor(RP_PLATE_STATE)))+
  geom_point() +
  facet_wrap(~day_of_week)

ggsave("tickets_plot.png")
