library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidytext)

nov2017 <- read_csv("Parking_Violations_Issued_in_November_2017.csv")
dec2017 <- read_csv("Parking_Violations_Issued_in_December_2017.csv")
jan2018 <- read_csv("Parking_Violations_Issued_in_January_2018.csv")
feb2018 <- read_csv("Parking_Violations_Issued_in_February_2018.csv")
mar2018 <- read_csv("Parking_Violations_Issued_in_March_2018.csv")
apr2018 <- read_csv("Parking_Violations_Issued_in_April_2018.csv")
may2018 <- read_csv("Parking_Violations_Issued_in_May_2018.csv")

violations <- bind_rows(nov2017, dec2017, jan2018, feb2018, mar2018, apr2018, may2018)

rm(list = c("nov2017", "dec2017", "jan2018", "feb2018", "mar2018", "apr2018", "may2018"))

# Explore the fields
str(violations)

# Top 10 Violations
violations %>% 
  group_by(VIOLATION_DESCRIPTION) %>%
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations))

# Violations by Month
violations %>% 
  group_by(month(TICKET_ISSUE_DATE)) %>%
  summarise("Violations" = n()) %>% 
  rename("Month" = `month(TICKET_ISSUE_DATE)`) %>% 
  mutate("Month" = month.abb[Month]) %>% 
  arrange(desc(Violations)) 

# Top 3 Violations by Month
violations %>% 
  group_by(month(TICKET_ISSUE_DATE), VIOLATION_DESCRIPTION) %>%
  summarise("Violations" = n()) %>% 
  rename("Month" = `month(TICKET_ISSUE_DATE)`) %>% 
  arrange(desc(Violations)) %>% 
  group_by(Month) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  mutate("Month" = month.abb[Month]) 


# Violations by Day of Week
violations %>% 
  mutate("day_of_week" = case_when(wday(TICKET_ISSUE_DATE) == 1 ~ "Sunday",
                                   wday(TICKET_ISSUE_DATE) == 2 ~ "Monday",
                                   wday(TICKET_ISSUE_DATE) == 3 ~ "Tuesday",
                                   wday(TICKET_ISSUE_DATE) == 4 ~ "Wednesday",
                                   wday(TICKET_ISSUE_DATE) == 5 ~ "Thursday",
                                   wday(TICKET_ISSUE_DATE) == 6 ~ "Friday",
                                   wday(TICKET_ISSUE_DATE) == 7 ~ "Saturday")) %>% 
  group_by(day_of_week) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations))
  
# Top 3 Violations by Day of Week
violations %>% 
  mutate("day_of_week" = case_when(wday(TICKET_ISSUE_DATE) == 1 ~ "Sunday",
                                   wday(TICKET_ISSUE_DATE) == 2 ~ "Monday",
                                   wday(TICKET_ISSUE_DATE) == 3 ~ "Tuesday",
                                   wday(TICKET_ISSUE_DATE) == 4 ~ "Wednesday",
                                   wday(TICKET_ISSUE_DATE) == 5 ~ "Thursday",
                                   wday(TICKET_ISSUE_DATE) == 6 ~ "Friday",
                                   wday(TICKET_ISSUE_DATE) == 7 ~ "Saturday")) %>% 
  group_by(day_of_week, VIOLATION_DESCRIPTION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  group_by(day_of_week) %>% 
  slice(1:3) %>% 
  ungroup()

# Top Violations by Day on 1400 BLOCK FLORIDA NW
violations %>% 
  filter(str_detect(LOCATION, "1400 BLOCK FLORIDA AVE NW")) %>% 
  mutate("day_of_week" = case_when(wday(TICKET_ISSUE_DATE) == 1 ~ "Sunday",
                                   wday(TICKET_ISSUE_DATE) == 2 ~ "Monday",
                                   wday(TICKET_ISSUE_DATE) == 3 ~ "Tuesday",
                                   wday(TICKET_ISSUE_DATE) == 4 ~ "Wednesday",
                                   wday(TICKET_ISSUE_DATE) == 5 ~ "Thursday",
                                   wday(TICKET_ISSUE_DATE) == 6 ~ "Friday",
                                   wday(TICKET_ISSUE_DATE) == 7 ~ "Saturday")) %>% 
  group_by(day_of_week, VIOLATION_DESCRIPTION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  group_by(day_of_week) %>% 
  slice(1:3) %>% 
  ungroup()

# Top Violations on 1500 Block W ST NW
violations %>% 
  filter(str_detect(LOCATION, "1500 BLOCK W ST NW")) %>% 
  mutate("day_of_week" = case_when(wday(TICKET_ISSUE_DATE) == 1 ~ "Sunday",
                                   wday(TICKET_ISSUE_DATE) == 2 ~ "Monday",
                                   wday(TICKET_ISSUE_DATE) == 3 ~ "Tuesday",
                                   wday(TICKET_ISSUE_DATE) == 4 ~ "Wednesday",
                                   wday(TICKET_ISSUE_DATE) == 5 ~ "Thursday",
                                   wday(TICKET_ISSUE_DATE) == 6 ~ "Friday",
                                   wday(TICKET_ISSUE_DATE) == 7 ~ "Saturday")) %>% 
  group_by(day_of_week, VIOLATION_DESCRIPTION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  group_by(day_of_week) %>% 
  slice(1:3) %>% 
  ungroup()

# Top Violations on 15TH ST NW East of the Park
violations %>% 
  filter(str_detect(LOCATION, "2300 BLOCK 15TH ST NW")) %>% 
  mutate("day_of_week" = case_when(wday(TICKET_ISSUE_DATE) == 1 ~ "Sunday",
                                   wday(TICKET_ISSUE_DATE) == 2 ~ "Monday",
                                   wday(TICKET_ISSUE_DATE) == 3 ~ "Tuesday",
                                   wday(TICKET_ISSUE_DATE) == 4 ~ "Wednesday",
                                   wday(TICKET_ISSUE_DATE) == 5 ~ "Thursday",
                                   wday(TICKET_ISSUE_DATE) == 6 ~ "Friday",
                                   wday(TICKET_ISSUE_DATE) == 7 ~ "Saturday")) %>% 
  group_by(day_of_week, VIOLATION_DESCRIPTION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  group_by(day_of_week) %>% 
  slice(1:3) %>% 
  ungroup()



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

data %>% filter(!is.na(LOCATION)) %>% select(LOCATION) %>% View()

data %>% 
  filter(str_detect(LOCATION, "1300 BLOCK CLIFTON")) %>% 

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
