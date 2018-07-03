library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidytext)
library(purrr)

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
summary(violations)
skim <- skim(violations)

# Explore Missing Values
violations %>% 
  summarise("Missing Descriptions" = sum(is.na(VIOLATION_DESCRIPTION)),
            "Missing Locations" = sum(is.na(LOCATION)),
            "Missing Date" = sum(is.na(TICKET_ISSUE_DATE)),
            "Total Observations" = n())


# Explore Missing Values by Field
violations %>% 
  summarise("Missing Values" = is.na(),
            "Total" = n())
# What are the violation codes for the missing violation description
violations %>% filter(is.na(VIOLATION_DESCRIPTION)) %>% group_by(VIOLATION_CODE) %>% tally()

# Update data to include ROSA Warnings as a description
violations %>% 
  mutate(VIOLATION_DESCRIPTION = case_when(VIOLATION_CODE == "P076" ~ "ROSA Warning",
                                           TRUE ~ VIOLATION_DESCRIPTION)) %>% View()

# Top 10 Violations
violations %>% 
  group_by(VIOLATION_DESCRIPTION) %>%
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
    top_n(10, Violations)


violations %>% 
  filter(!is.na(VIOLATION_DESCRIPTION)) %>% 
  group_by(VIOLATION_DESCRIPTION) %>%
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  top_n(10, Violations) %>% 
  ggplot(aes(x=Violations, y = reorder(VIOLATION_DESCRIPTION, Violations))) +
  geom_point()+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15))+
  ylab("")
  

# Violations by Month
violations %>% 
  group_by(month(TICKET_ISSUE_DATE)) %>%
  summarise("Violations" = n()) %>% 
  rename("Month" = `month(TICKET_ISSUE_DATE)`) %>% 
  mutate("Month" = month.abb[Month]) %>% 
  arrange(desc(Violations)) 

violations %>% 
  group_by(month(TICKET_ISSUE_DATE)) %>%
  summarise("Violations" = n()) %>% 
  rename("Month" = `month(TICKET_ISSUE_DATE)`) %>% 
  mutate("Month" = month.abb[Month]) %>% 
  ggplot(aes(x=factor(Month,levels = c("Nov","Dec","Jan","Feb","Mar","Apr", "May")), y=Violations))+
  geom_bar(stat="identity")+
  xlab("")+
  ggtitle("Parking Violations by Month")

# Top 10 Violation Locations
violations %>% 
  group_by(LOCATION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  top_n(10)

# Top 10 Violation Locations on Saturdays
violations %>% 
  filter(wday(TICKET_ISSUE_DATE) == 7) %>% 
  group_by(LOCATION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  top_n(10)


# Top 10 Violation Lociations on Sundays
violations %>% 
  filter(wday(TICKET_ISSUE_DATE) == 1) %>% 
  group_by(LOCATION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  top_n(10)

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

# Violations over time
violations %>% 
  ggplot(aes(x=strptime(TICKET_ISSUE_DATE,format = "%H"))) +
  geom_density()

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


# Top 10 Violations by DC License Plates
violations %>% 
  filter(RP_PLATE_STATE == "DC") %>% 
  group_by(VIOLATION_DESCRIPTION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  top_n(10, wt = Violations)

# Top 10 Violations by non-DC Plates
violations %>% 
  filter(!RP_PLATE_STATE == "DC") %>% 
  group_by(VIOLATION_DESCRIPTION) %>% 
  summarise("Violations" = n()) %>% 
  arrange(desc(Violations)) %>% 
  top_n(10, wt = Violations)


data %>% filter(!is.na(LOCATION)) %>% select(LOCATION) %>% View()

data %>% 
  filter(str_detect(LOCATION, "1300 BLOCK CLIFTON")) 

violations %>% 
  group_by(RP_PLATE_STATE) %>% 
  summarise("Violations" = n()) %>% 
  top_n(10, Violations) %>% 
  ungroup() %>% 
  filter(!is.na(RP_PLATE_STATE)) %>% 
  ggplot(aes(x=reorder(RP_PLATE_STATE, Violations), y = Violations)) +
  geom_point(stat="identity")+
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0,300000)) +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
  xlab("")+
  ylab("")+
  ggtitle("Violations by Vehicle License Plate State")

data %>% 
  filter(RP_PLATE_STATE %in% c("MD","DC","VA")) %>% 
  ggplot(aes(x=X, y=Y, color = factor(RP_PLATE_STATE)))+
  geom_point()

ggsave("tickets_plot.png")

# Timing of RPP Violations
violations %>% 
  filter(VIOLATION_DESCRIPTION == "RESIDENTIAL PERMIT PKING BEYOND LIMIT W/O PERMIT") %>%
  mutate("VIOLATION_TIME" = strftime(TICKET_ISSUE_DATE, format="%H:%M", tz = "UTC")) %>% View()
  ggplot(aes(x=VIOLATION_TIME)) +
  geom_density()
  
dc_map <- map_data("state") %>% 
  filter(grepl(pattern = "dist", x = region))

ggplot(dc_map, aes(x=long,y=lat))+
  geom_polygon() + theme_void()

library(ggmap)

dcmap <- ggmap

dc <- get_map("Washington, DC",zoom = 12)
dc_map <- ggmap(dc)              

derta <- violations %>% 
  sample_n(5000) %>% 
  ggplot(aes(x = X, y = Y))+
  geom_point(alpha=0.3)

violations %>% 
  mutate("day_of_week" = case_when(wday(TICKET_ISSUE_DATE) == 1 ~ "Sunday",
                                   wday(TICKET_ISSUE_DATE) == 2 ~ "Monday",
                                   wday(TICKET_ISSUE_DATE) == 3 ~ "Tuesday",
                                   wday(TICKET_ISSUE_DATE) == 4 ~ "Wednesday",
                                   wday(TICKET_ISSUE_DATE) == 5 ~ "Thursday",
                                   wday(TICKET_ISSUE_DATE) == 6 ~ "Friday",
                                   wday(TICKET_ISSUE_DATE) == 7 ~ "Saturday")) %>% 
sample_n(5000) %>% 
  ggplot(aes(x=X,y=Y))+
  geom_point(alpha=0.2,color="red")+
  facet_wrap(~day_of_week)

library(maps)

bd <- map('state', region = 'district of columbia',boundary = TRUE)

bd +
  geom_point(data=violations,aes(x=X,y=Y))


