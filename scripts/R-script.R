#setting up 
library(opendatatoronto)
library(tidyverse)
library(kableExtra)
library(lubridate)

#importing the dataset
all_data <-
  opendatatoronto::search_packages("Daily Shelter Occupancy") %>%
  opendatatoronto::list_package_resources() %>%
  dplyr::filter(name %in% c("daily-shelter-occupancy-2020.csv")) %>%
  group_split(name)%>%
  map_dfr(get_resource)

#saving it to a local repo
write_csv(all_data, "raw_data.csv")

#viewing column names
names(all_data)

#viewing 'shelter_city' values
unique(all_data$SHELTER_CITY)

#viewing 'sector' values
unique(all_data$SECTOR)

#cleaning data
all_data_cleaned <- all_data %>%
  tidyr::drop_na() %>%
  janitor::clean_names()

#changing "occupancy_date" data type to date
all_data_cleaned$occupancy_date <- as.Date(all_data_cleaned$occupancy_date)

#selecting specific columns to work with
toronto_shelters <- all_data_cleaned %>%
  mutate(occupancy_month = lubridate::month(occupancy_date, label = TRUE)) %>% 
  group_by(occupancy_month, sector) %>%
  summarise(occupancy = sum(occupancy),
            capacity = sum(capacity),
            usage = occupancy / capacity,
            .groups = "drop")

#plotting data
ggplot(toronto_shelters,
       aes(x = occupancy_month, y = usage, color = sector)) +
  geom_point() +
  ggtitle("Toronto Shelter Occupancy in 2020") +
  labs(x = "Month", y = "Usage")

#Youth data
toronto_shelters_Youth <- filter(all_data_cleaned, sector == 'Youth') %>%
  mutate(occupancy_month = lubridate::month(occupancy_date, label = TRUE)) %>% 
  group_by(occupancy_month, sector) %>%
  summarise(occupancy = sum(occupancy),
            capacity = sum(capacity),
            usage = occupancy / capacity,
            .groups = "drop")

#Co-ed data
toronto_shelters_Coed <- filter(all_data_cleaned, sector == 'Co-ed') %>%
  mutate(occupancy_month = lubridate::month(occupancy_date, label = TRUE)) %>% 
  group_by(occupancy_month, sector) %>%
  summarise(occupancy = sum(occupancy),
            capacity = sum(capacity),
            usage = occupancy / capacity,
            .groups = "drop")

#Men data
toronto_shelters_Men <- filter(all_data_cleaned, sector == 'Men') %>%
  mutate(occupancy_month = lubridate::month(occupancy_date, label = TRUE)) %>% 
  group_by(occupancy_month, sector) %>%
  summarise(occupancy = sum(occupancy),
            capacity = sum(capacity),
            usage = occupancy / capacity,
            .groups = "drop")

#Women data
toronto_shelters_Women <- filter(all_data_cleaned, sector == 'Women') %>%
  mutate(occupancy_month = lubridate::month(occupancy_date, label = TRUE)) %>% 
  group_by(occupancy_month, sector) %>%
  summarise(occupancy = sum(occupancy),
            capacity = sum(capacity),
            usage = occupancy / capacity,
            .groups = "drop")

#Family data
toronto_shelters_Families <- filter(all_data_cleaned, sector == 'Families') %>%
  mutate(occupancy_month = lubridate::month(occupancy_date, label = TRUE, abbr = FALSE)) %>% 
  group_by(occupancy_month, sector) %>%
  summarise(occupancy = sum(occupancy),
            capacity = sum(capacity),
            usage = occupancy / capacity,
            .groups = "drop")

summary(toronto_shelters_Families)

family_summary_table <- toronto_shelters_Families%>%
  summarise(
    Sector = "Family",
    Max_Usage = max(toronto_shelters_Families$usage)* 100,
    Min_Usage = min(toronto_shelters_Families$usage)*100,
    Difference = (max(toronto_shelters_Families$usage) - min(toronto_shelters_Families$usage))* 100)

family_summary_table %>%
  knitr::kable(caption = "Family Sector's Usage (in percentages)", digits = 2) %>%
  kableExtra::kable_styling()