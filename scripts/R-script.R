#### Preamble ####
# Author: Reem Alasadi
# Data: 4 February 2021
# Contact: reem.alasadi@mail.utoronto.ca


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
all_data <-
  opendatatoronto::search_packages("Daily Shelter Occupancy") %>%
  opendatatoronto::list_package_resources() %>%
  dplyr::filter(name %in% c("daily-shelter-occupancy-2020.csv")) %>%
  group_split(name)%>%
  map_dfr(get_resource)
write_csv(all_data, "C:/Users/Reem/Documents/INF2178 Experimental Design/Homelessness_in_Toronto_in_2020/toronto_shelters_in_2020/raw_data.csv")
head(all_data)
all_data %>%
  janitor::clean_names()
#changing "occupancy_date" data type to date
all_data$OCCUPANCY_DATE <- as.Date(all_data$OCCUPANCY_DATE)
#removing empty rows
all_data_cleaned <- tidyr::drop_na(all_data) %>%
  janitor::clean_names()
#selecting specific columns to work with
toronto_shelters <- all_data_cleaned %>%
  group_by(occupancy_date, sector) %>%
  summarise(occupancy = sum(occupancy),
            capacity = sum(capacity),
            usage = occupancy / capacity,
            .groups = "drop")
#plotting data
ggplot(toronto_shelters,
       aes(x = occupancy_date, y = occupancy, color = sector)) +
  geom_point() +
  ggtitle("Toronto Shelter Occupancy in 2020") +
  labs(x = "Date", y = "Occupancy")
#finding out how many shelters there are
unique(all_data$SHELTER_NAME)
#finding out how many cities there are
unique(all_data$SHELTER_CITY)
#capacity
sum(all_data$CAPACITY)
#occupancy
sum(all_data$OCCUPANCY)
#occupancy / capacity
sum(all_data_cleaned$occupancy/all_data_cleaned$capacity)
?type()
typeof(all_data$OCCUPANCY)
typeof(all_data$CAPACITY)
sum(toronto_shelters$occupancy/toronto_shelters$capacity)
citation("tidyverse")
citation("ggplot2")
citation("opendatatoronto")
citation("knitr")
print(knitr::kable, bibtex=TRUE)
citation("janitor")
#another plot
citation("dplyr")
#table
?summarise()
?sum()


toronto_shelters %>%
  tidyr::drop_na(occupancy, capacity)

toronto_shelters %>%
  filter(month(occupancy_date) == 1) %>%
  ggplot(aes(x = day(occupancy_date), y = occupancy, color = sector)) +
  geom_point(aes(group = sector)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(color = "Type",
       x = "Day",
       y = "Occupancy (number)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

toronto_shelters %>%
  dplyr::group_by(toronto_shelters$OCCUPANCY_DATE) %>%
  kableExtra::kbl(caption = "Count of entries by month for 2020") %>%
  kableExtra::kable_styling()
```{r, echo=FALSE}
'''toronto_shelters_table <- toronto_shelters %>%
  summarise(
    Date = dplyr::group_by(OCCUPANCY_DATE),
    Sector = sum(Sector),
    Occupancy = sum(occupancy),
    Capacity = sum(capacity),
    Usage = sum(usage)
  )'''

```


```{r, echo=FALSE, results='asis'}
'''toronto_shelters_table %>%
  knitr::kable(caption = "Count of entries by month for 2020") %>%
  kableExtra::kable_styling(full_width = FALSE, bootstrap_options = "striped")'''
```

#split 'occupancy date' to 3 columns, year-month-day, then group by month
  