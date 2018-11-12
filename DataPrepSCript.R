#Load Packages
library(tidyverse)
library(rio)
library(here)

#import data and clean names
data <- import(here("data", "R Project Data copy.xlsx")) %>%
  janitor::clean_names()

#view data
glimpse(data)

#tidy data using required functions
tidy_data <- data %>%
  filter(!is.na(family_id)) %>%
  gather(text, val, 2:3) %>%
  separate(text, c("text", "type"), sep= "_") %>%
  spread(type, val) %>%
  select(-text)

#view tidy data
glimpse(tidy_data)