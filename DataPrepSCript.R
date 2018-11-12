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

#Center DERS and Reactivity variables
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

#Create Reactivity Variable
tidy_data_new <- tidy_data %>% 
  mutate(reactivity = child_lego - child_baseline, ders_c = center_scale(ders), reactivity_c = center_scale(reactivity), ders_x_reactivity = ders_c * reactivity_c)

glimpse(tidy_data_new)

#A scatterplot of maternal emotion dysregulation (x-axis)
#and child internalizing behaviors (y-axis), including a regression line 
ggplot(tidy_data_new, aes(x=ders_c, y=int)) +
  geom_point(color="black") +
  geom_smooth(method = "lm") +
   labs(x="Maternal Emotion Dysregulation (DERS)",
       y="Child Internalizing Behaviors")

#A scatterplot of maternal emotion dysregulation (x-axis)
#and child externalizing behaviors (y-axis), including a regression line
ggplot(tidy_data_new, aes(x=ders_c, y=ext)) +
  geom_point(color="black") +
  geom_smooth(method = "lm") +
  labs(x="Maternal Emotion Dysregulation (DERS)",
       y="Child Externalizing Behaviors")

