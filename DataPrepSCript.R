#You will need to install kableExtra package in order for this to run

#Load Packages
library(tidyverse)
library(rio)
library(here)
library(kableExtra)

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
  as.numeric(scale(x, scale = FALSE))
}

#Create Reactivity Variable
tidy_data_new <- tidy_data %>% 
  mutate(reactivity = child_lego - child_baseline, ders_c = center_scale(ders), reactivity_c = center_scale(reactivity), ders_x_reactivity = ders_c * reactivity_c, ders_group = case_when(
    ders_c >= 0 ~ "high", 
    ders_c < 0 ~ "low"))

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

str(tidy_data_new)
#Descriptives Table
table <- tidy_data_new %>%
  summarize(DERS_mean = mean(ders, na.rm = TRUE), 
            DERS_SD = sd(ders, na.rm = TRUE), 
            Interalizing_mean = mean(int, na.rm = TRUE), 
            Internalizing_SD = sd(int, na.rm = TRUE), 
            Externalizing_mean = mean(ext, na.rm = TRUE), 
            Externalizing_SD = sd(ext, na.rm = TRUE), 
            Reactivity_mean = mean(reactivity, na.rm = TRUE), 
            Reactivity_SD = sd(reactivity, na.rm = TRUE))

kable(table, 
      format = "latex",
      booktabs = TRUE)

#Additional Table
table2 <- tidy_data_new %>%
  group_by(ders_group) %>%
  summarize(Interalizing_mean = mean(int, na.rm = TRUE), 
            Internalizing_SD = sd(int, na.rm = TRUE), 
            Externalizing_mean = mean(ext, na.rm = TRUE), 
            Externalizing_SD = sd(ext, na.rm = TRUE), 
            Reactivity_mean = mean(reactivity, na.rm = TRUE), 
            Reactivity_SD = sd(reactivity, na.rm = TRUE))

kable(table2, 
      format = "latex",
      booktabs = TRUE)