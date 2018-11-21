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
View(data) #Added for myself, feel free to remove

#tidy data using required functions
tidy_data <- data %>%
                   filter(!is.na(family_id)) %>% #This may be far beyond the scope of this project, but have you all thought of imputing the missing values?
                   gather(text, val, 2:3) %>%
                   separate(text, c("text", "type"), sep= "_") %>% #Awesome job gathering/separating data :)
                   spread(type, val) %>%
                   select(-text)

#view tidy data
glimpse(tidy_data)
View(tidy_data)

#Center DERS and Reactivity variables
center_scale <- function(x) {
                as.numeric(scale(x, scale = FALSE)) #This is a really neat bit of code! Way more efficient than what I've used in the past
                }


#Create Reactivity Variable
tidy_data_new <- tidy_data %>% 
                            mutate(reactivity = child_lego - child_baseline, 
                                   ders_c = center_scale(ders), 
                                   reactivity_c = center_scale(reactivity), 
                                   ders_x_reactivity = ders_c * reactivity_c, 
                                   ders_group = case_when(ders_c >= 0 ~ "high", 
                                   ders_c < 0 ~ "low")) #This part looks excellent! Really nice, easy-to-follow code

#View newly modified data
glimpse(tidy_data_new)
View(tidy_data_new) #This is entirely up to your preference, but I wonder if it would be easier to analyze data if similar variables were grouped together (e.g., ders juxtaposition ders_c)


#A scatterplot of maternal emotion dysregulation (x-axis)
#and child internalizing behaviors (y-axis), including a regression line 
ggplot(tidy_data_new, aes(x=ders_c, y=int)) +
       geom_point(color="black") +
       geom_smooth(method="lm") +
       labs(x="Maternal Emotion Dysregulation (DERS)",  #Beautiful and coherent visualization! Have you considered adding a theme to stamp your group's own style onto the plot?
            y="Child Internalizing Behaviors") #This plot is too great to not also have a title :)

#A scatterplot of maternal emotion dysregulation (x-axis)
#and child externalizing behaviors (y-axis), including a regression line
ggplot(tidy_data_new, aes(x=ders_c, y=ext)) +
       geom_point(color="black") +
       geom_smooth(method="lm") +
       labs(x="Maternal Emotion Dysregulation (DERS)",
            y="Child Externalizing Behaviors") #Similarly, I think this plot would look even better with a title!

#Data structure
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

#I think I could very well be missing something, but do you need meta data at the top (e.g., author, title, output) to export as html? I'm not as familiar with R scripts as compared to markdown files, so perhaps it is not the case.
#Overall, this is a really clean, efficient script--great job! I believe you included all the necessary components, however, I'm not seeing an example of inline code. Maybe this was only required for the final paper itself?