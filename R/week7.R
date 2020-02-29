# R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(lubridate)
library(GGally)

# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
              mutate(timeStart = ymd_hms(timeStart),
                     timeEnd = ymd_hms(timeEnd),
                     condition = factor(condition, levels = c("A", "B", "C"), labels = c("Block A", "Block B", "Control")),
                     gender = factor(gender, levels = c("M", "F"), labels = c("Male", "Female"))) %>%
              filter(q6 == 1) %>%
              select(-q6)

# Visualization
ggpairs(select(week7_tbl, starts_with("q")))
ggplot(week7_tbl, aes(x = timeStart, y = q1))+
  geom_point() +
  ylab("Q1 Score") +
  xlab("Date of Experiment")
ggplot(week7_tbl, aes(x = q1, y = q2, col = gender)) +
  geom_point(position = "jitter") 
ggplot(week7_tbl, aes(x = q1, y = q2)) +
  geom_point(position = "jitter") +
  facet_grid(. ~ gender) +
  xlab("Score on Q1") +
  ylab("Score on Q2")
week7_tbl %>%
  mutate(timeLapsed = difftime(timeEnd, timeStart, units = "secs")) %>%
  ggplot(aes(x = gender, y = timeLapsed)) +
    geom_boxplot() +
    labs(x= "Gender", y = "Time Elapsed (secs)")
ggplot(week7_tbl, aes(x = q5, y = q7, col = condition)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(position = position_jitter(0.1)) +
  labs(x = "Score on Q5", y = "Score on Q7", color = "Experimental Condition") + 
  theme(rect = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = gray.colors(1, .875,.875)),
        legend.key = element_rect())