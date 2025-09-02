library(rvest)
library(tidyverse)
library(stringr)
library(rlist)

#set working dir.

x <- readRDS("Team_Data.RData")
u <- readRDS("Team_Spends.RData")
pu <- readRDS("Player_Data_Modified.RData")

lastsea = x[[24]]
saveRDS(lastsea, file="Season23-24_Table.RData")
t1 <- pu[[23]]
k1 <- t1[t1$Player == "Erling Haaland",]
t2 <- pu[[24]]
k2 <- t2[t2$Player == "Erling Haaland",]
# Combine the two tables
combined_performance <- rbind(k1, k2)

# Convert necessary columns to numeric
combined_performance$Goals <- as.numeric(combined_performance$Goals)
combined_performance$Assists <- as.numeric(combined_performance$Assists)
combined_performance$`Penalty Kicks` <- as.numeric(combined_performance$`Penalty Kicks`)
combined_performance$`Yellow Cards` <- as.numeric(combined_performance$`Yellow Cards`)
combined_performance$`Red Cards` <- as.numeric(combined_performance$`Red Cards`)

# Summarize to get combined statistics
summary_performance <- combined_performance %>%
  summarise(
    Player = first(Player),
    Nation = first(Nation),
    Position = first(Position),
    Squad = first(Squad),
    Goals = sum(Goals, na.rm = TRUE),
    Assists = sum(Assists, na.rm = TRUE),
    Penalty_Kicks = sum(`Penalty Kicks`, na.rm = TRUE),
    Yellow_Cards = sum(`Yellow Cards`, na.rm = TRUE),
    Red_Cards = sum(`Red Cards`, na.rm = TRUE)
  )

# View the summarized performance

saveRDS(summary_performance, file="Haaland22-24_Table.RData")

t1 <- pu[[7]]
k1 <- t1[t1$Player == "Cristiano Ronaldo",]
t2 <- pu[[8]]
k2 <- t2[t2$Player == "Cristiano Ronaldo",]
t3 <- pu[[9]]
k3 <- t3[t3$Player == "Cristiano Ronaldo",]
# Combine the two tables
combined_performance <- rbind(k1, k2, k3)

# Convert necessary columns to numeric
combined_performance$Goals <- as.numeric(combined_performance$Goals)
combined_performance$Assists <- as.numeric(combined_performance$Assists)
combined_performance$`Penalty Kicks` <- as.numeric(combined_performance$`Penalty Kicks`)
combined_performance$`Yellow Cards` <- as.numeric(combined_performance$`Yellow Cards`)
combined_performance$`Red Cards` <- as.numeric(combined_performance$`Red Cards`)

# Summarize to get combined statistics
summary_performance <- combined_performance %>%
  summarise(
    Player = first(Player),
    Nation = first(Nation),
    Position = first(Position),
    Squad = first(Squad),
    Goals = sum(Goals, na.rm = TRUE),
    Assists = sum(Assists, na.rm = TRUE),
    Penalty_Kicks = sum(`Penalty Kicks`, na.rm = TRUE),
    Yellow_Cards = sum(`Yellow Cards`, na.rm = TRUE),
    Red_Cards = sum(`Red Cards`, na.rm = TRUE)
  )
saveRDS(summary_performance, file="Ronaldo07-10_Table.RData")

library(dplyr)
library(purrr)

# Process each table in `x` and add the Year column based on its index, with reversed years
arsenal_wins <- map2_df(x, rev(2000:2023), ~ {
  # Extract the data frame if it's wrapped in a list
  table <- if (is.list(.x) && length(.x) == 1) .x[[1]] else .x
  
  # Ensure Attendance is numeric
  table <- table %>%
    mutate(Attendance = as.numeric(Attendance))
  
  # Filter for Arsenal wins and add Year column
  table %>% 
    filter(Squad == "Arsenal" & W > 0) %>%
    mutate(Year = .y)  # Assign the reversed year based on the sequence (2023 to 2000)
})

# View the resulting table
arsenal_wins
saveRDS(arsenal_wins, file="arsenal_new_wins_Table.RData")

library(dplyr)
library(purrr)

# Process each table in `x` and add the Year column based on its index
man_city_wins <- map2_df(x, rev(2000:2023), ~ {
  # Extract the data frame if it's wrapped in a list
  table <- if (is.list(.x) && length(.x) == 1) .x[[1]] else .x
  
  # Ensure Attendance is numeric
  table <- table %>%
    mutate(Attendance = as.numeric(Attendance))
  
  # Filter for Manchester City wins and add Year column
  table %>%
    filter(Squad == "Manchester City" & W > 0) %>%
    mutate(Year = .y)  # Assign the year based on the sequence (2000 to 2023)
})

# View the resulting table
man_city_wins
saveRDS(man_city_wins, file="man_city_new_wins_Table.RData")


# Set working directory
setwd("C:/Users/Venu Gopal/Desktop/mth-208-course-project-g_29_ftw-main/mth-208-course-project-g_29_ftw-main/Data")

# Load player data
haaland_data <- readRDS("Haaland22-24_Table.RData")
ronaldo_data <- readRDS("Ronaldo07-10_Table.RData")

# Ensure all necessary columns exist, and fill missing columns with a default value if necessary
haaland_data <- haaland_data %>%
  mutate(
    Goals = ifelse(is.null(Goals), 0, Goals),
    Assists = ifelse(is.null(Assists), 0, Assists),
    Penalty_Kicks = ifelse(is.null(Penalty_Kicks), 0, Penalty_Kicks),
    Yellow_Cards = ifelse(is.null(Yellow_Cards), 0, Yellow_Cards),
    Red_Cards = ifelse(is.null(Red_Cards), 0, Red_Cards)
  )

ronaldo_data <- ronaldo_data %>%
  mutate(
    Goals = ifelse(is.null(Goals), 0, Goals),
    Assists = ifelse(is.null(Assists), 0, Assists),
    Penalty_Kicks = ifelse(is.null(Penalty_Kicks), 0, Penalty_Kicks),
    Yellow_Cards = ifelse(is.null(Yellow_Cards), 0, Yellow_Cards),
    Red_Cards = ifelse(is.null(Red_Cards), 0, Red_Cards)
  )

# Create comparison table
comparison_table <- data.frame(
  "Stat Name" = c("Name", "Nation", "Goals", "Assists", 
                  "Penalties", "Yellow Cards", "Red Cards"),
  "Player 1" = c("Erling Haaland", "NOR", 
                       haaland_data$Goals[1], haaland_data$Assists[1], haaland_data$Penalty_Kicks[1],
                       haaland_data$Yellow_Cards[1], haaland_data$Red_Cards[1]),
  "Player 2" = c("Cristiano Ronaldo", "POR", 
                          ronaldo_data$Goals[1], ronaldo_data$Assists[1], ronaldo_data$Penalty_Kicks[1],
                          ronaldo_data$Yellow_Cards[1], ronaldo_data$Red_Cards[1])
)

# Display the comparison table
comparison_table
saveRDS(comparison_table, file="plyr_comp_Table.RData")



