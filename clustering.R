# Goal: Continue with the analysis, cluster the data.
setwd("C:/Users/Taylor/Documents/Projects/NBA_Scorers/")
source('data collection.R')
library(dplyr)
library(plyr)
# Obtain the data from other file.
data = get_data()
# To cluster we need all of the player names
## and possession for each type of scoring opportunity
scores = c("transition", "isolation", "ball_handler", "roll_man", "post_up", "spot_up",
           "hand_off", "cut", "off_screen", "putback")
scores = sort(scores)

# Create a new list of data frames that are subsetted
new_list = list()
for(i in 1:length(scores)) {
  # Only the player and possession columns
  df_new = data[[i]][which(colnames(data[[i]]) %in% c("Player", "Poss"))]
  # Rename them by the player and what type of scoring it is.
  colnames(df_new) = c("Player", scores[i])
  # Add the new df to the new list
  new_list[[i]] = df_new
}
# Join all of the dataframes together
df_new = join_all(new_list, by = "Player", type = "full")
