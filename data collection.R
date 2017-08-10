# Goal: Scrape the data from stats.nba.com
library(stringr)
setwd("C:/Users/Taylor/Documents/Projects/NBA_Scorers/")
#################
# Unfortunatly was having problems scraping the data from stats.nba.com
# Copy and pasted each page into a txt file
# Loaded it and cleaned it
#################

# Create an empty dataframe
create_df = function() {
  # First create a dataframe for each type of scoring
  colnames = c("Player", "Team", "GP", "Poss", "Freq",
               "PPP", "Pts", "FGM", "FGA", "FG_Per", 
               "EFG_Per", "FT_Freq", "TO_Freq", 
               "SF_Freq", "And_1_Freq", "Score_Freq",
               "Percentile")
  df = as.data.frame(matrix(nrow=1, ncol=length(colnames)))
  colnames(df) = colnames
  return(df)
}

# Get the data
get_data = function() {
  # The types of scoring opportunities
  scores = c("transition", "isolation", "ball_handler", "roll_man", "post_up", "spot_up",
            "hand_off", "cut", "off_screen", "putbacks")
  # Sort both lists so the names line up
  files = sort(list.files(pattern = "*.txt"))
  scores = sort(scores)
  
  df_list = list()
  # Now read in each file, add data to data frame
  for(i in 1:length(files)) {
    # Read in first file
    f = file(files[i], "r")
    # Create a new dataframe
    df = create_df()
    # Read line by line, this is all by how the file was copied
    while(TRUE) {
      # Read each line by 2's since the player and his data are on seperate lines
      line = readLines(f, n = 2)
      # Check if there is data
      if(length(line) == 0) {
        break
      }
      # Start adding data to created df
      player = line[1]
      l2 = str_split(line[2], "\t")
      row = c(player, unlist(l2))
      df = rbind(df,row)
    }
    # Clean the data frame
    df = df[-1,]
    df$Freq = str_replace(df$Freq, "%","")
    df$FT_Freq = str_replace(df$FT_Freq, "%","")
    df$TO_Freq = str_replace(df$TO_Freq, "%","")
    df$SF_Freq = str_replace(df$SF_Freq, "%","")
    df$And_1_Freq = str_replace(df$And_1_Freq, "%","")
    df$Score_Freq = str_replace(df$Score_Freq, "%","")
    # Add the df to list of dfs
    df_list[[i]] = df
  }
  
  # Name the list
  names(df_list) = scores
  
  return(df_list)
}