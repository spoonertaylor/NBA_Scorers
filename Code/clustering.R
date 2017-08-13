# Goal: Continue with the analysis, cluster the data.
setwd("C:/Users/Taylor/Documents/Projects/NBA_Scorers/")
source('Code/data collection.R')
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
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
rownames(df_new) = df_new$Player
df_new = df_new[,-1]
# Now all we have all the players and all their posessions.
# First fill all the NA's with 0's.
df_new[is.na(df_new)] = 0

# Subset only players who had > 250 possessions.
## First create a total possessions.
df_new[which(colnames(df_new) %in% scores)] = 
              lapply(df_new[which(colnames(df_new) %in% scores)], as.numeric)
df_new$Total_Possessions = df_new$ball_handler + df_new$cut + df_new$hand_off +
                            df_new$isolation + df_new$off_screen + df_new$post_up +
                            df_new$putback + df_new$roll_man + df_new$spot_up + df_new$transition
df_new = df_new[df_new$Total_Possessions > 250,]

# Turn each column into frequency
df_freq = df_new
for(i in 1:length(scores)) {
  df_freq[,i] = df_new[,i] / df_new$Total_Possessions 
}
df_freq = df_freq[,-which(colnames(df_freq) == "Total_Possessions")]
################################################
# Now begin clustering
d = dist(as.matrix(df_freq))
clust = hclust(d)

# First cluster into 3 clusters
cuts = cutree(clust, 3)

clust_1 = names(cuts[which(cuts == 1)]); length(clust_1)
clust_2 = names(cuts[which(cuts == 2)]); length(clust_2)
clust_3 = names(cuts[which(cuts == 3)]); length(clust_3)

# Cluster 1: Ball Handlers -- n = 61, 21%
# Cluster 2: Wings -- n = 172, 58%
# Cluster 3: Bigs -- n=63, 21%

#### Try and plot the frequency of play type
ball_handlers = colSums(df_new[clust_1,])
wings = colSums(df_new[clust_2,])
bigs = colSums(df_new[clust_3,])

# Try plotting
for(i in 1:length(scores)) {
  ball_handlers[i] = ball_handlers[i] / ball_handlers[length(ball_handlers)]
  wings[i] = wings[i] / wings[length(wings)]
  bigs[i] = bigs[i] / bigs[length(bigs)]
}

ball_handlers = ball_handlers[1:(length(ball_handlers)-1)]
wings = wings[1:(length(wings)-1)]
bigs = bigs[1:(length(bigs)-1)]

pt_clust3 = as.data.frame(rbind(ball_handlers, wings, bigs))
pt_clust3$ball_iso = round(pt_clust3$ball_handler + pt_clust3$isolation,2)
pt_clust3$sp_off_trans_hand = round(pt_clust3$spot_up + pt_clust3$transition + 
                                  pt_clust3$hand_off + pt_clust3$off_screen,2)
pt_clust3$pick_cut = pt_clust3$cut + round(pt_clust3$post_up + pt_clust3$putback + pt_clust3$roll_man,2)
pt_clust3 = pt_clust3[,which(colnames(pt_clust3) %in%
                               c("ball_iso", "sp_off_trans_hand", "pick_cut"))]
pt_clust3$Player_Type = c("Ball Handlers", "Wings", "Bigs")
pt_clust3 = melt(pt_clust3, id = "Player_Type", value.name = "Prop. Possessions Used")
colnames(pt_clust3)[2] = "Play Type"

pt_clust3$Player_Type = factor(pt_clust3$Player_Type, levels = c("Bigs", "Wings", "Ball Handlers"))
pt_clust3$`Prop. Possessions Used` = round(pt_clust3$`Prop. Possessions Used`,2)
clust3 = ggplot(pt_clust3, aes(x=Player_Type, y = `Prop. Possessions Used`, fill=`Play Type`))+ 
  geom_bar(position = "fill",stat = "identity", width = .6) + 
  coord_flip() + theme(legend.position = "top") + 
  geom_text(aes(label = `Prop. Possessions Used`), position = position_stack(vjust = 0.5), size = 5); clust3

##################################
# Now try and cluster with 6 clusters
cuts6 = cutree(clust, 6)

clust_1 = names(cuts6[which(cuts6 == 1)]); length(clust_1)
clust_2 = names(cuts6[which(cuts6 == 2)]); length(clust_2)
clust_3 = names(cuts6[which(cuts6 == 3)]); length(clust_3)
clust_4 = names(cuts6[which(cuts6 == 4)]); length(clust_4)
clust_5 = names(cuts6[which(cuts6 == 5)]); length(clust_5)
clust_6 = names(cuts6[which(cuts6 == 6)]); length(clust_6)

# Cluster 1: Ball Handlers
# Cluster 2: Attack Wings
# Cluster 3: Shooting Wings
# Clustr 4: Bully Wings
# Cluster 5: Limited Bigs
# Cluster 6: Skilled Bigs

#### Try and plot the frequency of play type
ball_handlers = colSums(df_new[clust_1,])
attack_wings = colSums(df_new[clust_2,])
shoot_wings = colSums(df_new[clust_3,])
bully_wings = colSums(df_new[clust_4,])
lim_bigs = colSums(df_new[clust_5,])
skill_bigs = colSums(df_new[clust_6,])


# Try plotting
for(i in 1:length(scores)) {
  ball_handlers[i] = ball_handlers[i] / ball_handlers[length(ball_handlers)]
  attack_wings[i] = attack_wings[i] / attack_wings[length(attack_wings)]
  shoot_wings[i] = shoot_wings[i] / shoot_wings[length(shoot_wings)]
  bully_wings[i] = bully_wings[i] / bully_wings[length(bully_wings)]
  lim_bigs[i] = lim_bigs[i] / lim_bigs[length(lim_bigs)]
  skill_bigs[i] = skill_bigs[i] / skill_bigs[length(skill_bigs)]
}

ball_handlers = ball_handlers[1:(length(ball_handlers)-1)]
attack_wings = attack_wings[1:(length(attack_wings)-1)]
shoot_wings = shoot_wings[1:(length(shoot_wings)-1)]
bully_wings = bully_wings[1:(length(bully_wings)-1)]
lim_bigs = lim_bigs[1:(length(lim_bigs)-1)]
skill_bigs = skill_bigs[1:length(skill_bigs)-1]

pt_clust6 = as.data.frame(rbind(ball_handlers, attack_wings, shoot_wings, bully_wings,
                                lim_bigs, skill_bigs))
pt_clust6$Player_Type = c("Ball Handlers", "Attack Wings", "Shooting Wings",
                              "Bully Wings", "Limited Bigs", "Skilled Bigs")
pt_clust6 = melt(pt_clust6, id = "Player_Type", value.name = "Prop. Possessions Used")
colnames(pt_clust6)[2] = "Play Type"

pt_clust6$Player_Type = factor(pt_clust6$Player_Type, levels = c("Ball Handlers", "Attack Wings", "Shooting Wings",
                                                                 "Bully Wings", "Limited Bigs", "Skilled Bigs"))
pt_clust6$`Prop. Possessions Used` = round(pt_clust6$`Prop. Possessions Used`,2)

clust6 = ggplot(pt_clust6, aes(x=Player_Type, y = `Prop. Possessions Used`, fill=`Play Type`))+ 
  geom_bar(position = "fill",stat = "identity", width = .6) + 
  coord_flip() + theme(legend.position = "top"); clust6

###############
# Final clustering that the author does is:
cuts18 = cutree(clust, 18)

clusts = list()
for(i in 1:18) {
  clusts[[i]] = names(cuts18[which(cuts18 == i)])
}

# Find "Go to Plays" for each of the 18 clusters
#  The "go-to" play types were any for which the
# group average frequency exceeded the overall league average by at least 50 percent.

## League average frequency for each of the 8 play types
league_avgs = colSums(df_new)
league_avgs[1:(length(league_avgs)-1)] = league_avgs[1:(length(leage_avgs - 1))] / leage_avgs[length(league_avgs)]
league_avgs = league_avgs[-length(league_avgs)]

# Function to get the go to play type for each cluster
get_gotos = function(player) {
  # Find which cluster number each cluster belongs to by matching 
  ## a player from the author's list to it.
  c = -99
  for(i in 1:18) {
    if(player %in% clusts[[i]]) {
      c = i
      break
    }
  }
  if(c == -99) {return(NULL)}
  # Calculate freq's for each play type.
  type = colSums(df_new[clusts[[c]],])
  type[1:(length(type)-1)] = type[1:(length(type)-1)] / type[length(type)]   
  type = type[-length(type)]
  
  # Calculate which play types are "go tos"
  go_tos = list()
  for(i in 1:length(scores)) {
    if(type[i] >= (league_avgs[i]+(league_avgs[i]*.5))) {
      go_tos = c(go_tos, type[i]) 
    }
  }
  
  return(go_tos)
}

rock_poundersGT = get_gotos("Kemba Walker")
creatorsGT = get_gotos("Kyrie Irving") 
distGT = get_gotos("Isaiah Thomas")
secondaryGT = get_gotos("Kevin Durant")
glueGT = get_gotos("Nick Young")
offballGT = get_gotos("CJ Miles")
spotGT = get_gotos("Otto Porter Jr.")
strechGT = get_gotos("Davis Bertans")
pickbigsGT = get_gotos("Channing Frye")
ballstopsGT = get_gotos("Harrison Barnes")
pointforGT = get_gotos("LeBron James")
relshootGT = get_gotos("Andre Iguodala")
bigstouchGT = get_gotos("Richaun Holmes")
skillbigsGT = get_gotos("Nikola Jokic")
postbigsGT = get_gotos("Jonas Valanciunas")
parastbigsGT = get_gotos("Cody Zeller")
unskillbigsGT = get_gotos("Tyson Chandler")
rollbigsGT = get_gotos("Montrezl Harrell")

# Now get the most efficient and least efficient scorers in each group
ppp_list = list()
for(i in 1:length(scores)) {
  # Only the player and possession columns
  df_new = data[[i]][which(colnames(data[[i]]) %in% c("Player", "Poss", "Pts"))]
  # Rename them by the player and what type of scoring it is.
  colnames(df_new) = c("Player", paste0(scores[i], "_poss"), paste0(scores[i],"_pts"))
  # Add the new df to the new list
  ppp_list[[i]] = df_new
}
# Join all of the dataframes together
ppp_df = join_all(ppp_list, by = "Player", type = "full")
# Fill NA's with 0
ppp_df[is.na(ppp_df)] = 0

ppp_df[2:ncol(ppp_df)] = lapply(ppp_df[2:ncol(ppp_df)], as.numeric)
rownames(ppp_df) = ppp_df$Player
ppp_df = ppp_df[,-1]

get_scorers = function(player) {
  # Find which cluster number each cluster belongs to by matching 
  ## a player from the author's list to it.
  c = -99
  for(i in 1:18) {
    if(player %in% clusts[[i]]) {
      c = i
      break
    }
  }
  if(c == -99) {return(NULL)}
  
  # Calculate freq's for each play type.
  ppp = ppp_df[clusts[[c]],]

  # calculate the group's ppp
  group_ppp = colSums(ppp)
  group_ppp_poss = sum(group_ppp[seq(1,length(group_ppp),2)])
  group_ppp_pts = sum(group_ppp[seq(2,length(group_ppp),2)])
  group_ppp_fn = round(group_ppp_pts / group_ppp_poss,2)
  
  # Now calculate each player's PPP
  ppp$Total_Poss = rowSums(ppp[seq(1,ncol(ppp),2)])
  ppp$Total_Points = rowSums(ppp[seq(2,ncol(ppp),2)])
  ppp$PPP = round(ppp$Total_Points / ppp$Total_Poss,2)
  
  # Sort by PPP
  ppp = ppp[with(ppp, order(-PPP)),]
  
  if(nrow(ppp) >= 6) {
    most_eff = ppp[c(1:3),]
    least_eff = ppp[c((nrow(ppp)-2):nrow(ppp)), ]
  } else{
    n_players = floor(nrow(ppp) / 2)
    most_eff = ppp[c(1:n_players),]
    least_eff = ppp[c((nrow(ppp)-n_players),nrow(ppp)),]
  }
  
  return(list(group_ppp_fn, most_eff, least_eff))
}

rock_pounders = get_scorers("Kemba Walker")
creators = get_scorers("Kyrie Irving") 
dist = get_scorers("Isaiah Thomas")
secondary = get_scorers("Kevin Durant")
glue = get_scorers("Nick Young")
offball = get_scorers("CJ Miles")
spot = get_scorers("Otto Porter Jr.")
strech = get_scorers("Davis Bertans")
pickbigs = get_scorers("Channing Frye")
ballstops = get_scorers("Harrison Barnes")
pointfor = get_scorers("LeBron James")
relshoot = get_scorers("Andre Iguodala")
bigstouch = get_scorers("Richaun Holmes")
skillbigs = get_scorers("Nikola Jokic")
postbigs = get_scorers("Jonas Valanciunas")
parastbigs = get_scorers("Cody Zeller")
unskillbigs = get_scorers("Tyson Chandler")
rollbigs = get_scorers("Montrezl Harrell")