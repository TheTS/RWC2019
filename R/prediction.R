library(tidyverse)
library(randomForest)

# Making predictions using two different models (each uses a different dataset)

m1 <- read_rds('models/model_rf_lassen.rds')
m2 <- read_rds('models/model_rf_espn.rds')


# To make life easier, a function to work with both datasets
predict_rf <- function(team1, team2, model, pool_game = TRUE) {
  
  ranking <- read_rds('data/world_rankings_data.rds') %>% # TODO scrape this in real time
    mutate(team_abbr = ifelse(team_abbr=='TGA', 'TON', team_abbr))
  
  # Must be the espn dataset
  if("team_1_rank" %in% names(model$forest$xlevels)) {
    
    dummy.df <- read_rds('data/dummy_df.rds')[['espn']]
    
    dummy.df[1,] <- c(  
      team_1 = team1,
      team_2 = team2,
      team_1_rank = ranking %>% filter(team_abbr == team1) %>% pull(rank),
      team_2_rank = ranking %>% filter(team_abbr == team2) %>% pull(rank),
      venue = if_else(team1 == "JPN", 'Home', if_else(team2 == "JPN", 'Away', 'Neutral')),
      weeks_ago = 0,
      result = NA
    )
    
  } else {
    
    # Must be the lassen dataset
    dummy.df <- read_rds('data/dummy_df.rds')[['lassen']]
    
    dummy.df[1,] <- c(  
      team_1 = team1,
      team_2 = team2,
      cup_match = '0',
      late_stage = if_else(pool_game, '0', '1'),
      weeks_ago = 0,
      result = NA
    )
    
  }
    
  predict(model, newdata = dummy.df, type = 'prob')[2]
  
}



# Pool game structure -----------------------------------------------------

pool_A <- data.frame(
  pool = "Pool A",
  game = 1:10,
  team_1 = c("JPN","IRE","RUS","JPN","SCO","IRE","JPN","SCO","IRE","JPN"),
  team_2 = c("RUS","SCO","SAM","IRE","SAM","RUS","SAM","RUS","SAM","SCO")
)

pool_B <- data.frame(
  pool = "Pool B",
  game = 1:10,
  team_1 = c("NZL","ITA","ITA","RSA","NZL","RSA","NZL","RSA","NZL","NAM"),
  team_2 = c("RSA","NAM","CAN","NAM","CAN","ITA","NAM","CAN","ITA","CAN")
)

pool_C <- data.frame(
  pool = "Pool C",
  game = 1:10,
  team_1 = c("FRA","ENG","ENG","ARG","FRA","ENG","FRA","ARG","ENG","USA"),
  team_2 = c("ARG","TON","USA","TON","USA","ARG","TON","USA","FRA","TON")
)

pool_D <- data.frame(
  pool = "Pool D",
  game = 1:10,
  team_1 = c("AUS","WAL","FIJ","GEO","AUS","GEO","AUS","WAL","AUS","WAL"),
  team_2 = c("FIJ","GEO","URU","URU","WAL","FIJ","URU","FIJ","GEO","URU")
)

pool_games <- bind_rows(pool_A, pool_B, pool_C, pool_D)
rm(pool_A, pool_B, pool_C, pool_D)
  

# Predicting results ------------------------------------------------------

pool_games <- pool_games %>% 
  mutate(team1_m1 = map2(team_1, team_2, ~predict_rf(.x, .y, model=m1)) %>% unlist(),
         team1_m2 = map2(team_1, team_2, ~predict_rf(.x, .y, model=m2)) %>% unlist())

write_csv(pool_games, 'pool_game_predictions.csv')




