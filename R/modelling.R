library(tidyverse)
library(doParallel)
library(randomForest)
library(ranger)

cl <- registerDoParallel(cores = detectCores()-1)

# Fitting a Random Forest models to each dataset
#   -- (1) Predicting probability of team 1 winning


# Dataset 1: lassen data
df1 <- read_rds('data/lassen_match_data.rds') %>% 
  select(-date, -point_diff, -pool_match, -neutral) %>% 
  filter(weeks_ago < 1690)

m1 <- randomForest(result ~ ., 
                   data = df1,
                   ntree = 1500,
                   mtry = 2,
                   nodesize = 1,
                   maxnodes = 20,
                   replace = TRUE,
                   importance = TRUE)
m1
varImpPlot(m1)
write_rds(m1, 'models/model_rf_lassen.rds')


# Dataset 2: ESPN data
df2 <- read_rds('data/espn_match_data.rds') %>% 
  select(-date, -point_diff) %>% 
  mutate_at(vars(team_1, team_2), as.factor)

m2 <- randomForest(result ~ ., 
                   data = df2,
                   ntree = 1500,
                   mtry = 4,
                   nodesize = 12,
                   maxnodes = 25,
                   replace = TRUE,
                   importance = TRUE)
m2
varImpPlot(m2)
write_rds(m2, 'models/model_rf_espn.rds')


# Saving an empty dataset to preserve factor structure (makes preds easier)
write_rds(list(lassen=df1[0,], espn=df2[0,]), 'data/dummy_df.rds')



# TODO: Tune this properly
#   -- (2) Predicting winning or losing margin of team 1 (points difference)

# Dataset 2: ESPN data
df3 <- read_rds('data/espn_match_data.rds') %>% 
  select(-date, -result) %>% 
  mutate_at(vars(team_1, team_2), as.factor)

m3 <- randomForest(point_diff ~ ., 
                   data = df3,
                   ntree = 100,
                   mtry = 4,
                   replace = TRUE,
                   importance = TRUE)
m3
varImpPlot(m3)
write_rds(m3, 'models/model_rf_espn_pd.rds')


