library(tidyverse)
library(doParallel)
library(randomForest)
library(ranger)

cl <- registerDoParallel(cores = detectCores()-1)

# Fitting two Random Forest models to each dataset
#   -- (1) Using Ranger
#   -- (2) Using randomForest


# Dataset 1: lassen data
df1 <- read_rds('data/lassen_match_data.rds') %>% 
  select(-date, -point_diff, -pool_match, -neutral) %>% 
  filter(weeks_ago < 1690)

m1 <- ranger::ranger(result ~ ., 
                     data = df1,
                     mtry = 3,
                     num.trees = 1000, 
                     importance = 'impurity',
                     max.depth = 15,
                     replace = FALSE,
                     probability = TRUE,
                     splitrule = 'gini')
m1
ranger::importance(m1)
write_rds(m1, 'models/model_ranger_lassen.rds')

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

m2 <- ranger::ranger(result ~ ., 
                   data = df2,
                   mtry = 3,
                   num.trees = 1000, 
                   importance = 'impurity',
                   max.depth = 7,
                   replace = TRUE,
                   probability = TRUE,
                   splitrule = 'gini')
m2
ranger::importance(m2)
write_rds(m2, 'models/model_ranger_espn.rds')


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



