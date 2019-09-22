library(tidyverse)
library(rugger)
library(lubridate)
library(rvest)

# This (messy) script pulls data from several online sources to create the required
# datasets. Note if this is rerun it will include any RWC 2019 games already
# played. They can be excluded with filter(weeks_ago > x).


#------------------------------------------------------------------------
#     First dataset: lassen.co.nz
#------------------------------------------------------------------------

scrape_lassen <- function(date_from = '1871-01-01', year_to = '2019') {
  sesh <- html_session('http://www.lassen.co.nz/pickandgo.php')
  
  form <- html_form(sesh)[[1]]
  form <- set_values(form, txtfyear = date_from, txttyear = year_to)
  
  sesh <- submit_form(session = sesh,
                      form = form,
                      submit = 'Submit') 
  
  tables <- html_nodes(sesh, css = 'table')
  
  df <- tables[[2]] %>% 
    html_table()
}

clean_lassen_data <- function(data) {
  
  # General parsing of data into usable format
  df <- data %>% 
    mutate(date = as.Date(Date, format = "%a, %d %b %Y"),
           neutral = if_else(is.na(Neut.), 0, 1),
           cup_match = if_else(Tourn == 'WC', 1, 0, missing = 0),
           late_stage = if_else(Rnd %in% c('semi', 'Quart', 'Final'), 1, 0, missing = 0),
           pool_match = if_else(grepl('Pool', Tourn), 1, 0, missing = 0),
           weeks_ago = abs(as.integer(difftime(date, Sys.Date(), units = "weeks")))) %>% 
    
    separate(Score, into = c('T1_score', 'T2_score')) %>% 
    separate(Tries, into = c('T1_tries', 'T2_tries')) %>% 
    separate(Pnts, into = c('T1_pnty', 'T2_pnty')) %>% 
    separate(Match, into = c('team_1', 'team_2'), sep = ' v ') %>% 
    
    mutate_at(vars(ends_with('score'), ends_with('tries'), ends_with('pnty')), as.numeric) %>% 
    mutate(point_diff = T1_score - T2_score,
           try_diff = T1_tries - T2_tries,
           pnty_diff = T1_pnty - T2_pnty) %>% 
    
    select(-c(Date, Tourn, Rnd, matches('T1_|T2_'), Neut.)) %>% 
    filter(!is.na(weeks_ago)) %>% 
    mutate(team_1 = if_else(team_1 %in% c("JAP"), "JPN", team_1),
           team_2 = if_else(team_2 %in% c("JAP"), "JPN", team_2),
           team_1 = if_else(team_1 %in% c("SAF"), "RSA", team_1),
           team_2 = if_else(team_2 %in% c("SAF"), "RSA", team_2))
  
  # Reverse the data and bind it (so each team is represented for each game)
  df2 <- df %>% 
    mutate(team = team_1,
           team_1 = team_2,
           team_2 = team) %>% 
    mutate_at(vars(ends_with('_diff')), ~ . * -1) %>% 
    select(-team)
  
  df <- rbind(df, df2) %>% 
    select(-Venue, -try_diff, -pnty_diff) 
  
  # Only keep games where both countries have at least 10 games in total
  n_10 <- data.frame(team = c(df$team_1, df$team_2)) %>%
    group_by(team) %>% 
    tally() %>% 
    filter(n >= 10) %>% 
    pull(team)
  
  df <- df %>% 
    filter(team_1 %in% n_10) %>% 
    filter(team_2 %in% n_10) %>% 
    mutate(result = ifelse(point_diff > 0, 'won', 'lost')) %>% 
    mutate_at(vars(-weeks_ago, -date), as.factor) %>% 
    select(date, team_1, team_2, neutral, cup_match, late_stage, pool_match, weeks_ago, point_diff, result)
}




write_rds(clean_lassen_data(scrape_lassen()), 'data/lassen_match_data.rds')




#------------------------------------------------------------------------
#     Second dataset: espnscrum.com + world rankings
#------------------------------------------------------------------------

# Countries in the RWC 2019
get_country_rcw <- function() {
  countries <- c(
      c("South Africa",  "RSA"),
      c("Namibia",       "NAM"),
      c("United States", "USA"),
      c("Canada",        "CAN"),
      c("Japan",         "JPN"),
      c("England",       "ENG"),
      c("France",        "FRA"),
      c("Georgia",       "GEO"),
      c("Ireland",       "IRE"),
      c("Italy",         "ITA"),
      c("Russia",        "RUS"),
      c("Scotland",      "SCO"),
      c("Wales",         "WAL"),
      c("Australia",     "AUS"),
      c("Fiji",          "FIJ"),
      c("New Zealand",   "NZL"),
      c("Samoa",         "SAM"),
      c("Tonga",         "TON"),
      c("Argentina",     "ARG"),
      c("Uruguay",       "URU")) %>% 
    matrix(ncol=2, byrow=T) %>%
    as.data.frame() %>% 
    set_names(c('name', 'abr')) 
}


# Scrape match data for each country
scrape_espn <- function(country_list) {
  df <- list()
  
  for (i in (1:length(country_list$name)) ) {
    
    cat(as.character(country_list$name[i]), '\n')
    
    df[[i]] <- get_team_records(country_list$name[i], type = "matches") %>% 
      mutate(team = country_list$abr[i],
             opposition = gsub('v ', '', opposition),
             opposition = if_else(opposition == "USA", "United States", opposition),
             dater = floor_date(as.Date(date, format = "%d %b %Y"), 'quarter')) %>% 
      left_join(country_list, by = c('opposition' = 'name')) %>% 
      mutate_at(vars(`for`, against, difference, for_ht, against_ht), as.character) %>% 
      filter(result != "-") 
    
  }
  
  df <- bind_rows(df)
  
}


# Scrape ranking data (at quartely intervals by default)
scrape_rankings <- function(dates = seq(as.Date("2004/01/01"), as.Date("2019/09/20"), "quarter")) {

  df <- list()
  
  for (i in seq_along(dates)) {
    
    cat(as.character(dates[i]), '\n')
    
    df[[i]] <- get_rankings('men', date = dates[i])
  }
  
  df <- bind_rows(df)
  
}



# Add ranking and home/away status to match data --------------------------
merge_espn_rank_vanue <- function(city_data = 'data/ground_locs.csv') {
  
  espn = scrape_espn(get_country_rcw())
  rank = scrape_rankings() %>% 
    mutate(team_abbr = ifelse(team_abbr=='TGA', 'TON', team_abbr))
  
  df.city <- read_csv(city_data) %>% 
    select(-1, -2, -5)
  
  df <- left_join(espn, rank, by = c('team' = 'team_abbr', 'dater' = 'date')) %>% 
    rename(team_1 = team,
           team_2 = abr,
           team_1_points = points,
           team_1_rank = rank,
           team_1_prevpoints = previous_points) %>% 
    select(-team.y, -played, -previous_rank) %>% 
    left_join(rank, by = c('team_2' = 'team_abbr', 'dater' = 'date')) %>% 
    rename(team_2_points = points,
           team_2_rank = rank,
           team_2_prevpoints = previous_points,
           point_diff = difference) %>% 
    select(-team, -played, -previous_rank) %>% 
    select(team_1, team_2, result, `for`, against, point_diff, for_ht, against_ht, 
           ground, date, dater, team_1_rank, team_1_points, team_1_prevpoints, 
           team_2_rank, team_2_points, team_2_prevpoints) %>% 
    filter(complete.cases(.)) %>% 
    left_join(df.city, by = 'ground') %>% 
    mutate(venue = ifelse(ground_country == team_1, 'Home', ifelse(ground_country == team_2, 'Away', 'Neutral')),
           date = as.Date(date, format = "%d %b %Y")) %>% 
    select(-ground_country) %>% 
    mutate_at(vars(`for`, against, point_diff, for_ht, against_ht), as.numeric) %>% 
    mutate(weeks_ago = floor(as.numeric(difftime(Sys.Date(), date, units = 'weeks')))) %>% 
    select(-`for`, -against, -for_ht, -against_ht, -ground, -dater) %>% 
    mutate_at(vars(result, team_1_rank, team_2_rank, venue), as.factor) %>% 
    select(-matches('points')) %>% 
    filter(complete.cases(.)) %>% 
    mutate(result = fct_collapse(result, lost = c('lost', 'draw'))) %>% 
    select(date, team_1, team_2, team_1_rank, team_2_rank, venue, weeks_ago, point_diff, result)
}




write_rds(merge_espn_rank_vanue(), 'data/espn_match_data.rds')







