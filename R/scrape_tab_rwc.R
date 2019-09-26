library(tidyverse)
library(RSelenium)

# Function to scape RWC 2019 match winner odds from the NZ TAB
#   Note this uses selenium to open a google chrome browser and interact with
#   it to make all the odds visible.
get_tab_odds <- function(check_install = FALSE) {

  remDr <- rsDriver(chromever = '77.0.3865.40', check = check_install, verbose = FALSE)

  remDr$client$navigate('https://www.tab.co.nz/sports/competition/2003/matches')
  load_more <- NULL
  
  while(is.null(load_more)){
    Sys.sleep(2)
    load_more <- tryCatch({
      suppressMessages(
        remDr$client$findElement(using = 'css', '.content-loader__load-more-link')
      )
    }, error = function(e){NULL})
  }
  
  cat('TAB page loaded\n')
  
  load_more$clickElement()
  
  Sys.sleep(2)
  
  cat('Scraping teams\n')
  
  teams <- remDr$client$findElements(using = 'css', '.button--outcome__text-title') %>% 
    map(~.x$getElementText()) %>% 
    unlist() %>% 
    matrix(byrow=T, ncol=2) %>% 
    as.data.frame() %>% 
    setNames(c('team1_name', 'team2_name'))
  
  cat('Scraping odds\n')
  
  odds <- remDr$client$findElements(using = 'css', '.button--outcome__price') %>% 
    map(~.x$getElementText()) %>% 
    unlist() %>% 
    matrix(byrow=T, ncol=2) %>% 
    as.data.frame() %>% 
    setNames(c('team1_odds', 'team2_odds'))
  
  remDr$server$stop()
  
  cbind(teams, odds) %>% 
    mutate(date = Sys.Date()) %>% 
    distinct()
  
}

write_csv(get_tab_odds(), paste0('data/tab_odds/tab_odds(', Sys.Date(), ').csv'))


