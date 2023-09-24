# Load libs ---------------------------------------------------------------
library(glue)
library(tidyverse)
library(baseballr)
library(lubridate)
library(here)

# American league ---------------------------------------------------------------
al <- mlb_standings(season = 2023, league_id = 103, date = "2023-3-30") %>% 
  select(team_records_wins, team_records_team_name) %>% 
  mutate(date = "2023-03-30") %>% 
  head(0) 

dates <- as.character(seq(as.Date("2023-3-30"), as.Date("2023-9-20"), by="days"))


for (date in dates){
  tryCatch({print(date)
    ph <- mlb_standings(season = 2023, league_id = 103, date = date) %>% 
      select(team_records_wins, team_records_team_name) %>%  
      mutate(date = date)
    
    al <- al %>% 
      bind_rows(ph)
    
    print(glue("Done with {date}"))
  }, error=function(e){})
}

write_csv(al, here("data/american-league.csv"))

# National league ---------------------------------------------------------
nl <- mlb_standings(season = 2023, league_id = 103, date = "2023-3-30") %>% 
  select(team_records_wins, team_records_team_name) %>% 
  mutate(date = "2023-03-30") %>% 
  head(0) 

dates <- as.character(seq(as.Date("2023-3-30"), as.Date("2023-9-20"), by="days"))


#National league
for (date in dates){
  tryCatch({print(date)
    ph <- mlb_standings(season = 2023, league_id = 104, date = date) %>% 
      select(team_records_wins, team_records_team_name) %>%  
      mutate(date = date)
    
    nl <- nl %>% 
      bind_rows(ph)
    
    print(glue("Done with {date}"))
  }, error=function(e){})
}

write_csv(nl, here("data/national-league.csv"))


# Team names --------------------------------------------------------------
al %>% 
  bind_rows(nl) %>% 
  distinct(team_records_team_name) %>% 
  write_csv(here("data/team-names.csv"))

