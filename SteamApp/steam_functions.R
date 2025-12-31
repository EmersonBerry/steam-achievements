# functions for reading in Steam User Data

# get info on local games
get_user_games <- function(steam_id, key = user_key, format = "JSON"){
  
  user_games_json <- glue("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key={key}&steamid={steam_id}&format={format}&include_appinfo=1") %>% 
    readLines() %>% 
    fromJSON() %>% 
    suppressWarnings()
  
  user_games_df <- rbindlist(user_games_json$response$games, fill = TRUE)
  
  inputs <- user_games_df %>% 
    select(
      game_id = appid,
      name
    ) %>% 
    unique() 
  
  inputs
}

# function to take in game id and output a df with global achievement percents and local achievement date times
get_game_df <- function(game_id, name, key = user_key, steam_id, format = "JSON"){
  # if the game tracks achievements, combine local and global data and format
  tryCatch({
    # get user steam achievements for one game
    local_json <- glue("https://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v1/?key={key}&steamid={steam_id}&appid={game_id}") %>% 
      readLines() %>% 
      fromJSON() %>% 
      suppressWarnings()
    
    # get global achievement info for one game
    global_json <- glue("http://api.steampowered.com/ISteamUserStats/GetGlobalAchievementPercentagesForApp/v2/?gameid={game_id}&format={format}") %>% 
      readLines() %>% 
      fromJSON() %>% 
      suppressWarnings()
    
    local_raw <- rbindlist(local_json$playerstats$achievements, fill = TRUE)
    global_raw <- rbindlist(global_json$achievementpercentages$achievements, fill = TRUE)
    
    local_df <- local_raw %>% 
      mutate(
        unlocktime_dt = 
          case_when(
            unlocktime == 0 ~ NA_Date_,
            unlocktime != 0 ~ as_datetime(unlocktime)
          )) 
    
    final_df <- local_df %>%
      full_join(global_raw, by = c("apiname" = "name")) %>% 
      mutate(
        game = name
      ) %>% 
      group_by(game) %>% 
      mutate(
        max_ach_dt = max(unlocktime_dt, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        unlocktime_dt = 
          case_when(
            is.na(unlocktime_dt) ~ max_ach_dt + 1,
            !is.na(unlocktime_dt) ~ unlocktime_dt
          )
      )
    
  },
  error = function(e) print("")) #glue("{name} does not track achievements on Steam")))
  
  if(exists("final_df")){return(final_df)}
}

get_steam_data <- function(steam_id, key = user_key, format = "JSON", min_ach = 1){
  
  # first get user's game data
  user_games <- get_user_games(steam_id, key, format)
  
  # pull info for each game in users list, and put it all together in a df
  df_a <- pmap(.l = list(game_id = user_games$game_id, 
                         name = user_games$name, 
                         key = key, 
                         steam_id = steam_id), 
               .f = get_game_df, .progress = TRUE) %>% 
    rbindlist()
  
  
  
  # remove games where user hasn't unlocked at least 1 achievement
  df_final <- df_a %>%
    # filter(unlocktime_dt > "2020-01-01"| is.na(unlocktime_dt)) %>%
    group_by(game) %>%
    mutate(
      total_ach = sum(achieved),
      percent = as.numeric(percent)
    ) %>%
    ungroup() %>%
    filter(total_ach > min_ach) %>%
    select(
      -total_ach
    )
  
  df_final
}