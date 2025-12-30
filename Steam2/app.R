#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(rjson)
library(data.table)
library(glue)
library(dplyr)
library(purrr)
library(lubridate)
library(DT)
library(ggplot2)
library(viridis)
library(plotly)
library(flexdashboard)
library(shiny)

# helpful steam API documentation
# https://developer.valvesoftware.com/wiki/Steam_Web_API
# https://steamapi.xpaw.me/#
# https://partner.steamgames.com/doc/features/achievements#global_stats

user_key <- read.table("C:/Users/berry/Documents/R Projects/steam-achievements/my_key.txt") %>% unlist()

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

########################################################################################################################

ui <- fluidPage(

    # Application title
    titlePanel("Steam Achievements Dashboard"),

    sidebarLayout(
        sidebarPanel(
            selectInput("steam_id", label = "Select Steam User:",
                        choices = c("Emerson" = "76561198041360303", "Sade" = "76561198084220408")),
            uiOutput("select_input_r"),
            textOutput("perc_unlock_txt"),
            HTML("<br>"),
            textOutput("excl_text"),
            dataTableOutput("excluded_games_txt"),
            
            width = 4
        ),

        mainPanel(
           h4("Unlocked Achievements by global percentage achieved:"),
           plotlyOutput("plot_unlocked"),
           h4("Achievements to aim for next:"),
           dataTableOutput("rec_table"),
           HTML("<span style='color: #33cc00;'>Green Achievements </span> are easier than other achievements the user has already unlocked for a particular game. <br> 
                <span style='color: #ff6666;'>Red Achievements </span> are more difficult than any the user has already unlocked for that game. <br> <br> <br>")
        )
    )
)

server <- function(input, output) {
    
    # get list of all games in order to display games that were excluded from the dashboard
    df_all_games <- reactive({
      df <- get_user_games(steam_id = input$steam_id, key = user_key) #%>% pull(name) %>% unique()
      df
    })
    
    # get steam data for scatterplot
    df_plot <- reactive({
      df <- get_steam_data(steam_id = input$steam_id, key = user_key)
      df
    })
    
    # list of games to include in the dropdown menu
    list_s_games <- reactive({
      list1 <- df_plot() %>% pull(.data$game) %>% unique()
      list1
    })
    
    output$select_input_r <- renderUI({
      selectizeInput(inputId = "game_id",
                     label = "Select Game(s)",
                     choices = c("All", list_s_games()),
                     multiple = TRUE, 
                     selected = "All")
    })
    
    df_plot_f <- reactive({
      if("All" %in% input$game_id){
        df_f <- df_plot()
      }else{
        df_f <- df_plot() %>% filter(.data$game %in% input$game_id)
      }
    })
    
    # get list of games that are excluded from the dashboard
    list_games_ex <- reactive({
      
      list_games_inc <- df_plot_f() %>% pull(game) %>% unique()
      
      list1 <- df_all_games() %>%
        filter(
          !(.data$name %in% list_games_inc)
        ) %>%
        select(
          `Games Excluded` = .data$name) %>%
        DT::datatable(
          options = list(dom = 't', columnDefs = list(list(
            targets = 1, searchable = FALSE
          )))
        )
      
      list1
    })
    
    # label achievements based on difficulty level
    list_game_rec <- reactive({
      
      # get most difficult achievement unlocked per game
      min_ach_per_game <- df_plot_f() %>% 
        filter(.data$achieved == 1) %>% 
        group_by(.data$game) %>% 
        mutate(
          min_ach_percent = min(.data$percent)
        ) %>% 
        ungroup() %>% 
        select(
          game = .data$game,
          min_ach_percent
        ) %>% unique()
      
      # choose achievements to recommend
      df_gr <- df_plot_f() %>%
        left_join(min_ach_per_game, by = "game") %>% 
        filter(.data$achieved == 0) %>%
        mutate(
          rec = case_when(
            .data$percent >= min_ach_percent ~ 1,
            .data$percent < min_ach_percent ~ 0,
            TRUE ~ 999
          )
        ) %>% 
        arrange(-.data$percent) %>% 
        transmute(
          Game = game,
          Achievement = apiname,
          `% of users with achievement` = round(percent, 2),
          rec,
          percent
        )
      
      if("All" %in% input$game_id){
        df_gr_f <- df_gr
      }else{
        df_gr_f <- df_gr %>% filter(Game %in% input$game_id)
      }
      
      tab_rec <- df_gr_f %>% 
        arrange(-percent) %>% 
        DT::datatable(
          options = list(
            columnDefs = list(list(visible=FALSE, targets= c("rec", "percent"))),
            paging = TRUE,
            pageLength = 10,
            #info = FALSE,
            dom = "tip"
          ),
          # caption = "Achievements to aim for next"
        ) %>% 
        formatStyle(
          'rec',
          target = "row",
          backgroundColor = styleEqual(c(0, 1), c('#ffe6e6', '#ccffcc'))
        )
      
      tab_rec
      
    })
    
    #% Achievements Unlocked:
    output$perc_unlock_txt <- renderText({
      unlocked_num <- sum(df_plot_f()$achieved)
      unlocked_denom <- nrow(df_plot_f())
      unlocked_p <- glue("Achievements Unlocked: {round((unlocked_num/unlocked_denom)*100, 1)}% ({unlocked_num}/{unlocked_denom})")
      unlocked_p
    })
    
    output$excl_text <- renderText({
      "The following games are present in the user's Steam library, but either do not track achievements through Steam, or the user does not have any achievements unlocked for the game yet:"
    })
    
    output$excluded_games_txt <- renderDataTable({
      list_games_ex()
    })
    #The following games are present in the user's Steam library, but either do not track achievements through Steam, or the user does not have any achievements unlocked for the game yet:
    
    
    output$plot_unlocked <- renderPlotly ({
      ggplotly(ggplot(df_plot_f() %>%
                        filter(
                          achieved == 1# & # only plot achievements that have been unlocked
                          # .data$unlocktime_dt >= input$date_range[1] & # decided not to include date range input...
                          # .data$unlocktime_dt <= input$date_range[2]
                        ), 
                      aes(x = .data$unlocktime_dt, 
                          y = .data$percent,
                          color = .data$game, 
                          text = glue("{.data$game}
                              Achievement: {.data$apiname}
                              Unlocked: {.data$unlocktime_dt}
                              % with achievement: {round(.data$percent)}%"))) + 
                 geom_point() +
                 theme_minimal() +
                 # ggtitle("Unlocked achievements by global percentage achieved") +
                 xlab("Date user unlocked achievement") +
                 ylab("% of Steam users with achievement") +
                 labs(color="") +
                 scale_color_viridis(discrete = TRUE, option = "H") +
                 labs(""),
               tooltip = "text"
      )
    })
    
    
    ### Locked achievements to aim for next
    output$rec_table <- DT::renderDataTable({
      list_game_rec()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
