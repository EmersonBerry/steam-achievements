#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# In order to run this app, the user must obtain a Steam API key here: https://steamcommunity.com/dev
# The app defaults to showing the app author's personal Steam library data. Users may find their own
# steam id to display their own data here: https://help.steampowered.com/en/faqs/view/2816-BE67-5B69-0FEC
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
library(shinyalert)

# helpful steam API documentation
# https://developer.valvesoftware.com/wiki/Steam_Web_API
# https://steamapi.xpaw.me/#
# https://partner.steamgames.com/doc/features/achievements#global_stats

source("steam_functions.R")

########################################################################################################################

ui <- fluidPage(

    # Application title
    titlePanel("Steam Achievements Dashboard"),

    sidebarLayout(
        sidebarPanel(
            textInput("steam_id", label = "Enter Steam ID:", value = "76561198041360303"), # default to my steam id
            p("For help finding your personal Steam ID: https://help.steampowered.com/en/faqs/view/2816-BE67-5B69-0FEC"),
            uiOutput("select_input_r"),
            textOutput("perc_unlock_txt"),
            HTML("<br>"),
            p("The following games are present in the user's Steam library, but either do not track achievements through Steam, or the user does not have any achievements unlocked for the game yet:"),
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
  
  shinyalert(
      html = TRUE,
      text = tagList(
        textInput(inputId = "user_key", label = "Get your steam key (https://steamcommunity.com/dev) then enter it here:")
      )
    )
  

    # get list of all games in order to display games that were excluded from the dashboard
    df_all_games <- reactive({
      req(input$user_key)
      req(input$steam_id)
      df <- get_user_games(steam_id = input$steam_id, key = input$user_key) 
      df
    })
    
    # get steam data for scatterplot
    df_plot <- reactive({
      req(input$user_key)
      req(input$steam_id)
      df <- get_steam_data(steam_id = input$steam_id, key = input$user_key)
      df
    })
    
    # list of games to include in the dropdown menu
    list_s_games <- reactive({
      req(input$user_key)
      list1 <- df_plot() %>% pull(.data$game) %>% unique() %>% sort()
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
      req(input$user_key)
      if("All" %in% input$game_id){
        df_f <- df_plot()
      }else{
        df_f <- df_plot() %>% filter(.data$game %in% input$game_id)
      }
    })
    
    # get list of games that are excluded from the dashboard
    list_games_ex <- reactive({
      req(input$user_key)

      list_games_inc <- df_plot_f() %>% pull(game) %>% unique()
      
      list1 <- df_all_games() %>%
        filter(
          !(.data$name %in% list_games_inc)
        ) %>%
        select(
          `Games Excluded` = .data$name) %>% 
        DT::datatable(
          options = 
            list(dom = 't', 
                 columnDefs = list(list(targets = 1, searchable = FALSE)),
                 order = list(1, 'asc')
                 )
        )
      
      list1
    })
    
    # label achievements based on difficulty level
    list_game_rec <- reactive({
      req(input$user_key)
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
          )
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
      req(input$user_key)
      req(input$steam_id)
      unlocked_num <- sum(df_plot_f()$achieved)
      unlocked_denom <- nrow(df_plot_f())
      unlocked_p <- glue("Achievements Unlocked: {round((unlocked_num/unlocked_denom)*100, 1)}% ({unlocked_num}/{unlocked_denom})")
      unlocked_p
    })
    
    output$excluded_games_txt <- renderDataTable({
      req(input$user_key)
      list_games_ex()
    })

    
    output$plot_unlocked <- renderPlotly ({
      req(input$user_key)
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
      req(input$user_key)
      list_game_rec()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
