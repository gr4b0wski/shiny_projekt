library(shiny)
library(ggplot2)
library(plotly)

# Funkcje ekstrakcji danych
extract_white_players <- function(pgn_lines) {
  white_lines <- grep("^\\[White \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[White "(.*?)"\\]$', '\\1', white_lines)
}
extract_black_players <- function(pgn_lines) {
  black_lines <- grep("^\\[Black \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[Black "(.*?)"\\]$', '\\1', black_lines)
}
extract_results <- function(pgn_lines) {
  results <- grep("^\\[Result \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[Result "(.*?)"\\]$', '\\1', results)
}
extract_dates <- function(pgn_lines) {
  dates <- grep("^\\[UTCDate \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[UTCDate "(.*?)"\\]$', '\\1', dates)
}
extract_white_elo <- function(pgn_lines) {
  white_elo <- grep("^\\[WhiteElo \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[WhiteElo "(.*?)"\\]$', '\\1', white_elo)
}
extract_black_elo <- function(pgn_lines) {
  black_elo <- grep("^\\[BlackElo \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[BlackElo "(.*?)"\\]$', '\\1', black_elo)
}
extract_eco <- function(pgn_lines) {
  eco <- grep("^\\[ECO \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[ECO "(.*?)"\\]$', '\\1', eco)
}
extract_opening <- function(pgn_lines) {
  opening <- grep("^\\[Opening \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  sub('^\\[Opening "(.*?)"\\]$', '\\1', opening)
}

# Wczytaj dane graczy
load_data <- function(filepath, player_name) {
  pgn_text <- readLines(filepath)
  pgn_text <- pgn_text[nzchar(pgn_text)]
  
  white <- extract_white_players(pgn_text)
  black <- extract_black_players(pgn_text)
  result <- extract_results(pgn_text)
  date <- extract_dates(pgn_text)
  white_elo <- extract_white_elo(pgn_text)
  black_elo <- extract_black_elo(pgn_text)
  eco <- extract_eco(pgn_text)
  opening_variation <- extract_opening(pgn_text)
  opening <- sub("\\:.*", "", opening_variation)
  
  data <- data.frame(white, black, result, date, white_elo, 
                     black_elo, opening, opening_variation, eco)
  data <- data[data$white == player_name | data$black == player_name, ]
  data$date <- as.Date(data$date, format = "%Y.%m.%d")
  data$elo <- as.numeric(ifelse(data$white == player_name, 
                                data$white_elo, data$black_elo))
  data[order(data$date), ]
}

# Załaduj dane graczy
wittchen_data <- load_data("lichess_wittchen_2024-12-18.pgn", "wittchen")
tadziolul_data <- load_data("lichess_tadziolul_2024-12-18.pgn", "tadziolul")

# UI
ui <- navbarPage(
  "Chess Stats",
  id = "player",
  tabPanel(
    "gracz 1",
    fluidPage(
      titlePanel("Statystyki gracza 1"),
      fluidRow(
        column(5, dateRangeInput("dateRangeW", "Wybierz zakres dat:", 
                                 start = min(wittchen_data$date), 
                                 end = max(wittchen_data$date),
                                 min = min(wittchen_data$date), 
                                 max = max(wittchen_data$date))),
        column(3, selectInput("openingW", "Wybierz otwarcie:", 
                              choices = c("Wszystkie" = "all", unique(wittchen_data$opening)))),
        column(4, htmlOutput("statsW"))
      ),
      plotlyOutput("ratingPlotW", height = "500px")
    )
  ),
  tabPanel(
    "gracz 2",
    fluidPage(
      titlePanel("Statystyki gracza 2"),
      fluidRow(
        column(5, dateRangeInput("dateRangeT", "Wybierz zakres dat:", 
                                 start = min(tadziolul_data$date), 
                                 end = max(tadziolul_data$date),
                                 min = min(tadziolul_data$date), 
                                 max = max(tadziolul_data$date))),
        column(3, selectInput("openingT", "Wybierz otwarcie:", 
                              choices = c("Wszystkie" = "all", unique(tadziolul_data$opening)))),
        column(4, htmlOutput("statsT"))
      ),
      plotlyOutput("ratingPlotT", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  render_player_data <- function(data, dateRangeInput, openingInput, statsOutput, plotOutput) {
    library(dplyr) # Do agregacji danych
    
    # Dane do statystyk
    filteredDataStats <- reactive({
      data_filtered <- data[data$date >= input[[dateRangeInput]][1] &
                              data$date <= input[[dateRangeInput]][2], ]
      if (input[[openingInput]] != "all") {
        data_filtered <- data_filtered[data_filtered$opening == input[[openingInput]], ]
      }
      data_filtered
    })
    
    # Dane do wykresu
    filteredDataPlot <- reactive({
      data_filtered <- filteredDataStats()
      data_filtered <- data_filtered %>%
        group_by(date) %>%
        filter(row_number(desc(date)) == 1) %>% # Ostatni wpis dla każdej daty
        ungroup()
      data_filtered
    })
    
    output[[statsOutput]] <- renderUI({
      data_filtered <- filteredDataStats() # Używamy danych do statystyk
      won <- sum((data_filtered$result == "1-0" & data_filtered$white == data_filtered$white[1]) |
                   (data_filtered$result == "0-1" & data_filtered$black == data_filtered$black[1]))
      draw <- sum(data_filtered$result == "1/2-1/2")
      lost <- sum((data_filtered$result == "0-1" & data_filtered$white == data_filtered$white[1]) |
                    (data_filtered$result == "1-0" & data_filtered$black == data_filtered$black[1]))
      HTML(paste0("<span style='color:green;'>", won, "</span> - ",
                  "<span style='color:orange;'>", draw, "</span> - ",
                  "<span style='color:red;'>", lost, "</span>"))
    })
    
    output[[plotOutput]] <- renderPlotly({
      data_filtered <- filteredDataPlot() # Używamy danych do wykresu
      plot <- ggplot(data_filtered, aes(x = date, y = elo)) +
        geom_line(color = "black", linewidth = 0.5) +
        geom_point(aes(
          text = paste0(
            "Data: ", date, "<br>",
            "Ranking: ", elo
          )
        ), color = "black", size = 1.5) +
        labs(x = "Data", y = "Ranking", title = paste("Wykres rankingu")) +
        theme_minimal()
      ggplotly(plot, tooltip = "text") %>%
        layout(hoverlabel = list(align = "left"))
    })
  }
  
  
  render_player_data(wittchen_data, "dateRangeW", "openingW", "statsW", "ratingPlotW")
  render_player_data(tadziolul_data, "dateRangeT", "openingT", "statsT", "ratingPlotT")
}

# Run app
shinyApp(ui = ui, server = server)
