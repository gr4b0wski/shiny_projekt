#EZ


pgn_text <- readLines("C:\\Users\\kubam\\Downloads\\lichess_wittchen_2024-12-18.pgn")
pgn_text <- pgn_text[nzchar(pgn_text)] 

extract_white_players <- function(pgn_lines) {
  white_lines <- grep("^\\[White \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  white_players <- sub('^\\[White "(.*?)"\\]$', '\\1', white_lines)
  return(white_players)
}
extract_black_players <- function(pgn_lines) {
  black_lines <- grep("^\\[Black \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  black_players <- sub('^\\[Black "(.*?)"\\]$', '\\1', black_lines)
  return(black_players)
}
extract_results <- function(pgn_lines) {
  results <- grep("^\\[Result \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  results <- sub('^\\[Result "(.*?)"\\]$', '\\1', results)
  return(results)
}
extract_dates <- function(pgn_lines) {
  dates <- grep("^\\[UTCDate \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  dates <- sub('^\\[UTCDate "(.*?)"\\]$', '\\1', dates)
  return(dates)
}
extract_white_elo <- function(pgn_lines) {
  white_elo <- grep("^\\[WhiteElo \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  white_elo <- sub('^\\[WhiteElo "(.*?)"\\]$', '\\1', white_elo)
  return(white_elo)
}
extract_black_elo <- function(pgn_lines) {
  black_elo <- grep("^\\[BlackElo \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  black_elo <- sub('^\\[BlackElo "(.*?)"\\]$', '\\1', black_elo)
  return(black_elo)
}
extract_eco <- function(pgn_lines) {
  eco <- grep("^\\[ECO \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  eco <- sub('^\\[ECO "(.*?)"\\]$', '\\1', eco)
  return(eco)
}
extract_opening <- function(pgn_lines) {
  opening <- grep("^\\[Opening \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  opening <- sub('^\\[Opening "(.*?)"\\]$', '\\1', opening)
  return(opening)
}

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

#KUBAAA:

pgn_text2 <- readLines("C:\\Users\\kubam\\Downloads\\lichess_tadziolul_2024-12-18.pgn")
pgn_text2 <- pgn_text2[nzchar(pgn_text2)] 

extract_white_players <- function(pgn_lines2) {
  white_lines <- grep("^\\[White \"[^\"]+\"\\]$", pgn_lines2, value = TRUE)
  white_players <- sub('^\\[White "(.*?)"\\]$', '\\1', white_lines)
  return(white_players)
}
extract_black_players <- function(pgn_lines2) {
  black_lines <- grep("^\\[Black \"[^\"]+\"\\]$", pgn_lines2, value = TRUE)
  black_players <- sub('^\\[Black "(.*?)"\\]$', '\\1', black_lines)
  return(black_players)
}
extract_results <- function(pgn_lines2) {
  results <- grep("^\\[Result \"[^\"]+\"\\]$", pgn_lines2, value = TRUE)
  results <- sub('^\\[Result "(.*?)"\\]$', '\\1', results)
  return(results)
}
extract_dates <- function(pgn_lines2) {
  dates <- grep("^\\[UTCDate \"[^\"]+\"\\]$", pgn_lines2, value = TRUE)
  dates <- sub('^\\[UTCDate "(.*?)"\\]$', '\\1', dates)
  return(dates)
}
extract_white_elo <- function(pgn_lines2) {
  white_elo <- grep("^\\[WhiteElo \"[^\"]+\"\\]$", pgn_lines2, value = TRUE)
  white_elo <- sub('^\\[WhiteElo "(.*?)"\\]$', '\\1', white_elo)
  return(white_elo)
}
extract_black_elo <- function(pgn_lines) {
  black_elo <- grep("^\\[BlackElo \"[^\"]+\"\\]$", pgn_lines, value = TRUE)
  black_elo <- sub('^\\[BlackElo "(.*?)"\\]$', '\\1', black_elo)
  return(black_elo)
}
extract_eco <- function(pgn_lines2) {
  eco <- grep("^\\[ECO \"[^\"]+\"\\]$", pgn_lines2, value = TRUE)
  eco <- sub('^\\[ECO "(.*?)"\\]$', '\\1', eco)
  return(eco)
}
extract_opening <- function(pgn_lines2) {
  opening <- grep("^\\[Opening \"[^\"]+\"\\]$", pgn_lines2, value = TRUE)
  opening <- sub('^\\[Opening "(.*?)"\\]$', '\\1', opening)
  return(opening)
}

white2 <- extract_white_players(pgn_text2)
black2 <- extract_black_players(pgn_text2)
result2 <- extract_results(pgn_text2)
date2 <- extract_dates(pgn_text2)
white_elo2 <- extract_white_elo(pgn_text2)
black_elo2 <- extract_black_elo(pgn_text2)
eco2 <- extract_eco(pgn_text2)
opening_variation2 <- extract_opening(pgn_text2)
opening2 <- sub("\\:.*", "", opening_variation2)




#########################################################################

############################
#### WYWOŁANIE DLA CIEBIE ####
############################

#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("fastmap")

data <- data.frame(white, black, result, date, white_elo, 
                   black_elo, opening, opening_variation, eco)

library(shiny)
library(ggplot2)
library(plotly)
library(fastmap)


wittchen_data <- data[data$white == "wittchen" | data$black == "wittchen", ]
wittchen_data$date <- as.Date(wittchen_data$date, format = "%Y.%m.%d")  # Konwertuj kolumnę daty

# Oblicz aktualny ranking po każdej partii
wittchen_data <- wittchen_data[order(wittchen_data$date), ]
wittchen_data$elo <- as.numeric(ifelse(wittchen_data$white == "wittchen", 
                            wittchen_data$white_elo, 
                            wittchen_data$black_elo))

wittchen_data <- wittchen_data[order(wittchen_data$date), ]


ui <- fluidPage(
  titlePanel("Zmiana rankingu gracza Wittchen w czasie"),
  
  # Górny panel: wybór dat, otwarcia i statystyk
  fluidRow(
    column(
      width = 5,  # Wybór dat
      dateRangeInput(
        "dateRange",
        label = "Wybierz zakres dat:",
        start = min(wittchen_data$date),
        end = max(wittchen_data$date),
        min = min(wittchen_data$date),
        max = max(wittchen_data$date)
      )
    ),
    column(
      width = 3,  # Wybór otwarcia
      style = "padding-right: 10px;",
      selectInput(
        "opening", 
        label = "Wybierz otwarcie:", 
        choices = c("Wszystkie" = "all", unique(wittchen_data$opening)),
        selected = "all"
      )
    ),
    column(
      width = 4,  # Statystyki
      tags$div(
        style = "margin-top: 25px; font-size: 18px; font-weight: bold; text-align: left;",
        htmlOutput("stats")
      )
    )
  ),
  
  # Dolny panel: Wykres
  fluidRow(
    column(
      width = 12,
      plotlyOutput("ratingPlot", height = "500px")
    )
  )
)
  

server <- function(input, output, session) {
  
  # Przefiltrowane dane na podstawie zakresu dat i wybranego otwarcia
  filteredData <- reactive({
    data <- wittchen_data[wittchen_data$date >= input$dateRange[1] &
                            wittchen_data$date <= input$dateRange[2], ]
    if (input$opening != "all") {
      data <- data[data$opening == input$opening, ]
    }
    data
  })
  
  # Statystyki: wygrane, remisy, przegrane
  output$stats <- renderUI({
    data <- filteredData()
    won <- sum((data$result == "1-0" & data$white == "wittchen") | 
                 (data$result == "0-1" & data$black == "wittchen"))
    draw <- sum(data$result == "1/2-1/2")
    lost <- sum((data$result == "0-1" & data$white == "wittchen") | 
                  (data$result == "1-0" & data$black == "wittchen"))
    
    # Formatowanie statystyk z kolorami
    HTML(paste0(
      "<span style='color:green;'>", won, "</span>",
      " - ",
      "<span style='color:orange;'>", draw, "</span>",
      " - ",
      "<span style='color:red;'>", lost, "</span>"
    ))
  })
  
  # Generowanie wykresu
  output$ratingPlot <- renderPlotly({
    data <- filteredData()
    
    # Tworzenie kolumny tooltip z kolorowym wynikiem
    data$tooltip <- ifelse(
      (data$result == "1-0" & data$white == "wittchen") | 
        (data$result == "0-1" & data$black == "wittchen"),
      "<span style='color:green;'>WIN</span>",
      ifelse(
        data$result == "1/2-1/2",
        "<span style='color:orange;'>DRAW</span>",
        "<span style='color:red;'>LOSE</span>"
      )
    )
    
    # Generowanie wykresu
    plot <- ggplot(data, aes(x = date, y = elo)) +
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(aes(
        text = paste0(
          "Data: ", date, "<br>",
          "Ranking: ", elo, "<br>",
          "Result: ", tooltip
        )
      ), color = "black", size = 1.5) +
      labs(
        title = "Zmiana rankingu gracza Wittchen w czasie",
        x = "Data (2024)",
        y = "Ranking"
      ) +
      theme_minimal() +
      scale_x_date(date_labels = "%B", date_breaks = "1 month") +
      scale_y_continuous(breaks = seq(2000, 2300, by = 50))
    
    # Konwersja do plotly z obsługą HTML
    ggplotly(plot, tooltip = "text") %>% 
      layout(hoverlabel = list(align = "left"))  # Dopasowanie wyświetlania tekstu
  })
}

# Run the app
shinyApp(ui = ui, server = server)

#########################################################################

############################
#### WYWOŁANIE DLA MNIE ####
############################

data <- data.frame(white2, black2, result2, date2, white_elo2, 
                    black_elo2, opening2, opening_variation2, eco2)

kuba_data <- data[data$white2 == "tadziolul" | data$black2 == "tadziolul", ]
kuba_data$date <- as.Date(kuba_data$date, format = "%Y.%m.%d")  # Konwertuj kolumnę daty

# Oblicz aktualny ranking po każdej partii
kuba_data <- kuba_data[order(kuba_data$date), ]
kuba_data$elo <- as.numeric(ifelse(kuba_data$white2 == "tadziolul", 
                                       kuba_data$white_elo, 
                                       kuba_data$black_elo))

kuba_data <- kuba_data[order(kuba_data$date), ]


ui <- fluidPage(
  titlePanel("Zmiana rankingu gracza qbkuba7 w czasie"),
  
  # Górny panel: wybór dat, otwarcia i statystyk
  fluidRow(
    column(
      width = 5,  # Wybór dat
      dateRangeInput(
        "dateRange",
        label = "Wybierz zakres dat:",
        start = min(kuba_data$date),
        end = max(kuba_data$date),
        min = min(kuba_data$date),
        max = max(kuba_data$date)
      )
    ),
    column(
      width = 3,  # Wybór otwarcia
      style = "padding-right: 10px;",
      selectInput(
        "opening2", 
        label = "Wybierz otwarcie:", 
        choices = c("Wszystkie" = "all", unique(kuba_data$opening2)),
        selected = "all"
      )
    ),
    column(
      width = 4,  # Statystyki
      tags$div(
        style = "margin-top: 25px; font-size: 18px; font-weight: bold; text-align: left;",
        htmlOutput("stats")
      )
    )
  ),
  
  # Dolny panel: Wykres
  fluidRow(
    column(
      width = 12,
      plotlyOutput("ratingPlot", height = "500px")
    )
  )
)


server <- function(input, output, session) {
  
  # Przefiltrowane dane na podstawie zakresu dat i wybranego otwarcia
  filteredData <- reactive({
    data <- kuba_data[kuba_data$date >= input$dateRange[1] &
                        kuba_data$date <= input$dateRange[2], ]
    if (input$opening2 != "all") {
      data <- data[data$opening2 == input$opening2, ]
    }
    data
  })
  
  # Statystyki: wygrane, remisy, przegrane
  output$stats <- renderUI({
    data <- filteredData()
    won <- sum((data$result2 == "1-0" & data$white2 == "tadziolul") | 
                 (data$result2 == "0-1" & data$black2 == "tadziolul"))
    draw <- sum(data$result2 == "1/2-1/2")
    lost <- sum((data$result2 == "0-1" & data$white2 == "tadziolul") | 
                  (data$result2 == "1-0" & data$black2 == "tadziolul"))
    
    # Formatowanie statystyk z kolorami
    HTML(paste0(
      "<span style='color:green;'>", won, "</span>",
      " - ",
      "<span style='color:orange;'>", draw, "</span>",
      " - ",
      "<span style='color:red;'>", lost, "</span>"
    ))
  })
  
  # Generowanie wykresu
  output$ratingPlot <- renderPlotly({
    data <- filteredData()
    
    # Tworzenie kolumny tooltip z kolorowym wynikiem
    data$tooltip <- ifelse(
      (data$result2 == "1-0" & data$white2 == "tadziolul") | 
        (data$result2 == "0-1" & data$black2 == "tadziolul"),
      "<span style='color:green;'>WIN</span>",
      ifelse(
        data$result2 == "1/2-1/2",
        "<span style='color:orange;'>DRAW</span>",
        "<span style='color:red;'>LOSE</span>"
      )
    )
    
    # Generowanie wykresu
    plot <- ggplot(data, aes(x = date, y = elo)) +
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(aes(
        text = paste0(
          "Data: ", date, "<br>",
          "Ranking: ", elo, "<br>",
          "Result: ", tooltip
        )
      ), color = "black", size = 1.5) +
      labs(
        title = "Zmiana rankingu gracza qbkuba7 w czasie",
        x = "Data (2024)",
        y = "Ranking"
      ) +
      theme_minimal() +
      scale_x_date(date_labels = "%B", date_breaks = "1 month") +
      scale_y_continuous(breaks = seq(1400, 1800, by = 50))
    
    # Konwersja do plotly z obsługą HTML
    ggplotly(plot, tooltip = "text") %>% 
      layout(hoverlabel = list(align = "left"))  # Dopasowanie wyświetlania tekstu
  })
}

# Run the app
shinyApp(ui = ui, server = server)

