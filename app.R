library(shiny)

pgn_file <- "lichess_wittchen_2024-12-18.pgn"
pgn_text <- readLines(pgn_file)
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

dane <- data.frame(white, black, result, date, white_elo, 
                   black_elo, opening, opening_variation, eco)


#KUBAAA:

pgn_text2 <- readLines("lichess_tadziolul_2024-12-18.pgn")
pgn_text2 <- pgn_text2[nzchar(pgn_text2)] 


white2 <- extract_white_players(pgn_text2)
black2 <- extract_black_players(pgn_text2)
result2 <- extract_results(pgn_text2)
date2 <- extract_dates(pgn_text2)
white_elo2 <- extract_white_elo(pgn_text2)
black_elo2 <- extract_black_elo(pgn_text2)
eco2 <- extract_eco(pgn_text2)
opening_variation2 <- extract_opening(pgn_text2)
opening2 <- sub("\\:.*", "", opening_variation2)

dane2 <- data.frame(white2, black2, result2, date2, white_elo2, 
                    black_elo2, opening2, opening_variation2, eco2)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
