# Lichess Statistical Dashboard (R & Shiny)

An interactive web application built with R and Shiny for deep statistical analysis of personal chess games played on Lichess.org. 

Unlike standard chess trackers, this project features a **custom-built PGN (Portable Game Notation) parser** utilizing Regular Expressions (RegEx) to extract non-standard data points, such as mating squares, mating pieces, and advanced streak calculations, without relying on external chess libraries.

## Key Features & Analytics

### 1. Custom PGN Data Wrangling
The application parses raw `.pgn` text files from scratch. It uses complex RegEx patterns to extract:
- Player Elos, Openings, and ECO codes.
- **Mating Analysis:** Dynamically finds the exact board square where checkmate was delivered (e.g., `g7`) and identifies the mating piece (Queen, Rook, Pawn, etc.).
- **Game Length:** Extracts the total number of moves played in each match.

### 2. Time-Series Data Interpolation
To create smooth and realistic Elo rating progression charts, the app uses constant interpolation (`approx` function) to fill in the gaps for days when no games were played, ensuring the time-series data accurately reflects the rating on any given day.

### 3. Interactive Visualizations (Plotly & ggplot2)
- **Elo Progression Chart:** Interactive time-series plot with hover tooltips and dynamic date range filtering.
- **Mating Square Heatmap:** A custom-plotted chessboard visualizing the frequency of checkmates on specific squares.
- **Mating Piece Distribution:** Bar chart showcasing which pieces deliver the final blow (mapped to Unicode chess symbols).
- **Game Length Distribution:** Histogram analyzing the most common game durations.
- **Win/Loss/Draw Ratios:** Custom HTML/CSS animated progress bars dynamically reacting to date and opening filters.

### 4. Advanced Game Statistics
- Calculates the longest continuous **Winning and Losing Streaks**.
- Identifies the **Top 5 Best Wins** (highest-rated opponents defeated) and **Top 5 Worst Losses** (lowest-rated opponents).

## Tech Stack
- **Language:** R
- **Framework:** Shiny (Web UI & Server logic)
- **Data Manipulation:** `dplyr`, Base R (Regex, string manipulation)
- **Data Visualization:** `ggplot2`, `plotly` (Interactive charts)
- **Styling:** Custom CSS injected via HTML for a sleek Dark Mode UI.

## How to Run Locally

1. Clone the repository:
   ```bash
   git clone [https://github.com/yourusername/lichess_shiny_dashboard.git](https://github.com/yourusername/lichess_shiny_dashboard.git)
   ```
2. Open RStudio or your preferred R environment.
3. Ensure you have the required packages installed:
   ```R
   install.packages(c("shiny", "ggplot2", "plotly", "dplyr"))
   ```
4. Place your raw Lichess PGN files in the `www/` directory (e.g., `lichess_wittchen_2024-12-18.pgn`).
5. Run the app:
   ```R
   shiny::runApp()
   ```

## Dashboard Preview

*(Below is a preview of the interactive dashboard)*

![Dashboard Preview](preview.png)
