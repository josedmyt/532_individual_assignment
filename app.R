library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(arrow)

# Load data
songs <- read_parquet("data/processed/spotify_songs.parquet")

genre_choices <- c("All", sort(unique(na.omit(songs$playlist_genre))))

ui <- page_navbar(
  title = div(
    span("🎵 Spotifind", class = "fw-bold"),
    span(" · Spotify Song Explorer", class = "text-muted ms-2")
  ),
  theme = bs_theme(bootswatch = "flatly"),

  nav_panel(
    "Dashboard",

    layout_sidebar(
      sidebar = sidebar(
        h4("Filter Controls"),
        selectInput("genre", "Genre", choices = genre_choices, selected = "All"),
        sliderInput("popularity", "Popularity", min = 0, max = 100, value = c(0, 100)),
        sliderInput("danceability", "Danceability", min = 0, max = 1, value = c(0, 1), step = 0.01),
        actionButton("reset", "Reset Filters", class = "btn-secondary")
      ),

      layout_columns(
        value_box(
          title = "Songs Found",
          value = textOutput("song_count"),
          showcase = "🎵",
          theme = "primary"
        ),
        value_box(
          title = "Average Popularity",
          value = textOutput("avg_popularity"),
          showcase = "⭐",
          theme = "success"
        )
      ),

      card(
        card_header("Top Genres"),
        plotOutput("genre_plot", height = "300px")
      ),

      card(
        card_header("Filtered Songs"),
        DTOutput("songs_table")
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateSelectInput(session, "genre", selected = "All")
    updateSliderInput(session, "popularity", value = c(0, 100))
    updateSliderInput(session, "danceability", value = c(0, 1))
  })

  filtered_songs <- reactive({
    df <- songs %>%
      filter(
        track_popularity >= input$popularity[1],
        track_popularity <= input$popularity[2],
        danceability >= input$danceability[1],
        danceability <= input$danceability[2]
      )

    if (input$genre != "All") {
      df <- df %>% filter(playlist_genre == input$genre)
    }

    df
  })

  output$song_count <- renderText({
    nrow(filtered_songs())
  })

  output$avg_popularity <- renderText({
    df <- filtered_songs()
    if (nrow(df) == 0) return("—")
    round(mean(df$track_popularity, na.rm = TRUE), 1)
  })

  output$genre_plot <- renderPlot({
    df <- filtered_songs()

    validate(
      need(nrow(df) > 0, "No songs match the selected filters.")
    )

    df %>%
      count(playlist_genre, sort = TRUE) %>%
      slice_head(n = 6) %>%
      ggplot(aes(x = reorder(playlist_genre, n), y = n)) +
      geom_col(fill = "#1DB954") +
      coord_flip() +
      labs(
        x = "Genre",
        y = "Number of Songs",
        title = "Top Genres in Filtered Results"
      ) +
      theme_minimal()
  })

  output$songs_table <- renderDT({
    df <- filtered_songs() %>%
      select(
        Song = track_name,
        Artist = track_artist,
        Album = track_album_name,
        Genre = playlist_genre,
        Popularity = track_popularity
      ) %>%
      arrange(desc(Popularity))

    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)