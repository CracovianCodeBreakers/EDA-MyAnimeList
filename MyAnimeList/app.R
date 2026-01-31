# Sekcja bibliotek użytych w celu filtrowania i renderowania wykresów, wychodzimy z założenia, że sprzęt na którym jest uruchamiany plik Shiny zainstalowane ma już poniższe pakiety

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)

# sekcja filtrowania datasetu

dataset <- read.csv("anime-filtered.csv")

dataset$Japanese.name <- NULL
dataset$anime_id <- NULL
dataset$Name <- dataset$English.name
dataset$English.name <- NULL
dataset$sypnopsis <- NULL

summary(dataset)

dataset <- dataset %>%
  filter(Name == "Unknown")

dataset$Type <- as.factor(dataset$Type)
dataset$Source <- as.factor(dataset$Source)
dataset$Rating <- as.factor(dataset$Rating)

dataset$Episodes <- as.numeric(dataset$Episodes)

convert_to_minutes <- function(x){
  hours <- as.numeric(str_extract(x, "\\d+(?=\\s*hr)")) %>% replace_na(0)
  minutes <- as.numeric(str_extract(x, "\\d+(?=\\s*min)")) %>% replace_na(0)
  seconds <- as.numeric(str_extract(x, "\\d+(?=\\s*sec)")) %>% replace_na(0)
  
  return((hours*60)+minutes+(seconds/60))
}

dataset$Duration <- convert_to_minutes(dataset$Duration)

odstajace <- dataset %>%
  filter(Score >= 6.4 & Score <= 6.6)

filtered <- dataset %>%
  filter(Score <= 6.5 | Score >= 6.52)


# Sekcja interfejsu

ui <- fluidPage(
    titlePanel("MyAnimeList - aplikacja Shiny"),
    mainPanel(
      width=12,
      tabsetPanel(
        tabPanel(
          "Histogram średniego wyniku punktowego",
          sidebarLayout(
            sidebarPanel(
              sliderInput("bins", "Liczba słupków:", min = 1, max = 50, value = 30)
            ),
            mainPanel(
              plotOutput("scoreHist")
            )
          )
        ),
        tabPanel(
          "Wykres punktowy popularność do score względem rodzaju",
          sidebarLayout(
            sidebarPanel(
              selectInput("type", "Wybierz Typ Anime:", 
                          choices = c("TV", "Movie","Music", "OVA", "Special", "ONA" )),
              checkboxInput("show_smooth", "Dodaj linię trendu", FALSE)
            ),
            mainPanel(
              plotOutput("scoreScatter")
            )
          )
        ),
        tabPanel(
          "Wykres punktowy popularność do score względem źródła historii",
          sidebarLayout(
            sidebarPanel(
              selectInput("source", "Wybierz Typ Anime:", 
                          choices = c(
                            "4-koma manga", 
                            "Book",
                            "Card game", 
                            "Digital manga", 
                            "Game", 
                            "Light novel",
                            "Manga",
                            "Music",
                            "Novel",
                            "Original",
                            "Picture book",
                            "Other",
                            "Radio",
                            "Visual novel",
                            "Web manga"
                          )),
              checkboxInput("source_trend", "Dodaj linię trendu", FALSE)
            ),
            mainPanel(
              plotOutput("sourceScatter")
            )
          )
        ),
        tabPanel(
          "Histogram ilości odcinków serii",
          sidebarLayout(
            sidebarPanel(
              sliderInput("bins_episodes", "Liczba słupków:", min = 1, max = 50, value = 30),
              sliderInput("xlim_episodes", "Zakres liczby odcinków (Oś X):", 
                          min = 0, max = 1000, value = c(0, 50))
            ),
            mainPanel(
              plotOutput("episodesHist")
            )
          )
        ),
        tabPanel(
          "Wykres punktowy popularność do score względem gatunku",
          sidebarLayout(
            sidebarPanel(
              selectInput("genre", "Wybierz Gatunek Anime:", 
                          choices = c(
                            "Action", 
                            "Romance",
                            "Thriller", 
                            "Slice of Life", 
                            "Fantasy", 
                            "Sports",
                            "Comedy"
                          )),
              checkboxInput("genre_smooth", "Dodaj linię trendu", FALSE)
            ),
            mainPanel(
              plotOutput("genreScatter")
            )
          )
        ),
        tabPanel(
          "Wykres punktowy ilości odcinków do score względem typu emisji",
          sidebarLayout(
            sidebarPanel(
              selectInput("type_scatter", "Wybierz Typ Anime:", 
                          choices = c("TV", "Movie","Music", "OVA", "Special", "ONA" ))
            ),
            mainPanel(
              plotOutput("episodesScatter")
            )
          )
        ),
      )
    )
)

# Sekcja serwerowa/renderowanie plotów

server <- function(input, output) {

    output$scoreHist <- renderPlot({

        x    <- filtered$Score
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Wynik anime',
             main = 'Histogram średniego wyniku punktowego Anime')
    })
    
    output$scoreScatter <- renderPlot({
      
      req(input$type)
      
      data_to_plot <- filtered %>% 
        filter(Type == input$type)
      
      
      wykres <- ggplot(data_to_plot, aes(x=Popularity, y=Score, color=Type))+
        geom_point()
      
      if(input$show_smooth){
        wykres <- wykres + geom_smooth(method = "lm", color = "red", se = FALSE)
      }
        
      return(wykres)
        
    })
    
    output$sourceScatter <- renderPlot({
      
      req(input$source)
      
      data_to_plot <- filtered %>% 
        filter(Source == input$source)
      
      
      wykres <- ggplot(data_to_plot, aes(x=Popularity, y=Score, color=Source))+
        geom_point()
      
      if(input$source_trend){
        wykres <- wykres + geom_smooth(method = "lm", color = "red", se = FALSE)
      }
      
      return(wykres)
      
    })
    
    output$episodesHist <- renderPlot({
      
      filtered <- filtered %>%
        filter(!is.na(Episodes))
      
      ggplot(filtered, aes(x=Episodes))+
        geom_histogram(bins = input$bins_episodes)+
        scale_y_log10()+
        xlim(input$xlim_episodes)
    })
    
    output$genreScatter <- renderPlot({
      
      filtered <- filtered %>%
        filter(!is.na("Genres"))
      
      action <- filtered %>%
        filter(str_detect(Genres, "Action"))
      romance <- filtered %>%
        filter(str_detect(Genres, "Romance"))
      thriller <- filtered %>%
        filter(str_detect(Genres, "Thriller"))
      fantasy <- filtered %>%
        filter(str_detect(Genres, "Fantasy"))
      sports <- filtered %>%
        filter(str_detect(Genres, "Sports"))
      supernatural <- filtered %>%
        filter(str_detect(Genres, "Supernatural"))
      comedy <- filtered %>%
        filter(str_detect(Genres, "Comedy"))
      slice_of_life <- filtered %>%
        filter(str_detect(Genres, "Slice of Life"))
      
      genres <- bind_rows(
        action %>% mutate(Gatunek = "Action"),
        romance %>% mutate(Gatunek = "Romance"),
        thriller %>% mutate(Gatunek = "Thriller"),
        fantasy %>% mutate(Gatunek = "Fantasy"),
        sports %>% mutate(Gatunek = "Sports"),
        supernatural %>% mutate(Gatunek = "Supernatural"),
        comedy %>% mutate(Gatunek = "Comedy"),
        slice_of_life %>% mutate(Gatunek = "Slice of Life")
      )
      genres$Gatunek <- as.factor(genres$Gatunek)
      
      
      data_to_plot <- genres %>% 
        filter(Gatunek == input$genre)
      
      
      wykres <- ggplot(data_to_plot, aes(x=Popularity, y=Score, color=Gatunek))+
        geom_point()
      
      if(input$genre_smooth){
        wykres <- wykres + geom_smooth(method = "lm", color = "red", se = FALSE)
      }
      
      return(wykres)
      
    })
    
    output$episodesScatter <- renderPlot({
      
      data_to_plot <- filtered %>%
        filter(!is.na(Episodes)) %>%
        filter(Type == input$type_scatter)
      
      
      wykres <- ggplot(data_to_plot, aes(x=Score, y=Episodes, color=Type))+
        geom_point()
      
      if(input$source_trend){
        wykres <- wykres + geom_smooth(method = "lm", color = "red", se = FALSE)
      }
      
      return(wykres)
      
    })
}

# Wywołanie aplikacji Shiny

shinyApp(ui = ui, server = server)



# 1. Histogram score filtrowany
# 2. Scatterplot color=Type, x=popularity, y=score
# 3. Scatterplot x=populaity, y=score, color=source
# 4. histogram x=episodes y=count
# 5. scatterplot x=popularity y=score, color=gatunek
# 6. Scatterplot x=score, y=episodes, color=type