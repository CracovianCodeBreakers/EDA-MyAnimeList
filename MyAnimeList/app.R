# Sekcja bibliotek użytych w celu filtrowania i renderowania wykresów

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)

# sekcja filtrowania datasetu

dataset <- read.csv("../anime-filtered.csv")

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
    
}

# Wywołanie aplikacji Shiny

shinyApp(ui = ui, server = server)



# 1. Histogram score filtrowany
# 2. Scatterplot color=Type, x=popularity, y=score
# 3. Scatterplot x=populaity, y=score, color=source
# 4. histogram x=episodes y=count
# 5. scatterplot x=popularity y=score, color=gatunek
# 6. Scatterplot x=score, y=episodes, color=type