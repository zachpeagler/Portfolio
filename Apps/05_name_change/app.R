# Baby Name Change Over Time
## App A Day - Day 5
## Zach Peagler
## 10/16/2024

library(tidyverse)
library(shiny)
library(bslib)
library(MASS)
library(DT)
library(plotly)
library(scico)

## set working directory
setwd("C:/Github/App-A-Day/data/baby_names")
## get all .txt files in directory
dir <- list.files(pattern="\\.txt$")
## read txt files, separating by comma
data <- lapply(dir, read.delim, sep=",", header=FALSE)
## initialize dataframe
datjoin <- as.data.frame(data[1]) %>%
  rename(Name = "V1",
         Gender = "V2",
         Count = "V3") %>%
  mutate(Year = 1880)
## set variables for year and year range
## to make this more robust we could get the year from the file name and include
## it in the original data frame, but for now I'm not going to do that.
Year = 1880
yr_range <- c(1:144)
## for each in year range, we get the data from the list and 
## add it to the joined data frame
for (x in yr_range) {
  Year <- Year + 1
  newdata <- as.data.frame(data[x]) %>%
    rename(Name = "V1",
           Gender = "V2",
           Count = "V3") %>%
    mutate(Year = Year)
  datjoin <- rbind(datjoin, newdata)
}

## color palettes
palettes <- scico_palette_names()

# Define UI for application that draws a histogram
ui <- navbarPage("Baby Name Change Over Time",
                 theme = bs_theme(bootswatch = "flatly"),
                 sidebar = sidebar(),
                 selectInput("palette", "Select Color Palette", palettes,
                             selected = "oslo"),
        card("Percent change in the amount of people born with a certain name from 1880 to 2024",
             layout_sidebar(sidebar = sidebar(
               selectInput("change_name", "Select Name", names, selected = "Mary")
              ),
              plotlyOutput("change_plot")
              )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  Rpal <- reactive({input$palette})
  
  ## Change in a name over time
  output$change_plot <- renderPlotly({
    data <- Rdat() %>%
      subset(Name == input$change_name) %>%
      mutate(Percent_Change = (Count/lag(Count)*100)-100)
    p <- plot_ly(data, x = ~Year, y = ~Percent_Change, color=~Percent_Change, 
                 colors = scico(length(data$Name), begin = 0, end = 0.8,
                                palette = Rpal()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
