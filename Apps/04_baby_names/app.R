# Baby Names
## App A Day - Day 4
## Zach Peagler
## 10/15/2024

# dependencies
library(tidyverse)
library(shiny)
library(bslib)
library(MASS)
library(DT)
library(plotly)
library(data.table)
library(scico)

# file name
bn_file <- "C:/Github/App-A-Day/04_baby_names/Top50BabyNames1880to2024.csv"
# import data
bn_data <- read.csv(bn_file)

data <- bn_data %>%
  group_by(Name) %>%
  summarise_at(vars(Count), list(sum))
#objects
## names
names <- bn_data[!duplicated(bn_data$Name),2]
## genders
gender_filt <- c("F", "M")
## palettes
palettes <- scico_palette_names()
# UI
ui <- navbarPage("Baby Names",
                 theme = bs_theme(bootswatch = "litera"),
                 sidebar = sidebar(
                   markdown("#### Controls"),
                   selectInput("gender_filter", "Filter by Gender", gender_filt,
                               selected = "F"),
                   sliderInput("n_names", "Number of Names", min = 1, max = 50,
                               value = 10),
                   numericInput("date_min", "Start Date", min = 1880, max = 2024,
                                value = 1880),
                   numericInput("date_max", "End Date", min = 1880, max = 2024,
                                value = 2024),
                   selectInput("palette", "Select Color Palette", palettes,
                               selected = "oslo")
                 ),
                 nav_panel("Scatter",
                   card("Scatter Plot of Baby Names by Year",
                     plotlyOutput("scatter_plot")
                        )
                 ),
                 nav_panel("Bar",
                           card("Total Occurrence of Baby Names",
                                plotlyOutput("total_occurrence")),
                           card(layout_column_wrap(
                             card("Most Common Names",
                               plotlyOutput("topnames_bar")),
                             card("Least Common Names",
                             plotlyOutput("leastnames_bar"))
                           ))
                 ),
                 nav_panel("Data",
                           DTOutput("data")
                           )
  
)
# Server
server <- function(input, output) {
# Reactive functions
  Rn_names <- reactive({input$n_names})
  Rpal <- reactive({input$palette})
  Rdat <- reactive({
    ## make data object
    data <- bn_data
    ## filter by gender
    if (input$gender_filter == "F") {
      data <- subset(data, Gender == "F")
    } else {
      data <- subset(data, Gender == "M")
    }
    ## filter by year
    data <- subset(data, 
                    Year > input$date_min &
                    Year < input$date_max)
    return(data)
  })
  Rhead <- reactive({
    data <- Rdat()
    head <- data %>% group_by(Year) %>%
      slice_max(order_by = Count, n = Rn_names())
    return(head)
  })
  Rtail <- reactive({
    data <- Rdat()
    tail <- data %>% group_by(Year) %>%
      slice_min(order_by = Count, n = Rn_names())
    return(tail)
  })
# Scatter plot
    output$scatter_plot <- renderPlotly({
    data <- Rhead()
    p <- plot_ly(data, x = ~Year, y = ~Count, color=~Name, 
                colors = scico(length(data$Name), begin = 0, end = 0.8,
                               palette = Rpal()))
      
    })
# Bars page
## Most common names
    output$total_occurrence <- renderPlotly({
      data <- bn_data %>%
        group_by(Name, Gender) %>%
        summarise_at(vars(Count), list(sum))
      p <- plot_ly(data, x=~Name, y=~Count, color=~Gender, type = "bar", orientation = "v")
    })
## Most common names
    output$topnames_bar <- renderPlotly({
      data <- Rdat() %>%
        group_by(Name) %>%
        summarise_at(vars(Count), list(sum))
      data <- data[order(data$Count, decreasing = TRUE),] %>%
        head(n=Rn_names())
      p <- ggplot(data, aes(x=Name, y=Count, fill=Count))+
            geom_col()+
            scale_fill_scico(begin = 0, end = 0.8, palette = Rpal())+
            theme_bw()+
            theme(
              axis.text.x = element_text(size=12, angle=45, lineheight=1, hjust=1, vjust=1)
            )
      p <- ggplotly(p)
      return(p)
    })
## Least common names
    output$leastnames_bar <- renderPlotly({
      data <- Rdat() %>%
        group_by(Name) %>%
        summarise_at(vars(Count), list(sum))
      data <- data[order(data$Count, decreasing = TRUE),] %>%
        tail(n=Rn_names())
      p <- ggplot(data, aes(x=Name, y=Count, fill=Count))+
        geom_col()+
        scale_fill_scico(begin = 0, end = 0.8, palette = Rpal())+
        theme_bw()+
        theme(
          axis.text.x = element_text(size=12, angle=45, lineheight=1, hjust=1, vjust=1)
        )
      p <- ggplotly(p)
      return(p)
    })

    
## data output for Data page
    output$data <- renderDT({
      Rdat()[,2:4]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
