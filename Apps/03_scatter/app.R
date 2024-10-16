# Interactive Scatter Plot
## App A Day - Day 3
## Zach Peagler
## 10/14/2024

# dependencies
library(shiny)
library(tidyverse)
library(bslib)
library(scico)
library(showtext)
# graphical setup
font_add_google("Open Sans", family = "open")
font_add_google("Montserrat", family = "mont")
showtext_auto()
# data
dat <- iris
vars <- colnames(iris)
palettes <- scico_palette_names()
# UI
ui <- navbarPage("Interactive Scatter Plot",
    theme = bs_theme(bootswatch = "sandstone"),
    sidebar = sidebar(
        selectInput("pal", "Color Palette", choices = palettes,
                    selected = "lipari"),
        selectInput("x", "X Variable", choices = vars,
                    selected = "Sepal.Length"),
        selectInput("y", "Y Variable", choices = vars,
                    selected = "Sepal.Width"),
        selectInput("col", "Color Variable", choices = vars,
                    selected = "Species")
        ),
        # output plot on UI
        nav_panel("Plot",
          card(
           card_body(plotOutput("scatterPlot"), max_width = 500)
          )
        ),
    nav_spacer(),
    nav_item(tags$a("Github", href = "https://github.com/zachpeagler/AppADay/03_scatter"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # reactive expressions
  Rx <- reactive({input$x})
  Ry <- reactive({input$y})
  Rcol <- reactive({input$col})
  Rpal <- reactive({input$pal})
  # render plot on server
    output$scatterPlot <- renderPlot({
        p <- ggplot(iris, aes(x=.data[[Rx()]], y=.data[[Ry()]], color=.data[[Rcol()]]))+
        geom_point(size=3)+
        theme_bw()+
        theme(
          legend.position="right",
          text = element_text(size=16, family="mont", lineheight = 0.5)
        )
        if (Rcol() == "Species") {
          p <- p + scale_color_scico_d(begin = 0, end = 0.8, palette = Rpal())
        } else {
          p <- p + scale_color_scico(begin = 0, end = 0.8, palette = Rpal())
        }
        return(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
