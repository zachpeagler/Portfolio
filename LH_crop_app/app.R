library(shiny)
library(tidyverse)
library(scico)
library(tigris)
library(plotly)


# load data
LH_crops_state_csv <- "C:/Users/zachp/OneDrive/Documents/GitHub/Portfolio/Data/LH_crops_states.csv"
LH_crops_state <- read.csv(LH_crops_state_csv)
LH_crops_city_csv <- "C:/Users/zachp/OneDrive/Documents/GitHub/Portfolio/Data/LH_crops_cities.csv"
LH_crops_city <- read.csv(LH_crops_city_csv)

# graphical setup
## colors
p_palettes <- scico_palette_names()
## fonts
font_add_google("Open Sans", family = "open")
font_add_google("Montserrat", family = "mont")
showtext_auto()
## shapes
four_shapes = c(15,16,17,23)

# go ahead and make objects for cropnames and seasons
cropnames <- colnames(LH_crops_state[,13:221])
seasons <- c("Spring", "Summer", "Fall", "Winter")

# SHINY
## define UI logic
ui <- fluidPage(
    ### application title
    titlePanel("Localharvest farm products"),
        ### sidebar with select for product and palette and a checkbox for facet wrap
    sidebarLayout(
    sidebarPanel(
            selectInput("grp_by", "Group By", 
                        choices = c("city", "state"), selected="state"),
            selectInput("product","Select Product",
              choices = cropnames, selected = "corn"),
            selectInput("paltt","Select Palette",
                        choices = p_palettes, selected = "bilbao"),
            checkboxInput("f_wrap", "Facet Wrap by Season", value = FALSE),
            conditionalPanel(condition = "input.f_wrap == '0'",
                             selectInput("szn", "Select Season", 
                                         choices=seasons, selected = "Fall")),
            plotOutput("histCrop")
            ),
    mainPanel(
        ### main panel with plot
         plotlyOutput("map")
    )))

## define server logic
server <- function(input, output, session) {
  
  # reactive variables
  server_data <- reactive( if (input$grp_by == "city") server_data <- LH_crops_city
            else server_data <- LH_crops_city)
  where <- reactive( if (input$grp_by == "city") where <- "city" else where <- "state")
  Rprod <- reactive(input$product)
  Rpalette <- reactive(input$paltt)
  Rseason <- reactive(input$szn)
  Rfwrap <- reactive(input$f_wrap)
  
  radius <- server_data[[Rprod]]/max(server_data[[Rprod]]) * 30000
  
  # create map
    output$map <- renderPlotly({
      p <- ggplot(if (Rfwrap == TRUE) server_data else server_data %>% filter(Season == Rseason), aes(fill=Rprod))+
        geom_sf(color="black")+
        if (Rfwrap == TRUE) facet_wrap(~Season)+
        scale_fill_scico_d(begin=1, end=0, palette=Rpalette)+
        guides(fill = guide_legend(title = Rprod))+
        labs(title=paste("Sum of",Rprod, "production by", where),
             subtitle="For family farms in the 48 continental United States",
             caption="Data from localharvest (2024) and tigris (2022)")+
        coord_sf(clip = "off") +
        theme_map() +
        theme(legend.position = "inside",
              legend.position.inside = c(1,.35),
              legend.title.position = "top",
              legend.title = element_text(size=20, family="open"),
              strip.background = element_rect(fill=NA, color=NA),
              text = element_text(size=24, family="mont"),
              title = element_text(size=30, family="open", face="bold", lineheight = .5),
              plot.subtitle = element_text(size=24, family="mont", face="italic", lineheight = .5),
              plot.caption = element_text(size=20, family="mont", face="italic", lineheight = .5))
      ggplotly(p)
    })
}


## run application 
shinyApp(ui = ui, server = server)
