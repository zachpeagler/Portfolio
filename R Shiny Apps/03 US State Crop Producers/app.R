#### FAMILY FARMS STATE ####

#dependencies
library(shiny)
library(tidyverse)
library(scico)
library(showtext)
library(tigris)
library(ggthemes)
library(ggspatial)

# load data
LH_state_file <- "C:/Github/Portfolio/R Shiny Apps/03 US State Crop Producers/lh_state_cleaned.csv"
LH_state <- read.csv(LH_state_file)

usa <- states()
## filter down to the continental 48 states
usa48 <- usa %>%
  filter(NAME!="American Samoa",
         NAME!="Guam",
         NAME!="Commonwealth of the Northern Mariana Islands",
         NAME!="United States Virgin Islands",
         NAME!="Puerto Rico",
         NAME!="Hawaii",
         NAME!="Alaska",
         NAME!="District of Columbia") %>%
  mutate(lon = st_coordinates(st_centroid(.))[,1],
         lat = st_coordinates(st_centroid(.))[,2])
# order by state name
usa48 <- usa48[order(usa48$NAME),]

# merge state geometry info with production info
us_st_prod <- merge(usa48, LH_state, by.x="NAME", by.y="State")

# graphical setup
## colors
p_palettes <- scico_palette_names()
## fonts
font_add_google("Open Sans", family = "open")
font_add_google("Montserrat", family = "mont")
showtext_auto()

# go ahead and make objects for seasons and crops
seasons <- c("Spring", "Summer", "Fall", "Winter")
crops <- names(st_drop_geometry(us_st_prod[,47:208]))

# UI
ui <- fluidPage(
  ### application title
  titlePanel("Localharvest State Production"),
  
  ### sidebar with select for product and palette
  sidebarLayout(
    sidebarPanel(
      selectInput("product","Select Product",
                  choices = cutprods, selected = "corn"),
      selectInput("palette","Select Palette",
                  choices = p_palettes, selected = "bilbao"),
      selectInput("fwrap", "Facet Wrap by Season",
                  choices = c("TRUE, FALSE"), selected = "FALSE"),
      conditionalPanel(condition = "input.fwrap == 'FALSE'",
                       selectInput("season", "Select Season",
                                   choices = seasons, selected = "Fall"))
    ), #-- end sidebar
    ### main panel with plot
    mainPanel(
      plotOutput("map")
    ) #-- end mainpanel
  ) #-- end layout
) #-- end fluidpage

# server
server <- function(input, output) {
  
  # reactive variables
  Rseason <- reactive({input$season})
  Rprod <- reactive({input$product})
  Rpalette <- reactive({input$palette})
  
  # create map
  output$map <- renderPlot({
    
    # filter the data to only be the selected season
    LH_season <- LH_state %>% filter(Season == Rseason())
    
    # generate plot
    ggplot(us_st_prod, aes(fill=.data[[Rprod()]]))+
      geom_sf(color="black")+
      scale_fill_scico(begin=1, end=0, palette = gettext(Rpalette()))+
      guides(fill = guide_colorbar(title = Rprod()))+
      labs(title=paste("Number of farms that produce", Rprod(), "by state", sep =" "),
           subtitle="For family farms in the 48 continental United States",
           caption="Data from localharvest (2024) and tigris (2022)")+
      coord_sf(clip = "off") +
      annotation_scale(location="bl", text_cex = 2, text_family = "mont")+
      annotation_north_arrow(location="br", height=unit(0.5, "cm"), width=unit(0.5, "cm"))+
      theme_map() +
      theme(legend.position = "inside",
            legend.position.inside = c(0,.1),
            legend.title.position = "top",
            legend.direction = "horizontal",
            legend.title = element_text(size=20, family="open"),
            strip.background = element_rect(fill=NA, color=NA),
            text = element_text(size=24, family="mont"),
            title = element_text(size=30, family="open", face="bold", lineheight = .5),
            plot.subtitle = element_text(size=24, family="mont", face="italic", lineheight = .5),
            plot.caption = element_text(size=20, family="mont", face="italic", lineheight = .5))
  }) ## end map output
  
  # create map
  output$hist <- renderPlot({
    
  })
}

# run app
shinyApp(ui = ui, server = server)
