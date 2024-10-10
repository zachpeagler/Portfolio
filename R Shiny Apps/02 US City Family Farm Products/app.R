#### FAMILY FARMS CITY ####

#dependencies
library(shiny)
library(tidyverse)
library(scico)
library(showtext)
library(tigris)
library(sf)
library(ggthemes)
library(bslib)
library(leaflet)

# load data
## deployment file
LH_farm_file <- "lh_farms_cleaned.csv"
## read data
LH_farms <- read.csv(LH_farm_file)
## get cities from tigris
cities <- places(cb=TRUE)
## add lat and long from the geometry and
## create a matching ID to the one in LH_city
## then filter down to only those city state combos also in LH_city
cities_cut <- cities %>%
  mutate(lon = st_coordinates(st_centroid(.))[,1],
         lat = st_coordinates(st_centroid(.))[,2],
         ID = paste(NAME, STATE_NAME, sep=", ")) %>%
  filter(ID %in% LH_city$ID)
## remove duplicates
cities_cut <- cities_cut[!duplicated(cities_cut$ID),]
# rotate a cube in your head for awhile
## filter LH_city only to those in cities
LH_city_cut <- LH_city %>%
  filter(ID %in% cities_cut$ID)
## merge city geometry info with production info
city_prod <- merge(LH_city_cut, cities_cut, by.x="ID", by.y="ID")

# graphical setup
## colors
p_palettes <- scico_palette_names()
## fonts
font_add_google("Open Sans", family = "open")
font_add_google("Montserrat", family = "mont")
showtext_auto()

# make some objects
## season object
seasons <- c("Spring", "Summer", "Fall", "Winter")
## products object
products <- names(st_drop_geometry(city_prod[,35:195]))
## make non-continental states object
non_cont_states <- c("American Samoa", "Guam", "Commonwealth of the Northern Mariana Islands",
                     "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands",
                     "Puerto Rico", "Hawaii", "Alaska")

# UI
ui <- fluidPage(
  
  tags$head(
            includeCSS("styles.css"),
           ),
  ### application title
  titlePanel("US City Family Farm Products"),
  ### create leaflet map
  leafletOutput("map", width="100%", height-"100%"),
  ### absolute panel with controls and a barplot
  absolutePanel(id="controls", class="panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width= 330, height="auto",
                
                h2("Control Panel"),
                
      selectInput("product","Select Farm Product",
                  choices = products, selected = "corn"),
      selectInput("palette","Select Color Palette",
                  choices = p_palettes, selected = "bilbao"),
      selectInput("season", "Select Season",
                  choices = seasons, selected = "Fall"),
      sliderInput("ncities", "Number of Cities on Bar Plot", min = 1, max = 100, value = 10),
      plotOutput("bar")
  ) # end absolutePanel
) #-- end fluidpage

# server
server <- function(input, output) {
  
  # reactive variables
  Rseason <- reactive({input$season})
  Rprod <- reactive({input$product})
  Rpalette <- reactive({input$palette})
  Rtopn <- reactive({input$ncities})
  Rtopprod <- reactive({
    # df to hold top producers
      crop_prod %>%
      arrange(desc(.data[[input$product]])) %>%
      filter(Season == Rseason()) %>%
      head(input$nstates) })
  
  # create map
  output$map <- renderPlot({
    # filter the data to only be the selected season
    crop_prod_season <- crop_prod %>% filter(Season == Rseason())
    # plot
    ggplot()+
      geom_sf(data=usa48, color="black", fill=NA)+
      geom_point(data=ic_city[ic_city$apples>0 & ic_city$Season == "Fall",]%>%arrange(apples), aes(x=lon, y=lat, size=apples, color=apples), alpha=0.8)+
      labs(title=str_wrap("Number of Apple-Producing Farms Per City", 40),
           subtitle="For family farms in the 48 contiguous US states",
           caption="Data from localharvest.org (2024) and census.org (2022)")+ 
      scale_size_continuous(range=c(1,5))+
      scale_color_scico(begin=.9, end=0, palette=a_palette)+
      guides(size=FALSE, color = guide_colorbar(title="Apple-Producing Farms"))+
      theme_map()+
      annotation_scale(location="bl", text_cex = 2, text_family = "mont")+
      annotation_north_arrow(location="br", height=unit(0.5, "cm"), width=unit(0.5, "cm"))+
      theme(
        legend.position="inside",
        legend.direction = "horizontal",
        legend.title.position = "top",
        legend.position.inside = c(0.01,.06),
        legend.background = element_rect(fill=NA),
        text = element_text(size=24, family="mont"),
        legend.title = element_text(size=20, family="open", face="bold", lineheight = .5),
        title = element_text(size=30, family="open", face="bold", lineheight = .5),
        plot.subtitle = element_text(size=26, family="mont", face="italic", lineheight = .5),
        plot.caption = element_text(size=24, family="mont", face="italic", lineheight = .5))
  }) ## end map output renderPlot
  
  # barplot
  output$bar <- renderPlot({
    # plot  
    ggplot(Rtopprod(), aes(x= reorder(NAME, -.data[[Rprod()]]), y=.data[[Rprod()]], fill =.data[[Rprod()]]))+
      geom_col()+
      ylab("Crop Producing Farms")+
      xlab("State")+
      labs(title=paste("Number of family farms that produce", Rprod(), "by state", sep =" "),
           subtitle=paste("For the", Rtopn(), "continental states with the most", Rprod(), "producing family farms", sep=" "))+
      scale_fill_scico(begin=.8, end=0, palette = gettext(Rpalette()))+
      theme_minimal()+
      theme(
      legend.position = "none",
      text = element_text(size=24, family="mont"),
      title = element_text(size=30, family="open", face="bold", lineheight = .5),
      plot.subtitle = element_text(size=24, family="mont", face="italic", lineheight = .5),
      axis.text.x = element_text(size=20, family= "mont", angle=45, hjust=1, vjust=1.1)
      )
  }) # end barplot
  
} # end server function

# run app
shinyApp(ui = ui, server = server)
