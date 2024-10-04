#### FAMILY FARMS STATE ####

#dependencies
library(shiny)
library(tidyverse)
library(scico)
library(showtext)
library(sf)
library(ggthemes)

# load data
LH_state_file <- "C:/Users/zachp/OneDrive/Documents/GitHub/Portfolio/US State Family Farm Products/LH_state.shp"
LH_state <- st_read(LH_state_file)

# graphical setup
## colors
p_palettes <- scico_palette_names()
## fonts
font_add_google("Open Sans", family = "open")
font_add_google("Montserrat", family = "mont")
showtext_auto()

# go ahead and make objects for cropnames and seasons
seasons <- c("Spring", "Summer", "Fall", "Winter")
## drop all crops that have 0 farms producing them anywhere
LH_crops <- st_drop_geometry(LH_state[,13:221])
cnames <- colnames(LH_crops)
cropnames <- as.data.frame(cnames)
cropnames$csums <- sapply(LH_crops, sum)
crops_cut <- cropnames %>% filter(csums != 0)
cutprods <- crops_cut$cnames

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
      selectInput("season", "Select Season",
                  choices = seasons, selected = "Fall")
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
    ggplot(LH_season, aes_string(fill=Rprod()))+
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
  
}

# run app
shinyApp(ui = ui, server = server)
