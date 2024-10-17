#### FAMILY FARMS STATE ####

#dependencies
library(shiny)
library(tidyverse)
library(scico)
library(showtext)
library(tigris)
library(sf)
library(ggthemes)
library(ggspatial)
library(bslib)
library(DT)

# load data
## deployment file
LH_state_file <- "lh_state_cleaned.csv"
## read data
LH_state <- read.csv(LH_state_file)

# get state information from tigris
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
## order by state name
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

# move labels for easier labeling later
move_labels <- c("Connecticut", "Delaware", "District of Columbia",
                 "Maryland", "Massachusetts", "New Hampshire", "New Jersey",
                 "Rhode Island", "Vermont")

move_states <- us_st_prod %>%
  filter(NAME %in% move_labels) %>%
  arrange(lat) %>%
  mutate(xend = -65,
         yend = seq(min(lat)-5, max(lat)+5, length.out = n()))

# UI
ui <- navbarPage("US State Family Farm Products",
  theme = bs_theme(bootswatch = "sandstone"),
  ### sidebar with select for product and palette
  nav_panel("Map",
    layout_sidebar(
    sidebar = sidebar(
      selectInput("product","Select Farm Product",
                  choices = crops, selected = "corn"),
      selectInput("palette","Select Color Palette",
                  choices = p_palettes, selected = "oslo"),
      checkboxInput("fwrap", "Individual Maps for Each Season", value = FALSE),
      conditionalPanel(condition = "input.fwrap == FALSE",
                       selectInput("season", "Select Season",
                                   choices = seasons, selected = "Fall")),
      checkboxInput("labels", "Map Labels", value =FALSE),
                  ), #-- end sidebar
    card(card_header("Number of farms that produce the selected product by state",
                     class = "bg-primary"),
         layout_columns(
         plotOutput("map"),
         card(
           value_box("Product", theme = "primary",
                     value = textOutput("vb_prod")),
           value_box("Season", theme = "secondary",
                     value = textOutput("vb_seas")),
           value_box("Top Producer", theme = "light",
                     value = textOutput("vb_tp")),
         ), col_widths = c(9, 3)
         )
         ),
    card(card_body("For family farms in the 48 continental United States.
                     Data from localharvest (2024) and tigris (2022)")
    )
    ),
    card("Reactive Bar Plots",
      layout_sidebar(sidebar = sidebar(
        sliderInput("nstates", "Number of States on Bar Plot",
                    min = 1, max = 50, value = 25)
      ),
      card(height = 600,
        layout_column_wrap(
        plotOutput("bar_top"),
        plotOutput("bar_bot")
          ))
      )
    )
  ),# end map panel
  nav_panel("Data",
            card("Total Data",
              DTOutput("DT")
            ),
            card("Reactive Data Table",
                 DTOutput("DT_topprod")
            )
  ),
  nav_spacer(),
  nav_item(tags$a("Github", href = "https://github.com/zachpeagler"))
) #-- end UI

# server
server <- function(input, output) {

  # reactive variables
  Rseason <- reactive({input$season})
  Rprod <- reactive({input$product})
  Rpalette <- reactive({input$palette})
  Rfwrap <- reactive({input$fwrap})
  Rlab <- reactive({input$labels})
  Rtopn <- reactive({input$nstates})
  Rtopprod <- reactive({
    # df to hold top producers
      us_st_prod %>%
      arrange(desc(.data[[input$product]])) %>%
      filter(Season == Rseason()) %>%
      head(input$nstates) })
  Rtopperprod <- reactive({
    # df to hold top percent states
    us_st_prod %>%
      filter(Season == Rseason()) %>%
      mutate(PCT = (.data[[input$product]]/Farms) * 100) %>%
      arrange(PCT) %>%
      head(input$nstates) })
  # create map
  output$map <- renderPlot({
    # filter the data to only be the selected season
    us_st_season <- us_st_prod %>% filter(Season == Rseason())
    move_states_season <- move_states %>% filter(Season == Rseason())

    if (Rfwrap() == FALSE) {
      # generate plot
      p <- ggplot(us_st_season, aes(fill=.data[[Rprod()]]))+
        geom_sf(color="black")+
        scale_fill_scico(begin=1, end=0, palette = gettext(Rpalette()))+
        guides(fill = guide_colorbar(title = paste(Rprod(), "producing farms", sep=" ")))+
        coord_sf(clip = "off") +
        annotation_scale(location="bl", text_cex = 1, text_family = "mont")+
        annotation_north_arrow(location="br", height=unit(2, "cm"), width=unit(2, "cm"))+
        theme_map()+
        theme(
          text = element_text(size=16, family="mont"),
          legend.position = "bottom",
          legend.title.position = "top",
          legend.direction = "horizontal",
          legend.key.size = unit(1, "cm")
          )
 # end theme
      if (Rlab() == TRUE) {
        p <- p + geom_label(data = filter(us_st_season, !NAME %in% move_labels),
                    aes(x = lon, y = lat, label = .data[[Rprod()]]), fill = "grey90",
                    family = "mont", size = 6) +
           geom_label(data = move_states_season,
                      aes(x = xend, y = yend,
                          label = paste(STUSPS, .data[[Rprod()]], sep=": ")), fill = "grey90",
                          family = "mont", size = 6, hjust = 0) +
           geom_segment(data = move_states_season,
                        aes(lon, lat, xend = xend, yend = yend),
                        colour = "grey60", fill=NA, linewidth = 0.3)+
          xlim(-125, -60)
      } # end label if statement
    } # end fwrap if statement
    else {
      p <- ggplot(us_st_prod, aes(fill=.data[[Rprod()]]))+
        geom_sf(color="black")+
        scale_fill_scico(begin=1, end=0, palette = gettext(Rpalette()))+
        facet_wrap(~Season)+
        guides(fill = guide_colorbar(title = paste(Rprod(), "producing farms", sep=" ")))+
        labs(title=paste("Number of farms that produce", Rprod(), "by state", sep =" "),
             subtitle="For family farms in the 48 continental United States",
             caption="Data from localharvest (2024) and tigris (2022)")+
        coord_sf(clip = "off") +
        annotation_scale(location="bl", text_cex = 2, text_family = "mont")+
        annotation_north_arrow(location="br", height=unit(0.5, "cm"), width=unit(0.5, "cm"))+
        theme_map()+
        theme(
          legend.position = "bottom",
          legend.title.position = "top",
          legend.direction = "horizontal",
          text = element_text(size=16, family="mont"),
          strip.background = element_rect(fill=NA, color=NA),
          legend.key.size = unit(1, "cm")
        )
      if (Rlab() == TRUE) {
          p <- p + geom_label(data = filter(us_st_prod, !NAME %in% move_labels),
                            aes(x = lon, y = lat, label = .data[[Rprod()]]), fill = "grey90",
                            family = "mont", size = 6) +
                  geom_label(data = move_states,
                              aes(x = xend, y = yend,
                                  label = paste(STUSPS, .data[[Rprod()]], sep=": ")), fill = "grey90",
                                  family = "mont", size = 6, hjust = 0) +
                  geom_segment(data = move_states,
                                aes(lon, lat, xend = xend, yend = yend),
                                colour = "grey60", fill=NA, linewidth = 0.3)+
                  xlim(-125, -60)+
            theme(text = element_text(size=16, family="mont")
)
      } # end label if
    } # end frwap if
    print(p)
  }) ## end map output renderPlot

  # barplot
  output$bar_top <- renderPlot({
    # plot
    ggplot(Rtopprod(), aes(x= reorder(NAME, -.data[[Rprod()]]), y=.data[[Rprod()]], fill =.data[[Rprod()]]))+
      geom_col()+
      ylab(paste(Rprod(), "producing farm count", sep = " "))+
      xlab("State")+
      labs(title=paste("Number of family farms that produce", Rprod(), "by state", sep =" "),
           subtitle=paste("For the", Rtopn(), "continental states with the most", Rprod(), "producing family farms", sep=" "))+
      scale_fill_scico(begin=.8, end=0, palette = gettext(Rpalette()))+
      theme_minimal()+
      theme(
        legend.position = "none",
        text = element_text(size=12, family="mont"),
        title = element_text(size=16, family="open", face="bold", lineheight = .5),
        plot.subtitle = element_text(size=14, family="mont", face="italic", lineheight = .5),
        axis.text.x = element_text(size=12, family= "mont", angle=45, hjust=1, vjust=1.1)
      )
  }) # end barplot
  # barplot2
  output$bar_bot <- renderPlot({
    # plot
    ggplot(Rtopperprod(), aes(x= reorder(NAME, -PCT), y=PCT, fill =PCT))+
      geom_col()+
      ylab(paste(Rprod(), "producing farm percent", sep = " "))+
      xlab("State")+
      labs(title=paste("Percent of family farms that produce", Rprod(), "by state", sep =" "),
           subtitle=paste("For the", Rtopn(), "continental states with the highest percent of", Rprod(), "producing family farms", sep=" "))+
      scale_fill_scico(begin=.8, end=0, palette = gettext(Rpalette()))+
      theme_minimal()+
      theme(
        legend.position = "none",
        text = element_text(size=12, family="mont"),
        title = element_text(size=16, family="open", face="bold", lineheight = .5),
        plot.subtitle = element_text(size=14, family="mont", face="italic", lineheight = .5),
        axis.text.x = element_text(size=12, family= "mont", angle=45, hjust=1, vjust=1.1)
      )
  }) # end barplot2
  output$DT_topprod <- renderDT({
    Rtopprod()
  })

  output$DT <- renderDT({
    us_st_prod
  })
## get product name
  output$vb_prod <- renderText({
    Rprod()
  })
## get season
  output$vb_seas <- renderText({
    Rseason()
  })
## get the top producing state name.
## in the event of a tie, get both.
  output$vb_tp <- renderText({
    st_drop_geometry(Rtopprod())[1,1]
  })
} # end server function

# run app
shinyApp(ui = ui, server = server)
