# App A Day | Day One: Meteorites
Long script

## Introduction Scene

>*Fade in to shot of me at desk*

Hello! My name is Zach Peagler and I'll be making an app a day until I get a job as a data scientist or R/R Shiny developer. 
> calender, data, r logo

Today's day one, so I'm starting off strong with an R Shiny app that shows meteorite landing data taken from data.gov.
> the number one, data.gov

It features sidebar to control everything, a map using leaflet, some histogram, and a data viewer.
> preview the app

We'll be using the R packages shiny, tidyverse, bslib, scico, plotly, and DT, all of which will be linked in the description.
>show package logos^

## Start:

We'll go to our browser, download the meteorite landing data from data.gov, and put it in a file of your choice. 

Open up Rstudio and create a new R shiny app.

### Dependencies
#### Load the following dependencies.
- **shiny** for shiny functionality
- **tidyverse** for data wrangling and ggplot
- **leaflet** for mapping
- **bslib** for theming <br>
- **scico** for ggplot colors <br>
- **plotly** for interactive plots <br>
- **DT** for rendering datatables

```{r dependencies}
# Dependencies
library(shiny)
library(tidyverse)
library(leaflet)
library(bslib)
library(scico)
library(plotly)
library(DT)
```

### Data

#### Load the data
```{r}
meteorite_file <- "Meteorite_Landings.csv"
```
If testing locally, it can also help to use the actual path of the file so you can load the data into R, but when *deploying* this is how shiny wants the file reference formatted.

#### An example temp local file location
```{r}
meteorite_file <- "C:/Github/App-A-Day/01_meteorites/Meteorite_Landings.csv"
```

#### Clean the data
Rename mass, lat, and lon for clarity, then change the type of nametype, fall, and id to factors. Finally, filter so the mass is greater than 0 and omit undefineds.
```{r}
# Rename variables for clarity and omit NAs
met_dat <- read.csv(meteorite_file) %>%
  rename(mass = mass..g.,
         lat = reclat,
         lon = reclong) %>%
  mutate(nametype = as.factor(nametype),
         fall = as.factor(fall),
         id = as.factor(id)) %>%
  filter(mass > 0) %>%
  na.omit()
```

Wow! After getting the data in and cleaning it, we can see we've got the variables name, id, recclass, mass (g), recovery class, fall, year, reclat, reclong, and GeoLocation.
| Name | Type | Possible Values | Details |
| ---- | ---- | ---------------| ------- |
| name | chr | many(~38000) | name of meteorite |
| id | factor | many (~38000) | meteorite unique ID |
| mass (g) | numeric | ~0:~60000000 | meteorite mass in grams|
| recclass | factor | many (460) | meteorite class|
| fall | factor | fell: found | was meteorite found or did it fall |
| year | integer | ~800:2024 | year meteorite was recovered|
| lon | numeric | -100:100 | meteorite longitude |
| lat | numeric | -100:100 | meteorite latitude |


### Plan the analysis
Now that we can look at our data, we can start thinking about what variables we need for the analysis we want to do. 
#### **Goal 1:** Draw each meteorite landing as a circle on the map and have a user input for the size and color of the variable.
 1. Decide on variables to use for color and size. From the data, mass and year will probably be good, as they're numeric and we will be able to translate them easily to our leaflet function.

#### **Goal 2:** Be able to filter the data on the map by meteorite class and recovery date.
1. There are a lot of meteorite classes, so making this a list by hand won't be an option.
2. Recovery date we can add a min date and max date outputs to the global sidebar.

#### **Goal 3:** Be able to exclude the extremes and outliers from the data.
1. This is a pretty simple matter of calculating the interquartile range and quartiles of our selected variable, mass.

#### **Goal 4: Show a popup when you click on a circle on the map that contains detailed meteorite information.
1. We can make an observer to handle this and add a popup to leaflet.

#### **Goal 5: Draw histograms of interesting continuous variables from the data (mass, year, lat, and lon).
1. It'll be interesting to the the distribution of various variables. Let's draw them as histograms!

#### **Goal 6:** Add a data viewer.
1. A data viewer will be useful to inspect the data more closely, and can be easily implemented with the DT package.

### Variables
Make an object for meteor variables, another object to contain meteor classes, making sure there are no duplicates and it has "None" as an option.
Then get the 25% and 75% quantiles for mass, as well as the interquartile range.
Finally, make an object of all the palette names from scico.
```{r}
# Make variable object
met_vars <- c(
  "Mass" = "mass",
  "Year" = "year"
)
# Get each unique meteorite class
met_classes <- met_dat[!duplicated(met_dat$recclass),]$recclass
met_classes <- met_classes[order(met_classes)]
met_classes <- c(met_classes, "None")
met_classes <- as.factor(met_classes)
# Get outliers for mass
Qmass <- quantile(met_dat$mass, probs=c(.25, .75))
iqr_mass <- IQR(met_dat$mass)
# Make color palettes object
p_pals <- scico_palette_names()
```

### UI

We'll start the UI off with a navbarPage we title "Meteorite Landings"
```{r}
ui <- navbarPage("Meteorite Landings",
```

Then we'll add a theme. There are lots of possible themes. Bslib can use custom themes or existing [bootswatch](https://bootswatch.com/) themes.
```{r}
  theme = bs_theme(bootswatch = "sandstone"),
```

Then we'll add a sidebar. I'm electing to use a single global sidebar nested directly inside of the page rather than individual sidebars for each page.

```{r}
  sidebar = sidebar(
    markdown("##### **Global Settings**"),
    selectInput("palette", "Select Color Palette", p_pals, selected = "lipari"),
    checkboxInput("extremes", "Exclude Extremes", TRUE),
    checkboxInput("outliers", "Exclude Outliers", FALSE),
    numericInput("date_min", "Date Range Start", min=0, max=2024, value = 1975),
    numericInput("date_max", "Date Range End", min=0, max=2024, value=2024),
    selectInput("class_filter", "Filter by Meteorite Class",
                met_classes, selected = "None" ),
    markdown("##### **Map Settings**"),
    selectInput("size", "Size", met_vars, selected = "mass"), 
    selectInput("color", "Color", met_vars, selected = "year"),
    markdown("##### **Histogram Settings**"),
    sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 100,
                value = 30)
  ),
```

Then we'll do the nav_panel for the map.

```{r}
        nav_panel("Map",
          card(
            card_body(leafletOutput("map"), height=800)
          ),
          card(markdown("This map is made using R Shiny, leaflet, and meteorite landing data from
                          [Data.gov](https://catalog.data.gov/dataset/meteorite-landings) and shows over 38,000
                          meteorite landing locations."))
          ),
```

Then we'll do a nav_panel for our plots.

```{r}
        nav_panel("Plots",
            card("Histograms",
            layout_column_wrap(
              card(card_header("Year Histogram"),
              plotlyOutput("year_hist")
              ),
              card(card_header("Mass Histogram"),
              plotlyOutput("mass_hist")
              )
              ),
            layout_column_wrap(
              card(card_header("Latitude Histogram"),
                   plotlyOutput("lat_hist")
              ),
              card(card_header("Longitude Histogram"),
                   plotlyOutput("lon_hist")
              )
            )
            )
          ),
```

Then we'll finish our UI off with a nav_panel for the data explorer, a spacer, and then a link to the github repo for this project.
```{r}
        nav_panel("Data Explorer",
                  card(
                    DTOutput("met_DT")
                  )
                  ),
        nav_spacer(),
        nav_item(tags$a("Github", href = "https://github.com/zachpeagler/AppADay/01_meteorites"))
)
```
### Server

#### Reactive functions
```{r}
server <- function(input, output) {
  
  # Reactive function that filters data
  Rbins <- reactive({input$bins})
  Rpalette <- reactive({input$palette})
  
  Rmet_dat <- reactive({
    m_dat <- met_dat %>%
      filter(year > input$date_min,
             year < input$date_max
             )
    if (input$extremes == TRUE) {
      m_dat <- subset(m_dat, mass > (Qmass[1] - 3 * iqr_mass) &
                        mass < (Qmass[2] + 3 * iqr_mass)
      )
    }
    if (input$outliers == TRUE) {
      m_dat <- subset(m_dat, mass > (Qmass[1] - 1.5 * iqr_mass) &
                        mass < (Qmass[2] + 1.5 * iqr_mass)
                      )
    }
    if (input$class_filter != "None") {
      m_dat <- subset(m_dat, recclass == input$class_filter)
    }
    return(m_dat)
  })
```

#### Initialize map
```{r}
  # Map output
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93, lat=38, zoom = 3)
  })
```

#### Reactive expression to subset data by map bounds
```{r}
  # reactive expression to see whats inbounds
  recInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(met_dat[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lonRng <- range(bounds$east, bounds$west)
    
    subset(met_dat,
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lonRng[1] & lat <= lonRng[2])
  })
```

#### Observer to watch for changes in color and size

```{r}
  observe({
    colorBy <- input$color
    sizeBy <- input$size
      colordat <- Rmet_dat()[[colorBy]]
      pal <- colorBin(scico(length(colordat), begin = 0, end = 0.8,
                               palette = Rpalette(), categorical = FALSE),
                         colordat)
        radius <- Rmet_dat()[[sizeBy]] / max(Rmet_dat()[[sizeBy]]) * 100000
    
    leafletProxy("map", data = Rmet_dat()) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(~lon, ~lat, radius = radius, stroke = FALSE,
                 fillOpacity = 0.4, fillColor = pal(colordat),
                 layerId = ~id) %>%
      addLegend("bottomleft", pal=pal, values=colordat, title = colorBy)
  })
```

#### Popup function
```{r}
  showMetPopup <- function(id, lat, lon) {
    selectedMet <- met_dat[met_dat$id == id,]
    content <- paste("Name:", selectedMet$name, "<br/>",
                     "Class:", selectedMet$recclass, "<br/>",
                     "Mass:", selectedMet$mass, "grams", "<br/>",
                     "Year:", selectedMet$year, "<br/>",
                     "Coords:", selectedMet$lat, selectedMet$lon,
                     sep = " ")
    leafletProxy("map") %>% addPopups(lng = lon, lat = lat, popup = content)
  }
```

#### Popup observer
```{r}
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showMetPopup(event$id, event$lat, event$lng)
    })
  })
```

#### Mass Histogram
```{r}
  # Plot outputs
  ## Mass histogram
  output$mass_hist <- renderPlotly({
      options(scipen = 999)
      mass <- Rmet_dat()$mass
      bins <- seq(min(as.integer(mass)), max(as.integer(mass)), length.out = Rbins() + 1)
      `Mass Bins` <- cut(mass, bins)
      m_hist <- ggplot(data = Rmet_dat(), aes(x = mass, fill = `Mass Bins`, color = `Mass Bins`))+
                    geom_histogram(bins = Rbins())+
                    scale_fill_scico_d(begin=0, end=0.8, palette=Rpalette())+
                    scale_color_scico_d(begin=0, end=0.8, palette=Rpalette())+
                    theme_bw()+
                    xlab("mass (grams)")
      m_hist <- ggplotly(m_hist)
      return(m_hist)
  })
```

#### Year Histogram
```{r}
  ## Year histogram
  output$year_hist <- renderPlotly({
    options(scipen = 999)
    year <- Rmet_dat()$year
    bins <- seq(min(year), max(year), length.out = Rbins() + 1)
    `Year Bins` <- cut(year, bins)
    y_hist <- ggplot(data = Rmet_dat(), aes(x = year, fill = `Year Bins`, color = `Year Bins`))+
      geom_histogram(bins = Rbins()+1)+
      scale_fill_scico_d(begin=0, end=0.8, palette=Rpalette())+
      scale_color_scico_d(begin=0, end=.8, palette=Rpalette())+
      theme_bw()
    y_hist <- ggplotly(y_hist)
    return(y_hist)
  })
```

#### Longitude Histogram
```{r}
  ## Longitude histogram
  output$lon_hist <- renderPlotly({
    options(scipen = 999)
    lon <- Rmet_dat()$lon
    bins <- seq(min(lon), max(lon), length.out = Rbins() + 1)
    `Longitude Bins` <- cut(lon, bins)
    lon_hist <- ggplot(data = Rmet_dat(), aes(x = lon, fill = `Longitude Bins`, color = `Longitude Bins`))+
      geom_histogram(bins = Rbins()+1)+
      scale_fill_scico_d(begin=0, end=0.8, palette=Rpalette())+
      scale_color_scico_d(begin=0, end=.8, palette=Rpalette())+
      theme_bw()
    lon_hist <- ggplotly(lon_hist)
    return(lon_hist)
  })
```

#### Latitude Histogram
```{r}
  ## Latitude histogram
  output$lat_hist <- renderPlotly({
    options(scipen = 999)
    lat <- Rmet_dat()$lat
    bins <- seq(min(lat), max(lat), length.out = Rbins() + 1)
    `Latitude Bins` <- cut(lat, bins)
    lat_hist <- ggplot(data = Rmet_dat(), aes(x = lat, fill = `Latitude Bins`, color = `Latitude Bins`))+
      geom_histogram(bins = Rbins()+1)+
      scale_fill_scico_d(begin=0, end=0.8, palette=Rpalette())+
      scale_color_scico_d(begin=0, end=.8, palette=Rpalette())+
      theme_bw()
    lat_hist <- ggplotly(lat_hist)
    return(lat_hist)
  })
```

#### Data Table Output
```{r}
  output$met_DT <- renderDT({
    Rmet_dat()
  })
}

```

### Run and Deploy

Don't forget to add this to run the application!
```{r}
shinyApp(ui = ui, server = server)
```

## End Scene:

Thank you!

