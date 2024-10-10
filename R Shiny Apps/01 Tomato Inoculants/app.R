### TOMATO INOCULANTS ###
##### Setup #####
#dependencies
library(shiny)
library(tidyverse)
library(scico)
library(showtext)
library(bslib)
library(MASS)
library(ggpubr)
library(DT)
library(GWalkR)
library(rstatix)
library(lme4)
library(MuMIn)

# graphical setup
## palettes
p_palettes <- scico_palette_names()
bs_themes <- bootswatch_themes()
sls8 <- c("#0d2b45", "#203c56", "#544e68", "#8d697a",
          "#d08159", "#ffaa5e", "#ffd4a3", "#ffecd6")
## fonts
font_add_google("Open Sans", family = "open")
font_add_google("Montserrat", family = "mont")
showtext_auto()
## shapes
four_shapes = c(15,16,17,23)
# load data 
## li600 data
Li_data_file <- "TIP24_LI600.csv"
tfile <- "C:/Github/Portfolio/R Shiny Apps/01 Tomato Inoculants/TIP24_LI600.csv"
Li_data <- read.csv(tfile, stringsAsFactors = T) %>%
  mutate(Treatment = case_when(
    Row==1~"Control",
    Row==2~"Transplantation",
    Row==3~"Germination",
    Row==4~"Germ+Trans",
    TRUE~NA)) %>%
  # filter to leak% < 10 and gsw > 0
  filter(leak_pct<10, gsw > 0) %>%
  mutate(Date = parse_date_time(Date, orders = "mdy"),
         plant_fac = as.factor(paste(Row, Pot)),
         Treatment = factor(Treatment, levels=c("Control", "Germination", 
                                                "Transplantation", "Germ+Trans"))
        )
Li_data <- Li_data[,c("Treatment", "Time", "Date", "Row", "Pot", "plant_fac", "gsw", "VPDleaf", "PhiPS2", "rh_s", "Tleaf", "Qamb")]
## Get gsw and PhiPS2 quantiles and interquartile range (IQR)
Qgsw <- quantile(Li_data$gsw, probs=c(.25, .75), na.rm=FALSE)
Qps2 <- quantile(Li_data$PhiPS2, probs=c(.25, .75), na.rm=FALSE)
iqr_gsw <- IQR(Li_data$gsw)
iqr_ps2 <- IQR(Li_data$PhiPS2)
## fruit data
Fl_data_file <- "TIP24_Fruit.csv"
tfruitfile <- "C:/Github/Portfolio/R Shiny Apps/01 Tomato Inoculants/TIP24_Fruit.csv"
Fl_data <- read.csv(tfruitfile, stringsAsFactors = T) %>%
  mutate(Treatment = case_when(
    row==1~"Control",
    row==2~"Transplantation",
    row==3~"Germination",
    row==4~"Germ+Trans",
    TRUE~NA),
    fruit = 1,
    date_analysis = parse_date_time(date_analysis, orders = "mdy"),
    date_harvest = parse_date_time(date_harvest, orders="mdy"),
    d_harvest = format(date_harvest, format="%d"),
    d_analysis = format(date_analysis, format="%d"),
    d_diff = abs(as.integer(d_analysis)-as.integer(d_harvest)),
    plant_fac = as.factor(paste(row, plant)),
    Treatment = factor(Treatment, levels=c("Control","Germination", 
                                           "Transplantation",
                                           "Germ+Trans")),
    BER = as.factor(BER),
    fungus = as.factor(fungus),
    cracking = as.factor(cracking),
    ripeness = abs(1-(penetrometer/max(na.omit(penetrometer))))
  )
## Get mass and sugar quantiles and IQR
Qmass <- quantile(na.omit(Fl_data$mass), probs=c(.25, .75), na.rm=FALSE)
Qsug <- quantile(na.omit(Fl_data$sugar_avg), probs=c(.25, .75), na.rm=FALSE)
iqr_mass <- IQR(na.omit(Fl_data$mass))
iqr_sug <- IQR(na.omit(Fl_data$sugar_avg))

###### UI #####
ui <- navbarPage(title = "Tomato Inoculants",
                 theme = bs_theme(bg = "white", fg=sls8[1], primary = sls8[5],
                                  secondary = sls8[4], base_font = font_google("Open Sans")),
                 sidebar = sidebar(
                   selectInput("palette","Select Color Palette",
                               choices = p_palettes, selected = "bilbao"),
                   checkboxInput("outliers", "Exclude Outliers", FALSE),
                   checkboxInput("ber", "Exclude Blossom End Rot", FALSE)
                 ), # end global sidebar
  nav_panel("Fruit",
    tabsetPanel(
      tabPanel("Exploratory",
        card(card_header("Sugar"),
          card_body(layout_column_wrap(
            card(card_header("Probability Distribution Function Plot"),
              card_body(plotOutput("sug_pdf"))),
            card(card_header("Cumulative Distribution Function Plot"),
              card_body(plotOutput("sug_cdf")))
              ))),
        card(card_header("Mass"),
          card_body(layout_column_wrap(
            card(card_header("Probability Distribution Function Plot"),
              card_body(plotOutput("mass_pdf"))),
            card(card_header("Cumulative Distribution Function Plot"),
              card_body(plotOutput("mass_cdf")))
          )))
      ), # end exploratory tab
      tabPanel("Plots"),
      tabPanel("Statistical Tests"),
      tabPanel("Tableau-Style Explorer",
        card(card_header("Explanation of Fruit Variables"),
             card_body(markdown("The tomatoes were grown in 4 rows of 12 pots each, with each row corresponding to a different inoculation treatment. The data table below is formatted in a tidy format with each row corresponding to one fruit and each column representing a variable.<br>
                                **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
                                **row** is the row number of the tomato. (1:4) <br>
                                **plant** is the pot number of the tomato. (1:12) <br>
                                **plant_fac** is a combination of *row* and *plant*, and acts as an ID for every individual plant. (1 1: 4 12) <br>
                                **mass** is the mass in grams of the tomato, measured on an *SCALE NAME*. (~50:~400) <br>
                                **BER** corresponds to whether or not the tomato has blossom end rot, a disease caused by calcium deficiency that renders the fruit unmarketable. (0,1) <br>
                                **fungus** corresponds to whether or not the tomato has fungus growing on it. This is common on fruit with BER. (0,1) <br>
                                **cracking** corresponds to whether or not the tomato has cracks in its skin (0,1) <br>
                                **penetrometer** corresponds to the force in kilograms it takes to penetrate the flesh of the tomato (~0.5:~4) <br>
                                **ripeness** is the **penetrometer** value mapped from 0:1 and reversed, so that riper fruit are closer to 1 and unripe fruit are closer to 0. (0:1) <br>
                                **sugar.1** and **sugar.2** are measurements of the tomato juice's sugar concentration taken on a Fisher BRIX Refractometer (~2:~12) <br>
                                **sugar_avg** is the average of the two sugar measurements. <br>
                                **date_harvest** is the date the fruit was harvested (August 2024:October 2024) <br>
                                **date_analysis** is the date the fruit was analyzed in the lab (August 2024:October 2024) <br>
                                **d_diff** is the number of days from harvest to analysis. (0:6)"))),
        gwalkrOutput("f_gwalk"),
        card(card_body(markdown("For more information on using GWalkR's Tableau-style visualizations please see the author's [Github repo](https://github.com/Kanaries/GWalkR).")))
      )
    ) # end tab set
  ), # end fruit page
  nav_panel("Fluorescence",
    tabsetPanel(
      tabPanel("Exploratory",
       card(card_header("Stomatal Conductance (gsw)",
                        class = "bg-primary"),
            card_body((layout_column_wrap(
                card(card_header("Probability Distribution Function Plot"),
                     card_body(plotOutput("gsw_pdf"))),
                card(card_header("Cumulative Distribution Function Plot"),
                     card_body(plotOutput("gsw_cdf")))
              ))),
              card(card_header("One-sample Kolmogorov-Smirnov test results"),
                   card_body("Add a blurb here about Kolmogorov-Smirnov tests."),
                   card_body(layout_column_wrap(
                     value_box(
                       theme = value_box_theme(bg = sls8[1]),
                       title = "Normal p-value",
                       value = textOutput("gsw_nks"),
                       p("P-value < 0.05, indicating the data is significantly different.")
                     ),
                     value_box(
                       theme = value_box_theme(bg = sls8[2]),
                       title = "Log-normal p-value",
                       value = textOutput("gsw_lnks"),
                       p("P-value < 0.05, indicating the data is significantly different.")
                     ),
                     value_box(
                       theme = value_box_theme(bg = sls8[3]),
                       title = "Gamma p-value",
                       value = textOutput("gsw_gks"),
                       p("P-value < 0.05, indicating the data is significantly different.")
                     )
                   ))),
              card(card_body("Based on the above analysis, we can proceed under the assumption
                             that stomatal conductance follows either log normal or 
                             gamma distribution. However, looking at the shape of gsw (next tab over ->)
                             makes it clear that this variable is likely overdispersed."))
              ),
       card(card_header("Efficiency of Photosystem II (PhiPS2)",
                        class = "bg-secondary"),
            card_body(layout_column_wrap(
              card(card_header("Probability Distribution Function Plot"),
                   card_body(plotOutput("phi_pdf"))),
              card(card_header("Cumulative Distribution Function Plot"),
                   card_body(plotOutput("phi_cdf")))
                    ),
              card(card_header("One-sample Kolmogorov-Smirnov test results"),
                   card_body(layout_column_wrap(
                     value_box(
                       theme = value_box_theme(bg = sls8[5]),
                       title = "Normal p-value",
                       value = textOutput("phi_nks"),
                       p("P-value < 0.05, indicating the data is significantly different.")
                     ),
                     value_box(
                       theme = value_box_theme(bg = sls8[6]),
                       title = "Log-normal p-value",
                       value = textOutput("phi_lnks"),
                       p("P-value < 0.05, indicating the data is significantly different.")
                     ),
                     value_box(
                       theme = value_box_theme(bg = sls8[7]),
                       title = "Gamma p-value",
                       value = textOutput("phi_gks"),
                       p("P-value < 0.05, indicating the data is significantly different.")
                     )
                    )),
                   card_body("Based on the above, it appears that PhiPS2 is significantly different from
                             normal, log-normal, and gamma distributions. I'm at a loss as to 
                             how it is distributed."))
          ))
      ), # end exploratory tab
      tabPanel("Plots",
               card(card_header("Interactive Scatter Plot",
                                class = "bg-primary"),
                    card_body(plotOutput("fluoro_scatter"))
               ),
               card(card_header("Stomatal Conductance (gsw)",
                                class = "bg-primary"),
                    card_body((layout_column_wrap(
                      card(card_header("Stomatal Conductance by Date Across Treatments"),
                           card_body(plotOutput("gsw_date"))),
                      card(card_header("Stomatal Conductance Boxplot"),
                           card_body(plotOutput("gsw_box")))
                    )))
               ), # end gsw
               card(card_header("Efficiency of Photosystem II (PhiPS2)",
                                class = "bg-secondary"),
                    card_body((layout_column_wrap(
                      card(card_header("Plot"),
                           card_body(plotOutput(""))),
                      card(card_header("Plot"),
                           card_body(plotOutput("")))
                    )))
               ) # end phips2
               ),
      tabPanel("Statistical Tests",
               card(card_header("Stomatal Conductance (gsw)"),
                    card_body("Based on the exploratory analysis, either a gamma or
                              log-normal distribution are appropriate for gsw. To determine the
                              effect Treatment has on gsw, we'll use a generalized linear model with
                              gsw as our response variable, Treatment as a predictor, relative humidity
                              as a fixed effect, and plant as a random effect, with a gamma link function.
                              I'm unsure of whether to use a log or gamma family link function, so I'll
                              do both and compare them."),
                    card_body(layout_column_wrap(
                      card(card_header("GLM with Gamma family and log link"),
                         card_body(verbatimTextOutput("gsw_glm_gamma"), max_height= 500),
                         card_body(layout_column_wrap(
                           card(card_header("Pseudo-R2"),
                                card_body(verbatimTextOutput("gsw_glm_gamma_r2"))),
                           value_box(title = "AIC", value = textOutput("gsw_glm_gamma_AIC"))
                          ))
                        ),
                      card(card_header("GLM with gaussian family and log link"),
                           card_body(verbatimTextOutput("gsw_glm_log"), max_height= 500),
                           card_body(layout_column_wrap(
                             card(card_header("Pseudo-R2"),
                                  card_body(verbatimTextOutput("gsw_glm_log_r2"))),
                             value_box(title = "AIC", value = textOutput("gsw_glm_log_AIC"))
                           ))
                          )
                      ))) # end gsw card
               ), # end statistical tests panel
      tabPanel("Tableau-Style Explorer",
               card(card_header("Explanation of Fluorescence Variables"),
                    card_body(markdown("Fluorescence measurements were taken biweekly with a LI-COR LI-600 over the course of the trial. Data is presented in a tidy format with each row representing a single observation and each column representing a variable. <br>
                                **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
                                **Time** is the time at which the measurement was taken. <br>
                                **Date** is the date at which the measurement was taken. <br>
                                **Row** is the row number of the tomato. (1:4) <br>
                                **Pot** is the pot number of the tomato. (1:12) <br>
                                **plant_fac** is a combination of *Row* and *Pot*, and acts as an ID for every individual plant. (1 1: 4 12) <br>
                                **gsw** is the stomatal conductance (mol m-2 s-1) of the leaf. <br>
                                **VPDleaf** is the vapor pressure deficit (add units) of the leaf. <br>
                                **PhiPS2** is the efficiency of photosystem II of the leaf. It is unitless. (0:1) <br>
                                **rh_s** is the relative humidity (add units) of the leaf. <br>
                                **Tleaf** is the temperature (C) of the leaf. <br>
                                **Qamb** is the ambient light level (add units) at the time of the observation.
                                ")),
                    gwalkrOutput("li_gwalk"),
                    card_body(markdown("For more information on using GWalkR's Tableau-style visualizations please see the author's [Github repo](https://github.com/Kanaries/GWalkR)."))
                    )
              )
    ) # end tab set
  ), # end fluorescence page
  nav_spacer(),
  nav_item(tags$a("Github", href = "https://github.com/zachpeagler/Portfolio/tree/main/R%20Shiny%20Apps/01%20Tomato%20Inoculants"))
) # end ui

##### server #####
server <- function(input, output) {
## Reactive expressions
  Rpalette <- reactive({input$palette})
### Reactive dataframes
  RLi_data <- reactive({
    x <- Li_data
    if (input$outliers == TRUE) {
      x <- subset(x, gsw > (Qgsw[1]-1.5*iqr_gsw) &
                gsw < (Qgsw[2]+1.5*iqr_gsw) &
                PhiPS2 > (Qps2[1]-1.5*iqr_ps2) &
                PhiPS2 < (Qps2[2]+1.5*iqr_ps2))
    }
    return(x)
  })
  RFl_data <- reactive ({
    f <- Fl_data
    if (input$outliers == TRUE) {
      f <- subset(f, mass > (Qmass[1]-1.5*iqr_mass) &
             mass < (Qmass[2]+1.5*iqr_mass) &
             sugar_avg > (Qsug[1]-1.5*iqr_sug) &
             sugar_avg < (Qsug[2]+1.5*iqr_sug))
    }
    if (input$ber == TRUE){
      f <- subset(f, BER == 0)
    }
    return(f)
  })
### Reactive GLMs 
  Rgsw_glm_gamma <- reactive({
    glmer(gsw ~ Treatment + rh_s + (1 | plant_fac),
          data = RLi_data(), family = Gamma(link = "log"))
  })
  Rgsw_glm_log <- reactive({
    glmer(gsw ~ Treatment + rh_s + (1 | plant_fac),
          data = RLi_data(), family = gaussian(link = "log"))
  })
## Fruit
### Exploratory
#### sugar
#### CDF

  
## Fluorescence
### Exploratory
#### GSW
  ## gsw distributions
  gsw_n <- fitdistr(Li_data$gsw, "normal")
  gsw_ln <- fitdistr(Li_data$gsw, "lognormal")
  gsw_g <- fitdistr(Li_data$gsw, "gamma")
  gsw_seq <- seq(min(Li_data$gsw), max(Li_data$gsw), by=0.001)
  
  ### test gsw for gamma and plnorm distributions using ks test
  gsw_gks <- round(list2DF(ks.test(Li_data$gsw, "pgamma",shape=gsw_g$estimate[1],
          rate=gsw_g$estimate[2])[2]), 3)[1,1]
  gsw_lnks <- round(list2DF(ks.test(Li_data$gsw, "plnorm",meanlog=gsw_ln$estimate[1],
          sdlog=gsw_ln$estimate[2])[2]), 3)[1,1]
  gsw_nks <- round(list2DF(ks.test(Li_data$gsw, "pnorm",mean=gsw_n$estimate[1],
          sd=gsw_n$estimate[2])[2]), 3)[1,1]
  output$gsw_gks <- renderText({
    gsw_gks
  })
  output$gsw_lnks <- renderText({
    gsw_lnks
  })
  output$gsw_nks <- renderText({
    gsw_nks
  })
  
  ## PDFs
  gsw_pdf_n <- dnorm(gsw_seq, mean=gsw_n$estimate[1],
                     sd=gsw_n$estimate[2])
  gsw_pdf_ln <- dlnorm(gsw_seq, meanlog=gsw_ln$estimate[1],
                       sdlog=gsw_ln$estimate[2])
  gsw_pdf_g <- dgamma(gsw_seq, shape=gsw_g$estimate[1],
                      rate=gsw_g$estimate[2])
  gsw_pdf <- density(Li_data$gsw, n=length(gsw_pdf_ln))
  gsw_pdf_df <- as.data.frame(gsw_seq) %>%
    mutate(pdf_ln = gsw_pdf_ln,
           pdf_n = gsw_pdf_n,
           pdf_g = gsw_pdf_g)
## GSW PDF plot
  output$gsw_pdf <- renderPlot({
    ggplot(gsw_pdf_df)+
      geom_point(aes(x=gsw_seq, y=pdf_n, color="Normal"))+
      geom_point(aes(x=gsw_seq, y=pdf_ln, color="Lognormal"))+
      geom_point(aes(x=gsw_seq, y=pdf_g, color="Gamma"))+
      geom_point(aes(x=gsw_pdf$x, y=gsw_pdf$y, color="Data"))+
      theme_bw()+
      scale_color_scico_d(begin=.8, end=0, palette = gettext(Rpalette()))+
      ylab("PDF")+
      xlab("gsw")+
      theme(
        text = element_text(size=24, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.85,.6),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
  }) # end GSW PDF output
  ## CDFs
  gsw_cdf_n <- pnorm(gsw_seq, mean=gsw_n$estimate[1],
                     sd=gsw_n$estimate[2])
  gsw_cdf_ln <- plnorm(gsw_seq, meanlog=gsw_ln$estimate[1],
                       sdlog=gsw_ln$estimate[2])
  gsw_cdf_g <- pgamma(gsw_seq, shape=gsw_g$estimate[1],
                      rate=gsw_g$estimate[2])
  gsw_cdf <- ecdf(Li_data$gsw)(gsw_seq)
  
  gsw_cdf_df <- as.data.frame(gsw_seq) %>%
    mutate(cdf_ln = gsw_cdf_ln,
           cdf_n = gsw_cdf_n,
           cdf_g = gsw_cdf_g,
           cdf = gsw_cdf)
## GSW CDF Plot
  output$gsw_cdf <- renderPlot({
    ggplot(gsw_cdf_df)+
      geom_point(aes(x=gsw_seq, y=cdf_n, color="Normal"))+
      geom_point(aes(x=gsw_seq, y=cdf_ln, color="Lognormal"))+
      geom_point(aes(x=gsw_seq, y=cdf_g, color="Gamma"))+
      geom_point(aes(x=gsw_seq, y=cdf, color="Data"))+
      theme_bw()+
      scale_color_scico_d(begin=.8, end=0, palette = gettext(Rpalette()))+
      ylab("CDF")+
      xlab("gsw")+
      theme(
        text = element_text(size=24, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.85,.6),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
  })
  
  ## PhiPS2 distribution
  PhiPS2_n <- fitdistr(Li_data$PhiPS2, "normal")
  PhiPS2_ln <- fitdistr(Li_data$PhiPS2, "lognormal")
  PhiPS2_g <- fitdistr(Li_data$PhiPS2, "gamma")
  PhiPS2_seq <- seq(min(Li_data$PhiPS2), max(Li_data$PhiPS2), by=0.001)
  
  ### test gsw for gamma and plnorm distributions using ks test
  phi_gks <- list2DF(ks.test(Li_data$PhiPS2, "pgamma",shape=PhiPS2_g$estimate[1],
                                   rate=PhiPS2_g$estimate[2])[2])[1,1]
  phi_lnks <- list2DF(ks.test(Li_data$PhiPS2, "plnorm",meanlog=PhiPS2_ln$estimate[1],
                                    sdlog=PhiPS2_ln$estimate[2])[2])[1,1]
  phi_nks <- list2DF(ks.test(Li_data$PhiPS2, "pnorm",mean=PhiPS2_n$estimate[1],
                                   sd=PhiPS2_n$estimate[2])[2])[1,1]
  ## outputs for value boxes
  output$phi_gks <- renderText({
    phi_gks
  })
  output$phi_lnks <- renderText({
    phi_lnks
  })
  output$phi_nks <- renderText({
    phi_nks
  })
  ### PDFs
  PhiPS2_pdf_n <- dnorm(PhiPS2_seq, mean=PhiPS2_n$estimate[1],
                        sd=PhiPS2_n$estimate[2])
  PhiPS2_pdf_ln <- dlnorm(PhiPS2_seq, meanlog=PhiPS2_ln$estimate[1],
                          sdlog=PhiPS2_ln$estimate[2])
  PhiPS2_pdf_g <- dgamma(PhiPS2_seq, shape=PhiPS2_g$estimate[1],
                         rate=PhiPS2_g$estimate[2])
  PhiPS2_pdf <- density(Li_data$PhiPS2, n=length(PhiPS2_pdf_ln))
  PhiPS2_pdf_df <- as.data.frame(PhiPS2_seq) %>%
    mutate(pdf_ln = PhiPS2_pdf_ln,
           pdf_n = PhiPS2_pdf_n,
           pdf_g = PhiPS2_pdf_g)
  ## CDFs
  PhiPS2_cdf_n <- pnorm(PhiPS2_seq, mean=PhiPS2_n$estimate[1],
                        sd=PhiPS2_n$estimate[2])
  PhiPS2_cdf_ln <- plnorm(PhiPS2_seq, meanlog=PhiPS2_ln$estimate[1],
                          sdlog=PhiPS2_ln$estimate[2])
  PhiPS2_cdf_g <- pgamma(PhiPS2_seq, shape=PhiPS2_g$estimate[1],
                         rate=PhiPS2_g$estimate[2])
  PhiPS2_cdf <- ecdf(Li_data$PhiPS2)(PhiPS2_seq)
  PhiPS2_cdf_df <- as.data.frame(PhiPS2_seq) %>%
    mutate(cdf_ln = PhiPS2_cdf_ln,
           cdf_n = PhiPS2_cdf_n,
           cdf_g = PhiPS2_cdf_g,
           cdf = PhiPS2_cdf)
## PhiPS2 PDF output
  output$phi_pdf <- renderPlot({
    ggplot(PhiPS2_pdf_df)+
      geom_point(aes(x=PhiPS2_seq, y=pdf_n, color="Normal"))+
      geom_point(aes(x=PhiPS2_seq, y=pdf_ln, color="Lognormal"))+
      geom_point(aes(x=PhiPS2_seq, y=pdf_g, color="Gamma"))+
      geom_point(aes(x=PhiPS2_pdf$x, y=PhiPS2_pdf$y, color="Data"))+
      theme_bw()+
      scale_color_scico_d(begin=.8, end=0, palette = gettext(Rpalette()))+
      guides(color=guide_legend(title="Distribution"))+
      ylab("PDF")+
      xlab("PhiPS2")+
      theme(
        text = element_text(size=24, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.85,.6),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
  })
  
  output$phi_cdf <- renderPlot({
    ggplot(PhiPS2_cdf_df)+
      geom_point(aes(x=PhiPS2_seq, y=cdf_n, color="Normal"))+
      geom_point(aes(x=PhiPS2_seq, y=cdf_ln, color="Lognormal"))+
      geom_point(aes(x=PhiPS2_seq, y=cdf_g, color="Gamma"))+
      geom_point(aes(x=PhiPS2_seq, y=cdf, color="Data"))+
      theme_bw()+
      scale_color_scico_d(begin=.8, end=0, palette = gettext(Rpalette()))+
      guides(color=guide_legend(title="Distribution"))+
      ylab("CDF")+
      xlab("PhiPS2")+
      theme(
        text = element_text(size=24, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.85,.6),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
  })

## Statistical outputs
  output$gsw_glm_gamma <- renderPrint({ summary(Rgsw_glm_gamma())})
  output$gsw_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rgsw_glm_gamma())})
  output$gsw_glm_gamma_AIC <- renderText({ AIC(Rgsw_glm_gamma())})
  output$gsw_glm_log <- renderPrint({ summary(Rgsw_glm_log())})
  output$gsw_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rgsw_glm_log())})
  output$gsw_glm_log_AIC <- renderText({ AIC(Rgsw_glm_log())})
  
## GWalkR outputs
  output$li_gwalk <- renderGwalkr({gwalkr(na.omit(RLi_data()))})
  output$f_gwalk <- renderGwalkr({gwalkr(na.omit(RFl_data()[,c(13,1,2,18,3:7,19,8:12,17)]))})
}

# run app
shinyApp(ui = ui, server = server)
