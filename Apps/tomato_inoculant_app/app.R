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
library(car)
library(DT)
library(rstatix)
library(lme4)
library(MuMIn)
library(devtools)
install_github("zachpeagler/multiFitR")
library(multiFitR)
install_github("zachpeagler/multiFitRgg")
library(multiFitRgg)

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
#distributions
dists <- c("normal", "lognormal", "gamma", "exponential",
           "cauchy", "weibull")
sigcodes <- c("Psymbols", "Pvalues")
# load data
## li600 data
Li_data_file <- "TIP24_LI600.csv"
Li_data <- read.csv(Li_data_file, stringsAsFactors = T) %>%
  mutate(Treatment = case_when(
    Row==1~"Control",
    Row==2~"Transplantation",
    Row==3~"Germination",
    Row==4~"Germ+Trans",
    TRUE~NA)) %>%
  # filter to gsw > 0 as it biologically cannot be negative???
  filter(gsw > 0, PhiPS2 > 0.2) %>%
  mutate(Date = parse_date_time(Date, orders = "mdy"),
         Date = as.factor(Date),
         Date_num = as.numeric(Date),
         plant_fac = as.factor(paste(Row, Pot, sep="_")),
         Treatment = factor(Treatment, levels=c("Control", "Germination",
                                                "Transplantation", "Germ+Trans")),
         Treatment_num = as.numeric(Row)
         )
## subset data to only the columns of interest, and not 100 miscellaneous variables.
Li_data <- Li_data[,c("Treatment", "Time", "Date", "Date_num", "Row", "Pot",
                      "plant_fac", "gsw", "VPDleaf", "PhiPS2", "rh_s", "Tleaf",
                      "Qamb", "leak_pct", "Treatment_num")]
## Make fluoro variables
fluoro_vars <- c("gsw", "PhiPS2", "Date", "Time", "Treatment", "rh_s", "Tleaf",
                 "Qamb", "VPDleaf", "plant_fac", "Date_num", "Treatment_num")
## Get gsw and PhiPS2 quantiles and interquartile range (IQR)
Qgsw <- quantile(Li_data$gsw, probs=c(.25, .75), na.rm=FALSE)
Qps2 <- quantile(Li_data$PhiPS2, probs=c(.25, .75), na.rm=FALSE)
iqr_gsw <- IQR(Li_data$gsw)
iqr_ps2 <- IQR(Li_data$PhiPS2)
## multispeq data
m_data_file <- "TIP24_Multispeq.csv"
m_data <- read.csv(m_data_file) %>%
  mutate(Treatment = case_when(
    Row=="A"~"Control",
    Row=="B"~"Transplantation",
    Row=="C"~"Germination",
    Row=="D"~"Germ+Trans",
    TRUE~NA),
    Treatment = factor(Treatment, levels=c("Control", "Germination",
                                           "Transplantation", "Germ+Trans")),
    Row_num = case_when(
      Row=="A"~ 1,
      Row=="B"~ 2,
      Row=="C"~ 3,
      Row=="D"~ 4,
      TRUE~NA),
    plant_fac = as.factor(paste(Row_num, Pot)),
    Date = as.Date(time, "%m/%d/%Y"),
    Date = as.factor(Date),
  )
# trim the fat from the multispeq data
m_data <- m_data[,c("Row", "Pot", "plant_fac", "Treatment", "Device.ID",
                    "Leaf.Damage.", "Pests.Present.", "Time.of.Day",
                    "Date", "time", "Phi2", "Light.Intensity..PAR.",
                    "Leaf.Temperature", "FvP_over_FmP", "Ambient.Temperature",
                    "Ambient.Humidity")]
# set multispeq variables that will be used in scatter plot
m_vars <- c("plant_fac", "Row", "Pot", "Treatment", "Device.ID", "Date",
            "Time.of.Day", "Leaf.Damage.", "Pests.Present.",
            "Leaf.Temperature", "FvP_over_FmP", "Phi2", "Ambient.Temperature", 
            "Ambient.Humidity")
Qp2 <- quantile(m_data$Phi2, probs=c(.25, .75), na.rm=FALSE)
iqr_p2 <- IQR(m_data$Phi2)

## fruit data
Fl_data_file <- "TIP24_Fruit.csv"
Fl_data <- read.csv(Fl_data_file, stringsAsFactors = T) %>%
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
## make fruit variables
fruit_vars <- c("Treatment", "plant_fac", "BER", "fungus", "cracking", "date_analysis", "d_diff",
                "mass", "sugar_avg", "ripeness")
## Get mass and sugar quantiles and IQR
Qmass <- quantile(na.omit(Fl_data$mass), probs=c(.25, .75), na.rm=FALSE)
Qsug <- quantile(na.omit(Fl_data$sugar_avg), probs=c(.25, .75), na.rm=FALSE)
iqr_mass <- IQR(na.omit(Fl_data$mass))
iqr_sug <- IQR(na.omit(Fl_data$sugar_avg))
###### UI #####
ui <- navbarPage(title = "Tomato Inoculants",
                 theme = bs_theme(bg = "white",
                                  "navbar-fg" = sls8[1],
                                  fg=sls8[1], primary = sls8[5],
                                  secondary = sls8[4],
                                  base_font = font_google("Open Sans")),
                 sidebar = sidebar(
                   markdown("##### **Global Options**"),
                   selectInput("palette","Select color palette",
                               choices = p_palettes, selected = "bilbao"),
                    checkboxInput("outliers", "Exclude outliers", FALSE),
                   markdown("##### **Fluorescence Options**"),
                    sliderInput("leak_pct",
                                "Maximum allowable leak percent",
                                min = 0,
                                max = 100,
                                value =10),
                   markdown("##### **Fruit Options**"),
                        checkboxInput("omitna", "Exclude fruit not analyzed for sugar",
                                      FALSE),
                        checkboxInput("ber", "Exclude fruit w/ blossom end rot",
                                      FALSE),
                        sliderInput("min_mass",
                                    "Minimum Tomato Mass (g)",
                                    min = 0,
                                    max = 100,
                                    value = 0 )
                 ), # end global sidebar
   nav_panel("Fluorescence",
     tabsetPanel(
       tabPanel("Distributions",
        card(card_header("Li-600", class = "bg-primary"),
        card(card_header("Stomatal Conductance (gsw)",
                         class = "bg-secondary"),
             card(layout_sidebar(sidebar = sidebar(
               checkboxGroupInput("gsw_dists", "Distributions",
                                  choices = dists, selected = c("normal", "lognormal", "gamma"))
             ),
             card_body((layout_column_wrap(
               card(card_header("Probability Density Function Plot"),
                    card_body(plotOutput("gsw_pdf"))),
               card(card_header("Cumulative Distribution Function Plot"),
                    card_body(plotOutput("gsw_cdf")))
             )))
             )
             ),
             card(layout_column_wrap(
             card(card_header("One-sample Kolmogorov-Smirnov test results"),
                  verbatimTextOutput("gsw_mKS")
             ),
             card(card_header("Tests for Homogeneity of Variances"),
             card(card_header("Levene Test"),
                  verbatimTextOutput("gsw_levene")
             ),
             card(card_header("Bartlett Test"),
                  verbatimTextOutput("gsw_bartlett")
                  )
             )
             ))
            ),
       card(card_header("Efficiency of Photosystem II (PhiPS2)",
                        class = "bg-secondary"),
            card(layout_sidebar(sidebar = sidebar(
              checkboxGroupInput("phi_dists", "Distributions",
                                 choices = dists, selected = c("normal", "lognormal", "gamma"))
            ),
            card_body(layout_column_wrap(
              card(card_header("Probability Density Function Plot"),
                   card_body(plotOutput("phi_pdf"))),
              card(card_header("Cumulative Distribution Function Plot"),
                   card_body(plotOutput("phi_cdf")))
            ))
            )),
            card(layout_column_wrap(
            card(card_header("One-sample Kolmogorov-Smirnov test results"),
                 verbatimTextOutput("Lphi_mKS")
            ),
            card(card_header("Tests for Homogeneity of Variances"),
            card(card_header("Levene Test"),
                 verbatimTextOutput("phi_levene")
            ),
            card(card_header("Bartlett Test"),
                 verbatimTextOutput("phi_bartlett")
            )
            )
            ))
            ) #end PhiPS2
        ), # end Li-600
       card(card_header("MultispeQ", class = "bg-primary"),
            card(card_header("FvP over FmP",
                             class = "bg-secondary"),
                 card(layout_sidebar(sidebar = sidebar(
                   checkboxGroupInput("fvp_dists", "Distributions",
                                      choices = dists, selected = c("normal", "lognormal", "gamma"))
                 ),
                 card_body((layout_column_wrap(
                   card(card_header("Probability Density Function Plot"),
                        card_body(plotOutput("fvp_pdf"))),
                   card(card_header("Cumulative Distribution Function Plot"),
                        card_body(plotOutput("fvp_cdf")))
                 )))
                 )
                 ),
                 card(layout_column_wrap(
                 card(card_header("One-sample Kolmogorov-Smirnov test results"),
                      verbatimTextOutput("fvp_mKS")
                 ),
                 card(card_header("Tests for Homogeneity of Variances"),
                 card(card_header("Levene Test"),
                      verbatimTextOutput("fvp_levene")
                      ),
                 card(card_header("Bartlett Test"),
                      verbatimTextOutput("fvp_bartlett")
                 )
                 )
                 ))
            ),
            card(card_header("Efficiency of Photosystem II (PhiPS2)",
                             class = "bg-secondary"),
                 card(layout_sidebar(sidebar = sidebar(
                   checkboxGroupInput("p2_dists", "Distributions",
                                      choices = dists, selected = c("normal", "lognormal", "gamma"))
                 ),
                 card_body(layout_column_wrap(
                   card(card_header("Probability Density Function Plot"),
                        card_body(plotOutput("p2_pdf"))),
                   card(card_header("Cumulative Distribution Function Plot"),
                        card_body(plotOutput("p2_cdf")))
                 ))
                 )),
                 card(layout_column_wrap(
                 card(card_header("One-sample Kolmogorov-Smirnov test results"),
                      verbatimTextOutput("p2_mKS")
                 ),
                 card(card_header("Tests for Homogeneity of Variances"),
                 card(card_header("Levene Test"),
                      verbatimTextOutput("p2_levene")
                 ),
                 card(card_header("Bartlett Test"),
                      verbatimTextOutput("p2_bartlett")
                 )
                 )
                 ))
            ) #end PhiPS2
            )
       ), # end distributions tab
       tabPanel("Plots",
                card(card_header("Interactive Li-600 Scatter",
                                 class = "bg-primary"),
                     layout_sidebar(sidebar = sidebar(
                       selectInput("fluoro_x","X Variable",
                                   choices = fluoro_vars, selected = "Date"),
                       selectInput("fluoro_y","Y Variable",
                                   choices = fluoro_vars, selected = "gsw"),
                       selectInput("fluoro_col","Color Variable",
                                   choices = fluoro_vars, selected = "rh_s"),
                       sliderInput("fluoro_jit", "Jitter Amount", min=0, max=10, value =3),
                       checkboxInput("fluoro_fwrap", "Individual Plot Per Treatment", FALSE)
                     ),
                     card_body(plotOutput("fluoro_scatter")))
                ),
                card(card_header("Interactive MultispeQ Scatter",
                                 class = "bg-primary"),
                     layout_sidebar(sidebar = sidebar(
                       selectInput("m_x","X Variable",
                                   choices = m_vars, selected = "Date"),
                       selectInput("m_y","Y Variable",
                                   choices = m_vars, selected = "FvP_over_FmP"),
                       selectInput("m_col","Color Variable",
                                   choices = m_vars, selected = "Ambient.Humidity"),
                       sliderInput("m_jit", "Jitter Amount", min=0, max=10, value =3),
                       checkboxInput("m_fwrap", "Individual Plot Per Treatment", FALSE)
                     ),
                     card_body(plotOutput("m_scatter")))
                ),
                card(card_header("Boxplots", class = "bg-secondary"),
                       checkboxInput("fluoro_box_stats", "Show Statistical Tests", FALSE),
                       selectInput("bs_labels", "Show P symbols or P values",
                                   sigcodes, "Pvalues"),
                     card(card_header("Li-600", class = "bg-primary"),
                     card_body(layout_column_wrap(
                       card(card_header("Stomatal Conductance (gsw)"),
                            card_body(plotOutput("gsw_box"))),
                       card(card_header("Efficiency of Photosystem II (PhiPS2)"),
                            card_body(plotOutput("phi_box")))
                     ))),
                     card(card_header("MultispeQ", class = "bg-primary"),
                          card_body(layout_column_wrap(
                            card(card_header("Efficiency of Photosystem II (PhiPS2)"),
                                 card_body(plotOutput("p2_box"))),
                            card(card_header("Efficiency of Open Reaction Centers in Light"),
                                 card_body(plotOutput("fvp_box")))
                          ))),
                     card(markdown("The above boxplots use [Kruskal-Wallis tests](https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_test)
                                  for the comparison of all group means and [Mann-Whitney U tests](https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test)
                                  for pairwise comparisons between treatment groups."))
                )
       ), # end plot tab
       tabPanel("Statistical Tests",
                card(card_header("Li-600", class = "bg-primary"),
                card(card_header("Stomatal Conductance (gsw)", class = "bg-secondary"),
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
        ))), # end gsw card
        card(card_header("Photosystem II Efficiency (PhiPS2)", class = "bg-secondary"),
             card_body("Based on the exploratory analysis, PhiPS2 does not match any distribution.
                       Therefore, we will use our biological reasoning and look at what distribution PhiPS2
                       was most alike on the PDF and CDF. On a biological level, PhiPS2 is the efficiency of
                       a part of the clorophyll, and probably follows some sort of inverse square law, and based
                       on the PDF it's a bit right skewed, making a Gamma distribution a possible choice for analysis. In all likelyhood,
                       this should be evaluated using Bayesian inference.
                       When constructing our model, we should also take into account fixed effects, such as ambient light (Qamb),
                       and random effects, such as the pot along a row."),
             card_body(layout_column_wrap(
               card(card_header("GLM with Gamma family and log link"),
                    card_body(verbatimTextOutput("phi_glm_gamma"), max_height= 500),
                    card_body(layout_column_wrap(
                      card(card_header("Pseudo-R2"),
                           card_body(verbatimTextOutput("phi_glm_gamma_r2"))),
                      value_box(title = "AIC", value = textOutput("phi_glm_gamma_AIC"))
                    ))
               ),
               card(card_header("GLM with gaussian family and log link"),
                    card_body(verbatimTextOutput("phi_glm_log"), max_height= 500),
                    card_body(layout_column_wrap(
                      card(card_header("Pseudo-R2"),
                           card_body(verbatimTextOutput("phi_glm_log_r2"))),
                      value_box(title = "AIC", value = textOutput("phi_glm_log_AIC"))
                    ))
                )
            ))
          )
       ),
       card(card_header("MultispeQ", class = "bg-primary"),
       )
            
       ), # end statistical tests panel
       tabPanel("Data",
                card(card_header("Li-600 Data", class = "bg-primary"),
                DTOutput("li_DT")),
                card(card_header("MultispeQ Data", class = "bg-secondary"),
                DTOutput("m_DT"))
       ),
       tabPanel("Info",
                card(markdown("Fluorescence measurements were taken biweekly with a LI-COR LI-600 
                              and two PhotosynQ MultispeQ V2.0s over the course of the trial.
                              Data is presented in a tidy format with each row representing a single
                              observation and each column representing a variable. <br>
                              
       ### **Li-600**
       #### Explanatory Variables
          **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
          **Time** is the time at which the measurement was taken. <br>
          **Date** is the date at which the measurement was taken. <br>
          **Row** is the row number of the tomato. (1:4) <br>
          **Pot** is the pot number of the tomato. (1:12) <br>
          **plant_fac** is a combination of *Row* and *Pot*, and acts as an ID for every individual plant. (1 1: 4 12) <br>
          **rh_s** is the relative humidity (add units) of the leaf. <br>
          **Tleaf** is the temperature (C) of the leaf. <br>
          **Qamb** is the ambient light level (add units) at the time of the observation.
       #### Response Variables
          **gsw** is the stomatal conductance (mol m-2 s-1) of the leaf. <br>
          **PhiPS2** is the quantum yield. It is unitless. (0:1) <br>
       ### **MultispeQ**   
        #### Explanatory Variables
          **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
          **Date** is the date at which the measurement was taken. <br>
          **Row** is the row number of the tomato. (1:4) <br>
          **Pot** is the pot number of the tomato. (1:12) <br>
          **plant_fac** is a combination of *Row* and *Pot*, and acts as an ID for every individual plant. (1 1: 4 12) <br>
        #### Response Variables
          **PhiPS2** is the quantum yield. It is unitless. (0:1) <br>
          **FvP_over_FmP** is the efficiency of open reaction centers in light.<br>
          "))
       )
     ) # end tab set
   ), # end fluorescence page

  nav_panel("Fruit",
    tabsetPanel(
      tabPanel("Distributions",
        card(card_header("Sugar"),
          card_body(layout_column_wrap(
            card(card_header("Probability Density Function Plot"),
              card_body(plotOutput("sug_pdf"))),
            card(card_header("Cumulative Distribution Function Plot"),
              card_body(plotOutput("sug_cdf")))
              ))),
        card(card_header("Mass"),
          card_body(layout_column_wrap(
            card(card_header("Probability Density Function Plot"),
              card_body(plotOutput("mass_pdf"))),
            card(card_header("Cumulative Distribution Function Plot"),
              card_body(plotOutput("mass_cdf")))
          )))
      ), # end distributions tab
      tabPanel("Plots",
               card(card_header("Interactive Scatter Plot",
                                class = "bg-primary"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("fruit_x","X Variable",
                                  choices = fruit_vars, selected = "harvest_date"),
                      selectInput("fruit_y","Y Variable",
                                  choices = fruit_vars, selected = "mass"),
                      selectInput("fruit_col","Color Variable",
                                  choices = fluoro_vars, selected = "sugar_avg"),
                      sliderInput("fruit_jit", "Jitter Amount", min=0, max=10, value =3),
                      checkboxInput("fruit_fwrap", "Individual Plot Per Treatment", FALSE)
                    ),
                    card_body(plotOutput("fruit_scatter")))
               ),
               card(card_header("Boxplots", class = "bg-secondary"),
                    checkboxInput("fruit_box_stats", "Show Statistical Tests", FALSE),
                    card_body(layout_column_wrap(
                      card(card_header("Mass"),
                           card_body(plotOutput("mass_box"))),
                      card(card_header("Sugar Concentration"),
                           card_body(plotOutput("sug_box")))
                    )), # end card_body
                    card(markdown("The above boxplots use [Kruskal-Wallis tests](https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_test)
                                  for the comparison of all group means and [Mann-Whitney U tests](https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test)
                                  for pairwise comparisons between treatment groups."))
               ) # end sidebar layout and card
      ),
      tabPanel("Statistical Tests",
               card(card_header("Mass"),
                card_body(markdown("Mass stat tests blurb yabadabadebop.")),
                card_body(layout_column_wrap(
                 card(card_header("GLM with Gamma family and log link"),
                      card_body(verbatimTextOutput("mass_glm_gamma"), max_height= 500),
                      card_body(layout_column_wrap(
                        card(card_header("Pseudo-R2"),
                             card_body(verbatimTextOutput("mass_glm_gamma_r2"))),
                        value_box(title = "AIC", value = textOutput("mass_glm_gamma_AIC"))
                      ))
                 ),
                 card(card_header("GLM with gaussian family and log link"),
                      card_body(verbatimTextOutput("mass_glm_log"), max_height= 500),
                      card_body(layout_column_wrap(
                        card(card_header("Pseudo-R2"),
                             card_body(verbatimTextOutput("mass_glm_log_r2"))),
                        value_box(title = "AIC", value = textOutput("mass_glm_log_AIC"))
                      ))
                 )
               ))), # end mass
               card(card_header("Sugar Concentration (%)"),
                    card_body("Based on the exploratory analysis, sugar is probably normal."),
                    card_body(layout_column_wrap(
                      card(card_header("GLM with Gamma family and log link"),
                           card_body(verbatimTextOutput("sug_glm_normal"), max_height= 500),
                           card_body(layout_column_wrap(
                             card(card_header("Pseudo-R2"),
                                  card_body(verbatimTextOutput("sug_glm_normal_r2"))),
                             value_box(title = "AIC", value = textOutput("sug_glm_normal_AIC"))
                           ))
                      ),
                      card(card_header("GLM with gaussian family and log link"),
                           card_body(verbatimTextOutput("sug_glm_log"), max_height= 500),
                           card_body(layout_column_wrap(
                             card(card_header("Pseudo-R2"),
                                  card_body(verbatimTextOutput("sug_glm_log_r2"))),
                             value_box(title = "AIC", value = textOutput("sug_glm_log_AIC"))
                           ))
                      )
                    ))) # end sugar
               ),
      tabPanel("Data",
               DTOutput("fruit_data")
      ),
      tabPanel("Info",
         card(
          card_body(
            markdown("The tomatoes were grown in 4 rows of 12 pots each, with each row corresponding to a different inoculation treatment.
                     The data table is formatted in a tidy format with each row corresponding to one fruit and each column representing a variable.<br>
                      ### Explanatory Variables <br>
                      **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
                      **row** is the row number of the tomato. (1:4) <br>
                      **plant** is the pot number of the tomato. (1:12) <br>
                      **plant_fac** is a combination of *row* and *plant*, and acts as an ID for every individual plant. (1 1: 4 12) <br>
                      **date_harvest** is the date the fruit was harvested (August 2024:October 2024) <br>
                      **date_analysis** is the date the fruit was analyzed in the lab (August 2024:October 2024) <br>
                      **d_diff** is the number of days from harvest to analysis. (0:6)
                      ### Response Variables <br>
                      **mass** is the mass in grams of the tomato, measured on an *SCALE NAME*. (~50:~400) <br>
                      **BER** corresponds to whether or not the tomato has blossom end rot, a disease caused by calcium deficiency that renders the fruit unmarketable. (0,1) <br>
                      **fungus** corresponds to whether or not the tomato has fungus growing on it. This is common on fruit with BER. (0,1) <br>
                      **cracking** corresponds to whether or not the tomato has cracks in its skin (0,1) <br>
                      **penetrometer** corresponds to the force in kilograms it takes to penetrate the flesh of the tomato (~0.5:~4) <br>
                      **ripeness** is the **penetrometer** value mapped from 0:1 and reversed, so that riper fruit are closer to 1 and unripe fruit are closer to 0. (0:1) <br>
                      **sugar.1** and **sugar.2** are measurements of the tomato juice's sugar concentration taken on a Fisher BRIX Refractometer (~2:~12) <br>
                      **sugar_avg** is the average of the two sugar measurements. <br>")))
               ) # end info panel
    ) # end tab set
  ), # end fruit page
  nav_panel("Harvest",
  ),
  nav_spacer(),
  nav_item(tags$a("Github", href = "https://github.com/zachpeagler/Portfolio/tree/main/R%20Shiny%20Apps/01%20Tomato%20Inoculants"))
) # end ui

##### server #####
server <- function(input, output) {
## Reactive expressions
  Rpalette <- reactive({input$palette})
  Rleak_pct <- reactive({input$leak_pct})
  Rfluoro_x <- reactive({input$fluoro_x})
  Rfluoro_y <- reactive({input$fluoro_y})
  Rfluoro_col <- reactive({input$fluoro_col})
  Rfluoro_jit <- reactive({input$fluoro_jit * .1})
  Rfluoro_fwrap <- reactive({input$fluoro_fwrap})
  Rfluoro_box_stats <- reactive({input$fluoro_box_stats})
  Rbs_labels <- reactive({input$bs_labels})
  Rm_x <- reactive({input$m_x})
  Rm_y <- reactive({input$m_y})
  Rm_col <- reactive({input$m_col})
  Rm_jit <- reactive({input$m_jit * .1})
  Rm_fwrap <- reactive({input$m_fwrap})
  Rm_box_stats <- reactive({input$m_box_stats})
  Rgsw_dists <- reactive({input$gsw_dists})
  Rphi_dists <- reactive({input$phi_dists})
  Rp2_dists <- reactive({input$p2_dists})
  Rfvp_dists <- reactive({input$fvp_dists})

  ### Reactive dataframes
  RLi_data <- reactive({
    x <- Li_data
    if (input$outliers == TRUE) {
      x <- subset(x, gsw > (Qgsw[1]-1.5*iqr_gsw) &
                gsw < (Qgsw[2]+1.5*iqr_gsw) &
                PhiPS2 > (Qps2[1]-1.5*iqr_ps2) &
                PhiPS2 < (Qps2[2]+1.5*iqr_ps2))
    }
    x <- x %>% filter(leak_pct < Rleak_pct())
    return(x)
  })
  Rm_data <- reactive({
    x <- m_data
    if (input$outliers == TRUE) {
      x <- subset(x, Phi2 > (Qp2[1]-1.5*iqr_p2) &
                    Phi2 < (Qp2[2]+1.5*iqr_p2))
    }
    return(x)
  })
  RFl_data <- reactive({
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
    if (input$omitna == TRUE){
      f <- na.omit(f)
    }
    f <- subset(f, mass > input$min_mass)
    return(f)
  })
### Reactive GLMs
#### gsw
  Rgsw_glm_gamma <- reactive({
    glmer(gsw ~ Treatment + rh_s + (1 | plant_fac),
          data = RLi_data(), family = Gamma(link = "log"))
  })
  Rgsw_glm_log <- reactive({
    glmer(gsw ~ Treatment + rh_s + (1 | plant_fac),
          data = RLi_data(), family = gaussian(link = "log"))
  })
#### PhiPS2
  Rphi_glm_gamma <- reactive({
    glmer(PhiPS2 ~ Treatment + Qamb + (1 | plant_fac),
          data = RLi_data(), family = Gamma(link = "log"))
  })
  Rphi_glm_log <- reactive({
    glmer(PhiPS2 ~ Treatment + Qamb + (1 | plant_fac),
          data = RLi_data(), family = gaussian(link = "log"))
  })
## Fluorescence
### Distributions
#### GSW
  ## Li-600 gsw multiKS output
  output$gsw_mKS <- renderPrint({
    x <- RLi_data()$gsw
    gsw_dists <- Rgsw_dists()
    o <- multiKS_cont(x, gsw_dists)
    return(o)
  })
## GSW PDF plot
  output$gsw_pdf <- renderPlot({
    x <- RLi_data()$gsw
    gsw_dists <- Rgsw_dists()
    p <- multiPDF_plot(x, 100, gsw_dists)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("PDF")+
      xlab("gsw")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.75,.6),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  }) # end GSW PDF output
## GSW CDF Plot
  output$gsw_cdf <- renderPlot({
    x <- RLi_data()$gsw
    gsw_dists <- Rgsw_dists()
    p <- multiCDF_plot(x, 100, gsw_dists)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("CDF")+
      xlab("gsw")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.75,.6),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  })
## Li-600 PhiPS2 multiKS output
  output$Lphi_mKS <- renderPrint({
    x <- RLi_data()$PhiPS2
    phi_dists <- Rphi_dists()
    o <- multiKS_cont(x, phi_dists)
    return(o)
  })
# Li-600 Tests for homoscedasticity
  output$gsw_levene <- renderPrint({
    leveneTest(gsw~Treatment, data=RLi_data())
  })
  output$gsw_bartlett <- renderPrint({
    bartlett.test(gsw~Treatment, data=RLi_data())
  })
  output$phi_levene <- renderPrint({
    leveneTest(PhiPS2~Treatment, data=RLi_data())
  })
  output$phi_bartlett <- renderPrint({
    bartlett.test(PhiPS2~Treatment, data=RLi_data())
  })
# Multispeq tests for homoscedasticity
  output$p2_levene <- renderPrint({
    leveneTest(Phi2~Treatment, data=Rm_data())
  })
  output$p2_bartlett <- renderPrint({
    bartlett.test(Phi2~Treatment, data=Rm_data())
  })
  output$fvp_levene <- renderPrint({
    leveneTest(FvP_over_FmP~Treatment, data=Rm_data())
  })
  output$fvp_bartlett <- renderPrint({
    bartlett.test(FvP_over_FmP~Treatment, data=Rm_data())
  })
## PhiPS2 PDF output
  output$phi_pdf <- renderPlot({
    x <- RLi_data()$PhiPS2
    phi_dists <- Rphi_dists()
    p <- multiPDF_plot(x, 100, phi_dists)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("PDF")+
      xlab("PhiPS2")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.85,.6),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  })
## PhiPS2 CDF Plot
  output$phi_cdf <- renderPlot({
    x <- RLi_data()$PhiPS2
    phi_dists <- Rphi_dists()
    p <- multiCDF_plot(x, 100, phi_dists)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("CDF")+
      xlab("PhiPS2")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.75,.4),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  })
# Multispeq distributions
  output$p2_mKS <- renderPrint({
    x <- Rm_data()$Phi2
    phi_dists <- Rp2_dists()
    o <- multiKS_cont(x, phi_dists)
    return(o)
  })
  output$fvp_mKS <- renderPrint({
    x <- Rm_data()$FvP_over_FmP
    fvp_dists <- Rfvp_dists()
    o <- multiKS_cont(x, fvp_dists)
    return(o)
  })

## Multispeq PhiPS2 PDF output
  output$p2_pdf <- renderPlot({
    x <- Rm_data()$Phi2
    phi_dists <- Rp2_dists()
    p <- multiPDF_plot(x, 100, phi_dists)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("PDF")+
      xlab("PhiPS2")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.85,.6),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  })
## Multispeq PhiPS2 CDF Plot
  output$p2_cdf <- renderPlot({
    x <- Rm_data()$Phi2
    phi_dists <- Rp2_dists()
    p <- multiCDF_plot(x, 100, phi_dists)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("CDF")+
      xlab("PhiPS2")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.75,.4),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  })
## FvP/FmP PDF output
  output$fvp_pdf <- renderPlot({
    x <- Rm_data()$FvP_over_FmP
    ds <- Rfvp_dists()
    p <- multiPDF_plot(x, 100, ds)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("PDF")+
      xlab("FvP/FmP")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.85,.6),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  })
## PhiPS2 CDF Plot
  output$fvp_cdf <- renderPlot({
    x <- Rm_data()$FvP_over_FmP
    ds <- Rfvp_dists()
    p <- multiCDF_plot(x, 100, ds)+
      labs(title="")+
      theme_bw()+
      scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
      ylab("CDF")+
      xlab("FvP/FmP")+
      theme(
        text = element_text(size=20, family="mont"),
        legend.position="inside",
        legend.position.inside = c(.75,.4),
        legend.title = element_text(size=24, family = "mont", face= "bold"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
      )
    return(p)
  })
# Scatter plots
## LI-600 interactive scatter plot
  output$fluoro_scatter <- renderPlot({
    fs <- ggplot(data = RLi_data(), aes(x=.data[[Rfluoro_x()]], y = .data[[Rfluoro_y()]],
                               color = .data[[Rfluoro_col()]])) +
            geom_jitter(width=Rfluoro_jit())+
            scale_color_scico(begin=0.9, end=0, palette=Rpalette())+
            scale_x_discrete(guide=guide_axis(check.overlap=TRUE))+
            theme_minimal()+
            ylab(gettext(Rfluoro_y()))+
            xlab(gettext(Rfluoro_x()))+
            theme(
              text = element_text(size=24, family="mont"),
              axis.title = element_text(size=24, family = "mont", face= "bold"),
              title = element_text(size=30, family="open", face="bold", lineheight = .5)
            )
    if (Rfluoro_fwrap() ==TRUE){
      fs <- fs + facet_wrap(~Treatment)
    }
    return(fs)
  })
## Multispeq interactive scatter plot
  output$m_scatter <- renderPlot({
    ms <- ggplot(data = Rm_data(), aes(x=.data[[Rm_x()]], y = .data[[Rm_y()]],
                                        color = .data[[Rm_col()]])) +
      geom_jitter(width=Rm_jit())+
      scale_color_scico(begin=0.9, end=0, palette=Rpalette())+
      scale_x_discrete(guide=guide_axis(check.overlap=TRUE))+
      theme_minimal()+
      ylab(gettext(Rm_y()))+
      xlab(gettext(Rm_x()))+
      theme(
        text = element_text(size=24, family="mont"),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
        title = element_text(size=30, family="open", face="bold", lineheight = .5)
      )
    if (Rm_fwrap() ==TRUE){
      ms <- ms + facet_wrap(~Treatment)
    }
    return(ms)
  })
## gsw boxplot
  output$gsw_box <- renderPlot({
    gbox <- ggplot(data = RLi_data(), aes(x= Treatment, y = gsw, fill=Treatment, color=Treatment)) +
      geom_boxplot(alpha=.5, width=0.25)+
      geom_violin(alpha=0.5, width=1)+
      geom_jitter( width=.2, height=0)+
      scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
      scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
      ylab("Stomatal Conductance (mol m-2 s-1)")+
      theme_minimal()+
      theme(
        legend.position="none",
        text = element_text(size=24, family="mont", lineheight=0.5),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
        title = element_text(size=30, family="open", face="bold")
      )
    if (Rfluoro_box_stats() == TRUE) {
      if (Rbs_labels() == "Pvalues") {
      gbox <- gbox +
        stat_compare_means(comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
        stat_compare_means(label.x=3, size=8,family="mont")
      } else {
      gbox <- gbox +
        stat_compare_means(label = "p.signif", comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
          stat_compare_means(label = "p.signif", label.x=3, size=8, family="mont")
      }
    }
    return(gbox)
  })
## PhiPS2 boxplot
  output$phi_box <- renderPlot({
    pbox <- ggplot(data = RLi_data(), aes(x= Treatment, y = PhiPS2, fill=Treatment, color=Treatment)) +
      geom_boxplot(alpha=.5, width=0.25)+
      geom_violin(alpha=0.5, width=1)+
      geom_jitter( width=.2, height=0)+
      scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
      scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
      ylab("PhiPS2")+
      theme_minimal()+
      theme(
        legend.position="none",
        text = element_text(size=24, family="mont", lineheight=0.5),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
        title = element_text(size=30, family="open", face="bold")
      )
    if (Rfluoro_box_stats() == TRUE) {
      if (Rbs_labels() == "Pvalues") {
        pbox <- pbox +
          stat_compare_means(comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
          stat_compare_means(label.x=3, size=8,family="mont")
      } else {
        pbox <- pbox +
          stat_compare_means(label = "p.signif", comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
          stat_compare_means(label = "p.signif", label.x=3, size=8, family="mont")
      }
    }
    return(pbox)
  })
## multispeq PhiPS2 boxplot
  output$p2_box <- renderPlot({
    pbox <- ggplot(data = Rm_data(), aes(x= Treatment, y = Phi2, fill=Treatment, color=Treatment)) +
      geom_boxplot(alpha=.5, width=0.25)+
      geom_violin(alpha=0.5, width=1)+
      geom_jitter( width=.2, height=0)+
      scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
      scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
      ylab("PhiPS2")+
      theme_minimal()+
      theme(
        legend.position="none",
        text = element_text(size=24, family="mont", lineheight=0.5),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
        title = element_text(size=30, family="open", face="bold")
      )
    if (Rfluoro_box_stats() == TRUE) {
      if (Rbs_labels() == "Pvalues") {
        pbox <- pbox +
          stat_compare_means(comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
          stat_compare_means(label.x=3, size=8,family="mont")
      } else {
        pbox <- pbox +
          stat_compare_means(label = "p.signif", comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
          stat_compare_means(label = "p.signif", label.x=3, size=8, family="mont")
      }
    }
    return(pbox)
  })
## multispeq fvp boxplot
  output$fvp_box <- renderPlot({
    pbox <- ggplot(data = Rm_data(), aes(x= Treatment, y = FvP_over_FmP, fill=Treatment, color=Treatment)) +
      geom_boxplot(alpha=.5, width=0.25)+
      geom_violin(alpha=0.5, width=1)+
      geom_jitter( width=.2, height=0)+
      scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
      scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
      ylab("FvP/FmP")+
      theme_minimal()+
      theme(
        legend.position="none",
        text = element_text(size=24, family="mont", lineheight=0.5),
        axis.title = element_text(size=24, family = "mont", face= "bold"),
        title = element_text(size=30, family="open", face="bold")
      )
    if (Rfluoro_box_stats() == TRUE) {
      if (Rbs_labels() == "Pvalues") {
        pbox <- pbox +
          stat_compare_means(comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
          stat_compare_means(label.x=3, size=8,family="mont")
      } else {
        pbox <- pbox +
          stat_compare_means(label = "p.signif", comparisons = list(c("Control","Germination"),c("Control", "Transplantation"), c("Control", "Germ+Trans"), c("Transplantation", "Germination"), c("Transplantation", "Germ+Trans"), c("Germination", "Germ+Trans")), size=8, family="mont")+
          stat_compare_means(label = "p.signif", label.x=3, size=8, family="mont")
      }
    }
    return(pbox)
  })
# Stats
## gsw statistical outputs
  output$gsw_glm_gamma <- renderPrint({ summary(Rgsw_glm_gamma())})
  output$gsw_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rgsw_glm_gamma())})
  output$gsw_glm_gamma_AIC <- renderText({ AIC(Rgsw_glm_gamma())})
  output$gsw_glm_log <- renderPrint({ summary(Rgsw_glm_log())})
  output$gsw_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rgsw_glm_log())})
  output$gsw_glm_log_AIC <- renderText({ AIC(Rgsw_glm_log())})
## PhiPS2 statistical outputs
  output$phi_glm_gamma <- renderPrint({ summary(Rphi_glm_gamma())})
  output$phi_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rphi_glm_gamma())})
  output$phi_glm_gamma_AIC <- renderText({ AIC(Rphi_glm_gamma())})
  output$phi_glm_log <- renderPrint({ summary(Rphi_glm_log())})
  output$phi_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rphi_glm_log())})
  output$phi_glm_log_AIC <- renderText({ AIC(Rphi_glm_log())})
# Fruit


# data outputs
  output$li_DT <- renderDT({
    RLi_data()
  })
  output$m_DT <- renderDT({
    Rm_data()
  })

# Fruit outputs
}

# run app
shinyApp(ui = ui, server = server)
