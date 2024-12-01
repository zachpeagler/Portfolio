### TOMATO INOCULANTS ###
##### Setup #####
#dependencies
library(shiny)
library(tidyverse)
library(scico)
library(showtext)
library(bslib)
library(bsicons)
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
            "Time.of.Day", "Leaf.Damage.", "Pests.Present.", "Light.Intensity..PAR.",
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
    Date_harvest = as.factor(date_harvest),
    Date_analysis = as.factor(date_analysis),
    d_diff = abs(as.integer(d_analysis)-as.integer(d_harvest)),
    plant_fac = as.factor(paste(row, plant)),
    Treatment = factor(Treatment, levels=c("Control","Germination",
                                           "Transplantation",
                                           "Germ+Trans")),
    BER = as.numeric(BER),
    fungus = as.numeric(fungus),
    cracking = as.numeric(cracking),
    ripeness = abs(1-(penetrometer/max(na.omit(penetrometer))))
  )
## make fruit variables
fruit_vars <- c("Treatment", "plant_fac",
                "BER", "fungus", "cracking",
                "Date_harvest", "Date_analysis", "d_diff",
                "mass", "sugar_avg", "ripeness")
## Get mass and sugar quantiles and IQR
Qmass <- quantile(na.omit(Fl_data$mass), probs=c(.25, .75), na.rm=FALSE)
Qsug <- quantile(na.omit(Fl_data$sugar_avg), probs=c(.25, .75), na.rm=FALSE)
iqr_mass <- IQR(na.omit(Fl_data$mass))
iqr_sug <- IQR(na.omit(Fl_data$sugar_avg))

## harvest vars
hvars <- c("fruit_sum", "mass_sum",
           "BER_sum", "fungus_sum", "cracking_sum",
           "pBER", "pfungus", "pcracking", "Treatment")

## themes
#light = bs_theme(bg = "white",
#                 "navbar-fg" = sls8[1],
#                 fg=sls8[1], primary = sls8[5],
#                 secondary = sls8[4],
#                 base_font = font_google("Open Sans"))
light = bs_theme(bootswatch = "sandstone")
dark = bs_theme(bootswatch = "darkly")

## popovers
gear <- popover(
  bs_icon("gear"),
  selectInput("palette","Select color palette",
              choices = p_palettes, selected = "bilbao"),
  checkboxInput("dark_mode", "Dark Mode", FALSE),
  title = "Options"
)
link_github <- tags$a(bs_icon("GitHub"),
                      href = "https://github.com/zachpeagler/Portfolio/tree/main/Apps/tomato_inoculant_app")
###### UI #####
ui <- navbarPage(title = "Tomato Inoculants",
                 theme = light,
   nav_panel("Fluorescence",
      layout_sidebar(sidebar = sidebar(
        markdown("##### **Fluorescence Options**"),
        sliderInput("leak_pct",
                    "Maximum allowable leak percent - Li-600 only",
                    min = 0,
                    max = 100,
                    value =10),
        checkboxInput("outliers", "Exclude outliers", FALSE)
      ), # end sidebar
     tabsetPanel(
       tabPanel("Distributions",
        card(card_header("Li-600", class = "bg-primary"),
        card(card_header("Stomatal Conductance (gsw)",
                         class = "bg-secondary"),
             card(layout_sidebar(sidebar = sidebar(open = FALSE,
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
            card(layout_sidebar(sidebar = sidebar(open = FALSE,
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
                 card(layout_sidebar(sidebar = sidebar(open = FALSE,
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
                 card(layout_sidebar(sidebar = sidebar(open = FALSE,
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
                       selectInput("fluoro_size","Size Variable",
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
                       selectInput("m_size","Size Variable",
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
       tabPanel("Statistics",
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
            card(card_header("Open Reaction Center Efficiency in Light (FvP/FmP)", class = "bg-secondary"),
                 card_body("Based on the exploratory analysis, FvP/FmP is heteroscedastic and 
                           while significantly different from all continuous distributions, is most
                           similar to the normal, lognormal, and gamma distributions. I'm going to test it 
                           against the gamma and lognormal."),
        card_body(layout_column_wrap(
          card(card_header("GLM with Gamma family and log link"),
               card_body(verbatimTextOutput("fvp_glm_gamma"), max_height= 500),
               card_body(layout_column_wrap(
                 card(card_header("Pseudo-R2"),
                      card_body(verbatimTextOutput("fvp_glm_gamma_r2"))),
                 value_box(title = "AIC", value = textOutput("fvp_glm_gamma_AIC"))
               ))
          ),
          card(card_header("GLM with gaussian family and log link"),
               card_body(verbatimTextOutput("fvp_glm_log"), max_height= 500),
               card_body(layout_column_wrap(
                 card(card_header("Pseudo-R2"),
                      card_body(verbatimTextOutput("fvp_glm_log_r2"))),
                 value_box(title = "AIC", value = textOutput("fvp_glm_log_AIC"))
               ))
          )
        ))), # end fvp card
        card(card_header("Photosystem II Efficiency (PhiPS2)", class = "bg-secondary"),
             card_body("Based on the exploratory analysis, PhiPS2 from the Multispeq is not significantly 
                       different from the normal, lognormal, or gamma distributions. It is also heteroscedastic 
                       as determined by Levene and Bartlett tests. Based on this, PhiPS2 will be analyzed in a GLMER with
                       a lognormal or gamma distribution, using the same model parameters as the previous PhiPS2 model."),
             card_body(layout_column_wrap(
               card(card_header("GLM with Gamma family and log link"),
                    card_body(verbatimTextOutput("p2_glm_gamma"), max_height= 500),
                    card_body(layout_column_wrap(
                      card(card_header("Pseudo-R2"),
                           card_body(verbatimTextOutput("p2_glm_gamma_r2"))),
                      value_box(title = "AIC", value = textOutput("p2_glm_gamma_AIC"))
                    ))
               ),
               card(card_header("GLM with gaussian family and log link"),
                    card_body(verbatimTextOutput("p2_glm_log"), max_height= 500),
                    card_body(layout_column_wrap(
                      card(card_header("Pseudo-R2"),
                           card_body(verbatimTextOutput("p2_glm_log_r2"))),
                      value_box(title = "AIC", value = textOutput("p2_glm_log_AIC"))
                    ))
               )
             ))
        ) # end P2 card
       ) # end multispeq section
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
    ) # end sidebar layout
   ), # end fluorescence page

  nav_panel("Fruit",
    layout_sidebar(
      sidebar = sidebar(
        markdown("##### **Fruit Options**"),
        checkboxInput("omitna", "Exclude fruit not analyzed for sugar",
                      FALSE),
        checkboxInput("ber", "Exclude fruit w/ blossom end rot",
                      FALSE),
        sliderInput("min_mass",
                    "Minimum Tomato Mass (g)",
                    min = 0,
                    max = 100,
                    value = 0 ),
        checkboxInput("outliers", "Exclude outliers", FALSE)
      ), # end sidebar
    tabsetPanel(
      tabPanel("Distributions",
        card(card_header("Mass", class = "bg-primary"),
             card(layout_sidebar(sidebar = sidebar(open = FALSE,
                 checkboxGroupInput("mass_dists", "Distributions",
                  choices = dists, selected = c("normal", "lognormal", "gamma"))
                 ),
          card_body(layout_column_wrap(
            card(card_header("Probability Density Function Plot"),
              card_body(plotOutput("mass_pdf"))),
            card(card_header("Cumulative Distribution Function Plot"),
              card_body(plotOutput("mass_cdf")))
          ))
        )),
        card(layout_column_wrap(
          card(card_header("One-sample Kolmogorov-Smirnov Tests"),
               verbatimTextOutput("mass_mKS")
          ),
          card(card_header("Tests for Homogeneity of Variances"),
               card(card_header("Levene Test"),
                    verbatimTextOutput("mass_levene")
               ),
               card(card_header("Bartlett Test"),
                    verbatimTextOutput("mass_bartlett")
               )
          )
        ))
      ),
      card(card_header("Sugar", class = "bg-primary"),
           card(layout_sidebar(sidebar = sidebar(open = FALSE,
             checkboxGroupInput("sug_dists", "Distributions",
               choices = dists, selected = c("normal", "lognormal", "gamma"))
           ),
           card_body(layout_column_wrap(
             card(card_header("Probability Density Function Plot"),
                  card_body(plotOutput("sug_pdf"))),
             card(card_header("Cumulative Distribution Function Plot"),
                  card_body(plotOutput("sug_cdf")))
           ))
           )),
           card(layout_column_wrap(
             card(card_header("One-sample Kolmogorov-Smirnov Tests"),
                  verbatimTextOutput("sug_mKS")
             ),
             card(card_header("Tests for Homogeneity of Variances"),
                  card(card_header("Levene Test"),
                       verbatimTextOutput("sug_levene")
                  ),
                  card(card_header("Bartlett Test"),
                       verbatimTextOutput("sug_bartlett")
                  )
             )
           ))
      )
    ), # end distributions tab
      tabPanel("Plots",
               card(card_header("Interactive Scatter Plot",
                                class = "bg-primary"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("fruit_x","X Variable",
                                  choices = fruit_vars, selected = "Date_analysis"),
                      selectInput("fruit_y","Y Variable",
                                  choices = fruit_vars, selected = "mass"),
                      selectInput("fruit_col","Color Variable",
                                  choices = fruit_vars, selected = "BER"),
                      selectInput("fruit_size","Size Variable",
                                  choices = fruit_vars, selected = "sugar_avg"),
                      sliderInput("fruit_jit", "Jitter Amount", min=0, max=10, value =3),
                      checkboxInput("fruit_fwrap", "Individual Plot Per Treatment", FALSE)
                    ),
                    card_body(plotOutput("fruit_scatter")))
               ),
               card(card_header("Boxplots", class = "bg-primary"),
                    checkboxInput("fruit_box_stats", "Show Statistical Tests", FALSE),
                    selectInput("fb_labels", "Show P symbols or P values",
                                sigcodes, "Pvalues"),
                    card_body(layout_column_wrap(
                      card(card_header("Mass", class = "bg-secondary"),
                           card_body(plotOutput("mass_box"))),
                      card(card_header("Sugar Concentration", class = "bg-secondary"),
                           card_body(plotOutput("sug_box")))
                    )), # end card_body
                    card_body(layout_column_wrap(
                      card(card_header("Mass Grouped by Plant", class = "bg-secondary"),
                           card_body(plotOutput("massbp_box"))),
                      card(card_header("Sugar Concentration Grouped by Plant", class = "bg-secondary"),
                           card_body(plotOutput("sugbp_box")))
                    )), # end card_body
                    card(markdown("The above boxplots use [Kruskal-Wallis tests](https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_test)
                                  for the comparison of all group means and [Mann-Whitney U tests](https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test)
                                  for pairwise comparisons between treatment groups."))
               ) # end sidebar layout and card
      ),
      tabPanel("Statistics",
               card(card_header("Mass", class = "bg-secondary"),
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
                 ),
                 card(card_header("Linear Model"),
                      card_body(verbatimTextOutput("mass_glm_normal"), max_height= 500),
                      card_body(layout_column_wrap(
                        card(card_header("Pseudo-R2"),
                             card_body(verbatimTextOutput("mass_glm_normal_r2"))),
                        value_box(title = "AIC", value = textOutput("mass_glm_normal_AIC"))
                      ))
                 )
               ))), # end mass
               card(card_header("Sugar Concentration (%)", class = "bg-secondary"),
                    card_body("Based on the exploratory analysis, sugar is probably normal."),
                    card_body(layout_column_wrap(
                      card(card_header("GLM with Gamma family and log link"),
                           card_body(verbatimTextOutput("sug_glm_gamma"), max_height= 500),
                           card_body(layout_column_wrap(
                             card(card_header("Pseudo-R2"),
                                  card_body(verbatimTextOutput("sug_glm_gamma_r2"))),
                             value_box(title = "AIC", value = textOutput("sug_glm_gamma_AIC"))
                           ))
                      ),
                      card(card_header("GLM with gaussian family and log link"),
                           card_body(verbatimTextOutput("sug_glm_log"), max_height= 500),
                           card_body(layout_column_wrap(
                             card(card_header("Pseudo-R2"),
                                  card_body(verbatimTextOutput("sug_glm_log_r2"))),
                             value_box(title = "AIC", value = textOutput("sug_glm_log_AIC"))
                           ))
                      ),
                      card(card_header("Linear Model"),
                           card_body(verbatimTextOutput("sug_glm_normal"), max_height= 500),
                           card_body(layout_column_wrap(
                             card(card_header("Pseudo-R2"),
                                  card_body(verbatimTextOutput("sug_glm_normal_r2"))),
                             value_box(title = "AIC", value = textOutput("sug_glm_normal_AIC"))
                           ))
                      )
                    ))) # end sugar
               ),
      tabPanel("Data",
               card(card_header("Fruit Data", class = "bg-primary"),
                    DTOutput("fruit_data")
                    ),
               card(card_header("Fruit Data Grouped By Plant", class = "bg-secondary"),
                    DTOutput("fruit_databp")
               )
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
                      **mass** is the mass in grams of the tomato, measured on an Ohaus Scout. (~10:~400) <br>
                      **BER** corresponds to whether or not the tomato has blossom end rot, a disease caused by calcium deficiency that renders the fruit unmarketable. (0,1) <br>
                      **fungus** corresponds to whether or not the tomato has fungus growing on it. This is common on fruit with BER. (0,1) <br>
                      **cracking** corresponds to whether or not the tomato has cracks in its skin (0,1) <br>
                      **penetrometer** corresponds to the force in kilograms it takes to penetrate the flesh of the tomato (~0.5:~4) <br>
                      **ripeness** is the **penetrometer** value mapped from 0:1 and reversed, so that riper fruit are closer to 1 and unripe fruit are closer to 0. (0:1) <br>
                      **sugar.1** and **sugar.2** are measurements of the tomato juice's sugar concentration taken on a Fisher BRIX Refractometer (~2:~12) <br>
                      **sugar_avg** is the average of the two sugar measurements. <br>")))
               ) # end info panel
    ) # end tab set
    ) # end sidebar layout
  ), # end fruit page
  nav_panel("Harvest",
    layout_sidebar(
      sidebar = sidebar(
        numericInput("nbins", "Number of Mass Bins", 10),
      selectInput("h_var", "Distribution Variable", choices = hvars,
                  selected = "fruit_sum"
      )
      ),
      tabsetPanel(
        tabPanel(title = "Distributions",
                 card(card_header("Harvest by Plant", class = "bg-primary"),
                      card(layout_sidebar(sidebar = sidebar(open = FALSE,
                        checkboxGroupInput("hbp_dists", "Distributions",
                           choices = dists, selected = c("normal", "lognormal", "gamma"))
                      ),
                      card_body(layout_column_wrap(
                        card(card_header("Probability Density Function Plot"),
                             card_body(plotOutput("hbp_pdf"))),
                        card(card_header("Cumulative Distribution Function Plot"),
                             card_body(plotOutput("hbp_cdf")))
                      ))
                      )),
                      card_body(layout_column_wrap(
                        card(card_header("One-sample Kolmogorov-Smirnov Tests"),
                             verbatimTextOutput("hbp_mKS")
                        ),
                        card(card_header("Tests for Homogeneity of Variances"),
                             card(card_header("Levene Test"),
                                  verbatimTextOutput("hbp_levene")
                             ),
                             card(card_header("Bartlett Test"),
                                  verbatimTextOutput("hbp_bartlett")
                             )
                        )
                      ))
                 ), # end HBP card
                 card(card_header("Harvest by Mass Bin", class = "bg-primary"),
                      card(layout_sidebar(sidebar = sidebar(open = FALSE,
                        checkboxGroupInput("hmb_dists", "Distributions",
                                           choices = dists, selected = c("normal", "lognormal", "gamma"))
                      ),
                      card_body(layout_column_wrap(
                        card(card_header("Probability Density Function Plot"),
                             card_body(plotOutput("hmb_pdf"))),
                        card(card_header("Cumulative Distribution Function Plot"),
                             card_body(plotOutput("hmb_cdf")))
                      ))
                      )),
                      card_body(layout_column_wrap(
                        card(card_header("One-sample Kolmogorov-Smirnov Tests"),
                             verbatimTextOutput("hmb_mKS")
                        ),
                        card(card_header("Tests for Homogeneity of Variances"),
                             card(card_header("Levene Test"),
                                  verbatimTextOutput("hmb_levene")
                             ),
                             card(card_header("Bartlett Test"),
                                  verbatimTextOutput("hmb_bartlett")
                             )
                        )
                      ))
                 ) # end HBP card
                 ),
        tabPanel(title = "Plots",
         card(card_header("Harvest by Plant Bar Plot",
                          class = "bg-primary"),
          layout_sidebar(sidebar = sidebar(
            selectInput("hbp_y","Y Variable",
                        choices = hvars, selected = "mass"),
            selectInput("hbp_color","Color Variable",
                        choices = hvars, selected = "BER"),
            checkboxInput("hbp_fwrap", "Individual Plot Per Treatment", FALSE)
          ),
          card_body(plotOutput("hbp_col")))
         ),
         card(card_header("Harvest by Mass Bin Bar Plot",
                          class = "bg-primary"),
              layout_sidebar(sidebar = sidebar(
                selectInput("hmb_y","Y Variable",
                            choices = hvars, selected = "mass"),
                selectInput("hmb_color","Color Variable",
                            choices = hvars, selected = "BER"),
                checkboxInput("hmb_fwrap", "Individual Plot Per Treatment", FALSE)
              ),
              card_body(plotOutput("hmb_col")))
         ),
        ),
        tabPanel(title = "Statistics",
        ),
        tabPanel(title = "Data",
          card(card_header("Harvest Data", class = "bg-primary"),
               DTOutput("fl_sums")
          ),
          card(card_header("Harvest Data By Plant", class = "bg-primary"),
               DTOutput("fl_sums_bp")
          ),
          card(card_header("Harvest Data by Mass Bin", class = "bg-primary"),
               DTOutput("fl_mb")
          )
        ),
        tabPanel(title = "Info",
        ),
      ) # end tabset
    ) # end sidebar layout
  ), # end navpanel
  nav_spacer(),
  nav_item(
    gear
  ),
  nav_item(link_github)
) # end ui

##### server #####
server <- function(input, output, session) {
# theme observer
observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
))

# Reactive Values
## (it would be better to use actual ReactiveValues objects, but it's far enough
## along in the projec that I'm just going to keep doing it this way)
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
  Rmass_dists <- reactive({input$mass_dists})
  Rsug_dists <- reactive({input$sug_dists})
  Rfruit_x <- reactive({input$fruit_x})
  Rfruit_y <- reactive({input$fruit_y})
  Rfruit_col <- reactive({input$fruit_col})
  Rfruit_jit <- reactive({input$fruit_jit * .1})
  Rfruit_fwrap <- reactive({input$fruit_fwrap})
  Rfruit_box_stats <- reactive({input$fruit_box_stats})
  Rfb_labels <- reactive({input$fb_labels})
  Rnbins <- reactive({input$nbins})
  Rhbp_dists <- reactive({input$hbp_dists})
  Rhmb_dists <- reactive({input$hmb_dists})
  Rh_var <- reactive({input$h_var})
  Rhbp_y <- reactive ({input$hbp_y})
  Rhmb_y <- reactive ({input$hmb_y})
  Rhbp_color <- reactive({input$hbp_color})
  Rhmb_color <- reactive({input$hmb_color})
  Rhbp_fwrap <- reactive({input$hbp_fwrap})
  Rhmb_fwrap <- reactive({input$hmb_fwrap})
## Reactive dataframes
### Li-600 data
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
### Multispeq data
  Rm_data <- reactive({
    x <- m_data
    if (input$outliers == TRUE) {
      x <- subset(x, Phi2 > (Qp2[1]-1.5*iqr_p2) &
                    Phi2 < (Qp2[2]+1.5*iqr_p2))
    }
    return(x)
  })
### Fruit data
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
# fruit data by plant
  RFlbp_data <- reactive({
    f <- na.omit(RFl_data()) %>%
      group_by(Treatment, plant_fac) %>%
      summarise_at(vars(mass, sugar_avg), list(mean=mean, sd=sd))
    return(f)
  })
# fruit data summary
  RFl_summary <- reactive({
    f <- RFl_data() %>%
      group_by(Treatment) %>%
      summarise_at(vars(fruit, mass, BER, fungus, cracking, mass),
                   list(sum=sum)) %>%
      mutate(pBER = round(BER_sum/fruit_sum, 4),
             pfungus = round(fungus_sum/fruit_sum, 4),
             pcracking = round(cracking_sum/fruit_sum, 4)
      )
    return(f)
  })
# fruit data summary by plant
  RFlbp_summary <- reactive({
    f <- RFl_data() %>%
      group_by(Treatment, plant_fac) %>%
      summarise_at(vars(fruit, mass, BER, fungus, cracking, mass),
                   list(sum=sum)) %>%
      mutate(pBER = round(BER_sum/fruit_sum, 4),
             pfungus = round(fungus_sum/fruit_sum, 4),
             pcracking = round(cracking_sum/fruit_sum, 4)
      )
    return(f)
  })
# fruit data 
  RFl_data_mb <- reactive({
    f <- RFl_data() %>%
      mutate(mass_bin = cut(mass, breaks=Rnbins()))
    f <- f %>%
      group_by(Treatment, mass_bin) %>%
      summarise_at(vars(fruit, mass, BER, fungus, cracking), list(sum=sum)) %>%
      mutate(pBER = round(BER_sum/fruit_sum, 4),
             pfungus = round(fungus_sum/fruit_sum, 4),
             pcracking = round(cracking_sum/fruit_sum, 4)
      )
    return(f)
  })
## Reactive GLMs
### Li-600
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
### Multispeq
#### FvP/FmP
  Rfvp_glm_gamma <- reactive({
    glmer(FvP_over_FmP ~ Treatment + Ambient.Humidity + (1 | plant_fac),
          data = Rm_data(), family = Gamma(link = "log"))
  })
  Rfvp_glm_log <- reactive({
    glmer(FvP_over_FmP ~ Treatment + Ambient.Humidity + (1 | plant_fac),
          data = Rm_data(), family = gaussian(link = "log"))
  })
#### Phi2
  Rp2_glm_gamma <- reactive({
    glmer(Phi2 ~ Treatment + Light.Intensity..PAR. + (1 | plant_fac),
          data = Rm_data(), family = Gamma(link = "log"))
  })
  Rp2_glm_log <- reactive({
    glmer(Phi2 ~ Treatment + Light.Intensity..PAR. + (1 | plant_fac),
          data = Rm_data(), family = gaussian(link = "log"))
  })
### Fruit
#### Mass
  Rmass_glm_gamma <- reactive({
    glmer(mass ~ Treatment + (1 | plant_fac),
          data = RFl_data(), family = Gamma(link = "log"))
  })
  Rmass_glm_log <- reactive({
    glmer(mass ~ Treatment + (1 | plant_fac),
          data = RFl_data(), family = gaussian(link = "log"))
  })
  Rmass_glm_normal <- reactive({
    lmer(mass ~ Treatment + (1 | plant_fac),
         data = RFl_data())
  })
#### Mass
  Rsug_glm_gamma <- reactive({
    glmer(sugar_avg ~ Treatment + (1 | plant_fac),
          data = RFl_data(), family = Gamma(link = "log"))
  })
  Rsug_glm_log <- reactive({
    glmer(sugar_avg ~ Treatment + (1 | plant_fac),
          data = RFl_data(), family = gaussian(link = "log"))
  })
  Rsug_glm_normal <- reactive({
    lmer(sugar_avg ~ Treatment + (1 | plant_fac),
         data = RFl_data())
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
        legend.position="bottom",
        legend.title.position = "top",
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
        legend.position="bottom",
        legend.title.position = "top",
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
        legend.position="bottom",
        legend.title.position = "top",
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
        legend.position="bottom",
        legend.title.position = "top",
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
        legend.position="bottom",
        legend.title.position = "top",
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
        legend.position="bottom",
        legend.title.position = "top",
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
        legend.position="bottom",
        legend.title.position = "top",
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
            theme_bw()+
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
      theme_bw()+
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
      theme_bw()+
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
      theme_bw()+
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
      theme_bw()+
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
      theme_bw()+
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
## Li-600
### gsw statistical outputs
  output$gsw_glm_gamma <- renderPrint({ summary(Rgsw_glm_gamma())})
  output$gsw_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rgsw_glm_gamma())})
  output$gsw_glm_gamma_AIC <- renderText({ AIC(Rgsw_glm_gamma())})
  output$gsw_glm_log <- renderPrint({ summary(Rgsw_glm_log())})
  output$gsw_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rgsw_glm_log())})
  output$gsw_glm_log_AIC <- renderText({ AIC(Rgsw_glm_log())})
###PhiPS2 statistical outputs
  output$phi_glm_gamma <- renderPrint({ summary(Rphi_glm_gamma())})
  output$phi_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rphi_glm_gamma())})
  output$phi_glm_gamma_AIC <- renderText({ AIC(Rphi_glm_gamma())})
  output$phi_glm_log <- renderPrint({ summary(Rphi_glm_log())})
  output$phi_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rphi_glm_log())})
  output$phi_glm_log_AIC <- renderText({ AIC(Rphi_glm_log())})
## Multispeq
### FvP/FmP statistical outputs
  output$fvp_glm_gamma <- renderPrint({ summary(Rfvp_glm_gamma())})
  output$fvp_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rfvp_glm_gamma())})
  output$fvp_glm_gamma_AIC <- renderText({ AIC(Rfvp_glm_gamma())})
  output$fvp_glm_log <- renderPrint({ summary(Rfvp_glm_log())})
  output$fvp_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rfvp_glm_log())})
  output$fvp_glm_log_AIC <- renderText({ AIC(Rfvp_glm_log())})
###Phi2 statistical outputs
  output$p2_glm_gamma <- renderPrint({ summary(Rp2_glm_gamma())})
  output$p2_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rp2_glm_gamma())})
  output$p2_glm_gamma_AIC <- renderText({ AIC(Rp2_glm_gamma())})
  output$p2_glm_log <- renderPrint({ summary(Rp2_glm_log())})
  output$p2_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rp2_glm_log())})
  output$p2_glm_log_AIC <- renderText({ AIC(Rp2_glm_log())})
## data outputs
  output$li_DT <- renderDT({
    RLi_data()
  })
  output$m_DT <- renderDT({
    Rm_data()
  })

# Fruit
## Distributions
### Mass
#### mass multiKS output
output$mass_mKS <- renderPrint({
  x <- RFl_data()$mass
  ds <- Rmass_dists()
  o <- multiKS_cont(x, ds)
  return(o)
})
#### mass PDF plot
output$mass_pdf <- renderPlot({
  x <- RFl_data()$mass
  ds <- Rmass_dists()
  p <- multiPDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("PDF")+
    xlab("Mass")+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
}) # end mass PDF output
#### mass CDF Plot
output$mass_cdf <- renderPlot({
  x <- RFl_data()$mass
  ds <- Rmass_dists()
  p <- multiCDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("CDF")+
    xlab("Mass")+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
})

# mass homoscedasticity tests
output$mass_levene <- renderPrint({
  leveneTest(mass~Treatment, data=RFl_data())
})
output$mass_bartlett <- renderPrint({
  bartlett.test(mass~Treatment, data=RFl_data())
})
### Sugar
#### sug multiKS output
output$sug_mKS <- renderPrint({
  x <- na.omit(RFl_data()$sugar_avg)
  ds <- Rsug_dists()
  o <- multiKS_cont(x, ds)
  return(o)
})
#### mass PDF plot
output$sug_pdf <- renderPlot({
  x <- na.omit(RFl_data()$sugar_avg)
  ds <- Rsug_dists()
  p <- multiPDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("PDF")+
    xlab("Sugar Concentration")+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
}) # end mass PDF output
#### mass CDF Plot
output$sug_cdf <- renderPlot({
  x <- na.omit(RFl_data()$sugar_avg)
  ds <- Rsug_dists()
  p <- multiCDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("CDF")+
    xlab("Sugar Concentration")+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
})

# mass homoscedasticity tests
output$sug_levene <- renderPrint({
  leveneTest(sugar_avg~Treatment, data=RFl_data())
})
output$sug_bartlett <- renderPrint({
  bartlett.test(sugar_avg~Treatment, data=RFl_data())
})

## fruit interactive scatter plot
output$fruit_scatter <- renderPlot({
  fs <- ggplot(data = RFl_data(), aes(x=.data[[Rfruit_x()]], y = .data[[Rfruit_y()]],
                                      color = .data[[Rfruit_col()]])) +
    geom_jitter(width=Rfruit_jit())+
    scale_color_scico(begin=0.9, end=0, palette=Rpalette())+
    scale_x_discrete(guide=guide_axis(check.overlap=TRUE))+
    theme_bw()+
    ylab(gettext(Rfruit_y()))+
    xlab(gettext(Rfruit_x()))+
    theme(
      text = element_text(size=24, family="mont"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
      title = element_text(size=30, family="open", face="bold", lineheight = .5)
    )
  if (Rfruit_fwrap() ==TRUE){
    fs <- fs + facet_wrap(~Treatment)
  }
  return(fs)
})

## mass boxplot
output$mass_box <- renderPlot({
  pbox <- ggplot(data = RFl_data(), aes(x= Treatment, y = mass, fill=Treatment, color=Treatment)) +
    geom_boxplot(alpha=.5, width=0.25)+
    geom_violin(alpha=0.5, width=1)+
    geom_jitter( width=.2, height=0)+
    scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
    scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
    ylab("Mass (g)")+
    theme_bw()+
    theme(
      legend.position="none",
      text = element_text(size=24, family="mont", lineheight=0.5),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
      title = element_text(size=30, family="open", face="bold")
    )
  if (Rfruit_box_stats() == TRUE) {
    if (Rfb_labels() == "Pvalues") {
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

## sugar boxplot
output$sug_box <- renderPlot({
  pbox <- ggplot(data = RFl_data(), aes(x= Treatment, y = sugar_avg, fill=Treatment, color=Treatment)) +
    geom_boxplot(alpha=.5, width=0.25)+
    geom_violin(alpha=0.5, width=1)+
    geom_jitter( width=.2, height=0)+
    scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
    scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
    ylab("Sugar Concentration (%)")+
    theme_bw()+
    theme(
      legend.position="none",
      text = element_text(size=24, family="mont", lineheight=0.5),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
      title = element_text(size=30, family="open", face="bold")
    )
  if (Rfruit_box_stats() == TRUE) {
    if (Rfb_labels() == "Pvalues") {
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

## mass by plant boxplot
output$massbp_box <- renderPlot({
  pbox <- ggplot(data = RFlbp_data(), aes(x= Treatment, y = mass_mean, fill=Treatment, color=Treatment)) +
    geom_boxplot(alpha=.5, width=0.25)+
    geom_violin(alpha=0.5, width=1)+
    geom_jitter( width=.2, height=0)+
    scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
    scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
    ylab("Mean Mass (g)")+
    theme_bw()+
    theme(
      legend.position="none",
      text = element_text(size=24, family="mont", lineheight=0.5),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
      title = element_text(size=30, family="open", face="bold")
    )
  if (Rfruit_box_stats() == TRUE) {
    if (Rfb_labels() == "Pvalues") {
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

## sugar by plant boxplot
output$sugbp_box <- renderPlot({
  pbox <- ggplot(data = RFlbp_data(), aes(x= Treatment, y = sugar_avg_mean, fill=Treatment, color=Treatment)) +
    geom_boxplot(alpha=.5, width=0.25)+
    geom_violin(alpha=0.5, width=1)+
    geom_jitter( width=.2, height=0)+
    scale_fill_scico_d(begin=0.9, end=0, palette=Rpalette())+
    scale_color_scico_d(begin=0.9, end=0, palette=Rpalette())+
    ylab("Mean Sugar Concentration (%)")+
    theme_bw()+
    theme(
      legend.position="none",
      text = element_text(size=24, family="mont", lineheight=0.5),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
      title = element_text(size=30, family="open", face="bold")
    )
  if (Rfruit_box_stats() == TRUE) {
    if (Rfb_labels() == "Pvalues") {
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
# Fruit statistial outputs
### Mass statistical outputs
output$mass_glm_gamma <- renderPrint({ summary(Rmass_glm_gamma())})
output$mass_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rmass_glm_gamma())})
output$mass_glm_gamma_AIC <- renderText({ AIC(Rmass_glm_gamma())})
output$mass_glm_log <- renderPrint({ summary(Rmass_glm_log())})
output$mass_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rmass_glm_log())})
output$mass_glm_log_AIC <- renderText({ AIC(Rmass_glm_log())})
output$mass_glm_normal <- renderPrint({ summary(Rmass_glm_normal())})
output$mass_glm_normal_r2 <- renderPrint({ r.squaredGLMM(Rmass_glm_normal())})
output$mass_glm_normal_AIC <- renderText({ AIC(Rmass_glm_normal())})
### sugar statistical outputs
output$sug_glm_gamma <- renderPrint({ summary(Rsug_glm_gamma())})
output$sug_glm_gamma_r2 <- renderPrint({ r.squaredGLMM(Rsug_glm_gamma())})
output$sug_glm_gamma_AIC <- renderText({ AIC(Rsug_glm_gamma())})
output$sug_glm_log <- renderPrint({ summary(Rsug_glm_log())})
output$sug_glm_log_r2 <- renderPrint({ r.squaredGLMM(Rsug_glm_log())})
output$sug_glm_log_AIC <- renderText({ AIC(Rsug_glm_log())})
output$sug_glm_normal <- renderPrint({ summary(Rsug_glm_normal())})
output$sug_glm_normal_r2 <- renderPrint({ r.squaredGLMM(Rsug_glm_normal())})
output$sug_glm_normal_AIC <- renderText({ AIC(Rsug_glm_normal())})

# fruit data output
output$fruit_data <- renderDT({
  RFl_data()
})
output$fruit_databp <- renderDT({
  RFlbp_data()
})

# HARVEST

## Distributions
### Harvest by plant
output$hbp_pdf <- renderPlot({
  v <- Rh_var()
  x <- RFlbp_summary()[[v]]
  ds <- Rhbp_dists()
  p <- multiPDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("PDF")+
    xlab(gettext(Rh_var()))+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
})
output$hbp_cdf <- renderPlot({
  v <- Rh_var()
  x <- RFlbp_summary()[[v]]
  ds <- Rhbp_dists()
  p <- multiCDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("CDF")+
    xlab(gettext(Rh_var()))+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
})
### distribution tests
#### KS test
output$hbp_mKS <- renderPrint({
  v <- Rh_var()
  x <- RFlbp_summary()[[v]]
  ds <- Rhbp_dists()
  o <- multiKS_cont(x, ds)
  return(o)
})
#### homoscedasticity tests
output$hbp_levene <- renderPrint({
  v <- Rh_var()
  leveneTest(RFlbp_summary()[[v]]~Treatment, data=RFlbp_summary())
})
output$hbp_bartlett <- renderPrint({
  v <- Rh_var()
  bartlett.test(RFlbp_summary()[[v]]~Treatment, data=RFlbp_summary())
})
### harvest mass bin
output$hmb_pdf <- renderPlot({
  v <- Rh_var()
  x <- RFl_data_mb()[[v]]
  ds <- Rhmb_dists()
  p <- multiPDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("PDF")+
    xlab(gettext(Rh_var()))+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
})
output$hmb_cdf <- renderPlot({
  v <- Rh_var()
  x <- RFl_data_mb()[[v]]
  ds <- Rhmb_dists()
  p <- multiCDF_plot(x, 100, ds)+
    labs(title="")+
    theme_bw()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab("CDF")+
    xlab(gettext(Rh_var()))+
    theme(
      text = element_text(size=20, family="mont"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = element_text(size=24, family = "mont", face= "bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
    )
  return(p)
})
### distribution tests
#### KS test
output$hmb_mKS <- renderPrint({
  v <- Rh_var()
  x <- RFl_data_mb()[[v]]
  ds <- Rhmb_dists()
  o <- multiKS_cont(x, ds)
  return(o)
})
#### homoscedasticity tests
output$hmb_levene <- renderPrint({
  v <- Rh_var()
  leveneTest(RFl_data_mb()[[v]]~Treatment, data=RFl_data_mb())
})
output$hmb_bartlett <- renderPrint({
  v <- Rh_var()
  bartlett.test(RFl_data_mb()[[v]]~Treatment, data=RFl_data_mb())
})
## Plots
output$hbp_col <- renderPlot({
  v <- Rhbp_y()
  x <- RFlbp_summary()[[v]]
  p <- ggplot(data=RFlbp_summary(), aes(x=plant_fac, y=x, fill=.data[[Rhbp_color()]]))+
    geom_col()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab(gettext(Rh_var()))+
    xlab("Plant")+
    theme_minimal()+
    theme(
      legend.position="right",
      text = element_text(size=24, family="mont", face="bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
      axis.text.x = element_text(angle = 45, hjust=1, vjust=1)
    )
  if (Rhbp_fwrap() == TRUE) {
    p <- p + facet_wrap(~Treatment)
  }
  return(p)
})

output$hmb_col <- renderPlot({
  v <- Rhmb_y()
  x <- RFl_data_mb()[[v]]
  p <- ggplot(data=RFl_data_mb(), aes(x=mass_bin, y=x, fill=.data[[Rhmb_color()]]))+
    geom_col()+
    scale_color_scico_d(begin=0.9, end=0, palette = gettext(Rpalette()))+
    ylab(gettext(Rh_var()))+
    xlab("Mass Bin")+
    theme_minimal()+
    theme(
      legend.position="right",
      text = element_text(size=24, family="mont", face="bold"),
      axis.title = element_text(size=24, family = "mont", face= "bold"),
      axis.text.x = element_text(angle = 45, hjust=1, vjust=1)
    )
  if (Rhmb_fwrap() == TRUE) {
    p <- p + facet_wrap(~Treatment)
  }
  return(p)
})

## Stats

## Data
output$fl_sums <- renderDT({
  RFl_summary()
})
output$fl_sums_bp <- renderDT({
  RFlbp_summary()
})
output$fl_mb <- renderDT({
  RFl_data_mb()
})
} # end server

# run app
shinyApp(ui = ui, server = server)
