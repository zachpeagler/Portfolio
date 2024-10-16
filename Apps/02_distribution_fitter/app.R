# Distribution Fitter
## App A Day - Day 2
## Zach Peagler
## 10/13/2024

# dependencies
library(shiny)
library(bslib)
library(tidyverse)
library(MASS)
library(scico)
library(DT)
library(plotly)

# load example data
deployment_file <- "eggplant_data.csv"
#tfile <- "C:/Github/App-A-Day/02_distribution_fitter/eggplant_data.csv"
dat <- read.csv(deployment_file)

# make data objects
## variables to filter data with
waterfilters <- c("Control",
                  "Deficit",
                  "None")
phasefilters <- c("recovery",
                  "drought",
                  "None")
varietyfilters <- c("Mara",
                    "2778",
                    "2789",
                    "4841",
                    "None")
## variables to fit the distribution of
vars <- colnames(dat[,5:31])
## color palettes
pals <- scico_palette_names()
## continuous distributions
dists <- c("normal", "lognormal", "gamma", "exponential",
                   "cauchy", "t", "weibull", "logistic")
# functions
## cont_pdf
cont_pdf <- function(x, seq_length, distributions){
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  x_seq <- seq(min(x), max(x), length.out = seq_length+1)
  # create real density for x
  x_pdf <- density(x, n=seq_length+1)
  # initialize df of x and the real density
  pdf_df <- as.data.frame(x_seq) %>%
    mutate(dens = x_pdf$y)
  if ("normal" %in% distributions == TRUE) {
    x_n <- fitdistr(x, "normal")
    x_pdf_n <- dnorm(x_seq, mean=x_n$estimate[1],
                     sd = x_n$estimate[2])
    pdf_df <- pdf_df %>% mutate(pdf_normal = x_pdf_n)
  }
  if ("lognormal" %in% distributions  == TRUE) {
    x_ln <- fitdistr(x, "lognormal")
    x_pdf_ln <- dlnorm(x_seq, meanlog=x_ln$estimate[1],
                       sdlog = x_ln$estimate[2])
    pdf_df <- pdf_df %>% mutate(pdf_lognormal = x_pdf_ln)
  }
  if ("gamma" %in% distributions  == TRUE) {
    x_g <- fitdistr(x, "gamma")
    x_pdf_g <- dgamma(x_seq, shape=x_g$estimate[1],
                      rate=x_g$estimate[2])
    pdf_df <- pdf_df %>% mutate(pdf_gamma = x_pdf_g)
  }
  if ("exponential" %in% distributions  == TRUE) {
    x_exp <- fitdistr(x, "exponential")
    x_pdf_exp <- dexp(x_seq, rate = x_exp$estimate)
    pdf_df <- pdf_df %>% mutate(pdf_exponential = x_pdf_exp)
  }
  if ("cauchy" %in% distributions  == TRUE) {
    x_cau <- fitdistr(x, "cauchy")
    x_pdf_cau <- dcauchy(x_seq, location=x_cau$estimate[1],
                         scale = x_cau$estimate[2])
    pdf_df <- pdf_df %>% mutate(pdf_cauchy = x_pdf_cau)
  }
  if ("t" %in% distributions  == TRUE) {
    x_t <- fitdistr(x, "t")
    x_pdf_t <- dt(x_seq, df = x_t$estimate[3])
    pdf_df <- pdf_df %>% mutate(pdf_t = x_pdf_t)
  }
  if ("weibull" %in% distributions  == TRUE) {
    x_wei <- fitdistr(x, "weibull")
    x_pdf_wei <- dweibull(x_seq, shape = x_wei$estimate[1],
                          scale = x_wei$estimate[2])
    pdf_df <- pdf_df %>% mutate(pdf_weibull = x_pdf_wei)
  }
  if ("logistic" %in% distributions  == TRUE) {
    x_logis <- fitdistr(x, "logistic")
    x_pdf_logis <- dlogis(x_seq, x_logis$estimate[1],
                          x_logis$estimate[2])
    pdf_df <- pdf_df %>% mutate(pdf_logistic = x_pdf_logis)
  }
  
  return(pdf_df)
}
## cont cdf
cont_cdf <- function(x, seq_length, distributions){
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  x_seq <- seq(min(x), max(x), length.out = seq_length+1)
  # create real cumulative density for x
  x_cdf <- ecdf(x)(x_seq)
  # initialize df of x and the cumulative density
  cdf_df <- as.data.frame(x_seq) %>%
    mutate(dens = x_cdf)
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential",
                       "cauchy",
                       "t",
                       "weibull",
                       "logistic")
  }
  
  if ("normal" %in% distributions) {
    x_n <- fitdistr(x, "normal")
    x_cdf_n <- pnorm(x_seq, mean=x_n$estimate[1],
                     sd = x_n$estimate[2])
    cdf_df <- cdf_df %>% mutate(cdf_normal = x_cdf_n)
  }
  if ("lognormal" %in% distributions) {
    x_ln <- fitdistr(x, "lognormal")
    x_cdf_ln <- plnorm(x_seq, meanlog=x_ln$estimate[1],
                       sdlog = x_ln$estimate[2])
    cdf_df <- cdf_df %>% mutate(cdf_lognormal = x_cdf_ln)
  }
  if ("gamma" %in% distributions) {
    x_g <- fitdistr(x, "gamma")
    x_cdf_g <- pgamma(x_seq, shape=x_g$estimate[1],
                      rate=x_g$estimate[2])
    cdf_df <- cdf_df %>% mutate(cdf_gamma = x_cdf_g)
  }
  if ("exponential" %in% distributions) {
    x_exp <- fitdistr(x, "exponential")
    x_cdf_exp <- pexp(x_seq, rate = x_exp$estimate)
    cdf_df <- cdf_df %>% mutate(cdf_exponential = x_cdf_exp)
  }
  if ("cauchy" %in% distributions) {
    x_cau <- fitdistr(x, "cauchy")
    x_cdf_cau <- pcauchy(x_seq, 
                         location = x_cau$estimate[1],
                         scale = x_cau$estimate[2])
    cdf_df <- cdf_df %>% mutate(cdf_cauchy = x_cdf_cau)
  }
  if ("t" %in% distributions) {
    x_t <- fitdistr(x, "t")
    x_cdf_t <- pt(x_seq, df = x_t$estimate[3])
    cdf_df <- cdf_df %>% mutate(cdf_t = x_cdf_t)
  }
  if ("weibull" %in% distributions) {
    x_wei <- fitdistr(x, "weibull")
    x_cdf_wei <- pweibull(x_seq, shape = x_wei$estimate[1],
                          scale = x_wei$estimate[2])
    cdf_df <- cdf_df %>% mutate(cdf_weibull = x_cdf_wei)
  }
  if ("logistic" %in% distributions) {
    x_logis <- fitdistr(x, "logistic")
    x_cdf_logis <- plogis(x_seq, x_logis$estimate[1],
                          x_logis$estimate[2])
    cdf_df <- cdf_df %>% mutate(cdf_logistic = x_cdf_logis)
  }
  
  return(cdf_df)
}
## multiKS function
multiKS <- function(x, distributions) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential",
                       "cauchy",
                       "t",
                       "weibull",
                       "logistic")
  }
  KS_df <- data.frame(matrix(ncol=3, nrow=0))
  colnames(KS_df) <- c("Distribution", "Distance", "P-Value")
  # check normal
  if ("normal" %in% distributions) {
    x_n <- fitdistr(x, "normal")
    x_KS_n <- ks.test(x, "pnorm", mean=x_n$estimate[1],
                      sd = x_n$estimate[2])
    KS_n <- data.frame(matrix(ncol=0, nrow=1))
    KS_n$Distribution <- "Normal"
    KS_n$Distance <- if (is.null(x_KS_n$statistic)
                         == FALSE) {x_KS_n$statistic}
    else {"NA"}
    KS_n$PValue <- if (is.null(x_KS_n$p.value)
                       == FALSE) {x_KS_n$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_n)
  }
  if ("lognormal" %in% distributions) {
    x_ln <- fitdistr(x, "lognormal")
    x_KS_ln <- ks.test(x, "plnorm",
                       meanlog=x_ln$estimate[1],
                       sdlog = x_ln$estimate[2])[c(1, 2)]
    KS_ln <- data.frame(matrix(ncol=0, nrow=1))
    KS_ln$Distribution <- "Lognormal"
    KS_ln$Distance <- if (is.null(x_KS_ln$statistic)
                          == FALSE) {x_KS_ln$statistic}
    else {"NA"}
    KS_ln$PValue <- if (is.null(x_KS_ln$p.value)
                        == FALSE) {x_KS_ln$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_ln)
  }
  if ("gamma" %in% distributions) {
    x_g <- fitdistr(x, "gamma")
    x_KS_g <- ks.test(x, "pgamma", shape=x_g$estimate[1],
                      rate=x_g$estimate[2])
    KS_g <- data.frame(matrix(ncol=0, nrow=1))
    KS_g$Distribution <- "Gamma"
    KS_g$Distance <- if (is.null(x_KS_g$statistic)
                         == FALSE) {x_KS_g$statistic}
    else {"NA"}
    KS_g$PValue <- if (is.null(x_KS_g$p.value)
                       == FALSE) {x_KS_g$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_g)
  }
  if ("exponential" %in% distributions) {
    x_exp <- fitdistr(x, "exponential")
    x_KS_exp <- ks.test(x, "pexp", rate = x_exp$estimate)
    KS_exp <- data.frame(matrix(ncol=0, nrow=1))
    KS_exp$Distribution <- "Exponential"
    KS_exp$Distance <- if (is.null(x_KS_exp$statistic)
                           == FALSE) {x_KS_exp$statistic}
    else {"NA"}
    KS_exp$PValue <- if (is.null(x_KS_exp$p.value)
                         == FALSE) {x_KS_exp$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_exp)
  }
  if ("cauchy" %in% distributions) {
    x_cau <- fitdistr(x, "cauchy")
    x_KS_cau <- ks.test(x, "pcauchy",
                        location = x_cau$estimate[1],
                        scale = x_cau$estimate[2])
    KS_cau <- data.frame(matrix(ncol=0, nrow=1))
    KS_cau$Distribution <- "Cauchy"
    KS_cau$Distance <- if (is.null(x_KS_cau$statistic)
                           == FALSE) {x_KS_cau$statistic}
    else {"NA"}
    KS_cau$PValue <- if (is.null(x_KS_cau$p.value)
                         == FALSE) {x_KS_cau$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_cau)
  }
  if ("t" %in% distributions) {
    x_t <- fitdistr(x, "t")
    x_KS_t <- ks.test(x, "pt", df = x_t$estimate[3])
    KS_t <- data.frame(matrix(ncol=0, nrow=1))
    KS_t$Distribution <- "t"
    KS_t$Distance <- if (is.null(x_KS_t$statistic)
                         == FALSE) {x_KS_t$statistic}
    else {"NA"}
    KS_t$PValue <- if (is.null(x_KS_t$p.value)
                       == FALSE) {x_KS_t$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_t)
  }
  if ("weibull" %in% distributions) {
    x_wei <- fitdistr(x, "weibull")
    x_KS_wei <- ks.test(x, "pweibull",
                        shape = x_wei$estimate[1],
                        scale = x_wei$estimate[2])
    KS_wei <- data.frame(matrix(ncol=0, nrow=1))
    KS_wei$Distribution <- "Weibull"
    KS_wei$Distance <- if (is.null(x_KS_wei$statistic)
                           == FALSE) {x_KS_wei$statistic}
    else {"NA"}
    KS_wei$PValue <- if (is.null(x_KS_wei$p.value)
                         == FALSE) {x_KS_wei$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_wei)
  }
  if ("logistic" %in% distributions) {
    x_logis <- fitdistr(x, "logistic")
    x_KS_logis <- ks.test(x, "plogis", x_logis$estimate[1],
                          x_logis$estimate[2])
    KS_logis <- data.frame(matrix(ncol=0, nrow=1))
    KS_logis$Distribution <- "Loglogistic"
    KS_logis$Distance <- if (is.null(x_KS_logis$statistic)
                             == FALSE) {x_KS_logis$statistic}
    else {"NA"}
    KS_logis$PValue <- if (is.null(x_KS_logis$p.value)
                           == FALSE) {x_KS_logis$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_logis)
  }
  
  KS_df <- KS_df %>%
    mutate(
      Distribution = as.factor(Distribution),
      Distance = as.numeric(Distance),
      PValue = as.numeric(format(as.numeric(PValue),
                                 scientific = FALSE))
    )
  KS_df$Distance <- round(KS_df$Distance, 3)
  KS_df$PValue <- round(KS_df$PValue, 3)
  
  return(KS_df)
}

##### UI #####
ui <- navbarPage(title = "Distribution Fitter",
       theme = bs_theme(bootswatch = "sandstone"),
       sidebar = sidebar(
         markdown("#### **Controls**"),
         selectInput("var", 
                     "Variable to Fit Distributions Against",
                     choices = vars, 
                     selected = "Fv.Fm"),
         checkboxGroupInput("cont_dists", 
                            "Continuous Distributions",
                            choices = dists, 
                            selected = c("normal", "lognormal", "gamma")),
         sliderInput("length", 
                     "Length of Sequence",
                     min = 0, 
                     max = 100, 
                     value = 100),
         markdown("#### **Filters**"),
         selectInput("waterfilter", 
                     "Filter By Watering Treatment", 
                     choices = waterfilters,
                     selected = "None"),
         selectInput("phasefilter", 
                     "Filter By Phase Treatment", 
                     choices = phasefilters,
                     selected = "None"),
         selectInput("varietyfilter", 
                     "Filter By Plant Variety", 
                     choices = varietyfilters,
                     selected = "None"),
         selectInput("pal",
                     "Color Palette",
                     choices = pals,
                     selected = "lipari")
       ), # end sidebar
       # Show a plot of the generated distribution
       nav_panel("Plots",
       card(card_body(layout_column_wrap(
         card(card_header("Probability Density Function Plot", class = "bg-primary"),
           plotlyOutput("pdfPlot")),
         card(card_header("Cumulative Distribution Function Plot", class = "bg-info"),
              plotlyOutput("cdfPlot")
              )
         )))),
       nav_panel("KS Tests",
       card(card_header("Kolmogorov-Smirnov test results", class = "bg-primary"),
            width = 200,
            verbatimTextOutput("ks"))),
       nav_panel("Info",
       card(card_header("Info", class = "bg-info"),
         markdown("For more information on Probability Density Functions,
                         see [here](https://en.wikipedia.org/wiki/Probability_density_function)."),
         markdown("For more information on Cumulative Distribution Functions,
                         see [here](https://en.wikipedia.org/wiki/Cumulative_distribution_function)."),
         markdown("For more information on Kolmogorov-Smirnov tests, see
                  [here](https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test)."))
       ),
       nav_spacer(),
       nav_item(tags$a("Github", href = "https://github.com/zachpeagler/App-A-Day/tree/main/02_distribution_fitter"))
)

##### SERVER #####
server <- function(input, output) {
  # Reactive function to return a subsetted dataframe in the event of a filter
  # input. Can be easily built out to include more rules.
  Rdat <- reactive({
    rdat <- dat
    if (input$waterfilter == "Control") {
      rdat <- subset(rdat, Watering == "Control")
    } else if (input$waterfilter == "Deficit") {
      rdat <- subset(rdat, Watering == "Deficit")
    }
    if (input$phasefilter == "recovery") {
      rdat <- subset(rdat, Phase == "recovery")
    } else if (input$phasefilter == "drought") {
      rdat <- subset(rdat, Phase == "drought")
    }
    if (input$varietyfilter == "Mara") {
      rdat <- subset(rdat, Variety == "Mara")
    } else if (input$varietyfilter == "2778") {
      rdat <- subset(rdat, Variety == "2778")
    } else if (input$varietyfilter == "2789") {
      rdat <- subset(rdat, Variety == "2789")
    } else if (input$varietyfilter == "4841") {
      rdat <- subset(rdat, Variety == "4841")
    }
    return(rdat)
  })
  # Reactive function to get input variable
  Rvar <- reactive({input$var})
  # Reactive function to get distributions
  Rdists <- reactive({input$cont_dists})
  # Reactive function for length of sequence
  Rlength <- reactive({input$length})
  # Reactive function for palette
  Rpal <- reactive({input$pal})
  # Reactive function to return a dataframe of pdfs for the selected 
  # distributions against the target variables.
  Rpdf_df <- reactive({
    var_name <- Rvar()
    data <- Rdat()
    if (var_name %in% colnames(data)){
      cont_pdf(data[[var_name]], Rlength(), Rdists())
    } else {
      return(data.frame())
    }
  })
  # Reactive function to get CDF df
  Rcdf_df <- reactive({
    var_name <- Rvar()
    data <- Rdat()
    if (var_name %in% colnames(data)){
      cont_cdf(data[[var_name]], Rlength(), Rdists())
    } else {
      return(data.frame())
    }
  })
  ## PDF plotly output
  output$pdfPlot <- renderPlotly({
    distributions <- Rdists()
    data <- Rpdf_df()
    p <- plot_ly(data, x= ~x_seq, y= ~dens, type = 'scatter', mode = 'lines', name = 'Actual Density')
    if ("normal" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_normal, name='Normal')
    }
    if ("lognormal" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_lognormal, name='Lognormal')
    }
    if ("gamma" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_gamma, name='Gamma')
    }
    if ("exponential" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_exponential, name='Exponential')
    }
    if ("cauchy" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_cauchy, name='Cauchy')
    }
    if ("t" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_t, name='t')
    }
    if ("weibull" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_weibull, name='Weibull')
    }
    if ("logistic" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~pdf_logistic, name='LogLogistic')
    }
    p <- p %>% layout(legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           yanchor = "top",
                           y = -0.2,
                           title = list(text = "Distribution"))) 
    return(p)
  })
  ## CDF plotly output
  output$cdfPlot <- renderPlotly({
    distributions <- Rdists()
    data <- Rcdf_df()
    p <- plot_ly(data, x= ~x_seq, y= ~dens, type = 'scatter', mode = 'lines', name = 'Actual Density')
    
    if ("normal" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_normal, name='Normal')
    }
    if ("lognormal" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_lognormal, name='Lognormal')
    }
    if ("gamma" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_gamma, name='Gamma')
    }
    if ("exponential" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_exponential, name='Exponential')
    }
    if ("cauchy" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_cauchy, name='Cauchy')
    }
    if ("t" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_t, name='t')
    }
    if ("weibull" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_weibull, name='Weibull')
    }
    if ("logistic" %in% distributions == TRUE) {
      p <- p %>% add_trace(p, y=~cdf_logistic, name='LogLogistic')
    }
    p <- p %>% layout(legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           yanchor = "top",
                           y = -0.2,
                           title = list(text = "Distribution")))
    return(p)
  })
  ## multiKS output
  output$ks <- renderPrint({
    var_name <- Rvar()
    data <- Rdat()
    x <- multiKS(data[[var_name]], Rdists())
    return(x)
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
