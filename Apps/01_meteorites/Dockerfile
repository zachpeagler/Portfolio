# base shiny image
FROM rocker/shiny-verse

# make a directory in the container
RUN mkdir /home/shiny-app

# install R dependencies
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'tidyr', 'leaflet', 'bslib', 'scico', 'DT', 'plotly'))"

# copy the shiny app code
COPY /app /home/shiny-app

# expose application port
EXPOSE 8180

# run the R shiny app
CMD Rscript /home/shiny-app/app.R