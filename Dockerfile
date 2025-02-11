FROM rocker/shiny:4.4.2

# Install additional system dependencies if needed.
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# Install extra R packages that are not already included in the image.
RUN R -e "install.packages(c('shinythemes', 'survival', 'riskRegression', 'ggplot2', 'prodlim', 'plotly'), repos='https://cran.rstudio.com/')"

COPY . /app
WORKDIR /app

EXPOSE 8080

CMD R -e "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 8080)))"
