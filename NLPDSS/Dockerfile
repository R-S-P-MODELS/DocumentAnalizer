
FROM openanalytics/r-base

MAINTAINER Rafael Pereira "r.s.p.models@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0\
    libpoppler-cpp-dev \
    libxml2-dev\
    libgsl0ldbl

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown','shinydashboard','shinydashboardPlus'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

# install dependencies of the Application 
RUN R -e "install.packages(c('pdftools','tidytext','dplyr','wordcloud','tm','RWeka','Matrix','topicmodels','ggplot2','forcats','igraph','ggraph'), repos='https://cloud.r-project.org/',dependencies=TRUE)"


# copy the app to the image
COPY app.R  app.R
COPY Functions.R Functions.R
COPY UIDashboard.R UIDashboard.R
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R',port=3838,host='0.0.0.0',launch.browser=FALSE)"]
#CMD ["R", "-e", "shiny::runApp('/home/rafael/Downloads/APPs/DataExploration/APPLastVersion.R')"]
