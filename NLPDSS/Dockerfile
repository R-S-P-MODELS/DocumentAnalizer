

FROM r-base:3.6.3

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
    libssh2-1-dev
#    libssl1.0.0


# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev
# Install OpenJDK-8
#RUN apt-get update && \
 #   apt-get install -y openjdk-8-jdk && \
 #   apt-get install -y ant && \
 #   apt-get clean;

# Fix certificate issues
#RUN apt-get update && \
#    apt-get install ca-certificates-java && \
#    apt-get clean && \
#    update-ca-certificates -f;

# Setup JAVA_HOME -- useful for docker commandline
#ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/
#RUN export JAVA_HOME
# Install OpenJDK-8
RUN apt-get install -y default-jre
RUN apt-get install -y default-jdk
RUN R CMD javareconf

RUN java --version

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown','shinydashboard','shinydashboardPlus'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

RUN apt-get install -y \
	gsl-bin\
	libgsl0-dev

# install dependencies of the Application 
RUN R -e "install.packages(c('pdftools','tidytext','dplyr','tm','Matrix','ggplot2','forcats','igraph','ggraph'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

#RUN sudo add-apt-repository -y ppa:cran/poppler
RUN   sudo apt-get update
RUN   sudo apt-get install -y  libxml2-dev\
                             libpoppler-cpp-dev

RUN R -e "install.packages(c('tm','RWeka','topicmodels'), repos='https://cloud.r-project.org/',dependencies=TRUE)"


# copy the app to the image
COPY app.R  app.R
COPY Functions.R Functions.R
COPY UIDashboard.R UIDashboard.R
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R',port=3838,host='0.0.0.0',launch.browser=FALSE)"]
#CMD ["R", "-e", "shiny::runApp('/home/rafael/Downloads/APPs/DataExploration/APPLastVersion.R')"]
