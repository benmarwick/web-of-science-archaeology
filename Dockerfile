# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.4.1

# required
MAINTAINER Ben Marwick <bmarwick@uw.edu>

WORKDIR /web-of-science-archaeology
COPY . /web-of-science-archaeology

# go into the repo directory
RUN . /etc/environment \
  && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github('rstudio/renv')" \
  # install pkgs we need
  && R -e "renv::restore()" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  # render the manuscript into a docx
  && R -e "rmarkdown::render('analysis/paper/paper.qmd')"
  
  
# To run this container locally:

### STEP 1 ###
# Run on the terminal:
# docker build -t wos .

### STEP 2 ###
# Run on the terminal:
# docker run --rm -it -e ROOT=TRUE -e PASSWORD=rstudio -dp 8787:8787 wos

### STEP 3 ###
# Go to http://localhost:8787/ with your browswe. USERID=rstudio, PASSWORD=rstudio


### STEP 4 ####
# Clean and delete containes. Run on the terminal:
# docker ps -aq | xargs docker stop | xargs docker rm

# This will generate a folder called output in your home directory containing figures and tables 
