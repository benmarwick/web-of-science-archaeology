# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.4.1

# metadata
LABEL maintainer="Ben Marwick <bmarwick@uw.edu>"

# Switch to the existing rstudio user
USER rstudio

# Set the working directory to the project directory
WORKDIR /home/rstudio/web-of-science-archaeology

# Copy the project files
COPY --chown=rstudio:rstudio . .

# Install remotes and renv
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" && \
    R -e "remotes::install_github('rstudio/renv')"

# Restore the renv environment
RUN R -e "renv::restore()"

# Render the manuscript
RUN R -e "rmarkdown::render('analysis/paper/paper.qmd')"


# To run this container locally:

### STEP 1 ###
# Run on the terminal:
# docker build -t wos .

### STEP 2 ###
# Run on the terminal:
# docker run --rm -it -e ROOT=TRUE -e PASSWORD=rstudio -dp 8787:8787 wos

### STEP 3 ###
# Go to http://localhost:8787/ with your browser. USERID=rstudio, PASSWORD=rstudio


### STEP 4 ####
# Clean and delete containes. Run on the terminal:
# docker ps -aq | xargs docker stop | xargs docker rm

# This will generate a folder called output in your home directory containing figures and tables 
