# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.4.1

# metadata
LABEL maintainer="Ben Marwick <bmarwick@uw.edu>"

# Define a variable for the project directory
ENV PROJ_DIR /home/rstudio/web-of-science-archaeology

# Define renv global cache directory (inside the container)
ENV RENV_PATHS_CACHE=/renv/cache

# Set the working directory to the project directory
WORKDIR $PROJ_DIR

# Copy the project files
COPY . $PROJ_DIR

# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))" 

# Restore the renv environment
RUN R -e "renv::restore()"

# Set permissions
RUN chmod -R 777 /home/

# Ensure renv is activated in RStudio by writing to .Rprofile
RUN echo 'if (interactive()) renv::activate()' >> $PROJ_DIR/.Rprofile

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


