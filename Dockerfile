## This is a Docker Container that can be used to make the scripts in this repository fully reproducible ##
## For more information on how to use Docker to reproduce the analyses, see the project README file.

# Initiate RStudio cloud environment with version 4.1.2
FROM rocker/rstudio:4.5.1

LABEL maintainer="p.c.s.vermeent@gmail.com"
LABEL description="RStudio container with JAGS, wiener module, and groundhog-managed R packages"


#############################
# 1. Install system dependencies
#############################

RUN apt-get update && apt-get install -y \
    wget \
    build-essential \
    gfortran \
    libblas-dev \
    liblapack-dev \
    libwebp-dev \
    libwebpmux3 \
    libwebpdemux2 \
    libtiff5-dev \
    libjpeg-dev \
    libpng-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && apt-get clean


#############################
# 2. Copy project scripts + bibfiles
#############################

# Initiate folders for raw and intermediate data files (without sharing raw data files)
RUN mkdir -p /project/2_scripts \
             /project/bibfiles \
             /project/1_data/1_InputData \
             /project/1_data/2_IntermediateData \
             /project/1_data/3_AnalysisData \
             /project/3_output/Results \
             /project/3_output/SupplementResults \
             /project/4_manuscript

COPY 2_scripts/ /project/2_scripts/
COPY bibfiles/ /project/bibfiles/



#############################
# 3. Install groundhog system-wide
#############################

# Install groundhog
RUN R -e "install.packages('groundhog')"

# Authorize groundhog's package folder for root
RUN R -e "groundhog::set.groundhog.folder('/root/R_groundhog')"

# Install project packages
RUN Rscript /project/2_scripts/dependencies.R


#############################
# 4. Install JAGS + Wiener module
#############################

# Install JAGS and the wiener module
RUN apt-get update && . /etc/environment \
  && wget sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/JAGS-4.2.0.tar.gz  -O jags.tar.gz \
  && tar -xf jags.tar.gz \
  && cd JAGS* && ./configure && make -j4 && make install \
  && wget sourceforge.net/projects/jags-wiener/files/JAGS-WIENER-MODULE-1.1.tar.gz -O JAGS-WIENER-MODULE-1.1.tar.gz \
  && tar -xf JAGS-WIENER-MODULE-1.1.tar.gz \
  && cd JAGS* && ./configure && make -j4 && make install


#############################
# 5. Make project folders visible inside RStudio Cloud home directory
#############################

RUN ln -s /project/1_data /home/rstudio/1_data && \
    ln -s /project/2_scripts /home/rstudio/2_scripts && \
    ln -s /project/3_output /home/rstudio/3_output && \
    ln -s /project/4_manuscript /home/rstudio/4_manuscript


#############################
# 6. Give writing permission (to upload data files)
#############################

RUN chown -R rstudio:rstudio /project /home/rstudio

