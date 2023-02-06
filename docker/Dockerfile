# Set the base image to Ubuntu
FROM ubuntu:20.04

RUN mkdir /tools

# Installing dependencies for R scripts
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    python3-pip \
    r-base \
    r-base-dev

# Install python dependenncies requuired for the script
COPY ./scripts/requirements.txt /tools/
RUN pip3 install -r /tools/requirements.txt

# Set CRAN repository to use
RUN echo 'local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})' > ~/.Rprofile

# Install libs
RUN R -e 'install.packages("optparse");'
RUN R -e 'install.packages("ggplot2");'
RUN R -e 'install.packages("RColorBrewer");'
RUN R -e 'install.packages("cowplot");'
RUN R -e 'install.packages("gridExtra");'
RUN apt-get install -y curl

# Install gcloud sdk
RUN curl https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.tar.gz > /tmp/google-cloud-sdk.tar.gz
RUN mkdir -p /usr/local/gcloud \
  && tar -C /usr/local/gcloud -xvf /tmp/google-cloud-sdk.tar.gz \
  && /usr/local/gcloud/google-cloud-sdk/install.sh
ENV PATH $PATH:/usr/local/gcloud/google-cloud-sdk/bin

# copy scripts
COPY ./scripts/generate_report.R /tools/
COPY ./scripts/get_sb_name_datastore.py /tools/

# define entrypoint
ENTRYPOINT ["Rscript", "/tools/generate_report.R"]
