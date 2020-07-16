FROM rocker/r-ver:4.0.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libxml2-dev libbz2-dev liblzma-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://bioconductor.org/packages/3.11/bioc', CRAN = 'https://cran.rstudio.com', CRAN = 'https://bioconductor.org/packages/3.11/data/annotation', CRAN = 'https://bioconductor.org/packages/3.11/data/experiment', CRAN = 'https://bioconductor.org/packages/3.10/workflows'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("rtracklayer",upgrade="never", version = "1.48.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.3")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.0.3")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.14")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.29")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.1")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@d3cfeabb97cdd93b3ddd294fc3384701db3dcfdc")'
RUN Rscript -e 'remotes::install_github("ramnathv/htmlwidgets@6fcc4b03ed3fc42be76d4e43d863db3d85c8babb")'
RUN Rscript -e 'remotes::install_github("ropensci/plotly@1d1eddda377685cf900ed4a7b16e7796a24b8fe4")'
RUN Rscript -e 'remotes::install_github("jacobwindsor/igvShiny@cd4467caceffce344498c278895d85c5e03aa0b2")'
RUN Rscript -e 'remotes::install_github("cttobin/ggthemr@10eb50a78047e1eb0b9517004f9f7fc34702f539")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
CMD R -e "options('shiny.port'=$PORT,shiny.host='0.0.0.0');SQANTIExplorer::run_app()"
