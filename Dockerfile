FROM rocker/tidyverse:3.6.1

## Install packages
RUN install2.r -s remotes
COPY DESCRIPTION /pkg/DESCRIPTION
RUN Rscript -e "remotes::install_dev_deps(pkgdir = '/pkg/', dependencies=TRUE, upgrade = 'always', force = FALSE, quiet = FALSE)"
