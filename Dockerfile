# load rocker base-R image
# FROM rocker/r-ver:4.3.1
FROM node:21-alpine
WORKDIR /app
# install specific versions of CRAN packages from MRAN snapshots
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("BH", "1.84.0-0", dependencies = FALSE)'
RUN R -e 'remotes::install_version("classInt", "0.4-10", dependencies = FALSE)'
RUN R -e 'remotes::install_version("curl", "5.2.0", dependencies = FALSE)'
RUN R -e 'remotes::install_version("DBI", "1.2.2", dependencies = FALSE)'
RUN R -e 'remotes::install_version("e1071", "1.7-14", dependencies = FALSE)'
RUN R -e 'remotes::install_version("exactextractr", "0.10.0", dependencies = FALSE)'
RUN R -e 'remotes::install_version("googlePolylines", "0.8.4", dependencies = FALSE)'
RUN R -e 'remotes::install_version("isoband", "0.2.7", dependencies = FALSE)'
RUN R -e 'remotes::install_version("magrittr", "2.0.3", dependencies = FALSE)'
RUN R -e 'remotes::install_version("mapiso", "0.3.0", dependencies = FALSE)'
RUN R -e 'remotes::install_version("osrm", "4.1.1", dependencies = FALSE)'
RUN R -e 'remotes::install_version("proxy", "0.4-27", dependencies = FALSE)'
RUN R -e 'remotes::install_version("raster", "3.6-26", dependencies = FALSE)'
RUN R -e 'remotes::install_version("RcppSimdJson", "0.1.11", dependencies = FALSE)'
RUN R -e 'remotes::install_version("s2", "1.1.6", dependencies = FALSE)'
RUN R -e 'remotes::install_version("sf", "1.0-15", dependencies = FALSE)'
RUN R -e 'remotes::install_version("smoothr", "1.0.1", dependencies = FALSE)'
RUN R -e 'remotes::install_version("sp", "2.1-3", dependencies = FALSE)'
RUN R -e 'remotes::install_version("terra", "1.7-71", dependencies = FALSE)'
RUN R -e 'remotes::install_version("units", "0.8-5", dependencies = FALSE)'
RUN R -e 'remotes::install_version("wk", "0.9.1", dependencies = FALSE)'
RUN R -e 'remotes::install_version("Rcpp", "1.0.11", dependencies = FALSE)'

# copy source packages (*.tar.gz) to container
COPY source_packages /source_packages

# install 'enmRoute' package
RUN R -e 'install.packages(pkgs = "source_packages/enmRoute_0.1.0.tar.gz", repos = NULL)'
EXPOSE 3000

