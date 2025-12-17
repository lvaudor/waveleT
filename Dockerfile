FROM rocker/r2u:jammy

LABEL org.opencontainers.image.authors="Lise Vaudor <lise.vaudor@ens-lyon.fr>, Samuel Dunesme <samuel.dunesme@ens-lyon.fr>"
LABEL org.opencontainers.image.source="https://github.com/lvaudor/waveleT"

RUN locale-gen fr_FR.UTF-8

RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'install.packages("rstudioapi")'

RUN mkdir /app
ADD . /app
WORKDIR /app

RUN R -e 'remotes::install_local()'

EXPOSE 3840
HEALTHCHECK --interval=1m --timeout=3s CMD wget --no-verbose --tries=1 --spider http://localhost:3840/ || exit 1

RUN groupadd -g 1010 app && useradd -c 'app' -u 1010 -g 1010 -m -d /home/app -s /sbin/nologin app
USER app

CMD  ["R", "-f", "run_app.R"]