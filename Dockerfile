FROM rocker/r-ver:4.4.0

ENV TZ=Europe/Amsterdam

RUN apt-get update && apt-get install -y \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libyaml-dev \
    zlib1g-dev \
    pkg-config \
    pandoc \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff5-dev \
    libgit2-dev \
    cmake \
    libfontconfig1-dev \
    default-jdk \
    libgsl-dev \
    libfftw3-dev \
    && rm -rf /var/lib/apt/lists/*

ENV GIT_TERMINAL_PROMPT=0

WORKDIR /app

# Clone repository and copy only renv files first (better caching)
RUN git clone https://github.com/PCdLf/wearalyze.git /tmp/repo && \
    cp /tmp/repo/renv.lock /tmp/repo/.Rprofile /app/ && \
    mkdir -p /app/renv && \
    cp -r /tmp/repo/renv/* /app/renv/

# Override R's Makeconf to fix yaml compilation issue
# This is necessary because yaml package compilation fails with -Werror=format-security in this base image
RUN sed -i 's/-Werror=format-security/-Wno-error=format-security/g' /usr/local/lib/R/etc/Makeconf

# Restore R packages (this layer is cached unless renv.lock changes)
RUN R -e "options(renv.consent = TRUE); renv::restore()"

# Copy the rest of the application code
RUN cp -r /tmp/repo/* /app/ && rm -rf /tmp/repo

EXPOSE 8080

CMD ["R", "-e", "options('shiny.host' = '0.0.0.0', 'shiny.port' = 8080); rhino::app()"]