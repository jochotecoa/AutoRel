# Use an official R base image
FROM rocker/r-ver:4.4.2

# Install system dependencies required by R packages (curl, xml2, ssl, etc.)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY . /app

# Install R dependencies using the existing installer
# Note: This will also attempt to download the model
RUN Rscript install_AutoRel.R

# Create output directory
RUN mkdir -p /app/output

# Make the CLI script executable
RUN chmod +x /app/run_AutoRel.R

# Set the entrypoint to the CLI tool
ENTRYPOINT ["Rscript", "run_AutoRel.R"]

# Default command (shows help)
CMD ["--help"]
