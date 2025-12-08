# Use official Haskell image with GHC 9.8.4
FROM haskell:9.8.4

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    make \
    git \
    && rm -rf /var/lib/apt/lists/*

# Copy entire project (needed because cabal.project references multiple packages)
COPY . .

# Update cabal and install dependencies
RUN cabal update && cabal build all --only-dependencies

# Build the project
RUN cabal build all

# Default command
CMD ["/bin/bash"]
