# Use official Haskell image with GHC and Cabal
FROM haskell:9.6

# Set work directory
WORKDIR /app

# Copy your Haskell source files
COPY . .

# Optional: build dependencies first (caching layer)
RUN cabal update && cabal build --only-dependencies

# Build the project
RUN cabal build

# Run the app by default (update with your actual executable name)
CMD ["cabal", "run"]
