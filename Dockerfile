# ---- Stage 1: build ----
# Use official Haskell image with GHC and Cabal
FROM haskell:9.6 as builder

# Set work directory
WORKDIR /app

COPY reasoning-engine.cabal .

RUN echo "main :: IO ()\nmain = putStrLn \"Hello, World!\"" > Main.hs

# Optional: build dependencies first (caching layer)
RUN cabal update && cabal build --only-dependencies

# Copy your Haskell source files
COPY . .

# Build the project
RUN cabal build

# Run the app by default (update with your actual executable name)
#CMD ["cabal", "run"]
RUN cabal install exe:reasoning-engine --installdir=/app/bin --install-method=copy

# ---- Stage 2: slim runtime ----
# From 5GB to 100MB
FROM debian:bullseye-slim

# For any dynamically linked libstdc++ or libgmp
RUN apt-get update && apt-get install -y libgmp10 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/bin/reasoning-engine /app/reasoning-engine

EXPOSE 8080
CMD ["./reasoning-engine"]
