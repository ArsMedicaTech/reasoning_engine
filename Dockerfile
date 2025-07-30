# ---- Stage 1: build ----
# Use official Haskell image with GHC and Cabal
FROM haskell:9.6 AS builder

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

# Install unzip + AWS CLI (v2 for SSO or v1 for credentials file)
RUN apt-get update && apt-get install -y unzip awscli && rm -rf /var/lib/apt/lists/*

# For any dynamically linked libstdc++ or libgmp
RUN apt-get update && apt-get install -y libgmp10 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/bin/reasoning-engine /app/reasoning-engine

# It is recommended to not pass in sensitive data like AWS keys directly in the Dockerfile.
# But then again, these credentials are so narrowly scoped that it should not be a problem.
ARG AWS_ONTOLOGY_S3_ACCESS_KEY_ID
ARG AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY
ARG AWS_ONTOLOGY_S3_BUCKET

# Configure S3 access via env or volume
# e.g., AWS_PROFILE, AWS_ONTOLOGY_S3_ACCESS_KEY_ID, AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY
RUN aws configure set default.region us-east-1
RUN aws configure set default.output json
RUN aws configure set aws_access_key_id ${AWS_ONTOLOGY_S3_ACCESS_KEY_ID}
RUN aws configure set aws_secret_access_key ${AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY}

RUN apt-get update && apt-get install -y awscli && rm -rf /var/lib/apt/lists/* && \
    aws s3 cp s3://${AWS_ONTOLOGY_S3_BUCKET}/ontology.bin /app/ontology.bin

EXPOSE 8080
CMD ["./reasoning-engine"]
