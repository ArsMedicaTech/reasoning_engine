FROM haskell:9.6 AS builder

# It is recommended to not pass in sensitive data like AWS keys directly in the Dockerfile.
# But then again, these credentials are so narrowly scoped that it should not be a problem.
ARG AWS_ONTOLOGY_S3_ACCESS_KEY_ID
ARG AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY
ARG AWS_ONTOLOGY_S3_BUCKET

# Install unzip + AWS CLI (v2 for SSO or v1 for credentials file)
RUN apt-get update && apt-get install -y unzip awscli && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy Haskell parser and build it
COPY . .

RUN cabal update && cabal install exe:ontology-builder --installdir=/app/bin --install-method=copy

# Configure S3 access via env or volume
# e.g., AWS_PROFILE, AWS_ONTOLOGY_S3_ACCESS_KEY_ID, AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY
RUN aws configure set default.region us-east-1
RUN aws configure set default.output json
RUN aws configure set aws_access_key_id ${AWS_ONTOLOGY_S3_ACCESS_KEY_ID}
RUN aws configure set aws_secret_access_key ${AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY}

# Download and unzip SNOMED archive from S3
RUN mkdir data && \
    aws s3 cp s3://${AWS_ONTOLOGY_S3_BUCKET}/SNOMED.zip data/SNOMED.zip && \
    unzip data/SNOMED.zip -d data

# Run your parser, output ontology.bin
RUN ./dist/build/ontology-builder/ontology-builder \
      --desc-file data/sct2_Description_Full-en_US*.txt \
      --out ontology.bin

RUN aws s3 cp ontology.bin s3://${AWS_ONTOLOGY_S3_BUCKET}/ontology.bin
