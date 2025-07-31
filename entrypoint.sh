#!/bin/bash
set -e

echo "Starting entrypoint script..."
echo "AWS_ONTOLOGY_S3_BUCKET: ${AWS_ONTOLOGY_S3_BUCKET}"

# Download ontology file
aws s3 cp s3://${AWS_ONTOLOGY_S3_BUCKET}/ontology.bin /app/ontology.bin

# Check if the ontology file was downloaded successfully
if [ ! -f /app/ontology.bin ]; then
    echo "Ontology file not found at /app/ontology.bin"
    exit 1
fi

# Run the Haskell app
exec /app/reasoning-engine
#exec bash
