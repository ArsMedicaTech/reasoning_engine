include .env

docker-create:
	aws ecr create-repository --repository-name $(REASONING_ENGINE_IMAGE) --region us-east-1 || true


auth:
	aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin $(DOCKER_REGISTRY)


BUILD_ARGS=--build-arg AWS_ONTOLOGY_S3_ACCESS_KEY_ID=$(AWS_ONTOLOGY_S3_ACCESS_KEY_ID) --build-arg AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY=$(AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY) --build-arg AWS_ONTOLOGY_S3_BUCKET=$(AWS_ONTOLOGY_S3_BUCKET)

REASONING_ENGINE_BUILD_ARGS=

reasoning-engine-docker:
	docker build -t $(DOCKER_REGISTRY)/$(REASONING_ENGINE_IMAGE):$(REASONING_ENGINE_VERSION) $(REASONING_ENGINE_BUILD_ARGS) -f ./Dockerfile .
	docker push $(DOCKER_REGISTRY)/$(REASONING_ENGINE_IMAGE):$(REASONING_ENGINE_VERSION)
	kubectl rollout restart deployment $(REASONING_ENGINE_DEPLOYMENT) --namespace=$(NAMESPACE)


docker-build:
	docker build -t reasoning-engine .


ENV_VARS=-e AWS_ACCESS_KEY_ID=$(AWS_ONTOLOGY_S3_ACCESS_KEY_ID) -e AWS_SECRET_ACCESS_KEY=$(AWS_ONTOLOGY_S3_SECRET_ACCESS_KEY) -e AWS_ONTOLOGY_S3_BUCKET=$(AWS_ONTOLOGY_S3_BUCKET)

docker-run:
	docker run --rm $(ENV_VARS) -p 8080:8080 reasoning-engine


run:
	cabal run reasoning-engine


build-ontology:
	docker build -t ontology-builder $(BUILD_ARGS) -f ontology-builder/Dockerfile ./ontology-builder
