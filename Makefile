include .env

docker-create:
	aws ecr create-repository --repository-name $(REASONING_ENGINE_IMAGE) --region us-east-1 || true


auth:
	aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin $(DOCKER_REGISTRY)


REASONING_ENGINE_BUILD_ARGS=

reasoning-engine-docker:
	docker build -t $(DOCKER_REGISTRY)/$(REASONING_ENGINE_IMAGE):$(REASONING_ENGINE_VERSION) $(REASONING_ENGINE_BUILD_ARGS) -f ./Dockerfile .
	docker push $(DOCKER_REGISTRY)/$(REASONING_ENGINE_IMAGE):$(REASONING_ENGINE_VERSION)
	kubectl rollout restart deployment $(REASONING_ENGINE_DEPLOYMENT) --namespace=$(NAMESPACE)


docker-build:
	docker build -t reasoning-engine .

docker-run:
	docker run --rm reasoning-engine


run:
	cabal run reasoning-engine
