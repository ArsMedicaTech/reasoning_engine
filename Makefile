docker-build:
	docker build -t reasoning-engine .

docker-run:
	docker run --rm reasoning-engine


run:
	cabal run reasoning-engine
