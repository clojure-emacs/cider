build:
	DOCKER_BUILDKIT=0 docker build --no-cache -t cider-docker-dev .

run: build
	docker run -v $$PWD/src:/app/src -p 7888:7888 cider-docker-dev
