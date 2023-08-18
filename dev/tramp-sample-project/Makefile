build:
	DOCKER_BUILDKIT=0 docker build --no-cache -t cider-tramp-dev .

run: build
	docker run -p 7888:7888 -p 8022:22 cider-tramp-dev

ssh:
	ssh-keygen -R "[localhost]:8022"
	echo "Password is: cider"
	ssh root@localhost -p 8022
