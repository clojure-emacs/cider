FROM clojure:temurin-17-lein-bullseye
ENV DEBIAN_FRONTEND=noninteractive
ENV NREPL_PORT=7888
WORKDIR /root/app
COPY . /root/app
RUN lein deps
EXPOSE 7888
RUN lein classpath
CMD ["lein", "repl", ":headless", ":host", "0.0.0.0", ":port", "7888"]
