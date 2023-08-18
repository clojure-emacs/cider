FROM clojure:temurin-17-lein-bullseye
ENV DEBIAN_FRONTEND=noninteractive
ENV NREPL_PORT=7888

RUN apt-get update \
    && apt-get install -y openssh-server locales \
    && mkdir /var/run/sshd

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN locale-gen en_US.UTF-8
RUN locale-gen en en_US en_US.UTF-8
RUN dpkg-reconfigure locales

RUN echo 'root:cider' | chpasswd

RUN sed -i 's/^#* *PasswordAuthentication .*/PasswordAuthentication yes/' /etc/ssh/sshd_config
RUN sed -i 's/^#* *PermitRootLogin .*/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -i 's/^#* *ChallengeResponseAuthentication .*/ChallengeResponseAuthentication yes/' /etc/ssh/sshd_config

# SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd

EXPOSE 22

CMD ["/usr/sbin/sshd", "-D"]

WORKDIR /usr/src/app

COPY . /usr/src/app

RUN lein deps

RUN echo "export JAVA_HOME=${JAVA_HOME}" >> /root/.bashrc
RUN echo "export LEIN_HOME=${LEIN_HOME}" >> /root/.bashrc
RUN echo "export LEIN_JAVA_CMD=${LEIN_JAVA_CMD}" >> /root/.bashrc
RUN echo "export LEIN_JVM_OPTS=${LEIN_JVM_OPTS}" >> /root/.bashrc
RUN echo "export LEIN_ROOT=${LEIN_ROOT}" >> /root/.bashrc
RUN echo "export NREPL_PORT=${NREPL_PORT}" >> /root/.bashrc
RUN echo "export PATH=${PATH}" >> /root/.bashrc
