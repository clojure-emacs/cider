# Use an official Clojure runtime as a parent image
FROM clojure:temurin-17-lein-bullseye

# Set environment variables to non-interactive (this prevents some prompts)
ENV DEBIAN_FRONTEND=noninteractive

# Define environment variable for nREPL port
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

# Set root password
RUN echo 'root:cider' | chpasswd

RUN sed -i 's/^#* *PasswordAuthentication .*/PasswordAuthentication yes/' /etc/ssh/sshd_config
RUN sed -i 's/^#* *PermitRootLogin .*/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -i 's/^#* *ChallengeResponseAuthentication .*/ChallengeResponseAuthentication yes/' /etc/ssh/sshd_config

# SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd

# Expose SSH port
EXPOSE 22

CMD ["/usr/sbin/sshd", "-D"]

# Set the working directory in the docker image
WORKDIR /usr/src/app

# Copy the current directory contents into the directory at /usr/src/app in the image
COPY . /usr/src/app

# Install any needed packages specified in project.clj
RUN lein deps

# Forward all relevant env vars for ssh sessions
RUN echo "export JAVA_HOME=${JAVA_HOME}" >> /root/.bashrc
RUN echo "export LEIN_HOME=${LEIN_HOME}" >> /root/.bashrc
RUN echo "export LEIN_JAVA_CMD=${LEIN_JAVA_CMD}" >> /root/.bashrc
RUN echo "export LEIN_JVM_OPTS=${LEIN_JVM_OPTS}" >> /root/.bashrc
RUN echo "export LEIN_ROOT=${LEIN_ROOT}" >> /root/.bashrc
RUN echo "export NREPL_PORT=${NREPL_PORT}" >> /root/.bashrc
RUN echo "export PATH=${PATH}" >> /root/.bashrc

# Make port 7888 available to the world outside this container (nREPL default port)
#EXPOSE 7888

# Run lein repl when the container launches, on port defined by NREPL_PORT
#CMD ["lein", "repl", ":headless", ":host", "0.0.0.0", ":port", "7888"]
