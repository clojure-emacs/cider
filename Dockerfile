FROM travisci/ci-garnet:packer-1490989530

RUN apt-get update -y \
 && apt-get install -y \
      autogen \
      ca-certificates \
      curl \
      gcc \
      git \
      libgmp-dev \
      m4 \
      make \
      pkg-config \
      python \
      ruby \
      xz-utils

USER travis
ENV HOME=/home/travis
ENV PATH="${HOME}/local/evm/bin:${HOME}/local/cask/bin:${HOME}/local/bin:${PATH}"

ADD travis-ci/ ${HOME}/cider-setup/travis-ci/
RUN echo ". ${HOME}/cider-setup/travis-ci/prompt.sh" >> ${HOME}/.bashrc

RUN ${HOME}/cider-setup/travis-ci/install-gnutls.sh
RUN ${HOME}/cider-setup/travis-ci/install-evm.sh
RUN ${HOME}/cider-setup/travis-ci/install-cask.sh

RUN evm install emacs-25.3-travis --use
RUN evm install emacs-26-pretest-travis
RUN evm install emacs-git-snapshot-travis

WORKDIR /home/travis/cider
CMD /bin/bash
