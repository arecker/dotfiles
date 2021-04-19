FROM debian:stable

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    bash \
    curl \
    dnsutils \
    git \
    jq \
    make \
    man \
    stow \
    sudo \
    tree \
    wget

RUN useradd -m -s /bin/bash alex \
    && echo "alex ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/me

ADD . /home/alex/src/dotfiles
RUN chown -R alex:alex /home/alex

USER alex
WORKDIR /home/alex

RUN rm /home/alex/.bashrc /home/alex/.profile
RUN cd /home/alex/src/dotfiles && make
RUN echo 'export PS1="\$ "' >> /home/alex/.bashrc
