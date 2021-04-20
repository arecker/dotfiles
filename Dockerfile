FROM debian:stable

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y make stow sudo

RUN useradd -m -s /bin/bash alex \
    && echo "alex ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/me

ADD . /home/alex/src/dotfiles
RUN chown -R alex:alex /home/alex

USER alex
WORKDIR /home/alex

RUN rm /home/alex/.bashrc /home/alex/.profile
RUN cd /home/alex/src/dotfiles && make
