FROM debian:stable

RUN \
    echo 'deb http://deb.debian.org/debian stable main \n\
deb-src http://deb.debian.org/debian stable main \n\
deb http://security.debian.org/debian-security stable/updates main \n\
deb-src http://security.debian.org/debian-security stable/updates main \n\
deb http://deb.debian.org/debian stable-updates main \n\
deb-src http://deb.debian.org/debian stable-updates main' > /etc/apt/sources.list && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y make stow sudo

RUN useradd -m -s /bin/bash alex \
    && echo "alex ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/me

ADD . /home/alex/src/dotfiles
RUN chown -R alex:alex /home/alex

USER alex
WORKDIR /home/alex

RUN rm /home/alex/.bashrc /home/alex/.profile
