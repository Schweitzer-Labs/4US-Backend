FROM ubuntu:18.04
RUN apt-get update
RUN apt install -y python3-pip
RUN pip3 install aws-sam-cli
RUN apt install -y uuid-runtime
RUN apt-get install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg \
        lsb-release
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
RUN echo \
      "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
      $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null
RUN apt-get update
RUN apt-get install -y docker-ce docker-ce-cli containerd.io
RUN apt-get install -y build-essential libssl-dev
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.38.0/install.sh | bash
RUN export NVM_DIR="$HOME/.nvm" && . "$NVM_DIR/nvm.sh" && nvm install 12.22.1 && nvm use 12.22.1
RUN apt-get install -y npm
RUN dockerd &
CMD cd build-sam && export LC_ALL=C.UTF-8 && export LANG=C.UTF-8 && make dep && make
