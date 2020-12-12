FROM fpco/stack-build-small:lts-16 as build

# インストールに時間のかかるものは先に個別にインストールしておいて
# Docker のキャッシュを効かせる。
RUN stack setup
WORKDIR /root/.stack/global-project
COPY stack.yaml stack.yaml.lock ./
RUN echo "name: fake" >package.yaml
RUN stack build aeson
RUN stack build rio
RUN stack build wai
RUN stack build warp
RUN stack build servant-server
RUN stack build persistent
RUN stack build persistent-template

RUN apt update && \
    apt install -y \
        libmecab-dev \
        libmysqlclient-dev \
        libpcre3-dev

WORKDIR /opt/build
COPY package.yaml stack.yaml stack.yaml.lock ./
RUN stack build --only-dependencies

COPY app app
COPY src src
COPY test test
RUN mkdir bin
RUN stack --local-bin-path /opt/build/bin build --copy-bins

FROM ubuntu
RUN apt update && \
    apt install -y \
        libatomic1 \
        wget \
        libmecab2 \
        mecab-ipadic-utf8 \
        mecab
RUN wget http://launchpadlibrarian.net/429633018/libmysqlclient20_5.7.26-1_amd64.deb
RUN apt install -y ./libmysqlclient20*
RUN mkdir /opt/bot
WORKDIR /opt/bot
COPY --from=build /opt/build/bin/ ./
EXPOSE 3000
ENV LANG C.UTF-8
ENTRYPOINT ["/opt/bot/bot"]

