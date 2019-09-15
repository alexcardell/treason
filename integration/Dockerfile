FROM node:12.10.0-alpine

WORKDIR /home/node/app

ENV NPM_CONFIG_PREFIX=/home/node/.npm-global

# ENV PATH="$NPM_CONFIG_PREFIX/bin:$PATH"

RUN apk add bash tmux

USER node

# RUN mkdir ~/.npm-global \
#     && npm install -g esy@0.5.6 \
#     && export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"

RUN yarn global add esy@0.5.6

COPY . .

# RUN esy install \
#     && esy pesy \
#     && esy build
