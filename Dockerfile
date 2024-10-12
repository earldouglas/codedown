FROM node:lts-slim

COPY . /app
RUN npm install -g codedown

ENTRYPOINT [ "codedown" ]