FROM haskell:latest
RUN stack --version

RUN stack setup
RUN stack --resolver lts build stack
RUN stack exec simple-counter-backend

EXPOSE 3000