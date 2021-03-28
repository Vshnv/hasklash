FROM fpco/stack-build:latest 
WORKDIR /app
COPY . /app
ENTRYPOINT stack run
