FROM fpco/stack-build:latest 
WORKDIR /app
COPY . /app
RUN ["stack", "run"]
