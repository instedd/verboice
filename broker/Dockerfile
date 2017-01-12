FROM erlang:17.5.6

# Install dependencies
RUN apt-get update && \
  DEBIAN_FRONTEND=noninteractive apt-get install -y libzmq3-dev sox libsox-fmt-mp3 festival postfix curl ruby && \
  apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install deps
ADD Makefile /app/
ADD rebar /app/
ADD rebar.config /app/

WORKDIR /app
RUN make deps

# Install the application
ADD . /app

# Compile
RUN make compile

# Run
CMD make run
