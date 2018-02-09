FROM erlang:17.5.6

# Install dependencies
RUN apt-get update && \
  DEBIAN_FRONTEND=noninteractive apt-get install -y libzmq3-dev sox libsox-fmt-mp3 festival postfix curl ruby && \
  apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install gems for tz
RUN /usr/bin/gem install bundler -v 1.13.6
RUN /usr/bin/gem install tzinfo -v 0.3.44
RUN /usr/bin/gem install activesupport -v 3.2.22

# Install broker deps
ADD Makefile /app/
ADD rebar /app/
ADD rebar.config /app/

WORKDIR /app
RUN make deps

# Install the application
ADD . /app
RUN mkdir -p tmp/www log

# Compile
RUN make compile

ENV BROKER_BIND=any

# Run
CMD make run
