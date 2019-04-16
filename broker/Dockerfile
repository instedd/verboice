FROM erlang:17.5.6

# Hack to keep using this old erlang image based on debian:jessie
# (removes reference to jessie-updates repository)
RUN sed -i '/jessie-updates/d' /etc/apt/sources.list

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
ENV ERL_CRASH_DUMP_SECONDS=0

# Run
CMD make run
