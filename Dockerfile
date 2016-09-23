FROM instedd/nginx-rails:1.9

# Install ZMQ
RUN apt-get update && \
  DEBIAN_FRONTEND=noninteractive apt-get install -y libzmq3-dev sox libsox-fmt-mp3 && \
  apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
