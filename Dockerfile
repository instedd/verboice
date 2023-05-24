FROM ruby:2.3.8

# Cleanup expired Let's Encrypt CA (Sept 30, 2021)
RUN sed -i '/^mozilla\/DST_Root_CA_X3/s/^/!/' /etc/ca-certificates.conf && update-ca-certificates -f

RUN echo 'deb http://archive.debian.org/debian stretch main\n\
  deb http://archive.debian.org/debian-security stretch/updates main' > /etc/apt/sources.list

# Install dependencies
RUN apt-get -q update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
      libzmq3-dev sox libsox-fmt-mp3 festival nodejs && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /app

# Install gem bundle
ADD Gemfile /app/
ADD Gemfile.lock /app/
RUN bundle install --jobs 3 --without development test

# Install the application
ADD . /app

# Precompile assets
RUN bundle exec rake assets:precompile RAILS_ENV=production SECRET_KEY_BASE=secret

ENV RAILS_LOG_TO_STDOUT=true
ENV RAILS_ENV=production
EXPOSE 80

# Add scripts
ADD docker/database.yml /app/config/database.yml

CMD ["puma", "-e", "production", "-b", "tcp://0.0.0.0:80"]
