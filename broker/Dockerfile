FROM erlang:17.5.6 AS dev

# Hack to keep using this old erlang image based on debian:jessie
# (removes reference to jessie-updates repository and use archive.debian.org)
RUN sed -i '/jessie-updates/d' /etc/apt/sources.list && \
    sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's/security.debian.org/archive.debian.org/g' /etc/apt/sources.list

# # Cleanup expired Let's Encrypt CA (Sept 30, 2021)
RUN sed -i '/^mozilla\/DST_Root_CA_X3/s/^/!/' /etc/ca-certificates.conf && update-ca-certificates -f

# Install dependencies (must use --force-yes because apt keys expired)
RUN apt-get -q update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes \
      libzmq3-dev sox libsox-fmt-mp3 festival postfix curl ruby && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install gems for tz (gem API has been removed, so we download and install the
# gemspec manually. A better solution would be to upgrade rubygems, but `gem
# update --system` relies on the removed API.
#RUN /usr/bin/gem install --no-ri --no-rdoc bundler -v 1.13.6 && \
#    /usr/bin/gem install --no-ri --no-rdoc tzinfo -v 0.3.44 && \
#    /usr/bin/gem install --no-ri --no-rdoc concurrent-ruby -v 1.1.9 && \
#    /usr/bin/gem install --no-ri --no-rdoc activesupport -v 3.2.22
RUN cd /tmp && \
    for gemspec in activesupport-3.2.22 bundler-1.13.6 concurrent-ruby-1.1.9 i18n-0.6.11 multi_json-1.15.0 tzinfo-0.3.44; do \
      wget --quiet https://rubygems.org/downloads/${gemspec}.gem; \
    done && \
    gem install --local --no-ri --no-rdoc *.gem && \
    rm *.gem

ENV BROKER_BIND=any
ENV ERL_CRASH_DUMP_SECONDS=0

WORKDIR /app

FROM dev AS release

# Install broker deps
ADD Makefile /app/
ADD rebar /app/
ADD rebar.config /app/
RUN make deps

# Install the application
ADD . /app
RUN mkdir -p tmp/www log

# Compile
RUN make compile

# Run
CMD make run
