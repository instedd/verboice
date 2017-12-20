Welcome to Verboice [![Build Status](https://travis-ci.org/instedd/verboice.svg?branch=master)](https://travis-ci.org/instedd/verboice)
=================

Voice is the most universal and inclusive means of communication, and it's an ideal way to expand the reach and impact of health and humanitarian technologies. Verboice is a free and open-source tool that makes it easy for anyone to create and run projects that interact via voice, allowing your users to listen and record messages in their own language and dialect or answer questions with a phone keypad. Verboice projects can start small and scale up, making it possible to improve lives even in communities previously closed off by literacy and technological barriers.


Getting Started
-------------
[Start using Verboice now](http://verboice.instedd.org)

[Verboice Help](https://github.com/instedd/verboice/wiki)

[What's coming next?](https://github.com/instedd/verboice/milestones)

[Installing your own server](https://github.com/instedd/verboice/wiki/Installing)

Docker development
------------------

IMPORTANT: as of Sep 23rd, 2016, only Verboice's web app and DB components are dockerised. This is mainly useful to test interactions with other InSTEDD platform components that DON'T involve actually making or receiving calls (for example: managing channels with the help of Guisso and Pigeon).

`docker-compose.yml` file build a development environment mounting the current folder and running rails in development environment.

Run the following commands to have a stable development environment.

```
$ docker-compose run --rm --no-deps web bundle install
$ docker-compose run --rm web bash
root@web_1 $ rake db:setup db:seed
$ docker-compose up
```

You can also run the frontend unit tests inside the docker container. Here's how:

```
$ docker-compose run --rm web rake db:test:prepare
$ docker-compose run --rm web rspec
```

Deploying with Capistrano
-------------------------

Verboice is deployed with Capistrano. After `bundle install`ing, run:

```
$ cap -s branch=feature/my_branch deploy HOSTS=verboice-stg.instedd.org RVM=1
```

This will deploy `feature/my_branch` code to the host at `verboice-stg.instedd.org` using RVM. Your mileage may vary.
