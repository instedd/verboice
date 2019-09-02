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

We use [`dockerdev`](https://github.com/waj/dockerdev) to get domain names for the components of the app - so it can interoperate with other apps from the InSTEDD platform. Although it's optional, your first step to have Verboice running on Docker should be to install `dockerdev` - it needs to be running before creating any Verboice container, network and other objects.

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

Testing with Zeus
-----------------

Compounding Docker and Rails load times makes for a terrible out of the box testing experience. To mitigate that, we use Zeus (https://github.com/burke/zeus). Zeus pre-loads your Rails application so you only pay the initialization cost once.

The `Dockerfile.dev` creates an image with Zeus already installed on it. To run tests with Zeus, first you need to:

1. From the app root, run: `zeus init`. This is a one time process that will create a couple of files `custom_plan.rb` and `zeus.json`.
1. To start Zeus, from the app root, run: `zeus start`.
1. From another terminal, run `zeus test spec` to run tests.

Deploying with Capistrano
-------------------------

Verboice is deployed with Capistrano. After `bundle install`ing, run:

```
$ cap -s branch=feature/my_branch deploy HOSTS=verboice-stg.instedd.org RVM=1
```

This will deploy `feature/my_branch` code to the host at `verboice-stg.instedd.org` using RVM. Your mileage may vary.

Intercom
--------

Verboice supports Intercom as its CRM platform. To load the Intercom chat widget, simply start Verboice with the env variable `INTERCOM_APP_ID` set to your Intercom app id (https://www.intercom.com/help/faqs-and-troubleshooting/getting-set-up/where-can-i-find-my-workspace-id-app-id).

Verboice will forward any conversation with a logged user identifying them through their email address. Anonymous, unlogged users will also be able to communicate.

If you don't want to use Intercom, you can simply omit `INTERCOM_APP_ID` or set it to `''`.

To test the feature in development, add the `INTERCOM_APP_ID` variable and its value to the `environment` object inside the `web` service in `docker-compose.yml`.
