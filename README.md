# ecology [![Build Status](https://travis-ci.com/irreverent-pixel-feats/ecology.svg?branch=master)](https://travis-ci.com/irreverent-pixel-feats/ecology)

## Description

Using a multi-repo approach in software development means you have smaller projects in bite sized portions for newcomers to digest, without having to ignore the noise of the other projects in a mono-repo. The problem with the multi-repo approach is the administrative overhead.  

ecology is a tool for eliminating the administrative overhead of setting up new repos, configuring & maintaining CI and a way to reduce CIaaS [TravisCI, CircleCI, Bitbucket Pipelines] boilerplate.

The boring menial work that ecology automates is replaced with a streamlined collaboration process to determine whether the project should exist or not. 

ecology is an Haskell application. The definition of `EcologyProject` is [here](https://github.com/irreverent-pixel-feats/ecology/blob/1c90cfaf1f003c0441356a297ca1dda3f9a55461/ecology-core/src/Irreverent/Ecology/Core/Data.hs#L328-L340). You'll find an example ecology template for a multiproject haskell project [here](https://github.com/irreverent-pixel-feats/haskell-multiproject.template) 

### `TL;DR`

Manages and tracks an ecology of projects

### More

#### Why

I want to be able to create lots of repos for fast experimentation, modularity of work and sharing of code.

Couple of problems with it:

1. Its slow to create new repos
    - Create the repo
    - Bootstrap it for the given language
    - Setup CI for it (TravisCI, CircleCI, Bitbucket pipelines, etc...)
    - Setup notifications for it (slack/hipchat/etc...)
2. Have to manage those repos
    - e.g. change in slack/hipchat creds?
        - With TravisCI, creds are configured per repo, have 100 projects? Those are configured on a per repo basis,
          its going to be a long day.
    - update a template?
        - We don't want to have to go through and manually merge it through, at the very least
          we want automated attempts to happen with PRs raised.
        - Ultimately we want to be able to cope without having to update projects on template changes at
          all.
        - Same is true of any change really, be nice if we could provide a script to run and have it traverse the desired
          repos, make the changes and raise a PR on it for me to look at.
3. Documentation
    - When you have lots of repos it becomes easy to get lost. It would be nice to be able to generate a static site that
      says what a project is, and who are the "experts" in that project etc...
    - No project naming standards. In practice they end up resulting in long names for projects that
      you never remember off the top of your head when looking. We like our project names short and punchy,
      but would like to be able to search for a project by "what it is" via tags.

#### Ecology

Projects as Code.

API keys stored in AWS SMS parameter store, Ecology will sync them across projects that share the
same parameters.

Changes are shot out to all projects on updates.

When new projects are detected, new repos are created on GitHub/Bitbucket, CI is setup for them, and any other special things (e.g. Docker Hub repos) are setup for them.

More documentation to come.

## See Also

- [An ecology template for a multiproject haskell project](https://github.com/irreverent-pixel-feats/haskell-multiproject.template)
- [ecology launch announcement blog post](http://irreverentpixelfeats.com/posts/development/2018-07-21-ecology.html)
- [ambiata/anatomy](https://github.com/ambiata/anatomy)

## Building the lot

``` shell
bin/ci.common
```

## Building the projects

Each project can be built with the command:

``` shell
./mafia build
```

The first time you ever run it on your system it might take a while, as it will build and install
[`haskell-mafia/mafia`](https://github.com/haskell-mafia/mafia) on your system.

## Running the tests

``` shell
./mafia test
```
