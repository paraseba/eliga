# eliga

A Hipchat bot to gather standup status updates, process them, format them, and
resend them via Hipchat or email, etc

## Features

- Seats on hipchat listening to users updates
- reminds users to send their updates as the time comes close to the limit
- knows when it has all the updates
- persists the state to support restarts
- switchable backends (rigth now memory and ElasticSearch)
- extensible to other chat systems
- can learn to do more things (composable behaviors)

These are all WIP

## Motivation

This is a project used to teach Clojure to people with experience in other
languages

## License

Copyright © 2014 Nicolas Berger & Sebastian Galkin

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
