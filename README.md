## Issue Tracker

- required software:
  - node (>= 16.10)
  - npm (>= 8.17)
  - docker (>= 20.10)
  - cabal (>= 3.8)
  - ghc (>= 9.4)

- instructions:
  - From the `env` directory, run `docker-compose up -d` to start the database in a docker container
  - Run `cabal build` to build the back end code.
  - Run `cabal run init` to intialize the database (will prompt you to type 'y' to continue)
  - Run `cabal run dev-server`. This will build the client-side javascript bundle and start a server.
  - Open browser and navigate to `http://localhost:8080'

