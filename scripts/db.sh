#!/bin/sh

docker exec -it dev-issues-tracker-postgres psql --username=postgres --dbname=main

