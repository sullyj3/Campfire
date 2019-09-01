This repo contains both the frontend, in Elm, and the backend, in Haskell.

To build the backend, in the `backend` directory, do `stack build`.

To build the frontend, do `elm-app build`.

To run the backend, do `stack exec backend-exe`.
When running the backend, supply the URL of the postgres database with the Env variable DATABASE_URL.

To run the frontend, use `elm-app start`
When running the frontend, supply the backend API url with the Env variable ELM_APP_API_URL.
