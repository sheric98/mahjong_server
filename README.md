# Mahjong Server

This is a server that runs Mahjong using Python flask to host the server, and the mahjong logic
and game itself is written in Elm.

## Run Server

To run the server, simply enter `flask run`.

## Compiling / Changing Elm

If you want to make any changes to the Elm files, to compile, simply run `make` while
inside the src directory. This will output a `main.js`. All you have to do from
here is `mv main.js ../static` to move it to the appropriate location. You
will have to restart the server for the changes to take effect.
