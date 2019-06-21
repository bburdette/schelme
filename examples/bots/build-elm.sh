#  #!/bin/bash

clear

clear

# this seems to help clear the screen before the build.
# 'clear' alone sometimes doesn't happen until the build is almost done.
echo "starting elm make... wait for 'build complete'!"

# elm 19!
time elm make src/Main.elm --output ./server/static/main.js
# print this because elm doesn't print a message when the link
# step is finally done.
echo build complete! 
