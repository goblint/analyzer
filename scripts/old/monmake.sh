#!/bin/sh
# https://github.com/guard/guard/wiki/Analysis-of-inotify-events-for-different-editors
# http://stackoverflow.com/questions/11930442/make-inotifywait-group-multiple-file-updates-into-one
# sublime-text-3: use -e moved_to instead of -e modify
while change=$(inotifywait -r -q -e modify src); do
  ext=${change##*.}
  # only recompile if some ocaml source file changes
  if [ $ext != "ml" ] && [ $ext != "mli" ] && [ $ext != "mll" ] && [ $ext != "mly" ]; then
    continue
  fi
  clear
  echo $change
  make
  if [ $? -eq 0 ]; then
    notify-send "Build ok!"
    if [ $1 ]; then # type of regression tests to run, e.g. "file"
        ./scripts/spec/regression.sh $1
    fi
    if [ $? -eq 0 ]; then
      paplay /usr/share/sounds/freedesktop/stereo/complete.oga
    else
      : # paplay /usr/share/sounds/freedesktop/stereo/bell.oga
    fi
  else
    # TODO find a way to save stdout to var and still print it to avoid calling make again just for the error message
    msg=$(make)
    # send error message to sublime plugin using netcat
    echo -e "`pwd`\n$msg" | nc localhost 9999
    # notify-send -i stop Build failed!
    paplay /usr/share/sounds/freedesktop/stereo/bell.oga
  fi
done
