#!/bin/sh
# https://github.com/guard/guard/wiki/Analysis-of-inotify-events-for-different-editors
# http://stackoverflow.com/questions/11930442/make-inotifywait-group-multiple-file-updates-into-one
while file=$(inotifywait -r -q -e moved_to src); do
  ext=${file##*.}
  # only recompile if some ocaml source file changes
  if [ $ext != "ml" ] && [ $ext != "mli" ] && [ $ext != "mll" ] && [ $ext != "mly" ]; then
    continue
  fi
  clear
  make
  if [ $? -eq 0 ]; then
    clear
    notify-send "Build ok!"
    #./test.sh file
    ./scripts/regression.sh ${1-"file"}
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
