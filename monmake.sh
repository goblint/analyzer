#!/bin/sh
while inotifywait -r -q -e modify src; do
	clear
	make
	if [ $? -eq 0 ]; then
		clear
		#./test.sh file
		./test.sh "spec --sets spec.file src/spec/file.spec"
		#firefox result/file.c.html
	else
		notify-send -i stop Build failed!
	fi
done
