#!/bin/sh
while inotifywait -r -q -e modify src; do
	clear
	make
	if [ $? -eq 0 ]; then
		clear
		./fileTest.sh 3
		#firefox result/file.c.html
	fi
done
