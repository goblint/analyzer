# Using C-Reduce to minimize failing test cases

This folder contains some "interestingness" scripts that have been useful when using C-Reduce to find cases that trouble Goblint. These scripts need to be adapted. They are called using `creduce script.sh source.c`. This will replace the source file with a minimized version that satisfies the script. 

The scripts have to work in an arbitrary temporary directory, hence the paths to anything outside have to be hardcoded. On the other hand, the file is copied and modified within that temporary directory: do not use an absolute path if you want to create variations of the source file (such as `sed "..." $FILE.c > $FILE.new.c`). If C-Reduce goes on for a long time without showing progress, it's likely that something is wrong.  

You may have to disable the [buggy line marker pass](https://github.com/csmith-project/creduce/issues/195). One brutish way of doing this is to edit `/usr/bin/creduce` or its homebrew equivalent and comment out the line with `pass_line_markers`. 