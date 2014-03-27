@echo off
if exist setup-x86.exe (
	start setup-x86.exe -P git,wget,unzip,make,m4,gcc,gcc4-core,libmpfr4,autoconf,flexdll,libncurses-devel,ruby,code2html,curl,ocaml,ocaml-compiler-libs,patch,openssh
) else (
	echo "Download Cygwin here: http://cygwin.com/setup-x86.exe"
	pause
)