debug_tmp=$debug
export debug=false # temporarily disable debug output
n=0
c=0
dir=${2-"tests/regression/18-file"}
for f in $dir/*.c; do
	./scripts/spec/check.sh $f ${1-"file"} 2>/dev/null | python scripts/spec/regression.py $f && ((c++))
	((n++))
done
debug=$debug_tmp
msg="passed $c/$n tests"
echo $msg
if [ $c -eq $n ]; then
	exit 0
else
	notify-send -i stop "$msg"
	exit 1
fi
