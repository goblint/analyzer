failure=0
res=$(./goblint --sets result fast_xml tests/arinc_wcet/extracted.json 2>&1)
if echo $res | grep -q -e 'deadline'; then
   echo "Test 0 failed, deadline violated"
   failure=1
fi
if echo $res | grep -q -e 'error'; then
   echo "Test 0 failed, error:"
   echo $res
   failure=1
fi
if echo $res | grep -q -e 'NB! Execution'; then
   echo "Test 0 failed, end not reached:"
   echo $res
   failure=1
fi

res=$(./goblint --sets result fast_xml tests/arinc_wcet/minimal_problematic.json 2>&1)
if echo $res | grep -q -e 'deadline'; then
   echo "Test 1 failed, deadline violated"
   failure=1
fi
if echo $res | grep -q -e 'error'; then
   echo "Test 1 failed, error:"
   echo $res
   failure=1
fi
if echo $res | grep -q -e 'NB! Execution'; then
   echo "Test 1 failed, end not reached:"
   echo $res
   failure=1
fi

res=$(./goblint --sets result fast_xml tests/arinc_wcet/event_up_and_sth_else.json 2>&1)
if echo $res | grep -q -e 'deadline'; then
   echo "Test 2 failed, deadline violated"
   failure=1
fi
if echo $res | grep -q -e 'error'; then
   echo "Test 2 failed, error:"
   echo $res
   failure=1
fi
if echo $res | grep -q -e 'NB! Execution'; then
   echo "Test 2 failed, end not reached:"
   echo $res
   failure=1
fi

res=$(./goblint --sets result fast_xml tests/arinc_wcet/missing_set_event.json 2>&1)
if ! echo $res | grep -q -e 'NB! Execution'; then
   echo "Test 2 failed, end not reached:"
   echo $res
   failure=1
fi


exit $failure
