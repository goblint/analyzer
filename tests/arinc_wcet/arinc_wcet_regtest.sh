if ./goblint --set arinc_cfg_id 0 --sets result fast_xml tests/arinc_wcet/extracted.json | grep -q 'deadline'; then
   echo "Test 0 failed, deadline violated"
fi
if ./goblint --set arinc_cfg_id 1 --sets result fast_xml tests/arinc_wcet/minimal_problematic.json | grep -q 'deadline'; then
   echo "Test 1 failed, deadline violated"
fi
