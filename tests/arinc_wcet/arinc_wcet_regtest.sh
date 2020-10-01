if ./goblint --set arinc_cfg_id 0 | grep -q 'deadline'; then
   echo "Test 0 failed, deadline violated"
fi
if ./goblint --set arinc_cfg_id 1 | grep -q 'deadline'; then
   echo "Test 1 failed, deadline violated"
fi
