  $ goblint 01-simple_rc.c
  Live lines: 12
  No lines with dead code found by solver.
  Total lines (logical LoC): 12
  [Warning][Race] Memory location myglobal@01-simple_rc.c:4:5-4:13 (race with conf. 110):
    write with [mhp:{tid=[main, t_fun@01-simple_rc.c:17:3-17:40]},
              lock:{mutex1}, thread:[main, t_fun@01-simple_rc.c:17:3-17:40]] (conf. 110) (01-simple_rc.c:10:3-10:22)
    write with [mhp:{tid=[main]; created={[main, t_fun@01-simple_rc.c:17:3-17:40]}}, lock:{mutex2}, thread:[main]] (conf. 110) (01-simple_rc.c:19:3-19:22)
    read with [mhp:{tid=[main, t_fun@01-simple_rc.c:17:3-17:40]}, lock:{mutex1}, thread:[main, t_fun@01-simple_rc.c:17:3-17:40]] (conf. 110) (01-simple_rc.c:10:3-10:22)
    read with [mhp:{tid=[main]; created={[main, t_fun@01-simple_rc.c:17:3-17:40]}}, lock:{mutex2}, thread:[main]] (conf. 110) (01-simple_rc.c:19:3-19:22)
  
  Summary for all memory locations:
  	safe:            0
  	vulnerable:      0
  	unsafe:          1
  	-------------------
  	total:           1
