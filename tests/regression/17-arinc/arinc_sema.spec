w1 "LAP_Se_CreateSemaphore"
w2 "LAP_Se_WaitSemaphore"
w3 "LAP_Se_SignalSemaphore"


1	-w1>	1	LAP_Se_CreateSemaphore(_)
1	-w2>	1	LAP_Se_WaitSemaphore(_)
1	-w3>	1	LAP_Se_SignalSemaphore(_)



//1		-("cur>max" if cur > max)>		created		LAP_Se_CreateSemaphore(_, cur, max, $id, _)
//(created,wait)	->		(wait if $id.cur == 0 else created)		LAP_Se_WaitSemaphore($id, time, _)	$id.cur--
//(created,wait)	-("cur>max" if cur > max)>	(wait if $id.cur == 0 else created)	LAP_Se_SignalSemaphore($id, _)	$id.cur++