// optimistic memory analysis (w/o stack memory)

w1 "pointer $ is not saved [leak]"
w2 "freeing unallocated pointer $ [segfault]"
w3 "writing to unallocated pointer $ [segfault]"
w4 "overwriting unfreed pointer $ [leak]"
w5 "unsafe function usage [buffer overflow]"
w6 "copying into overlapping memory $ [undefined]"
w7 "using uninitialized memory $ [undefined]"
w8 "buffer overread $ [segfault]"
w9 "buffer overflow $ [segfault]"
w10 "reading unallocated pointer $ [segfault]"

// ----------------------------------------------
// unallocated memory
// ----------------------------------------------
start 	-w1> 	start 				malloc(_)
start 	-w1> 	start 				calloc(_,_)
start 	-w2> 	start 				_ = realloc($this, 0)
start 	-w2> 	start 				free($this)
start 	-w3> 	start 				*$this = _ 
start  	->   	ui_alloc 			$this = malloc(_)
start  	->   	ui_alloc 			$this = aligned_alloc(_,_)
start 	->   	ui_alloc  			$this = realloc(_,_)
start  	->   	alloc 				$this = calloc(_,_)
// Usage
start 	-w3> 	start 				_ = memset($this,_,_)
start 	-w3>    start 				_ = fread($this,_,_,_)
start 	-w3>    start 				_ = fgets($this,_,_)
start 	-w3>    start 				_ = gets_s($this,_)
start 	-w3,w5> start 				_ = gets($this)
start 	-w10>   start 				_ = fputs($this,_)
start 	-w10>   start 				_ = puts($this)
start 	-w10>   start 				_ = fwrite($this,_,_,_)
start 	-w3> 	start 				_ = memcpy($this,_,_)
start 	-w3> 	start 				_ = memmove($this,_,_)
start 	-w3> 	start 				_ = strcpy($this,_)
start 	-w3> 	start 				_ = strncpy($this,_,_)
start 	-w3> 	start 				_ = strcat($this,_)
start 	-w3> 	start 				_ = strncat($this,_,_)
// Assignments
start 	-w3>  start 				$this[i] = _
start 	-> 	  alloc 				$this = memset($other,_,i) when $other in alloc || length($other) == i
start 	->    alloc 				$this = fread($other,n,m,_) when $other in alloc || length($other) == (n*m)
start 	->    alloc 				$this = fgets($other,i,_) when $other in alloc || length($other) == i
start 	->    alloc 				$this = gets_s($other,i) when $other in alloc || length($other) == i
start 	-> 	  start 				$this = memcpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
start 	-> 	  start 				$this = memmove($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
start 	-> 	  alloc 				$this = strcpy($p1,$p2) when $p2 in alloc && length($p1) == length($p2)
start 	-> 	  start 				$this = strncpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
start 	-> 	  start 				$this = strncat($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)

// ----------------------------------------------
// unitialized memory
// ----------------------------------------------
ui_alloc 	-> 		start   	_ = realloc($this, 0)
ui_alloc 	->   	start   	free($this)
ui_alloc    -w4> 	ui_alloc    $this = malloc(_)
ui_alloc    -w4> 	alloc    	$this = calloc(_,_)
ui_alloc    -w4> 	ui_alloc    $this = aligned_alloc(_,_)
ui_alloc    -w4> 	ui_alloc    $this = realloc($other,_)
ui_alloc    -> 		ui_alloc    $this = realloc($this,_)
ui_alloc 	-w9> 	ui_alloc 	_ = memset($this,_,i) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = memcpy($this,_,i) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = memmove($this,_,i) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = strncpy($this,_,i) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = fread($this,n,m,_) when length($this) may < (n*m)
ui_alloc 	-w9> 	ui_alloc 	_ = fgets($this, i, _) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = gets_s($this, i) when length($this) may < i
ui_alloc 	-w6,w7> ui_alloc 	_ = memcpy($this,$this,_)
ui_alloc 	-w6,w7> ui_alloc 	_ = memmove($this,$this,_)
ui_alloc 	-w6,w7> ui_alloc 	_ = strcpy($this,$this)
ui_alloc 	-w6,w7> ui_alloc 	_ = strncpy($this,$this,_)
ui_alloc 	-w7> 	ui_alloc 	_ = memcpy(_,$this,_)
ui_alloc 	-w7> 	ui_alloc 	_ = memmove(_,$this,_)
ui_alloc 	-w7> 	ui_alloc 	_ = strcpy(_,$this)
ui_alloc 	-w7> 	ui_alloc 	_ = strncpy(_,$this,_)
ui_alloc 	->   	alloc 	  	_ = memcpy($this,$other,i) when $other in alloc && length($this) == i && length($other) >= i
ui_alloc 	->   	alloc 	  	_ = memmove($this,$other,i) when $other in alloc && length($this) == i && length($other) >= i
ui_alloc 	->   	alloc 	  	_ = strcpy($this,$other) when $other in alloc && length($this) == length($other)
ui_alloc 	->   	alloc 	  	_ = strncpy($this,$other,i) when $other in alloc && length($this) == i && length($other) >= i
ui_alloc 	-> 	 	alloc 		_ = memset($this,_,i) when length($this) == i
ui_alloc 	-> 	 	alloc 		_ = fread($this,n,m,_) when length($this) == (n*m)
// Usage
ui_alloc 	-w7,w8> ui_alloc 	_ = memcpy(_,$this,i) when length($this) may < i
ui_alloc 	-w7,w8> ui_alloc 	_ = memmove(_,$this,i) when length($this) may < i
ui_alloc 	-w7,w8> ui_alloc 	_ = strncpy(_,$this,i) when length($this) may < i
ui_alloc 	-w5> 	ui_alloc 	_ = gets($this)
ui_alloc 	-w7>    ui_alloc 	_ = puts($this)
ui_alloc 	-w7>    ui_alloc 	_ = fputs($this,_)
ui_alloc 	-w7>    ui_alloc 	_ = fwrite($this,_,_,_)
ui_alloc 	-w7> 	ui_alloc 	_ = strcat(_,$this)
ui_alloc 	-w7> 	ui_alloc 	_ = strncat(_,$this,_)
ui_alloc 	-w5> 	ui_alloc 	_ = strcat($this,_)
ui_alloc 	-w5> 	ui_alloc 	_ = strncat($this,_,_)
// Assignments
ui_alloc 	-w9> 	ui_alloc 		$this[i] = _ when length($this) may <= i
ui_alloc 	-w4> 	alloc 			$this = memset($other,_,i) when $other in alloc || length($other) == i
ui_alloc 	-w4>  	alloc 			$this = fread($other,n,m,_) when $other in alloc || length($other) == (n*m)
ui_alloc 	-w4>  	alloc 			$this = fgets($other,i,_) when $other in alloc || length($other) == i
ui_alloc 	-w4>  	alloc 			$this = gets_s($other,i) when $other in alloc || length($other) == i
ui_alloc 	-w4>  	start 			$this = memcpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_alloc 	-w4> 	start 			$this = memmove($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_alloc 	-w4> 	alloc 			$this = strcpy($p1,$p2) when $p2 in alloc && length($p1) == length($p2)
ui_alloc 	-w4> 	start 			$this = strncpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_alloc 	-w4> 	start 			$this = strncat($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_alloc 	-w4> 	start 			$this = _

// ----------------------------------------------
// initialized memory
// ----------------------------------------------
alloc  	->   	start   	_ = realloc($this, 0)
alloc   ->   	start   	free($this)
alloc  	-w4> 	ui_alloc    $this = malloc(_)
alloc  	-w4> 	alloc    	$this = calloc(_,_)
alloc 	->   	ui_alloc    $this = realloc($this,i) when length($this) may > i
alloc 	-w4> 	ui_alloc    $this = realloc(_,_)
alloc 	-w9> 	ui_alloc 	_ = memcpy($this,$other,i) when length($this) may < i
alloc 	-w9> 	ui_alloc 	_ = memset($this,_,i) when length($this) may < i
alloc 	-w9> 	ui_alloc 	_ = strncpy($this,$other,i) when length($this) may < i
alloc 	->  	ui_alloc 	_ = memcpy($this,$other,_) when $other may in ui_alloc
alloc 	->  	ui_alloc 	_ = memmove($this,$other,_) when $other may in ui_alloc
alloc 	->   	ui_alloc 	_ = strcpy($this,$other) when $other may in ui_alloc
alloc 	->   	ui_alloc 	_ = strncpy($this,$other,_) when $other may in ui_alloc
alloc 	-w6>  	ui_alloc 	_ = memcpy($this,$this,_)
alloc 	-w6>  	ui_alloc 	_ = strcpy($this,$this)
alloc 	-w6>  	ui_alloc 	_ = strncpy($this,$this,_)
alloc 	-w8>  	alloc 		_ = memcpy(_,$this,i) when length($this) may < i
alloc 	-w8>  	alloc 		_ = memmove(_,$this,i) when length($this) may < i
alloc 	-w8>  	alloc 		_ = strncpy(_,$this,i) when length($this) may < i
alloc 	-w5> 	alloc 		_ = gets($this)
alloc 	-w9> 	alloc 		_ = memset($this,_,i) when length($this) may < i
alloc 	-w9> 	alloc 		_ = fread($this,n,m,_) when length($this) may < (n*m)
alloc 	-w9>    alloc 	  	_ = fgets($this, i, _) when length($this) may < i
alloc 	-w9>    alloc 	  	_ = gets_s($this, i) when length($this) may < i
alloc 	-w8>    alloc 		_ = fwrite($this,n,m,_) when length($this) may < (n*m)
alloc 	-w5> 	ui_alloc 	_ = strcat($this,$other) when length($other) may > length($this)	
alloc 	-w5> 	ui_alloc 	_ = strncat($this,_,i) when length($this) may < i
// Assignments
alloc 	-w9> 	alloc 		$this[i] = _ when length($this) may <= i
alloc 	-w4> 	alloc 		$this = memset($other,_,i) when $other in alloc || length($other) == i
alloc 	-w4>  	alloc 		$this = fread($other,n,m,_) when $other in alloc || length($other) == (n*m)
alloc 	-w4>  	alloc 		$this = fgets($other,i,_) when $other in alloc || length($other) == i
alloc 	-w4>  	alloc 		$this = gets_s($other,i) when $other in alloc || length($other) == i
alloc 	-w4>  	start 		$this = memcpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
alloc 	-w4> 	start 		$this = memmove($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
alloc 	-w4> 	alloc 		$this = strcpy($p1,$p2) when $p2 in alloc && length($p1) == length($p2)
alloc 	-w4> 	start 		$this = strncpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
alloc 	-w4> 	start 		$this = strncat($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
alloc 	-w4> 	start 		$this = _

// ----------------------------------------------
// end states
// ----------------------------------------------
start        ->   end      _

// ----------------------------------------------
// unfreed memory
// ----------------------------------------------
_end "pointer is never freed [leak]"
_END "unfreed pointers: $"
