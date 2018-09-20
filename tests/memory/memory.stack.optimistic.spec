// optimistic memory analysis (w/ stack memory)

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
w11 "freeing stack memory $ [segfault]"
w12 "reallocating stack memory $ [segfault]"

// ----------------------------------------------
// init / detecting stack memory
// ----------------------------------------------
init -> ui_stack 	$this when stack_alloc($this)
// Rest ends up automatically in start

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
start  	->   	ui_stack 			$this = alloca(_)
start  	->   	ui_stack 			$this = __builtin_alloca(_)
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
start 	-> 	  alloc 				$this = strncpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
start 	-> 	  alloc 				$this = strncat($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)

// ----------------------------------------------
// unitialized memory
// ----------------------------------------------
ui_alloc 	-> 		start   	_ = realloc($this, 0)
ui_alloc 	->   	start   	free($this)
ui_alloc    -w4> 	ui_alloc    $this = malloc(_)
ui_alloc    -w4> 	alloc    	$this = calloc(_,_)
ui_alloc    -w4> 	ui_stack    $this = alloca(_)
ui_alloc    -w4> 	ui_alloc    $this = aligned_alloc(_,_)
ui_alloc    -w4> 	ui_alloc    $this = realloc($other,_)
ui_alloc    -> 		ui_alloc    $this = realloc($this,_)
ui_alloc 	-w9> 	ui_alloc 	_ = memset($this,_,i) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = memcpy($this,_,i) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = memmove($this,_,i) when length($this) may < i
ui_alloc 	-w9> 	ui_alloc 	_ = strcpy($this,$other) when length($this) may < (length($other) + 1)
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
ui_alloc 	->   	alloc 	  	_ = memcpy($this,$other,i) when ($other in alloc || $other in stack) && length($this) == i && length($other) >= i
ui_alloc 	->   	alloc 	  	_ = memmove($this,$other,i) when ($other in alloc || $other in stack) && length($this) == i && length($other) >= i
ui_alloc 	->   	alloc 	  	_ = strcpy($this,$other) when ($other in alloc || $other in stack) && length($this) == length($other)
ui_alloc 	->   	alloc 	  	_ = strncpy($this,$other,i) when ($other in alloc || $other in stack) && length($this) == i && length($other) >= i
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
// unitialized stack memory
// ----------------------------------------------
ui_stack 	-w12> 	start   	_ = realloc($this, _)
ui_stack 	-w11> 	start   	_ = realloc($this, 0)
ui_stack 	-w11>   start   	free($this)
ui_stack    -w4> 	ui_alloc    $this = malloc(_)
ui_stack    -w4> 	alloc    	$this = calloc(_,_)
ui_stack    -w4> 	ui_stack    $this = alloca(_)
ui_stack    -w4> 	ui_alloc    $this = aligned_alloc(_,_)
ui_stack    -w4> 	ui_alloc    $this = realloc($other,_)
ui_stack 	-w9> 	ui_stack 	_ = memset($this,_,i) when length($this) may < i
ui_stack 	-w9> 	ui_stack 	_ = memcpy($this,_,i) when length($this) may < i
ui_stack 	-w9> 	ui_stack 	_ = memmove($this,_,i) when length($this) may < i
ui_stack 	-w9> 	ui_stack 	_ = strcpy($this,$other) when length($this) may < (length($other) + 1)
ui_stack 	-w9> 	ui_stack 	_ = strncpy($this,_,i) when length($this) may < i
ui_stack 	-w9> 	ui_stack 	_ = fread($this,n,m,_) when length($this) may < (n*m)
ui_stack 	-w9> 	ui_stack 	_ = fgets($this, i, _) when length($this) may < i
ui_stack 	-w9> 	ui_stack 	_ = gets_s($this, i) when length($this) may < i
ui_stack 	-w6,w7> ui_stack 	_ = memcpy($this,$this,_)
ui_stack 	-w6,w7> ui_stack 	_ = memmove($this,$this,_)
ui_stack 	-w6,w7> ui_stack 	_ = strcpy($this,$this)
ui_stack 	-w6,w7> ui_stack 	_ = strncpy($this,$this,_)
ui_stack 	-w7> 	ui_stack 	_ = memcpy(_,$this,_)
ui_stack 	-w7> 	ui_stack 	_ = memmove(_,$this,_)
ui_stack 	-w7> 	ui_stack 	_ = strcpy(_,$this)
ui_stack 	-w7> 	ui_stack 	_ = strncpy(_,$this,_)
ui_stack 	->   	stack 	  	_ = memcpy($this,$other,i) when ($other in alloc || $other in stack) && length($this) == i && length($other) >= i
ui_stack 	->   	stack 	  	_ = memmove($this,$other,i) when ($other in alloc || $other in stack) && length($this) == i && length($other) >= i
ui_stack 	->   	stack 	  	_ = strcpy($this,$other) when ($other in alloc || $other in stack) && length($this) == length($other)
ui_stack 	->   	stack 	  	_ = strncpy($this,$other,i) when ($other in alloc || $other in stack) && length($this) == i && length($other) >= i
ui_stack 	-> 	 	stack 		_ = memset($this,_,i) when length($this) == i
ui_stack 	-> 	 	stack 		_ = fread($this,n,m,_) when length($this) == (n*m)
// Usage
ui_stack 	-w7,w8> ui_stack 	_ = memcpy(_,$this,i) when length($this) may < i
ui_stack 	-w7,w8> ui_stack 	_ = memmove(_,$this,i) when length($this) may < i
ui_stack 	-w7,w8> ui_stack 	_ = strncpy(_,$this,i) when length($this) may < i
ui_stack 	-w5> 	ui_stack 	_ = gets($this)
ui_stack 	-w7>    ui_stack 	_ = puts($this)
ui_stack 	-w7>    ui_stack 	_ = fputs($this,_)
ui_stack 	-w7>    ui_stack 	_ = fwrite($this,_,_,_)
ui_stack 	-w7> 	ui_stack 	_ = strcat(_,$this)
ui_stack 	-w7> 	ui_stack 	_ = strncat(_,$this,_)
ui_stack 	-w5> 	ui_stack 	_ = strcat($this,_)
ui_stack 	-w5> 	ui_stack 	_ = strncat($this,_,_)
// Assignments
ui_stack 	-w9> 	ui_stack 		$this[i] = _ when length($this) may <= i
ui_stack 	-w4> 	alloc 			$this = memset($other,_,i) when $other in alloc || length($other) == i
ui_stack 	-w4>  	alloc 			$this = fread($other,n,m,_) when $other in alloc || length($other) == (n*m)
ui_stack 	-w4>  	alloc 			$this = fgets($other,i,_) when $other in alloc || length($other) == i
ui_stack 	-w4>  	alloc 			$this = gets_s($other,i) when $other in alloc || length($other) == i
ui_stack 	-w4>  	start 			$this = memcpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_stack 	-w4> 	start 			$this = memmove($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_stack 	-w4> 	alloc 			$this = strcpy($p1,$p2) when $p2 in alloc && length($p1) == length($p2)
ui_stack 	-w4> 	start 			$this = strncpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_stack 	-w4> 	start 			$this = strncat($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
ui_stack 	-w4> 	start 			$this = _


// ----------------------------------------------
// initialized memory
// ----------------------------------------------
alloc  	->   	start   	_ = realloc($this, 0)
alloc   ->   	start   	free($this)
alloc  	-w4> 	ui_alloc    $this = malloc(_)
alloc  	-w4> 	alloc    	$this = calloc(_,_)
alloc   -w4> 	ui_stack    $this = alloca(_)
alloc 	->   	ui_alloc    $this = realloc($this,i) when length($this) may > i
alloc 	-w4> 	ui_alloc    $this = realloc(_,_)
alloc 	-w9> 	ui_alloc 	_ = memcpy($this,$other,i) when length($this) may < i
alloc 	-w9> 	ui_alloc 	_ = memset($this,_,i) when length($this) may < i
alloc 	-w9> 	ui_alloc 	_ = strcpy($this,$other) when length($this) may < (length($other) + 1)
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
// initialized stack memory
// ----------------------------------------------
stack  	-w12>   start   	_ = realloc($this, _)
stack  	-w11>   start   	_ = realloc($this, 0)
stack   -w11>   start   	free($this)
stack  	-w4> 	ui_alloc    $this = malloc(_)
stack  	-w4> 	alloc    	$this = calloc(_,_)
stack   -w4> 	ui_stack    $this = alloca(_)
stack 	->   	ui_alloc    $this = realloc($this,i) when length($this) may > i
stack 	-w4> 	ui_alloc    $this = realloc(_,_)
stack 	-w9> 	ui_stack 	_ = memcpy($this,$other,i) when length($this) may < i
stack 	-w9> 	ui_stack 	_ = memset($this,_,i) when length($this) may < i
stack 	-w9> 	ui_stack 	_ = strcpy($this,$other) when length($this) may < (length($other) + 1)
stack 	-w9> 	ui_stack 	_ = strncpy($this,$other,i) when length($this) may < i
stack 	->  	ui_stack 	_ = memcpy($this,$other,_) when $other may in ui_alloc
stack 	->  	ui_stack 	_ = memmove($this,$other,_) when $other may in ui_alloc
stack 	->   	ui_stack 	_ = strcpy($this,$other) when $other may in ui_alloc
stack 	->   	ui_stack 	_ = strncpy($this,$other,_) when $other may in ui_alloc
stack 	-w6>  	ui_stack 	_ = memcpy($this,$this,_)
stack 	-w6>  	ui_stack 	_ = strcpy($this,$this)
stack 	-w6>  	ui_stack 	_ = strncpy($this,$this,_)
stack 	-w8>  	stack 		_ = memcpy(_,$this,i) when length($this) may < i
stack 	-w8>  	stack 		_ = memmove(_,$this,i) when length($this) may < i
stack 	-w8>  	stack 		_ = strncpy(_,$this,i) when length($this) may < i
stack 	-w5> 	stack 		_ = gets($this)
stack 	-w9> 	stack 		_ = memset($this,_,i) when length($this) may < i
stack 	-w9> 	stack 		_ = fread($this,n,m,_) when length($this) may < (n*m)
stack 	-w9>    stack 	  	_ = fgets($this, i, _) when length($this) may < i
stack 	-w9>    stack 	  	_ = gets_s($this, i) when length($this) may < i
stack 	-w8>    stack 		_ = fwrite($this,n,m,_) when length($this) may < (n*m)
stack 	-w5> 	ui_stack 	_ = strcat($this,$other) when length($other) may > length($this)	
stack 	-w5> 	ui_stack 	_ = strncat($this,_,i) when length($this) may < i
// Assignments
stack 	-w9> 	stack 		$this[i] = _ when length($this) may <= i
stack 	-w4> 	stack 		$this = memset($other,_,i) when $other in alloc || length($other) == i
stack 	-w4>  	stack 		$this = fread($other,n,m,_) when $other in alloc || length($other) == (n*m)
stack 	-w4>  	stack 		$this = fgets($other,i,_) when $other in alloc || length($other) == i
stack 	-w4>  	stack 		$this = gets_s($other,i) when $other in alloc || length($other) == i
stack 	-w4>  	start 		$this = memcpy($p1,$p2,i) when ($p1 in stack && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
stack 	-w4> 	start 		$this = memmove($p1,$p2,i) when ($p1 in stack && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
stack 	-w4> 	stack 		$this = strcpy($p1,$p2) when $p2 in alloc && length($p1) == length($p2)
stack 	-w4> 	start 		$this = strncpy($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
stack 	-w4> 	start 		$this = strncat($p1,$p2,i) when ($p1 in alloc && length($p1) >= i) || ($p2 in alloc && length($p2) >= i && length($p1) == i)
stack 	-w4> 	start 		$this = _

// ----------------------------------------------
// end states
// ----------------------------------------------
start       ->   end        _
ui_stack    ->   end        _
stack       ->   end        _

// ----------------------------------------------
// unfreed memory
// ----------------------------------------------
_end "pointer is never freed [leak]"
_END "unfreed pointers: $"
