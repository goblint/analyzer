implement so that parsed intervals automatically include constant in interval instead oif linexp definition. 
f.x. a + b + 2 < 2 
should become
a + b = beta, beta \in [-inf, 2]
and not
a + b + 2 = beta, beta \in [-inf, 0]
easier handling of .info and also simplifies scaling (i think).
