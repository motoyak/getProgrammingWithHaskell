fastFib n1 _ 0 = n1
fastFib n1 _ 1 = n1
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)