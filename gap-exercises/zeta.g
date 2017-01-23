
H := n -> Sum([1..n],i->Float(1/i));
B := n -> H(n) + EXP_FLOAT(H(n))*LOG_FLOAT(H(n));
D := n -> B(n) - Sigma(n); # Always positive for n > 1 <=> RH.

