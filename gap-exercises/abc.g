
abcTriplesByRadical := function ( radical, bound )

  local  triples, values, values_last, factors, a, b, c, rad_abc, ratio;

  factors := Union(Factors(radical),[1]);
  values  := [1];
  repeat
    values_last := values;
    values := Filtered(Union(List(factors,p->values*p)),n->n<bound);
  until values = values_last;
  triples := [];
  for a in values do
    for b in values do
      if b >= a then break; fi;
      if a + b in values and Gcd(a,b) = 1 then
        c       := a + b;
        rad_abc := Product(Set(Factors(a*b*c)));
        ratio   := LOG_FLOAT(Float(c))/LOG_FLOAT(Float(rad_abc));
        if ratio > 7/5 then Add(triples,[a,b,c]); fi;
      fi;
    od;
  od;
  return triples;
end;

