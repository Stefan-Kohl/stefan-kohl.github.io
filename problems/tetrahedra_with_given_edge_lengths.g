
IsEdgeLengthsOfTetrahedron := function ( lngs )

  local  check, S6, T, reps, perm;

  check := function ( lngs )

    local  M, triangle;

    triangle := function ( L )
      return L[1] + L[2] > L[3]
         and L[1] + L[3] > L[2]
         and L[2] + L[3] > L[1];
    end;

    if not triangle(lngs{[1,2,3]}) then return false; fi;
    if not triangle(lngs{[1,4,5]}) then return false; fi;
    if not triangle(lngs{[2,4,6]}) then return false; fi;
    if not triangle(lngs{[3,5,6]}) then return false; fi;
    
    M := [ [ 0,         lngs[1]^2 , lngs[2]^2, lngs[4]^2, 1      ],
           [ lngs[1]^2, 0,          lngs[3]^2, lngs[5]^2, 1      ],
           [ lngs[2]^2, lngs[3]^2 , 0,         lngs[6]^2, 1      ],
           [ lngs[4]^2, lngs[5]^2 , lngs[6]^2, 0,         1      ],
           [ 1,         1,          1,         1,         0      ] ];

    return DeterminantMat(M) > 0;
  end;

  if   not IsList(lngs) or not ForAll(lngs,IsPosInt) or Length(lngs) <> 6
  then return false; fi;

  S6   := SymmetricGroup(6);
  T    := Group((1,2,4)(3,6,5),(1,3,5)(2,6,4),(1,2)(5,6));
  reps := List(RightCosets(S6,T),Representative);
  for perm in reps do
    if check(Permuted(lngs,perm)) then return true; fi;
  od;
  return false;
end;

PartitionsWhichAreEdgeLengthsOfTetrahedron := function ( n )
  return Filtered(Partitions(n,6),IsEdgeLengthsOfTetrahedron);
end;



