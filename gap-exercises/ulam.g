
UlamSpiral := function ( size, filename )

  local  spiral, smallprimes, n, p, r,
         middle, edgelength, edgepos, direction, i, j, zero, one;

  smallprimes := Filtered([2..size],IsPrimeInt);
  spiral      := NullMat(size,size,GF(2));

  if size mod 2 = 0 then middle := [size/2,size/2];
                    else middle := [(size + 1)/2,(size + 1)/2]; fi;

  zero := Zero(GF(2)); one := One(GF(2));

  spiral[middle[1]][middle[2]] := one;

  for p in smallprimes do
    i := middle[1]; j := middle[2];
    edgelength := 2; edgepos := 1; direction := 0; r := 1;
    for n in [2..size^2] do
      if   direction = 0 then j := j + 1;
      elif direction = 1 then i := i + 1;
      elif direction = 2 then j := j - 1;
      elif direction = 3 then i := i - 1; fi;
      r       := r + 1;
      edgepos := edgepos + 1;
      if r = p then
        if n > p then spiral[i][j] := one; fi;
        r := 0;
      fi;
      if edgepos = edgelength then
        direction := (direction + 1) mod 4;
        if direction in [0,2] then edgelength := edgelength + 1; fi;
        edgepos := 1;
      fi;
    od;
  od;

  SaveAsBitmapPicture(spiral,filename);
end;

