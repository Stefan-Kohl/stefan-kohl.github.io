
RationalPointsOnUnitSphere := function ( max_abc, size, filename )

  local  picture, solutions, a, b, c, d, sum,
         pixelcoords, mirror, zero, one, i, j;

  zero := Zero(GF(2)); one := One(GF(2));

  picture := NullMat(size,size,GF(2)); # create a white picture:
  for i in [1..size] do for j in [1..size] do picture[i][j] := one; od; od;

  solutions := 0;
  for a in [1..max_abc] do
    Print("a = ",a,", #solutions = ",solutions,"\n");
    for b in [1..a] do
      for c in [1..b] do
        sum := a^2 + b^2 + c^2;
        d := RootInt(sum);
        if d^2 = sum then
          pixelcoords := List( size * [a,b,c]/d, Int ) + 1;
          for mirror in Arrangements([1..3],2) do
            picture[pixelcoords[mirror[1]]][pixelcoords[mirror[2]]] := zero;
          od;
          solutions := solutions + 1;
        fi;
      od;
    od;
  od;

  SaveAsBitmapPicture(picture,filename);
end;

