
FindPolygons := function ( max_n )

  local  search, intersects, twosquares, directions, polygons;

  intersects := function ( p1, p2, p3, p4, strict )

    local  d1, d2, c1, c2, x, y, I1, I2, min;

    if strict then min := 0; else min := 1; fi;
    if   (p1[1] = p2[1] and not p3[2] = p4[2])
      or (p3[1] = p4[1] and not p1[2] = p2[2])
    then
      return intersects(Reversed(p1),Reversed(p2),
                        Reversed(p3),Reversed(p4),strict);
    fi;
    if Length(Set([p1[1],p2[1],p3[1],p4[1]])) = 1 then
      I1 := [Minimum(p1[2],p2[2])..Maximum(p1[2],p2[2])];
      I2 := [Minimum(p3[2],p4[2])..Maximum(p3[2],p4[2])];
      return Length(Intersection(I1,I2)) > min;
    fi;
    if p1[1] = p2[1] and p3[2] = p4[2] then
      if    p3[2] in [Minimum(p1[2],p2[2])+min..Maximum(p1[2],p2[2])-min]
        and p1[1] in [Minimum(p3[1],p4[1])+min..Maximum(p3[1],p4[1])-min]
      then return true;
      else return false; fi;
    fi;
    if p3[1] = p4[1] and p1[2] = p2[2] then
      if    p1[2] in [Minimum(p3[2],p4[2])+min..Maximum(p3[2],p4[2])-min]
        and p3[1] in [Minimum(p1[1],p2[1])+min..Maximum(p1[1],p2[1])-min]
      then return true;
      else return false; fi;
    fi;
    d1 := (p2[2]-p1[2])/(p2[1]-p1[1]);
    d2 := (p4[2]-p3[2])/(p4[1]-p3[1]);
    p2[1] := Int(p2[1]); p4[1] := Int(p4[1]);
    c1 := p1[2]-p1[1]*d1;
    c2 := p3[2]-p3[1]*d2;
    if d1 = d2 then
      if c1 = c2 then
        I1 := [Minimum(p1[1],p2[1])..Maximum(p1[1],p2[1])];
        I2 := [Minimum(p3[1],p4[1])..Maximum(p3[1],p4[1])];
        return Length(Intersection(I1,I2)) > min;
      else
        return false;
      fi;
    fi;
    x  := (c2-c1)/(d1-d2);
    return x >= Minimum(p1[1],p2[1])+min and x <= Maximum(p1[1],p2[1])-min
       and x >= Minimum(p3[1],p4[1])+min and x <= Maximum(p3[1],p4[1])-min;
  end;

  twosquares := function ( n )

    local  sqrs, signs;

    signs := Tuples([-1,1],2);
    sqrs  := Filtered(Tuples([0..n],2),c->c[1]^2+c[2]^2=n^2);
    return Set(Concatenation(List(sqrs,s->List(signs,sgn->[s[1]*sgn[1],
                                                           s[2]*sgn[2]]))));
  end;

  search := function ( x, y, n, polygon )

    local  next, p, same_x, same_y;

    if x = 0 and y = 0 then
      Print("Found polygon ",polygon,"\n");
      Add(polygons,polygon);
      return;
    fi;
    if n > max_n then return; fi;
    next := Set(directions[n],d->[x,y]+d);
    next := Filtered(next,p->Minimum(p) >= 0);
    next := Filtered(next,
                     p->not ForAny([2..Length(polygon)-2],
                            i->intersects(polygon[i],polygon[i+1],
                                          [x,y],p,true)));
    next := Filtered(next,
                     p->not ForAny([1,Length(polygon)-1],
                            i->intersects(polygon[i],polygon[i+1],
                                          [x,y],p,false)));
    if   polygon[Length(polygon)-1][1] = x
    then same_x := [x]; else same_x := []; fi;
    if   polygon[Length(polygon)-1][2] = y
    then same_y := [y]; else same_y := []; fi;
    next := Filtered(next,
                     p->not p[1] in same_x and not p[2] in same_y);
    for p in next do
      search(p[1],p[2],n+1,Concatenation(polygon,[p]));
    od;
  end;

  directions := List([1..max_n],twosquares);
  polygons := [];
  search(1,0,2,[[0,0],[1,0]]);
  return polygons;
end;


