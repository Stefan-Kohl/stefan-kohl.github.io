#############################################################################
##
#W  autorb.g                                                      Stefan Kohl
##
##  This file contains the routines used for the computations in the paper
##
##  `Classifying Finite Simple Groups with Respect to the Number of Orbits
##   Under the Action of the Automorphism Group'.
##

#############################################################################
##
#F  AdmissibleTriples( <MaxOmega> )
##
##  The `admissible' triples (l,p,f), as described in the paper.
##
AdmissibleTriples := function ( MaxOmega )

  local  Try, sol;

  Try := function ( l, p, f )

    if     p^(l*f)/(6*l*(l+1)*f) <= MaxOmega
       and not [l,p,f] in sol
    then
      Add(sol,[l,p,f]);
      Try(l,p,f+1);
      Try(l,NextPrimeInt(p),f);
      Try(l+1,p,f);
    fi;
  end;
  
  sol := [];
  Try(1,2,1);
  return Set(sol);
end;

# A little auxiliary function.

Ceil := function ( n )
  if   IsInt( n ) then return n;
  else return Int( n + 1 ); fi;
end;

#############################################################################
##
#F  NrTableAutomorphismOrbits( <grp> )
##
##  The number of orbits of the character table automorphism group on the
##  set of conjugacy classes of the group <grp>, where <grp> is the GAP name
##  of the group to be investigated.
##
NrTableAutomorphismOrbits := function ( grp )

  local  tbl, G, orb;

  tbl := CharacterTable( grp );
  G   := TableAutomorphisms( tbl, Irr( tbl ) );
  orb := Orbits( G, [ 1 .. NrConjugacyClasses(tbl) ] );
  return Length( orb );
end;

#############################################################################
##
#F  BoundByCharPols( <G>, <d>, <GraphAut>, <Patience> )
##
##  The argument <d> is the size of the centre (or equivalently, the order of
##  the group of diagonal automorphisms), the argument <GraphAut> is the
##  order of the group of graph automorphisms and <Patience> is a value which
##  specifies how long we want to continue our pseudo-random search if no
##  more elements with `new' characteristic polynomials are found.
##  Usually, <Patience> = 200 should be sufficient to get the results given
##  in the paper.
##
BoundByCharPols := function ( G, d, GraphAut, Patience )

  local  ords, ord, ordsfused, pols, pol, c, imgs,
         g, h, n, m, l, F, f, ff, x, LastHit,
         bound, contrib, i, j, k;

  ords := [ ]; pols := [ ]; g := One( G ); i := 1;
  F := FieldOfMatrixGroup( G );
  f := FrobeniusAutomorphism( F ); ff := Order( f );
  x := Indeterminate( F );
  l := DimensionOfMatrixGroup( G );
  repeat
    n := Order( g );
    for m in DivisorsInt( n ) do
      h := g^m; ord := n / m;
      if not ord in ords then
        AddSet( ords, ord ); pols[ ord ] := [];
        for j in [ 1 .. Length( pols ) ] do
          if not IsBound( pols[ j ] )
          then pols[ j ] := [ ]; fi;
        od;
      fi;
      pol := CharacteristicPolynomial( h );
      c := CoefficientsOfUnivariatePolynomial( pol );
      imgs := List( [0 .. ff - 1 ],
                    j -> Sum( [ 0 .. l ],
                              k -> c[k+1]^(f^j)*x^k ) );
      if Intersection( pols[ord], imgs ) = [] then
        Add(pols[ord],pol); LastHit := i;
      fi;
    od;
    i := i + 1; g := PseudoRandom( G );
  until i - LastHit > Patience;
  bound := 0;
  for ord in [ 1 .. Length( pols ) ] do
    if     pols[ ord ] <> [ ]
       and (     Gcd( ord, d ) = 1 
             or ( ord mod d = 0 and Gcd(ord/d,d) <> 1 )
             or ord = d )
    then
      if Gcd(ord,d) = 1 and ord <> 1 then
        ordsfused := Filtered( ords,
                               n -> n/Gcd(n,d) = ord );
      else ordsfused := [ord]; fi;
      contrib := Ceil( Sum( ordsfused,
                            n -> Length( pols[ n ] ) ) /
                       ( d^2 * GraphAut ) );
      bound := bound + contrib;
    fi;
  od;
  return bound;
end;

#############################################################################
##
#F  Omega( <G> )
##
##  Computation of the orbit number by brute force.
##
omega := function ( G )

local  ccl, rep, A, autgen, outer, pos, Out, orb, i;

  ccl    := ConjugacyClasses( G );
  rep    := List( ccl, Representative );
  A      := AutomorphismGroup( G );
  autgen := GeneratorsOfGroup( A );
  outer  := Filtered( autgen, a -> not IsInnerAutomorphism( a ) );
  Out := Group(()); pos := [ ];
  for i in [ 1 .. Length( outer ) ] do
    pos[i] := List([ 1 .. Length( ccl ) ],
                   j -> Position(List(rep,g->IsConjugate(G,g,Image(outer[i],
                                                                   rep[j]))),
                                 true));
    Out := ClosureGroup( Out, SortingPerm( pos[ i ] ) );
  od;
  return Length( Orbits( Out, [ 1 .. Length( ccl ) ] ) );
end;

#############################################################################
##
#E  autorb.g . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
