#############################################################################
##
#W  elliptic.gi                                                   Stefan Kohl
##
##  This file contains implementations of methods and functions for 
##  computing in the elliptic curve point groups E( a , b )/p
##  (only in affine Weierstrass form)  
##

InstallGlobalFunction( EllipticCurvePoint,

  function ( Fam, P )
   
    local  X, Y;

    X := P[ 1 ]; Y := P[ 2 ];
    if X <> infinity
      and (Y^2) mod Fam!.p <> (X^3 + Fam!.a*X + Fam!.b) mod Fam!.p
    then Error( "The given point must be on the specified curve" ); fi;

    if   X  = infinity 
    then Y := infinity;
    else X := X mod Fam!.p; Y := Y mod Fam!.p;
    fi;

    return Objectify( NewType( Fam,     IsPointOnEllipticCurve
                                    and IsAffineWeierstrassRep ),
                      rec( x := X, y := Y ) );
  end );

InstallGlobalFunction( EllipticCurveGroup,

  function ( a, b, p )
     
    local F, G, X, Y, FamName, ready, Point;
  
    if   not (     IsInt(a) and IsInt(b) 
               and IsPosInt(p) and IsPrimeInt(p) and p >= 5 )
    then Error( "E(a,b)/p : <a> and <b> have to be integers, ",
                " and p has to be a prime >= 5" ); fi;
    if   (4*a^3 + 27*b^2) mod p = 0 
    then Error( "<a> and <b> must satisfy 4*a^3 + 27*b^2 <> 0 (mod <p>)" );
    fi;

    FamName := Concatenation ( "E(", String(a), ",", String(b), ")/",
                               String( p ) );

    F := NewFamily( FamName, IsPointOnEllipticCurve );
    SetName( F, FamName );

    F!.a := a;
    F!.b := b;
    F!.p := p;

    X := 0; ready := false;
    repeat
      if Legendre( X^3 + a*X + b, p ) = 1
      then Y := RootMod( X^3 + a*X + b, p );
           Point := EllipticCurvePoint( F, [ X, Y ] );
           if not IsBound( G ) 
           then G := GroupByGenerators( [ Point ] );
           else G := ClosureGroup( G, Point );
           fi;
           if   p > 31 and Size( G ) > p - 2 * RootInt( p )
           then ready := true; fi;
      fi;
      X := X + 1;
    until X = p or ready;

    SetIsWholeFamily( G, true );
    
    SetName( G, Concatenation( "EllipticCurveGroup(", String( a ),
                               ",", String( b ), ",", String( p ), ")" ) );

    return G;
  end );

InstallMethod( PrintObj,
               "for element in E(a,b)/p, (AffineWeierstrassRep)",
               true, [ IsPointOnEllipticCurve and IsAffineWeierstrassRep ], 0,

  function( p )
    Print( "EllipticCurvePoint( ", FamilyObj( p ),
                           ", [ ",p!.x,", ", p!.y, " ] )" );
  end );

InstallMethod( ViewObj,
               "for element in E(a,b)/p, AffineWeierstrassRep",
               true, [ IsPointOnEllipticCurve and IsAffineWeierstrassRep ], 0,

  function( p )
    if   p!.x <> infinity
    then Print( "( ",p!.x,", ", p!.y, " )" );
    else Print( "infinity" );
    fi;                    
  end );

InstallMethod( \=,
               "for two elements in E(a,b)/p, AffineWeierstrassRep",
               IsIdenticalObj,
               [ IsPointOnEllipticCurve and IsAffineWeierstrassRep,
                 IsPointOnEllipticCurve and IsAffineWeierstrassRep ],
               0,

  function( x, y )
    return x!.x = y!.x and x!.y = y!.y;
  end );

InstallMethod( \<,
               "for two elements in E(a,b)/p, AffineWeierstrassRep",
               IsIdenticalObj,
               [ IsPointOnEllipticCurve and IsAffineWeierstrassRep,
                 IsPointOnEllipticCurve and IsAffineWeierstrassRep ],
               0,

  function( x, y )
    return [x!.x, x!.y] < [y!.x, y!.y];
  end );

InstallMethod( \*,
               "for two elements in E(a,b)/p, AffineWeierstrassRep",
               IsIdenticalObj,
               [ IsPointOnEllipticCurve and IsAffineWeierstrassRep,
                 IsPointOnEllipticCurve and IsAffineWeierstrassRep ],
               0,

  function( p1, p2 )
    
    local lambda, p3, p, h;

    p := FamilyObj( p1 )!.p;
    if   (p1!.x <> infinity) and (p2!.x <> infinity) 
    then
      if   p1!.x = p2!.x and p1!.y = (- p2!.y) mod FamilyObj( p2 )!.p
      then p3 := rec( x := infinity, y := infinity );
      else
        if   p1!.x <> p2!.x 
        then h := QuotientMod( 1, p1!.x - p2!.x, FamilyObj( p1 )!.p );
             if h = fail then return Gcd( p1!.x - p2!.x, p ); fi;
             lambda := (p1!.y - p2!.y) * h;
        else h := QuotientMod( 1, 2 * p1!.y, FamilyObj( p1 )!.p );
             if h = fail then return Gcd( 2 * p1!.y, p ); fi;
             lambda := (3 * p1!.x^2 + FamilyObj( p1 )!.a) * h;
        fi;
        p3 := rec();
        p3.x := lambda^2 - p1!.x - p2!.x;
        p3.y := - (lambda * (p3.x - p1!.x) + p1!.y);
      fi;
    else
      if p1!.x = infinity then p3 := rec( x := p2!.x, y := p2!.y );
                          else p3 := rec( x := p1!.x, y := p1!.y ); fi;
    fi;
    return EllipticCurvePoint( FamilyObj( p1 ), [ p3.x, p3.y ] );
  end );

InstallMethod( OneOp,
               "for an element in E(a,b)/p, AffineWeierstrassRep",
               true,
               [ IsPointOnEllipticCurve ], 0,
               x -> EllipticCurvePoint( FamilyObj( x ),
                                        [ infinity, infinity ] )
             ); 

InstallMethod( InverseOp,
               "for an element in E(a,b)/p, AffineWeierstrassRep",
               true,
               [ IsPointOnEllipticCurve and IsAffineWeierstrassRep ], 0,
               
  function ( p )

    if   p!.x = infinity 
    then return EllipticCurvePoint( FamilyObj( p ), [ infinity, infinity ] );
    else return EllipticCurvePoint( FamilyObj( p ),
                                    [ p!.x, (- p!.y) mod FamilyObj( p )!.p ] );
    fi;
  end );

InstallMethod( Random,
               "for group E(a,b)/p",
               true,
               [ CategoryCollections( IsPointOnEllipticCurve )
                 and IsWholeFamily ], 0,

  function ( G )

    local X, Y, a, b, p;

    a := ElementsFamily( FamilyObj( G ) )!.a;
    b := ElementsFamily( FamilyObj( G ) )!.b;
    p := ElementsFamily( FamilyObj( G ) )!.p;

    repeat
      X := Random( [0 .. p - 1] );
    until Legendre( X^3 + a*X + b, p ) = 1;
    Y := RootMod( X^3 + a*X + b, p );
    return EllipticCurvePoint( ElementsFamily( FamilyObj( G ) ), [ X, Y ] );
  end );

#############################################################################
##
#E  elliptic.gi  . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
##
