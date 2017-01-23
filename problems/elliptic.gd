#############################################################################
##
#W  elliptic.gd                                                   Stefan Kohl
##
##  This file contains declarations of functions etc. for computing with 
##  elliptic curves
##

DeclareCategory( "IsPointOnEllipticCurve", IsScalar);

DeclareCategoryCollections( "IsPointOnEllipticCurve" );

DeclareRepresentation( "IsAffineWeierstrassRep", IsComponentObjectRep,
                      ["x","y"] );

DeclareGlobalFunction( "EllipticCurvePoint" );

DeclareGlobalFunction( "EllipticCurveGroup" );

#############################################################################
##
#E  elliptic.gd  . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
##


