############################################################################
##
#W  compile.g                  GAP Exercises                     Stefan Kohl
##
##  This file contains a function to compile the GAP exercises.
##

#############################################################################
##
#F  CompileGAPExercises( <path> ) . . . . . . . . . compile the GAP exercises
##
##  This function builds the GAP exercise book in the file formats LaTeX,
##  PDF, HTML and ASCII-text. The argument <path> is the path name where the
##  sources are stored.
##
##  The compilation is done using the GAPDoc package by Frank Lübeck and
##  Max Neunhöffer.
##
BindGlobal( "CompileGAPExercises", 

  function ( path )

    path := USER_HOME_EXPAND( path );
    MakeGAPDocDoc( path, "gap-exercises.xml", [ ],
                   "GAP-Exercises", "../../../" );
  end );

#############################################################################
##
#E  compile.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here