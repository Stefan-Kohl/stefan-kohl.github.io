#############################################################################
##
#W  homepage.g            Homepage Maintenance Utility            Stefan Kohl
##
# HPDir    := "/home/kohl/Homepage-XHTML/";
# HPDir    := "C:/Kohl/Homepage/";
# HPDir    := "D:/HOMEPAGE/WWW.XHTML/";
# HPDir    := "E:/Homepage/WWW/";
# HPDir    := "C:/User/Homepage/WWW/";
HPDir    := "/user/Homepage/WWW/";  
HPFileName := Filename(Directory(HPDir),"homepage.input");
Read(Concatenation(HPDir,"data.g"));

BuildHomepage := function ( lastupdate )

  local  IsEarlier, LowercaseTags,
         hpfile, str, rep, parts, l, triples, triple,
         filename, date, data, htmlfile;

  IsEarlier := function ( date1, date2 )

    local  months, d1, d2;

    months := ["Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec"];
    d1    := SplitString(date1,'-');
    d1[2] := Position(months,d1[2]);
    d1    := List(Reversed(d1),Int);
    d2    := SplitString(date2,'-');
    d2[2] := Position(months,d2[2]);
    d2    := List(Reversed(d2),Int);
    return d1 < d2;
  end;

  hpfile := InputTextFile(HPFileName);
  str    := ReadAll(hpfile);
  CloseStream(hpfile);
  for rep in REPLACEMENTS do
    str := ReplacedString(str,rep[1],rep[2]);
  od;
  parts   := SplitString(str,'§'); 
  l       := (Length(parts) - 1)/3;
  triples := List([1..l],i->parts{[3*i-1..3*i+1]});
  for triple in triples do
    filename := triple[1]; date := triple[2];
    data := ReplacedString(triple[3],"{DATE}",date);
    if IsEarlier(date,lastupdate) then continue; fi;
    htmlfile := OutputTextFile(Filename(Directory(HPDir),filename),false);
    WriteAll(htmlfile,data{[2..Length(data)]});
    CloseStream(htmlfile);
  od;
end;

#############################################################################
##
#E  homepage.g . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
