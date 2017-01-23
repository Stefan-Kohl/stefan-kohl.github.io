
# This function computes all Carmichael numbers
# with 3 prime factors, where the smallest of them equals p1

CarmichaelsWith3Factors := function (p1)

  local p2,p3,a,b,minb,maxb,
        p2Numerator,p3Numerator,Denominator,p2Prime,p3Prime,
        Prime,Divisor,MaxDivisor,Pos,MaxPrime,
        Result;
  
  Result := []; MaxPrime := 10000;
  if (IsPrime(p1)) and (p1 <> 2) then
    Prime := [];
    for Pos in [1..MaxPrime] do Prime[Pos] := true; od; Prime[1] := false;
    Divisor := 2; MaxDivisor := RootInt(MaxPrime);
    while Divisor <= MaxDivisor do
      Pos := 2 * Divisor;
      while Pos <= MaxPrime do 
        Prime[Pos] := false;
        Pos := Pos + Divisor;  
      od;
      if Divisor = 2 then Divisor := 3; else Divisor := Divisor + 2; fi;
    od;
    for a in [2..p1 - 1] do
      minb := Int(p1^2/a);
      maxb := Int(p1^3/(a*(p1 - 2)));
      if minb < a then minb := a; fi;
      for b in [minb..maxb] do
        p2Numerator := a*b + (p1 - 1)*a - p1;
        p3Numerator := a*b + (p1 - 1)*b - p1;
        Denominator := a*b - p1^2;
        if p2Numerator mod Denominator = 0 then
          if p3Numerator mod Denominator = 0 then
            p2 := p2Numerator/Denominator;
            p3 := p3Numerator/Denominator;
            if (p2 >= 3) and (p3 >= 3) and (p2 > p1) and (p3 > p1) then
              if p2 <= MaxPrime then p2Prime := Prime[p2]; 
                                else p2Prime := IsPrime(p2); fi;         
              if p2Prime then
                if p3 <= MaxPrime then p3Prime := Prime[p3];
                                  else p3Prime := IsPrime(p3); fi;  
                if p3Prime then
                  if    ((p1*p2 - 1) mod (p3 - 1) = 0)
                    and ((p1*p3 - 1) mod (p2 - 1) = 0)
                    and ((p2*p3 - 1) mod (p1 - 1) = 0)
                  then
                    AddSet(Result,[[p1,p2,p3],p1*p2*p3]);  
                  fi;    
                fi;    
              fi;      
            fi;
          fi;      
        fi;
      od;
    od;
  fi;
  return Result;  
end;


# This function generates a complete table for all p1 <= Maxp1
# (the Carmichael numbers divisible by p1 having a factor smaller
# than p1 (and so appear in the table corresponding to this factor)
# are also listed (before the new ones, separated by an horizontal bar))

CarmichaelsWith3FactorsTable := function (Maxp1)

  local  Carmichaels,News,Olds,CarmNumbers,Total,MaxTotal,SameTotal,
         CarmNum,InLine,Possiblep1,p1,Carm,CenterLine;

  CenterLine := function (arg)

    local Line,Width; 

    Line  := Concatenation(List(arg,String));
    Width := SizeScreen()[1];
    Print(String("",Int((Width - Length(Line))/2)),Line,"\n");
  end;

  Carmichaels := []; CarmNumbers := []; MaxTotal := 0;
  Possiblep1 := Filtered([1..Maxp1],p1 -> IsPrime(p1) and p1 <> 2);
  Print("\n");
  CenterLine("Carmichael numbers with 3 factors from which one is given");
  Print("\n\n\n");
  for p1 in Possiblep1 do
    CenterLine("Given factor : ",String(p1,5)); Print("\n\n");
    CenterLine("+--------+--------------+--------------------+-----------------------------+");
    CenterLine("|   p1   |      p2      |         p3         |              n              |");
    CenterLine("+--------+--------------+--------------------+-----------------------------+");
    Olds := Filtered(Carmichaels,Carm -> Position(Carm[1],p1) <> fail);
    Sort(Olds,function(Carm1,Carm2) return Carm1[1] < Carm2[1]; end);
    for Carm in Olds do
      CenterLine("| ",String(Carm[1][1],6)," | ",String(Carm[1][2],12),
                " | ",String(Carm[1][3],18)," | ",String(Carm[2],27)," |");
    od;
    CenterLine("+--------+--------------+--------------------+-----------------------------+");
    News := CarmichaelsWith3Factors(p1);
    Sort(News,function(Carm1,Carm2) return Carm1[1] < Carm2[1]; end);
    for Carm in News do
      CenterLine("| ",String(Carm[1][1],6)," | ",String(Carm[1][2],12),
                " | ",String(Carm[1][3],18)," | ",String(Carm[2],27)," |");
    od;
    Total := Length(Olds) + Length(News);
    if Total > MaxTotal then MaxTotal := Total; fi;
    Add(CarmNumbers,[p1,Total]);
    Append(Carmichaels,News);
    CenterLine("+--------+--------------+--------------------+-----------------------------+");
    Print("\n\n\n\n");
  od;
  Print("A total of ",Length(Carmichaels)," Carmichael numbers was found.\n\n\n\n");
  for Total in [0..MaxTotal] do
    SameTotal := Filtered(CarmNumbers,CarmNum->CarmNum[2] = Total);
    if SameTotal <> [] then
      CenterLine(Total," Carmichael numbers were found for p1 = "); Print("\n");
      InLine := 0;
      for CarmNum in SameTotal do
        if InLine = 10 then Print("\n"); InLine := 0; fi;
        Print(String(CarmNum[1],6)," "); InLine := InLine + 1;
      od;
      Print("\n\n\n\n");
    fi;
  od;  
end;
