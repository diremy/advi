%!
% PostScript prologue for advi.
%
/advi@Dict 20 dict def 
advi@Dict begin
/floatstring 20 string def 
/printfloat { floatstring cvs print } def
/printpos { 
     (dvi) print printfloat  (,) print printfloat (\n) print 
} def
/reference matrix currentmatrix bind def
/absolute { matrix currentmatrix itransform reference transform } def
/printpos { 
     (dvi) print printfloat  (,) print printfloat  (\n) print 
} def
/printCP { 
     currentpoint printpos
} def
/printEP {
   @beginspecial @setspecial tx@Dict begin
   newpath 0 360 0 0 0 0 Ellipse closepath
   currentpoint
   (dvi) print printfloat  (,) print  printfloat (\n) print 
} def
end
% END advi.pro
