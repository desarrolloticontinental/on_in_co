def var x-items as int.

output to c:\tmp\set06.txt.
for each almmmatg no-lock where codcia = 001
    and fching >= 9/01/2006
    and fching <= 9/30/2006 break by codfam :
    if first-of(codfam) then x-items = 0.
    x-items = x-items + 1.
    if last-of(codfam) 
    then display codfam x-items.
end.    
output close.
