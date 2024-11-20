output to c:\tmp\gratuita.txt.
for each ccbcdocu where codcia = 001
    and lookup(trim(coddoc), 'fac,bol') > 0
    and fchdoc >= 01/01/2005 and fchdoc <= date(06,30,2005)
    and flgest <> 'a'
    and (fmapgo = '900' or index(glosa, 'gratuita') > 0)
    no-lock:
    display fchdoc coddoc nrodoc (if codmon = 1 then imptot else imptot * tpocmb)
        glosa with stream-io width 320.
end.
output close.
    
