def var x-coddoc as char init 'fac,bol,n/c,tck'.
def var i as int.

do i = 1 to 4:
    for each gn-divi no-lock where codcia = 001:
        for each ccbcdocu where codcia = 001
            and coddiv = gn-divi.coddiv
            and coddoc = entry(i,x-coddoc)
            and fchdoc >= 12/15/2009:
            puntos = 0.
            if flgest = 'A' then next.
            IF ccbcdocu.coddoc = 'N/C' AND ccbcdocu.cndcre = 'N' THEN NEXT. /* NO Otros */
            IF ccbcdocu.coddiv = '00000' THEN NEXT. /* NO ATE */
            run vtamay/puntosbonus(
                ccbcdocu.CodCia,
                ccbcdocu.CodDoc,
                ccbcdocu.CodDiv,
                ccbcdocu.NroDoc).
        end.            
    end.
end.
