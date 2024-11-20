def var x-coddoc as char init 'fac,bol,n/c'.
def var i as int.

do i = 1 to 3:
    for each gn-divi no-lock where codcia = 001 and coddiv <> '00000':
        for each ccbcdocu where codcia = 001
            and coddiv = gn-divi.coddiv
            and coddoc = entry(i,x-coddoc)
            and fchdoc >= 01/01/2009:
            puntos = 0.
            if flgest = 'A' then next.
            run vta/puntosbonus(
                ccbcdocu.CodCia,
                ccbcdocu.CodDoc,
                ccbcdocu.CodDiv,
                ccbcdocu.NroDoc).
        end.            
    end.
end.
