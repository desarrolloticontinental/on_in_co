def stream errores.
output stream errores to c:\tmp\errores.txt.
disable triggers for load of ate.ccbcdocu.

for each ate.gn-divi no-lock where codcia = 001
    and lookup(trim(coddiv), '00003,00008,00014') > 0:
FOR EACH lima.ccbcdocu NO-LOCK WHERE lima.ccbcdocu.codcia = 001
        AND lima.ccbcdocu.coddiv = ate.gn-divi.coddiv
        AND LOOKUP(TRIM(lima.ccbcdocu.coddoc), 'FAC,BOL,N/C') > 0
        AND lima.ccbcdocu.fchdoc >= 01/01/2007
        AND lima.ccbcdocu.fchdoc <= 04/29/2007:
    display lima.ccbcdocu.coddiv lima.ccbcdocu.coddoc lima.ccbcdocu.nrodoc
        lima.ccbcdocu.fchdoc with stream-io no-box no-labels.
    pause 0.
    find ate.ccbcdocu of lima.ccbcdocu exclusive-lock no-error.
    if not available ate.ccbcdocu then do:
        display stream errores
            lima.ccbcdocu.codcia lima.ccbcdocu.coddoc lima.ccbcdocu.nrodoc.
        next.
    end.            
    ate.ccbcdocu.nrocard = lima.ccbcdocu.nrocard.
    ate.ccbcdocu.puntos = lima.ccbcdocu.puntos.
    release ate.ccbcdocu.
END.
end.
output stream errores close.
