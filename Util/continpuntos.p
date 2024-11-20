disable triggers for load of ate.ccbcdocu.
disable triggers for load of ate.ccbddocu.

def var x-coddoc as char.
def var i as int.

x-coddoc = 'FAC,BOL,N/C'.

for each ate.gn-divi no-lock where codcia = 001
        and lookup(trim(coddiv), '00001,00002,00003,00014,00008') > 0:
    display ate.gn-divi.coddiv.
    pause 0.
    do i = 1 to 3:
        display entry(i, x-coddoc).
        pause 0.
        for each lima.ccbcdocu no-lock where codcia = 001
                and coddiv = ate.gn-divi.coddiv
                and coddoc = entry(i, x-coddoc)
                and fchdoc >= 01/01/2007:
            find first ate.ccbcdocu of lima.ccbcdocu exclusive-lock no-error.
            if not available ate.ccbcdocu then do:
                create ate.ccbcdocu.
                buffer-copy lima.ccbcdocu to ate.ccbcdocu.
                for each lima.ccbddocu of lima.ccbcdocu no-lock:
                    create ate.ccbddocu.
                    buffer-copy lima.ccbddocu to ate.ccbddocu.
                end.
            end.
            else do:
                assign
                    ate.ccbcdocu.fchdoc = lima.ccbcdocu.fchdoc
                    ate.ccbcdocu.puntos = lima.ccbcdocu.puntos
                    ate.ccbcdocu.nrocard = lima.ccbcdocu.nrocard.
            end.
            release ate.ccbcdocu.
        end.            
    end.
end.        
