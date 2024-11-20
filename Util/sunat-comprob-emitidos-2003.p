def var x-cuenta as int.
def var x-imptot as dec format '->>>,>>>,>>>.99'.

for each gn-divi where codcia = 001 no-lock:
    x-cuenta = 0.
    x-imptot = 0.
    for each ccbcdocu where codcia = 001
        and coddiv = gn-divi.coddiv
        and lookup(coddoc, 'fac,bol') > 0
        and fchdoc >= 01/01/2003
        and fchdoc <= 03/30/2003
        and flgest <> 'a' no-lock:
        x-cuenta = x-cuenta + 1.
        if ccbcdocu.codmon = 1
        then x-imptot = x-imptot + imptot.
        else x-imptot = x-imptot + (imptot * tpocmb).
    end.
    display gn-divi.coddiv x-cuenta x-imptot.
end.
