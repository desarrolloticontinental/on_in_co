def var x-cuenta as int.
def var x-imptot as dec format '->>>,>>>,>>9.99'.

output to c:\tmp\loquequieras.txt.
for each almacen where codcia = 001 no-lock:
    x-cuenta = 0.
    x-imptot = 0.
    for each almcmov where codcia = 001
        and codalm = almacen.codalm
        and tipmov = 's'
        and codmov = 03
        and flgest <> 'a'
        and fchdoc >= 01/01/2003
        and fchdoc <= 03/30/2003
        no-lock:
        x-cuenta = x-cuenta + 1.
        for each almdmov of almcmov no-lock:
            x-imptot = x-imptot + vctomn1 * candes * factor.
        end.
    end.
    display almacen.codalm almacen.coddiv x-cuenta x-imptot.
end.
output close.
