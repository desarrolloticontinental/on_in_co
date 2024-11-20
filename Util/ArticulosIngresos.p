
DEFINE VARIABLE cantidad LIKE almdmov.Candes.
DEFINE VARIABLE costo LIKE almdmov.VctoMn1.

output to d:\sie\compras07_10-12.txt.
for each Almmmatg where
    Almmmatg.codcia = 1 and
    Almmmatg.catconta[1] = "MP" no-lock,
    each almdmov where
    almdmov.codcia = Almmmatg.codcia and
    almdmov.codmat = Almmmatg.codmat and
    almdmov.fchdoc >= 10/01/07 and
    almdmov.fchdoc <= 12/31/07 and
    almdmov.hradoc >= "" and
    almdmov.TipMov = "I" and
    (almdmov.codmov = 02 or
    almdmov.codmov = 06) no-lock:
    cantidad = almdmov.Candes / almdmov.Factor.
    costo = cantidad * almdmov.VctoMn1.
    display
        almdmov.codmat
        Almmmatg.desmat
        Almmmatg.undbas
        cantidad
        almdmov.VctoMn1
        costo   COLUMN-LABEL "Costo total"
        with stream-io width 132.
end.
output close.
