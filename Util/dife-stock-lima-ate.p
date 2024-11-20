def var x-fecha as date.
def var x-stkact-1 as dec.
def var x-stkact-2 as dec.

x-fecha = date(06,30,2006).

output to c:\tmp\diferencias.txt.
for each ate.almmmatg where codcia = 001 no-lock:
    for each ate.almacen no-lock where lookup(trim(ate.almacen.codalm), '03,04,05') > 0:
        /* corte al 30/06/2006 */
        x-stkact-1 = 0.
        x-stkact-2 = 0.
        find last ate.almstkal where ate.almstkal.codcia = 001
            and ate.almstkal.codalm = ate.almacen.codalm
            and ate.almstkal.codmat = ate.almmmatg.codmat
            and ate.almstkal.fecha <= x-fecha
            no-lock no-error.
        if available ate.almstkal then x-stkact-1 = ate.almstkal.stkact.

        find last lima.almsub where lima.almsub.codcia = 001
            and lima.almsub.codalm = ate.almacen.codalm
            and lima.almsub.codmat = ate.almmmatg.codmat
            and lima.almsub.fchdoc <= x-fecha
            no-lock no-error.
        if available lima.almsub then x-stkact-2 = lima.almsub.stksub.
        if x-stkact-1 <> x-stkact-2
        then display ate.almacen.codalm ate.almmmatg.codmat x-stkact-1 x-stkact-2.
    end.
end.
output close.
