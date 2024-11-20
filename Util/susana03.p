def var x-numitm as dec no-undo.
def var x-stkact as dec no-undo.
def var x-valtot as dec no-undo.
def var x-valact as dec no-undo.
def var x-fecha  as date no-undo.

x-fecha = date(07,31,2005).

OUTPUT TO C:\TMP\SUSANA03.TXT.
for each almmmatg NO-LOCK where codcia = 1 and tpoart <> 'D' 
    AND NOT (CODFAM = '008' AND SUBFAM > '200')
    break by codfam:
    if first-of(codfam)
    then assign 
            x-numitm = 0
            x-valtot = 0.
    x-stkact = 0.
    x-valact = 0.
    for each almmmate where codcia = 1 and codmat = almmmatg.codmat
        and lookup(trim(codalm), '11,22,16,03,03a,04,04a,05,05a,131') > 0
        no-lock:
        find last AlmStkAl of almmmate where fecha <= x-Fecha
            no-lock no-error.
        if available AlmStkAl then do:
            x-stkact = x-stkact + AlmStkAl.stkact.
            if almmmatg.monvta = 2
            then x-valact = x-valact + AlmStkAl.stkact * almmmatg.ctolis.
            else x-valact = x-valact + AlmStkAl.stkact * almmmatg.ctolis / almmmatg.tpocmb.
        end.
    end.
    if x-stkact > 0 
    then assign
            x-numitm = x-numitm + 1
            x-valtot = x-valtot + x-valact.
    if last-of(codfam) 
    then display codfam x-numitm x-valtot
        with no-labels stream-io width 100.
end.
OUTPUT CLOSE.
