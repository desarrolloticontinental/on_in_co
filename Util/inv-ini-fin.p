def var x-stkalm as dec no-undo.
def var x-ctouni as dec no-undo.

output to c:\tmp\inv-fin-12.txt.
for each almmmate where codcia = 001 and codalm = '12' no-lock,
        first almmmatg of almmmate no-lock:
    x-ctouni = 0.
    x-stkalm = 0.
    find last almstkal where almstkal.codcia = 001
        and almstkal.codalm = almmmate.codalm
        and almstkal.codmat = almmmate.codmat
        and almstkal.fecha <= 12/31/2003
        no-lock no-error.
    if available almstkal then x-stkalm = AlmStkal.StkAct.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmate.codmat
        and almstkge.fecha <= 12/31/2003
        no-lock no-error.
    if available almstkge then x-ctouni = AlmStkge.ctouni.
    if x-stkalm <> 0 then
    display almmmate.codmat almmmatg.desmat desmar x-stkalm undbas x-ctouni
        with stream-io no-box width 320.
end.
output close.
