def var x-stkact as dec.
output to c:\tmp\dife080506.txt.
for each invrecont where codcia = 001 and codalm = '11'
    and fchinv = 05/08/2006:
    x-stkact = 0.
    find last almstkal where almstkal.codcia = 001
        and almstkal.codalm = '11'
        and almstkal.codmat = invrecont.codmat
        and almstkal.fecha <= invrecont.fchinv
        no-lock no-error.
    if available almstkal then x-stkact = AlmStkal.StkAct.
    if x-stkact <> invrecont.caninv then
    display invrecont.codmat invrecont.caninv x-stkact.
end.
output close.
