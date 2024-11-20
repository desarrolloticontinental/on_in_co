def temp-table detalle like almmmatg
    field candes as dec.

for each ccbcdocu no-lock where codcia = 001
    and lookup(coddoc, 'fac,bol') > 0
    and flgest <> 'a'
    and lookup(codalm, '11,22') > 0
    and fchdoc >= date(11,01,2004) and fchdoc <= date(04,30,2005),
    first gn-clie where gn-clie.codcia = 000
        and gn-clie.codcli = ccbcdocu.codcli
        and gn-clie.canal = '0008',
    each ccbddocu of ccbcdocu no-lock,
    first almmmatg of ccbddocu no-lock:
    find detalle of almmmatg exclusive-lock no-error.
    if not available detalle then create detalle.
    buffer-copy almmmatg to detalle
        assign
            detalle.candes = detalle.candes + ccbddocu.candes * ccbddocu.factor.
end.

output to c:\tmp\venta22.txt.
for each detalle:
    display detalle.codmat detalle.desmat detalle.desmar detalle.candes detalle.undbas
        with stream-io no-labels width 320.
end.    
output close.
