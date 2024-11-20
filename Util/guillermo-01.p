def temp-table detalle like ccbcdocu.

for each ccbcdocu no-lock where codcia = 001
    and (coddoc = 'fac' or coddoc = 'bol')
    and flgest = 'c'
    and fchdoc >= 12/15/07
    and fchdoc <= 04/30/08
    and (codven = '015' or codven = '901' or codven = '173' or codven = '902'):
    create detalle.
    buffer-copy ccbcdocu to detalle.
end.

for each ccbcdocu no-lock where codcia = 001
    and coddoc = 'n/c'
    and flgest <> 'a'
    and fchdoc >= 12/15/07
    and fchdoc <= 04/30/08
    and (codven = '015' or codven = '901' or codven = '173' or codven = '902')
    and cndcre <> 'n':
    create detalle.
    buffer-copy ccbcdocu to detalle.
end.

output to c:\tmp\guillermo.txt.
for each detalle no-lock:
    display
        detalle.coddiv
        detalle.codven
        detalle.fchdoc
        detalle.coddoc
        detalle.nrodoc
        detalle.codcli
        detalle.nomcli
        detalle.codmon
        detalle.imptot
        with stream-io no-box width 320.
end.
output close.
