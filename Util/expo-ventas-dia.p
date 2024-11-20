
def temp-table detalle
    field fchdoc as date
    field codcli as char
    field nomcli as char
    field imptot as dec
    field coddpto like CcbCDocu.CodDpto 
    field coddist like CcbCDocu.CodDist.

for each ccbcdocu no-lock where codcia = 001
    and ccbcdocu.coddiv = '00015'
    and fchdoc >= 01/01/2007
    and (coddoc = 'fac' or coddoc = 'bol')
    and flgest <> 'a':
    find detalle where detalle.fchdoc = ccbcdocu.fchdoc
        and detalle.codcli = ccbcdocu.codcli
        and detalle.coddpto = ccbcdocu.coddpto
        and detalle.coddist = ccbcdocu.coddist
        exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        assign
            detalle.codcli = ccbcdocu.codcli
            detalle.nomcli = ccbcdocu.nomcli
            detalle.fchdoc = ccbcdocu.fchdoc
            detalle.coddpto = ccbcdocu.coddpto
            detalle.coddist = ccbcdocu.coddist.
    end.
    if ccbcdocu.codmon = 1
    then detalle.imptot = detalle.imptot + ccbcdocu.imptot.
    else detalle.imptot = detalle.imptot + ccbcdocu.imptot * ccbcdocu.tpocmb.
end.    
output to c:\tmp\expo-resum.txt.
for each detalle:
display 
    detalle.codcli format 'x(11)'
    detalle.nomcli format 'x(50)'
    detalle.fchdoc
    detalle.imptot format '>>>,>>>,>>9.99'
    detalle.coddpto
    detalle.coddist
    with stream-io no-box width 320.
end.
output close.
