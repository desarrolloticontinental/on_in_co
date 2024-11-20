def var x-fecha as date.
def temp-table t-doc like ccbcdocu
    field semana as dec extent 4
    index llave01 as primary codcli coddoc nrodoc.
def var x-semana as int.

x-fecha = date(08,21,2005).
for each ccbcdocu where codcia = 001
    and coddoc = 'fac'
    and flgest = 'P' no-lock:
    x-semana = 0.
    if fchvto >= x-fecha and fchvto <= x-fecha + 27
    then do:
        if fchvto >= x-fecha and fchvto <= x-fecha + 6
        then x-semana = 1.
        if fchvto >= x-fecha + 7 and fchvto <= x-fecha + 13
        then x-semana = 2.
        if fchvto >= x-fecha + 14 and fchvto <= x-fecha + 20
        then x-semana = 3.
        if fchvto >= x-fecha + 21 and fchvto <= x-fecha + 27
        then x-semana = 4.
    end.
    if x-semana = 0 then next.
    find t-doc where t-doc.codcli = ccbcdocu.codcli 
        and t-doc.coddoc = ccbcdocu.coddoc
        and t-doc.nrodoc = ccbcdocu.nrodoc
        exclusive-lock no-error.
    if not available t-doc then create t-doc.
    buffer-copy ccbcdocu to t-doc
        assign
            t-doc.semana[x-semana] = t-doc.semana[x-semana] +
                                (if ccbcdocu.codmon = 1 then ccbcdocu.sdoact
                                else ccbcdocu.sdoact * ccbcdocu.tpocmb).
end.

output to c:\tmp\porcobrar001.txt.
for each t-doc:
    display t-doc.codcli t-doc.nomcli t-doc.coddoc t-doc.nrodoc t-doc.coddiv t-doc.fchdoc
        t-doc.fchvto t-doc.codmon t-doc.sdoact t-doc.tpocmb
        t-doc.semana[1] t-doc.semana[2] t-doc.semana[3] t-doc.semana[4]
        with stream-io width 320.
end.    
output close.
