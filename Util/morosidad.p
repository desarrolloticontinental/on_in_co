def var x-imptot as dec.

output to c:\tmp\morosidad.txt.
for each ccbccaja where codcia = 001 
    and coddiv = '00000'
    and coddoc = 'i/c'
    and fchdoc >= date(01,15,2005)
    and fchdoc <= date(07,15,2005) no-lock,
    each ccbdcaja where ccbdcaja.codcia = ccbccaja.codcia
        and ccbdcaja.coddoc = ccbccaja.coddoc
        and ccbdcaja.nrodoc = ccbccaja.nrodoc no-lock,
    first ccbcdocu where ccbcdocu.codcia = 001
        and ccbcdocu.coddoc = ccbdcaja.codref
        and ccbcdocu.nrodoc = ccbdcaja.nroref
        and lookup(trim(ccbcdocu.fmapgo), '000,001,002') = 0
        and ccbcdocu.flgest = 'C' no-lock,
    first gn-convt where gn-convt.codig = ccbcdocu.fmapgo no-lock:
    
    find first ccbddocu of ccbcdocu where ccbddocu.codmat = '005206'
        no-lock no-error.
    if not available ccbddocu 
    then do:
        find first ccbddocu of ccbcdocu where ccbddocu.codmat = '005207'
            no-lock no-error.
        if not available ccbddocu then next.        
    end.        
    x-imptot = 0.
    for each ccbddocu of ccbcdocu no-lock where 
            (ccbddocu.codmat = '005206' or ccbddocu.codmat = '005207'):
        x-imptot = x-imptot + ccbddocu.implin.
    end.            

    display ccbcdocu.codcli ccbcdocu.nomcli ccbcdocu.codven 
        ccbcdocu.fmapgo column-label 'Fma. Pago'
        gn-convt.nombr  column-label 'Descripcion'
        gn-convt.totdias column-label 'Dias'
        ccbcdocu.coddoc
        ccbcdocu.nrodoc
        ccbcdocu.codven
        (if ccbcdocu.codmon = 1 then x-imptot else x-imptot * ccbcdocu.tpocmb) column-label 'Importe Productos'
        (if ccbcdocu.codmon = 1 then ccbcdocu.imptot else ccbcdocu.imptot * ccbcdocu.tpocmb) column-label 'Importe Total'
        (if ccbdcaja.codmon = 1 then ccbdcaja.imptot else ccbdcaja.imptot * ccbcdocu.tpocmb) column-label 'Importe Pagado'
        ccbcdocu.fchdoc column-label 'Emision'
        ccbcdocu.fchvto column-label 'Vencimiento'
        ccbccaja.fchdoc column-label 'Cancelacion'
        ccbccaja.nrodoc column-label 'I/Caja'
        with stream-io width 320 title 'cancelaciones desde el 15/01/2005 al 15/07/2005 en nuevos soles'.
end.
output close.
        
