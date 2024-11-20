output TO c:\tmp\notas-de-credito.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'n/c'
    AND coddiv = '00000'
    AND fchdoc >= 05/01/09
    AND fchdoc <= 05/31/09
    AND flgest <> 'a'
    AND codcli = '20109072177',
    EACH almcmov NO-LOCK WHERE almcmov.codcia = 1
    AND almcmov.codalm = ccbcdocu.codalm
    AND almcmov.tipmov = 'i'
    AND almcmov.codmov = 09
    AND almcmov.nrodoc = INTEGER(ccbcdocu.nroped),
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST almmmatg OF ccbddocu NO-LOCK:
    DISPLAY 
        ccbcdocu.coddoc     COLUMN-LABEL 'Cod'
        ccbcdocu.nrodoc     COLUMN-LABEL 'Numero'
        ccbcdocu.fchdoc     COLUMN-LABEL 'Fecha'
        almcmov.nrodoc      COLUMN-LABEL 'Ingreso al|Almacen'
        almcmov.fchdoc      COLUMN-LABEL 'Fecha'
        ccbddocu.codmat     COLUMN-LABEL 'Producto'
        desmat              COLUMN-LABEL 'Descripcion'
        ccbddocu.candes     COLUMN-LABEL 'Cantidad'
        ccbddocu.undvta     COLUMN-LABEL 'Unidad'
        ccbddocu.implin     COLUMN-LABEL 'Importe'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

