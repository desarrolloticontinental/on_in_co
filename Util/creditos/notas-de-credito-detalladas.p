DEF VAR s-codcli AS CHAR INIT '20109072177'.

OUTPUT TO c:\tmp\notas-de-credito.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND codcli = s-codcli
    AND coddoc = 'N/C'
    AND fchdoc >= 01/01/09
    AND coddiv = '00000'
    AND flgest <> 'a',
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST almmmatg OF ccbddocu NO-LOCK
    BY ccbcdocu.nrodoc:
    DISPLAY 
        ccbcdocu.coddoc COLUMN-LABEL 'Doc'
        ccbcdocu.nrodoc COLUMN-LABEL 'Numero'
        ccbcdocu.fchdoc COLUMN-LABEL 'Emision'
        ccbcdocu.codref COLUMN-LABEL 'Ref'
        ccbcdocu.nroref COLUMN-LABEL 'Numero'
        ccbddocu.codmat COLUMN-LABEL 'Material'
        desmat COLUMN-LABEL 'Descripcion'
        desmar COLUMN-LABEL 'Marca'
        candes  COLUMN-LABEL 'Cantidad'
        ccbcdocu.codmon COLUMN-LABEL 'Moneda'
        ccbddocu.undvta COLUMN-LABEL 'Unidad'
        preuni COLUMN-LABEL 'Unitario'
        implin COLUMN-LABEL 'Importe'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

