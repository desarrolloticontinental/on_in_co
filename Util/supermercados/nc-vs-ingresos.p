OUTPUT TO c:\tmp\makro.txt.
PUT UNFORMATTED
    'DOC|NUMERO|FECHA|DEVOLUCION|NO. INGRESO|REF|NUM REF|COD BARRAS|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|IMPORTE'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'n/c'
    AND fchdoc >= 01/01/2014
    AND codcli = '20492092313'
    AND flgest <> "A",
    FIRST almcmov NO-LOCK WHERE Almcmov.CodCia = 1
    AND  Almcmov.CodAlm = CcbCDocu.CodAlm 
    AND  Almcmov.TipMov = "I"
    AND  Almcmov.CodMov = CcbCDocu.CodMov 
    AND  Almcmov.NroDoc = INTEGER(CcbCDocu.NroPed),
    EACH ccbddocu OF ccbcdocu NO-LOCK, FIRST almmmatg OF ccbddocu NO-LOCK:
    PUT UNFORMATTED
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        almcmov.nrorf2 '|'
        almcmov.nrodoc '|'
        almcmov.codref '|'
        almcmov.nrorf1 '|'
        almcmov.libre_c01 '|'
        ccbddocu.codmat '|'
        almmmatg.desmat '|'
        ccbddocu.candes '|'
        ccbddocu.undvta '|'
        ccbddocu.implin
        SKIP.

END.
