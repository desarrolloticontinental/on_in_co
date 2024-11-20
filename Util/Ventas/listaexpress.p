DEF VAR x-estado AS CHAR.
OUTPUT TO d:\tmp\listaexpress.txt.
PUT UNFORMATTED
    'CODIGO|NUMERO|EMISION|VENCIMIENTO|COND VTA|NRO ORDEN|'
    'CLIENTE|RUC|IMPORTE TOTAL|ESTADO|'
    'ARTICULO|DESCRIPCION|UNIDAD|CANTIDAD'  
    '|IMPORTE LINEA|LINEA|SUBLINEA'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddiv = '00506'
    AND fchdoc >= 01/01/2016
    AND LOOKUP(coddoc, 'bol,tck,fac') > 0,
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST almmmatg OF ccbddocu NO-LOCK,
    FIRST almtfami OF almmmatg NO-LOCK,
    FIRST almsfami OF almmmatg NO-LOCK:
    RUN gn/fflgestccb (ccbcdocu.flgest, OUTPUT x-estado).
    PUT UNFORMATTED
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.fchvto '|'
        ccbcdocu.fmapgo '|'
        ccbcdocu.nroord '|'
        ccbcdocu.nomcli '|'
        ccbcdocu.codcli '|'
        ccbcdocu.imptot '|'
        x-estado '|'
        ccbddocu.codmat '|'
        almmmatg.desmat '|'
        ccbddocu.undvta '|'
        ccbddocu.candes '|'
        ccbddocu.implin - ccbddocu.impdto2 '|'
        almtfami.codfam ' - '  Almtfami.desfam '|'
        AlmSFami.subfam ' - ' AlmSFami.dessub
        SKIP.



END.
