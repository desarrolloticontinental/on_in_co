DEF TEMP-TABLE detalle
    FIELD codcia LIKE ccbcdocu.codcia
    FIELD coddoc LIKE ccbcdocu.coddoc
    FIELD nrodoc LIKE ccbcdocu.nrodoc
    FIELD fchdoc LIKE ccbcdocu.fchdoc
    FIELD codcli LIKE ccbcdocu.codcli
    FIELD nomcli LIKE ccbcdocu.nomcli
    FIELD codmon LIKE ccbcdocu.codmon
    FIELD codmat LIKE ccbddocu.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD implin LIKE ccbddocu.implin.
DEF BUFFER b-cdocu FOR ccbcdocu.
    DEF VAR x-can AS DEC.
    DEF VAR x-imptot AS DEC.
    DEF VAR x-coe AS DEC.
    DEF VAR f-factor AS DEC.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'n/c'
    AND codven = '173'
    AND fchdoc >= 12/01/08
    AND flgest <> 'a':
        /* NOTAS DE CREDITO por OTROS conceptos */
       IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" THEN DO:
           RUN PROCESA-NOTA.
           NEXT.
       END.
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK, FIRST almmmatg OF ccbddocu NO-LOCK:
        CREATE detalle.
        BUFFER-COPY ccbcdocu TO detalle.
        ASSIGN
            detalle.codmat = ccbddocu.codmat
            detalle.desmat = almmmatg.desmat
            detalle.implin = ccbddocu.implin.
    END.
END.
OUTPUT TO c:\tmp\creditos.txt.
FOR EACH detalle:
    EXPORT DELIMITER '|' DETALLE.
    /*DISPLAY detalle WITH STREAM-IO NO-BOX WIDTH 320.*/
END.
OUTPUT CLOSE.


PROCEDURE procesa-nota:

    FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
        AND B-CDOCU.CodDoc = CcbCdocu.Codref 
        AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDOCU THEN RETURN.

    ASSIGN
        x-Can = 0                       /* ¿¿¿ OJO ??? */
        x-ImpTot = B-CDOCU.ImpTot.      /* <<< OJO <<< */

    /* buscamos si hay una aplicación de fact adelantada */
    FIND FIRST Ccbddocu OF B-CDOCU WHERE Ccbddocu.implin < 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbddocu THEN x-ImpTot = x-ImpTot + ABSOLUTE(Ccbddocu.ImpLin).
    /* ************************************************* */
    x-Coe = Ccbcdocu.ImpTot / x-ImpTot.     /* OJO */
    FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:
        /* ***************** FILTROS ********************************* */
        FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
            AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almmmatg THEN NEXT.
        IF Ccbddocu.implin <= 0 THEN NEXT.       /* <<< OJO <<< */
        /* ************************************************************ */
        F-FACTOR  = 1. 
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = Ccbddocu.UndVta
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
           F-FACTOR = Almtconv.Equival.
           IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.

        CREATE detalle.
        BUFFER-COPY ccbcdocu TO detalle.
        ASSIGN
            detalle.codmat = ccbddocu.codmat
            detalle.desmat = almmmatg.desmat
            detalle.implin = ccbddocu.implin * x-coe.
    END.  

END.

