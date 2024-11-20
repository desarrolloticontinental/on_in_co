DEF VAR x-codven AS CHAR NO-UNDO.
DEF VAR x-coddoc AS CHAR INIT 'FAC,BOL,N/C' NO-UNDO.
DEF TEMP-TABLE t-ddocu LIKE ccbddocu.
DEF TEMP-TABLE t-cdocu LIKE ccbcdocu.
DEF BUFFER b-cdocu FOR ccbcdocu.
DEF VAR x-can AS INT.
DEF VAR x-imptot AS DEC.
DEF VAR x-coe AS DEC.
DEF VAR f-factor AS DEC.

x-codven = '151'.   /* Autoservicios */

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND fchdoc >= 01/01/06
    AND fchdoc <= 12/31/06
    AND flgest <> 'a'
    AND LOOKUP(codven, x-codven) > 0
    AND LOOKUP(coddoc, x-coddoc) > 0:
    CREATE t-cdocu.
    BUFFER-COPY ccbcdocu TO t-cdocu.
    IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" THEN DO:
        RUN PROCESA-NOTA.
        NEXT.
    END.
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
        CREATE t-ddocu.
        BUFFER-COPY ccbddocu TO t-ddocu.
    END.

END.

OUTPUT TO c:\tmp\autoservicios2006.txt.
FOR EACH t-cdocu NO-LOCK,
    EACH t-ddocu OF t-cdocu NO-LOCK,
    FIRST almmmatg OF t-ddocu NO-LOCK,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 0
    AND gn-clie.codcli = t-cdocu.codcli:
    PUT 
        t-cdocu.coddiv FORMAT 'x(5)' '|'
        t-cdocu.codven '|'
        t-cdocu.coddoc '|'
        t-cdocu.nrodoc '|'
        t-cdocu.fchdoc '|'
        t-cdocu.codref '|'
        t-cdocu.nroref '|'
        t-cdocu.codcli '|'
        t-cdocu.nomcli '|'
        t-cdocu.fmapgo '|'
        t-cdocu.glosa '|'
        t-cdocu.codmon '|'
        t-cdocu.tpocmb '|'
        t-cdocu.porigv '|'
        t-ddocu.codmat '|'
        desmat '|'
        desmar  FORMAT 'x(15)' '|'
        codfam '|'
        t-ddocu.candes '|'
        t-ddocu.undvta '|'
        t-ddocu.aftigv '|'
        t-ddocu.implin '|'
        gn-clie.CodDept '|'
        gn-clie.CodProv '|'
        gn-clie.CodDist
        SKIP.
END.
OUTPUT CLOSE.


PROCEDURE PROCESA-NOTA:
    FOR EACH CcbDdocu OF CcbCdocu:
        x-can = IF CcbDdocu.CodMat = "00005" THEN 1 ELSE 0.
    END.
    FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia AND
                       B-CDOCU.CodDoc = CcbCdocu.Codref AND
                       B-CDOCU.NroDoc = CcbCdocu.Nroref 
                       NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDOCU THEN RETURN.
    x-ImpTot = B-CDOCU.ImpTot.     /* <<< OJO <<< */
    /* buscamos si hay una aplicación de fact adelantada */
    FIND FIRST Ccbddocu OF B-CDOCU WHERE Ccbddocu.implin < 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbddocu THEN x-ImpTot = x-ImpTot + ABSOLUTE(Ccbddocu.ImpLin).
    x-coe = Ccbcdocu.ImpTot / x-ImpTot.
    FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:
        FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia AND
                            Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almmmatg THEN NEXT.
        IF Ccbddocu.implin <= 0 THEN NEXT.       /* <<< OJO <<< */
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas AND  
                            Almtconv.Codalter = Ccbddocu.UndVta
                            NO-LOCK NO-ERROR.
        F-FACTOR  = 1. 
        IF AVAILABLE Almtconv THEN DO:
           F-FACTOR = Almtconv.Equival.
           IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.
        CREATE t-ddocu.
        BUFFER-COPY ccbddocu TO t-ddocu
            ASSIGN
                t-ddocu.coddiv = ccbcdocu.coddiv
                t-ddocu.coddoc = ccbcdocu.coddoc
                t-ddocu.nrodoc = ccbcdocu.nrodoc
                t-ddocu.implin = CcbDdocu.ImpLin * x-coe
                t-ddocu.candes = CcbDdocu.CanDes * x-can.
    END.  

END PROCEDURE.

