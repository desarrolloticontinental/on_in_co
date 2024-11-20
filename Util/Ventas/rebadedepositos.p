DEF VAR x-linea AS CHAR FORMAT 'x(100)'.
DEF VAR x-nrodoc AS CHAR FORMAT 'x(9)'.
DEF VAR x-imptot AS DEC.
DEF VAR x-rebate AS DEC FORMAT '>>.9999' DECIMALS 4.
DEF VAR x-sdoact AS DEC.
DEF VAR x-sdodoc AS DEC.

DEF WORK-TABLE detalle LIKE ccbcdocu
    FIELD pgoref LIKE ccbcdocu.fmapgo
    FIELD fchref AS DATE
    FIELD impref AS DEC
    FIELD sdoref AS DEC
    FIELD impapl AS DEC
    FIELD porreb AS DEC.

DEF BUFFER b-detalle FOR detalle.
DEF BUFFER b-cdocu FOR ccbcdocu.    

DEF TEMP-TABLE depositos LIKE ccbcdocu
    FIELD rebade AS DEC
    INDEX llave01 AS PRIMARY codcia coddoc codcli fchdoc nrodoc.

INPUT FROM c:\tmp\depositos.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF SUBSTRING (x-linea,1,1) <> '' THEN DO:
        ASSIGN
            x-nrodoc = SUBSTRING(x-linea,1,9)
            x-imptot = DECIMAL(SUBSTRING(x-linea,10,21))
            x-rebate = DECIMAL(SUBSTRING(x-linea,46)) * 100.
        FIND b-cdocu WHERE b-cdocu.codcia = 1
            AND b-cdocu.coddoc = 'bd'
            AND b-cdocu.nrodoc = x-nrodoc
            NO-LOCK.
        IF b-cdocu.flgest = 'A' THEN NEXT.
        CREATE depositos.
        BUFFER-COPY b-cdocu TO depositos
            ASSIGN 
                depositos.imptot = x-imptot
                depositos.rebade = x-rebate.
    END.
END.
INPUT CLOSE.

FOR EACH depositos BREAK BY codcia BY coddoc BY codcli BY fchdoc BY nrodoc:
    x-sdoact = depositos.imptot.
    x-rebate = depositos.rebade.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
        AND LOOKUP(ccbcdocu.coddoc, 'fac,bol') > 0
        AND ccbcdocu.codcli = depositos.codcli
        AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0
        AND ccbcdocu.fmapgo = '002'
        AND flgest <> 'a'
        /*AND coddiv = '00000'*/
        AND fchdoc >= 11/01/2010
        AND fchdoc <= 03/31/2011
        /*AND nroped BEGINS '040'*/
        BY ccbcdocu.codcia BY ccbcdocu.fchdoc BY ccbcdocu.coddoc BY ccbcdocu.nrodoc:
        x-sdodoc = ccbcdocu.imptot.
        FIND LAST b-detalle WHERE b-detalle.codcia = 1
            AND b-detalle.codref = ccbcdocu.coddoc
            AND b-detalle.nroref = ccbcdocu.nrodoc
            NO-LOCK NO-ERROR.
        
        IF AVAILABLE b-detalle THEN DO:
            /* Verificamos si aun existe un saldo */
            IF b-detalle.sdoref <= b-detalle.impapl THEN NEXT.
            x-sdodoc = b-detalle.sdoref - b-detalle.impapl.
        END.
        CREATE detalle.
        BUFFER-COPY depositos TO detalle.
        ASSIGN
            detalle.impapl = MINIMUM(x-sdoact, x-sdodoc)
            detalle.codref = ccbcdocu.coddoc
            detalle.nroref = ccbcdocu.nrodoc
            detalle.pgoref = ccbcdocu.fmapgo
            detalle.fchref = ccbcdocu.fchdoc
            detalle.impref = ccbcdocu.imptot        /* x-sdodoc */
            detalle.sdoref = x-sdodoc
            detalle.porreb = x-rebate.
        x-sdoact = x-sdoact - detalle.impapl.
        ASSIGN detalle.sdoact = x-sdoact.
        IF x-sdoact <= 0 THEN LEAVE.
    END.
    IF LAST-OF(depositos.codcli) THEN DO:
        /* buscamos el resto de facturas */
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
            AND LOOKUP(ccbcdocu.coddoc, 'fac,bol') > 0
            AND ccbcdocu.codcli = depositos.codcli
            AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0
            AND flgest <> 'a'
            /*AND coddiv = '00000'*/
            AND fchdoc >= 11/01/2010
            AND fchdoc <= 03/31/2011
            /*AND nroped BEGINS '040'*/
            BY ccbcdocu.codcia BY ccbcdocu.fchdoc BY ccbcdocu.coddoc BY ccbcdocu.nrodoc:
            x-sdodoc = ccbcdocu.imptot.
            FIND LAST b-detalle WHERE b-detalle.codcia = 1
                AND b-detalle.codref = ccbcdocu.coddoc
                AND b-detalle.nroref = ccbcdocu.nrodoc
                NO-LOCK NO-ERROR.
            IF AVAILABLE b-detalle THEN DO:
                /* Verificamos si aun existe un saldo */
                IF b-detalle.sdoref <= b-detalle.impapl THEN NEXT.
                x-sdodoc = b-detalle.sdoref - b-detalle.impapl.
            END.
            CREATE detalle.
            BUFFER-COPY depositos 
                EXCEPT depositos.coddoc depositos.nrodoc depositos.fchdoc depositos.imptot depositos.sdoact
                TO detalle.
            ASSIGN
                detalle.impapl = 0
                detalle.codref = ccbcdocu.coddoc
                detalle.nroref = ccbcdocu.nrodoc
                detalle.pgoref = ccbcdocu.fmapgo
                detalle.fchref = ccbcdocu.fchdoc
                detalle.impref = ccbcdocu.imptot        /* x-sdodoc */
                detalle.sdoref = x-sdodoc
                detalle.porreb = 0.
        END.
        /* buscamos nota de credito */
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
            AND ccbcdocu.coddoc = 'n/c'
            AND ccbcdocu.cndcre = "D"
            AND ccbcdocu.codcli = depositos.codcli
            AND flgest <> 'a'
            AND coddiv = '00000'
            AND fchdoc >= 11/01/2010
            AND fchdoc <= 03/31/2011:
            CREATE detalle.
            BUFFER-COPY depositos TO detalle.
            ASSIGN
                detalle.codref = ccbcdocu.coddoc
                detalle.nroref = ccbcdocu.nrodoc
                detalle.fchref = ccbcdocu.fchdoc
                detalle.pgoref = ccbcdocu.fmapgo
                detalle.impref = -1 * ccbcdocu.imptot 
                detalle.sdoref = -1 * ccbcdocu.imptot 
                detalle.porreb = 0
                detalle.sdoact = 0.
        END.
    END.
END.

OUTPUT TO c:\tmp\rebadedepositos.prn.
FOR EACH detalle:
    DISPLAY
        detalle.codref
        detalle.nroref
        detalle.codcli
        detalle.nomcli
        detalle.fchref
        detalle.impref
        detalle.sdoref
        detalle.coddoc
        detalle.nrodoc
        detalle.fchdoc
        detalle.pgoref
        detalle.imptot
        detalle.impapl
        detalle.sdoact
        detalle.porreb
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

