DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-Fecha AS DATE NO-UNDO.

x-Fecha = DATE(01,01,2014).

DEF TEMP-TABLE t-cocmp LIKE lg-cocmp 
    FIELD pRowid AS ROWID
    INDEX Llave01 AS PRIMARY codcia tpodoc nrodoc.

DEF TEMP-TABLE t-docmp LIKE lg-docmp 
    FIELD pRowid AS ROWID
    INDEX llave01 AS PRIMARY codcia tpodoc nrodoc codmat.

FOR EACH lg-cocmp NO-LOCK WHERE lg-cocmp.codcia = s-codcia
    AND lg-cocmp.tpodoc = "N"
    AND lg-cocmp.fchdoc >= x-Fecha:
    CREATE t-cocmp.
    BUFFER-COPY lg-cocmp
        TO t-cocmp
        ASSIGN t-cocmp.prowid = ROWID(lg-cocmp).
END.
FOR EACH t-cocmp:
    FOR EACH lg-docmp NO-LOCK WHERE lg-docmp.codcia = s-codcia
        AND lg-docmp.coddiv = t-cocmp.coddiv
        AND lg-docmp.tpodoc = t-cocmp.tpodoc
        AND lg-docmp.nrodoc = t-cocmp.nrodoc:
        CREATE t-docmp.
        BUFFER-COPY lg-docmp
            TO t-docmp
            ASSIGN t-docmp.prowid = ROWID(lg-docmp).
    END.
END.
MESSAGE 'inicializamos temporales' VIEW-AS ALERT-BOX WARNING.
/* FOR EACH t-cocmp WHERE NOT (t-cocmp.codcia = 001             */
/*                             AND t-cocmp.fchdoc >= 01/01/2013 */
/*                             AND t-cocmp.fchdoc <= 12/31/2013 */
/*                             AND t-cocmp.tpodoc = 'N'         */
/*                             AND lg-cocmp.flgsit = 'T'):      */
/*     DELETE t-cocmp.                                          */
/* END.                                                         */
FOR EACH t-docmp:
    t-docmp.canaten = 0.
END.

MESSAGE 'cargamos compras' VIEW-AS ALERT-BOX WARNING.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 'I'
    AND almcmov.codmov = 02
    AND almcmov.flgest <> 'A'
    AND almcmov.fchdoc >= x-Fecha,
    EACH almdmov OF almcmov NO-LOCK:
    FIND t-cocmp WHERE t-cocmp.codcia = s-codcia
        AND t-cocmp.tpodoc ='N'
        AND t-cocmp.nrodoc = INTEGER(almcmov.nrorf1)
        AND t-cocmp.codalm = almcmov.codalm
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-cocmp THEN NEXT.
    FIND FIRST t-docmp WHERE t-docmp.codcia = s-codcia
        AND t-docmp.tpodoc = 'N'
        AND t-docmp.nrodoc = INTEGER(almcmov.nrorf1)
        AND t-docmp.codmat = almdmov.codmat
        NO-ERROR.
    IF NOT AVAILABLE t-docmp THEN NEXT.
/*     DISPLAY almcmov.codalm almcmov.nrorf1 almdmov.candes. */
/*     PAUSE 0.                                              */
    ASSIGN
        t-docmp.canaten = t-docmp.canaten + almdmov.candes.
END.
/* buscamos errores */
MESSAGE 'buscamos errores' VIEW-AS ALERT-BOX WARNING.
FOR EACH t-cocmp WHERE t-cocmp.codcia = s-codcia:
    DISPLAY t-cocmp.nrodoc.
    PAUSE 0.
    IF CAN-FIND(FIRST t-docmp WHERE t-docmp.codcia = s-codcia
                AND t-docmp.tpodoc = t-cocmp.tpodoc
                AND t-docmp.coddiv = t-cocmp.coddiv
                AND t-docmp.nrodoc = t-cocmp.nrodoc
                AND t-docmp.canpedi > t-docmp.canaten
                NO-LOCK)
        THEN t-cocmp.flgsit = "P".
END.
FOR EACH t-cocmp:
    FIND lg-cocmp WHERE ROWID(lg-cocmp) = t-cocmp.prowid.
    IF lg-cocmp.flgsit <> t-cocmp.flgsit THEN lg-cocmp.flgsit = t-cocmp.flgsit.
    FOR EACH t-docmp WHERE t-docmp.codcia = s-codcia
        AND t-docmp.coddiv = t-cocmp.coddiv
        AND t-docmp.tpodoc = t-cocmp.tpodoc
        AND t-docmp.nrodoc = t-cocmp.nrodoc:
        FIND lg-docmp WHERE ROWID(lg-docmp) = t-docmp.prowid.
        IF t-docmp.canaten <> lg-docmp.canate THEN lg-docmp.canaten = t-docmp.canate.
    END.
END.
