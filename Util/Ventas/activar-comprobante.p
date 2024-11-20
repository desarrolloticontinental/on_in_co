DEF VAR x-coddoc AS CHAR NO-UNDO.
DEF VAR x-nrodoc AS CHAR NO-UNDO.
DEF BUFFER b-docu FOR ccbcdocu.

ASSIGN
    x-coddoc = 'BOL'
    x-nrodoc = '26900030269'.

FIND ccbcdocu WHERE codcia = 1
    AND coddoc = x-coddoc
    AND nrodoc = x-nrodoc
    NO-ERROR.
IF NOT AVAILABLE ccbcdocu THEN RETURN.
IF ccbcdocu.flgest <> 'A' THEN DO:
    MESSAGE 'No está anulado' SKIP
        ccbcdocu.flgest ccbcdocu.imptot ccbcdocu.sdoact VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
ASSIGN
    ccbcdocu.glosa = ''
    ccbcdocu.sdoact = imptot
    ccbcdocu.flgest = 'P'.

CASE ccbcdocu.codped:
    WHEN 'P/M' THEN RUN Mostrador.
    WHEN 'PED' THEN DO:
        IF ccbcdocu.coddiv = '00000' 
        THEN RUN Credito-Ate.
        ELSE RUN Credito.
    END.
    OTHERWISE DO:
        MESSAGE 'por ahora no soportado'.
        RETURN.
    END.
END CASE.
RETURN.

PROCEDURE Mostrador:
/* **************** */
MESSAGE 'mostrador'.
FIND faccpedi WHERE faccpedi.codcia = 1
    AND faccpedi.coddoc = ccbcdocu.codped
    AND faccpedi.nroped = ccbcdocu.nroped
    NO-LOCK.
IF NOT AVAILABLE faccpedi THEN DO:
    MESSAGE 'No se pudo recuperar el detalle' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
/* FOR EACH facdpedi OF faccpedi NO-LOCK:     */
/*     CREATE ccbddocu.                       */
/*     BUFFER-COPY facdpedi TO ccbddocu       */
/*         ASSIGN                             */
/*         ccbddocu.coddiv = ccbcdocu.coddiv  */
/*         ccbddocu.coddoc = ccbcdocu.coddoc  */
/*         ccbddocu.nrodoc = ccbcdocu.nrodoc  */
/*         ccbddocu.fchdoc = ccbcdocu.fchdoc  */
/*         ccbddocu.candes = facdpedi.canped. */
/* END.                                       */

FIND LAST almcmov WHERE almcmov.codcia = 1
    AND almcmov.codalm = ccbcdocu.codalm
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 02
    AND almcmov.codref = ccbcdocu.coddoc
    AND almcmov.nroref = ccbcdocu.nrodoc
    AND almcmov.flgest = 'A'
    NO-ERROR.
IF NOT AVAILABLE almcmov OR almcmov.fchdoc <> ccbcdocu.fchdoc THEN DO:
    MESSAGE 'No se pudo recuperar los movimientos de almacen' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
almcmov.flgest = ''.
FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
    CREATE almdmov.
    BUFFER-COPY almcmov TO almdmov
        ASSIGN
        almdmov.codmat = ccbddocu.codmat
        almdmov.factor = ccbddocu.factor
        almdmov.candes = ccbddocu.candes
        almdmov.codund = ccbddocu.undvta.
    DISPLAY almdmov.codmat almdmov.candes almdmov.factor almdmov.codund.
END.

END PROCEDURE.


PROCEDURE Credito:
/* **************** */

FIND faccpedi WHERE faccpedi.codcia = 1
    AND faccpedi.coddoc = ccbcdocu.codped
    AND faccpedi.nroped = ccbcdocu.nroped
    NO-LOCK.
IF NOT AVAILABLE faccpedi THEN DO:
    MESSAGE 'No se pudo recuperar el detalle' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
FOR EACH facdpedi OF faccpedi NO-LOCK:
    CREATE ccbddocu.
    BUFFER-COPY facdpedi TO ccbddocu
        ASSIGN
        ccbddocu.coddiv = ccbcdocu.coddiv
        ccbddocu.coddoc = ccbcdocu.coddoc
        ccbddocu.nrodoc = ccbcdocu.nrodoc
        ccbddocu.fchdoc = ccbcdocu.fchdoc
        ccbddocu.candes = facdpedi.canped.
END.

FIND almcmov WHERE almcmov.codcia = 1
    AND almcmov.codalm = ccbcdocu.codalm
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 02
    AND almcmov.nrodoc = INTEGER (ccbcdocu.nrosal).
IF NOT AVAILABLE almcmov OR almcmov.fchdoc <> ccbcdocu.fchdoc THEN DO:
    MESSAGE 'No se pudo recuperar los movimientos de almacen' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
almcmov.flgest = ''.
FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
    CREATE almdmov.
    BUFFER-COPY almcmov TO almdmov
        ASSIGN
        almdmov.codmat = ccbddocu.codmat
        almdmov.factor = ccbddocu.factor
        almdmov.candes = ccbddocu.candes
        almdmov.codund = ccbddocu.undvta.
    DISPLAY almdmov.codmat almdmov.candes almdmov.factor almdmov.codund.
END.

END PROCEDURE.

PROCEDURE Credito-Ate:
/* **************** */

FIND faccpedi WHERE faccpedi.codcia = 1
    AND faccpedi.coddoc = ccbcdocu.codped
    AND faccpedi.nroped = ccbcdocu.nroped
    NO-LOCK.
IF NOT AVAILABLE faccpedi THEN DO:
    MESSAGE 'No se pudo recuperar el detalle' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
FOR EACH facdpedi OF faccpedi NO-LOCK:
    CREATE ccbddocu.
    BUFFER-COPY facdpedi TO ccbddocu
        ASSIGN
        ccbddocu.coddiv = ccbcdocu.coddiv
        ccbddocu.coddoc = ccbcdocu.coddoc
        ccbddocu.nrodoc = ccbcdocu.nrodoc
        ccbddocu.fchdoc = ccbcdocu.fchdoc
        ccbddocu.candes = facdpedi.canped.
END.
FIND b-docu WHERE b-docu.codcia = 1
    AND b-docu.coddoc = 'G/R'
    AND b-docu.codref = ccbcdocu.coddoc
    AND b-docu.nroref = ccbcdocu.nrodoc
    AND b-docu.flgest = 'P'
    EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE b-docu THEN b-docu.flgest = 'F'.
RELEASE b-docu.

END PROCEDURE.
