DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-fchdoc AS DATE NO-UNDO.

ASSIGN 
    s-fchdoc = DATE(12,31,2013).

DEF TEMP-TABLE t-cmov LIKE almcmov.
DEF TEMP-TABLE t-dmov LIKE almdmov.
DEF TEMP-TABLE t-matg LIKE almmmatg.

/* Cargamos temporal */
RUN Carga-Temporal.

/* generamos cabeceras */
FOR EACH t-dmov NO-LOCK:
    FIND FIRST t-cmov WHERE t-cmov.codcia = t-dmov.codcia
        AND t-cmov.codalm = t-dmov.codalm
        AND t-cmov.tipmov = t-dmov.tipmov
        AND t-cmov.codmov = t-dmov.codmov
        AND t-cmov.nroser = t-dmov.nroser
        AND t-cmov.nrodoc = t-dmov.nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-cmov THEN DO:
        CREATE t-cmov.
        BUFFER-COPY t-dmov TO t-cmov ASSIGN t-cmov.usuario = 'ADMIN'.
    END.
END.
/* FOR EACH t-dmov, FIRST t-cmov OF t-dmov:                                          */
/*     DISPLAY t-cmov.codalm t-dmov.codmat t-dmov.tipmov t-dmov.codmov t-dmov.candes */
/*         WITH STREAM-IO NO-BOX WIDTH 320.                                          */
/* END.                                                                              */

/* Ahora sí grabamos las tablas */
FOR EACH t-cmov NO-LOCK:
    CREATE almcmov.
    BUFFER-COPY t-cmov TO almcmov.
END.
FOR EACH t-dmov NO-LOCK:
    CREATE almdmov.
    BUFFER-COPY t-dmov TO almdmov.
    FIND FIRST t-matg WHERE t-matg.codcia = t-dmov.codcia
        AND t-matg.codmat = t-dmov.codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-matg THEN DO:
        CREATE t-matg.
        ASSIGN
            t-matg.codcia = t-dmov.codcia
            t-matg.codmat = t-dmov.codmat.
    END.
END.
/* Actualizamos kardex */
DEF VAR x-fchini AS DATE NO-UNDO.
x-fchini = s-fchdoc - 30.
FOR EACH t-matg NO-LOCK:
    DISPLAY 'recalculando:' t-matg.codmat WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RUN alm/calc-costo-promedio (t-matg.codmat, x-fchini ).
END.

PROCEDURE Carga-Temporal:

    FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
        EACH almdmov NO-LOCK WHERE almdmov.codcia = s-codcia
        AND almdmov.codalm = almacen.codalm
        AND almdmov.codmov = 98
        AND almdmov.fchdoc = s-fchdoc
        AND almdmov.codmat <> '024394':
        /* Grabamos el movimiento inverso */
        CREATE t-dmov.
        BUFFER-COPY almdmov
            TO t-dmov
            ASSIGN
            t-dmov.NroItm = 1
            t-dmov.CodCia = s-codcia
            t-dmov.TipMov = (IF almdmov.tipmov = "I" THEN "S" ELSE "I")
            t-dmov.CodMov = 97
            .
    END.

END PROCEDURE.

