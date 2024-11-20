
DEF VAR s-codcia AS INTE INIT 001.
DEF VAR x-Comprobantes AS CHAR INIT 'FAC,BOL,TCK' NO-UNDO.
DEF VAR x-Error AS CHAR FORMAT 'x(50)' NO-UNDO.
DEF VAR k AS INT NO-UNDO.

/* A partir del almacen buscamos la venta */
FOR EACH gn-divi NO-LOCK WHERE codcia = s-codcia,
    EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia
    AND almacen.coddiv = gn-divi.coddiv,
    EACH almcmov WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 02
    AND almcmov.fchdoc >= 01/01/2013
    AND almcmov.fchdoc <= 03/31/2013.
    k = 0.
    FOR EACH almdmov OF almcmov NO-LOCK.
        k = k + 1.
    END.
    IF almcmov.flgest = 'A' AND k > 0 THEN DO:
        DISPLAY 'Anulado' almcmov.fchdoc almcmov.codalm almcmov.nroser almcmov.nrodoc FORMAT '9999999' almcmov.flgest.
        PAUSE 0.
/*         /* Borramos detalle */                                                                           */
/*         FOR EACH almdmov OF almcmov:                                                                     */
/*             DELETE almdmov.                                                                              */
/*         END.                                                                                             */
/*         /* buscamos si está anulado el comprobante */                                                    */
/*         FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia                                                   */
/*             AND ccbcdocu.coddoc = almcmov.codref                                                         */
/*             AND ccbcdocu.nrodoc = almcmov.nroref                                                         */
/*             NO-LOCK NO-ERROR.                                                                            */
/*         IF NOT AVAILABLE ccbcdocu OR ccbcdocu.flgest <> 'A' THEN DO:                                     */
/*             /* Inconsistencia: NO se encontró el comprobante origen => borramos movimiento de almacén */ */
/*             DELETE almcmov.                                                                              */
/*         END.                                                                                             */
        NEXT.       /* Pasamos al siguiente registro */
    END.
    /* buscamos informacion en el comprobante */
    FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddoc = almcmov.codref
        AND ccbcdocu.nrodoc = almcmov.nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN DO:
        /* Inconsistenca: NO existe el comprobante orige => anulamos todos los movimientos de almacén */
        DISPLAY 'No FAC' almcmov.fchdoc almcmov.codalm almcmov.nroser almcmov.nrodoc FORMAT '9999999' .
        PAUSE 0.
/*         /* Borramos detalle */                          */
/*         FOR EACH almdmov OF almcmov:                    */
/*             DELETE almdmov.                             */
/*         END.                                            */
/*         DELETE almcmov.                                 */
        NEXT.       /* Pasamos al siguiente registro */
    END.
    /* revisamos el detalle */
    FOR EACH almdmov OF almcmov:
        FIND ccbddocu OF ccbcdocu WHERE ccbddocu.codmat = almdmov.codmat
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbddocu THEN DO:
            /* Inconsistencia: NO existe el producto en el comprobante */
            DISPLAY 'NO producto' almcmov.fchdoc almcmov.codalm almcmov.nroser almcmov.nrodoc FORMAT '9999999' almdmov.codmat.
            PAUSE 0.
/*             DELETE almdmov. */
        END.
        ELSE IF almdmov.candes * almdmov.factor <> ccbddocu.candes * ccbddocu.factor THEN DO:
            /* Inconsistencia: Cantidades diferentes */
            DISPLAY 'Diferencia Cantidades' almcmov.fchdoc almcmov.codalm almcmov.nroser almcmov.nrodoc FORMAT '9999999' almdmov.codmat.
            PAUSE 0.
/*             DELETE almdmov. */
        END.
    END.
END.

