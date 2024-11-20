/* IMPORTAR ARCHIVOS DE LOS CONOS  */

DEF VAR s-codcia AS INT NO-UNDO.

/* VENTAS */
DEF TEMP-TABLE t-cdocu LIKE ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbcdocu.d.
REPEAT:
    CREATE t-cdocu.
    IMPORT t-cdocu NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
DEF TEMP-TABLE t-ddocu LIKE ccbddocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbddocu.d.
REPEAT:
    CREATE t-ddocu.
    IMPORT t-ddocu NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-cdocu WHERE t-cdocu.coddoc <> "":
    CREATE ccbcdocu.
    BUFFER-COPY t-cdocu TO ccbcdocu NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    FOR EACH t-ddocu OF t-cdocu:
        CREATE ccbddocu.
        BUFFER-COPY t-ddocu TO ccbddocu NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.

DEF TEMP-TABLE t-clie LIKE gn-clie.
DISABLE TRIGGERS FOR LOAD OF gn-clie.
INPUT FROM C:\tmp\FeriaPlazaNorte\gn-clie.d.
REPEAT:
    CREATE t-clie.
    IMPORT t-clie NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-clie WHERE t-clie.codcli <> "":
    FIND FIRST gn-clie OF t-clie NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN DO:
        CREATE gn-clie.
        BUFFER-COPY t-clie TO gn-clie NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.

DEF TEMP-TABLE t-card LIKE gn-card.
DISABLE TRIGGERS FOR LOAD OF gn-card.
INPUT FROM C:\tmp\FeriaPlazaNorte\gn-card.d.
REPEAT:
    CREATE t-card.
    IMPORT t-card NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-card WHERE t-card.nrocard <> "":
    FIND FIRST gn-card WHERE gn-card.nrocard =  t-card.nrocard NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-card THEN DO:
        CREATE gn-card.
        BUFFER-COPY t-card TO gn-card NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.

/* ALMACENES */
DEF TEMP-TABLE t-cmov LIKE almcmov.
DISABLE TRIGGERS FOR LOAD OF almcmov.
INPUT FROM C:\tmp\FeriaPlazaNorte\almcmov.d.
REPEAT:
    CREATE t-cmov.
    IMPORT t-cmov NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
DEF TEMP-TABLE t-dmov LIKE almdmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
INPUT FROM C:\tmp\FeriaPlazaNorte\almdmov.d.
REPEAT:
    CREATE t-dmov.
    IMPORT t-dmov NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-cmov WHERE t-cmov.codalm <> "":
    CREATE almcmov.
    BUFFER-COPY t-cmov TO almcmov NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    FOR EACH t-dmov OF t-cmov:
        CREATE almdmov.
        BUFFER-COPY t-dmov TO almdmov NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.

/* CAJA */
DEF TEMP-TABLE t-ccaja LIKE ccbccaja.
DISABLE TRIGGERS FOR LOAD OF ccbccaja.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbccaja.d.
REPEAT:
    CREATE t-ccaja.
    IMPORT t-ccaja NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
DEF TEMP-TABLE t-dcaja LIKE ccbdcaja.
DISABLE TRIGGERS FOR LOAD OF ccbdcaja.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbdcaja.d.
REPEAT:
    CREATE t-dcaja.
    IMPORT t-dcaja NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-ccaja WHERE t-ccaja.coddiv <> '':
    CREATE ccbccaja.
    BUFFER-COPY t-ccaja TO ccbccaja NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    FOR EACH t-dcaja OF t-ccaja:
        CREATE ccbdcaja.
        BUFFER-COPY t-dcaja TO ccbdcaja NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.

DEF TEMP-TABLE t-bcmov LIKE ccbcmov.
DISABLE TRIGGERS FOR LOAD OF ccbcmov.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbcmov.d.
REPEAT:
    CREATE t-bcmov.
    IMPORT t-bcmov NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-bcmov WHERE t-bcmov.nrodoc <> "":
    CREATE ccbcmov.
    BUFFER-COPY t-bcmov TO ccbcmov NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.

DEF TEMP-TABLE t-bdmov LIKE ccbdmov.
DISABLE TRIGGERS FOR LOAD OF ccbdmov.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbdmov.d.
REPEAT:
    CREATE t-bdmov.
    IMPORT t-bdmov NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-bdmov WHERE t-bdmov.coddoc <> "":
    FIND FIRST ccbdmov WHERE ccbdmov.codcia = t-bdmov.codcia
        AND ccbdmov.coddoc = t-bdmov.coddoc
        AND ccbdmov.nrodoc = t-bdmov.nrodoc
        AND ccbdmov.codref = t-bdmov.codref
        AND ccbdmov.nroref = t-bdmov.nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbdmov THEN DO:
        CREATE ccbdmov.
        BUFFER-COPY t-bdmov TO ccbdmov NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.

DEF TEMP-TABLE t-cmvto LIKE ccbcmvto.
DISABLE TRIGGERS FOR LOAD OF ccbcmvto.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbcmvto.d.
REPEAT:
    CREATE t-cmvto.
    IMPORT t-cmvto NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
DEF TEMP-TABLE t-dmvto LIKE ccbdmvto.
DISABLE TRIGGERS FOR LOAD OF ccbdmvto.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbdmvto.d.
REPEAT:
    CREATE t-dmvto.
    IMPORT t-dmvto NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-cmvto WHERE t-cmvto.coddoc <> "":
    CREATE ccbcmvto.
    BUFFER-COPY t-cmvto TO ccbcmvto NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    FOR EACH t-dmvto WHERE t-dmvto.codcia = t-cmvto.codcia
        AND t-dmvto.coddoc = t-cmvto.coddoc
        AND t-dmvto.nrodoc = t-cmvto.nrodoc:
        CREATE ccbdmvto.
        BUFFER-COPY t-dmvto TO ccbdmvto NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.

DEF TEMP-TABLE t-cierr LIKE ccbcierr.
DISABLE TRIGGERS FOR LOAD OF ccbcierr.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbcierr.d.
REPEAT:
    CREATE t-cierr.
    IMPORT t-cierr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-cierr WHERE t-cierr.usuario <> "":
    CREATE ccbcierr.
    BUFFER-COPY t-cierr TO ccbcierr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.

DEF TEMP-TABLE t-decl LIKE ccbdecl.
DISABLE TRIGGERS FOR LOAD OF ccbdecl.
INPUT FROM C:\tmp\FeriaPlazaNorte\ccbdecl.d.
REPEAT:
    CREATE t-decl.
    IMPORT t-decl NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
INPUT CLOSE.
FOR EACH t-decl WHERE t-decl.usuario <> "":
    CREATE ccbdecl.
    BUFFER-COPY t-decl TO ccbdecl NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.

/* DEF TEMP-TABLE t-dtickets LIKE vtadtickets.         */
/* DISABLE TRIGGERS FOR LOAD OF vtadtickets.           */
/* INPUT FROM C:\tmp\FeriaPlazaNorte\vtadtickets.d.    */
/* REPEAT:                                             */
/*     CREATE t-dtickets.                              */
/*     IMPORT t-dtickets.                              */
/* END.                                                */
/* INPUT CLOSE.                                        */
/* FOR EACH t-dtickets WHERE t-dtickets.codpro <> "":  */
/*     CREATE vtadtickets.                             */
/*     BUFFER-COPY t-dtickets TO vtadtickets NO-ERROR. */
/*     IF ERROR-STATUS:ERROR THEN UNDO, NEXT.          */
/* END.                                                */
