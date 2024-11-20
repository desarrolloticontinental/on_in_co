/*  MENSAJE DE BLOQUEO POR DEUDA VENCIDA */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-DiasAtraso AS INT INIT 10 NO-UNDO.
DEF VAR x-TpoCmbCmp AS DEC NO-UNDO.
DEF VAR x-TpoCmbVta AS DEC NO-UNDO.
DEF VAR x-DeudaSoles AS DEC NO-UNDO.

DEF TEMP-TABLE detalle LIKE ccbcdocu.

/* BUSCAMOS DOCUMENTOS DE CARGO */
FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia
    AND flgest = 'P'
    AND (TODAY - fchvto) > x-DiasAtraso,
    FIRST facdocum OF ccbcdocu NO-LOCK WHERE tpodoc = YES:  
    CREATE detalle.
    BUFFER-COPY ccbcdocu TO detalle.
    /* TODO A SOLES */
    IF detalle.codmon = 2 THEN DO:
        /* buscamos el tipo de cambio a aplicar */
        FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc
            USE-INDEX Cmb01 NO-LOCK NO-ERROR.
        IF NOT AVAIL Gn-Tcmb 
        THEN FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc
                USE-INDEX Cmb01 NO-LOCK NO-ERROR.
        IF AVAIL Gn-Tcmb 
            THEN ASSIGN
                    x-TpoCmbCmp = Gn-Tcmb.Compra
                    x-TpoCmbVta = Gn-Tcmb.Venta.
        ASSIGN
            detalle.sdoact = detalle.sdoact * x-TpoCmbVta.
    END.
END.

/* BUSCAMOS DOCUMENTOS DE ABONO */
FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia
    AND flgest = 'P',
    FIRST facdocum OF ccbcdocu NO-LOCK WHERE tpodoc = NO:  
    CREATE detalle.
    BUFFER-COPY ccbcdocu TO detalle.
    ASSIGN
        detalle.sdoact = detalle.sdoact * -1.
    /* TODO A SOLES */
    IF detalle.codmon = 2 THEN DO:
        /* buscamos el tipo de cambio a aplicar */
        FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc
            USE-INDEX Cmb01 NO-LOCK NO-ERROR.
        IF NOT AVAIL Gn-Tcmb 
        THEN FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc
                USE-INDEX Cmb01 NO-LOCK NO-ERROR.
        IF AVAIL Gn-Tcmb 
            THEN ASSIGN
                    x-TpoCmbCmp = Gn-Tcmb.Compra
                    x-TpoCmbVta = Gn-Tcmb.Venta.
        ASSIGN
            detalle.sdoact = detalle.sdoact * x-TpoCmbVta.
    END.
END.
/* acumulamos deuda por cliente */
FOR EACH detalle BREAK BY detalle.codcli:
    IF FIRST-OF(detalle.codcli) THEN x-DeudaSoles = 0.
    x-DeudaSoles = x-DeudaSoles + detalle.sdoact.
    IF LAST-OF(detalle.codcli) THEN DO:
        IF x-DeudaSoles > 0 THEN DO:
            DISPLAY
                detalle.codcli
                detalle.nomcli
                x-deudasoles
                WITH STREAM-IO NO-BOX WIDTH 320.
        END.
    END.
END.
