DEF VAR x-linea AS CHAR FORMAT 'x(9)'.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00000'.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS'.
DEFINE BUFFER b-CDocu FOR CcbCDocu.

FIND FacCfgGn WHERE FacCfgGn.CodCia = s-CodCia NO-LOCK NO-ERROR.

INPUT FROM c:\tmp\gr-anular.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    x-linea = SUBSTRING(x-linea,1,3) + SUBSTRING(x-linea,5,6).
    DISPLAY x-linea.
    PAUSE 0.
    FIND ccbcdocu WHERE codcia = 1
        AND coddoc = 'g/r'
        AND nrodoc = x-linea
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcdocu THEN RUN borra-cabecera.
END.
INPUT CLOSE.

PROCEDURE borra-cabecera:

    /* CONSISTENCIA DE TRACKING */
    FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
        AND Faccpedi.coddoc = Ccbcdocu.codped
        AND Faccpedi.nroped = Ccbcdocu.nroped
        NO-LOCK.
    FIND Vtatrack03 WHERE VtaTrack03.CodCia = s-codcia
        AND VtaTrack03.CodDiv = Faccpedi.coddiv
        AND VtaTrack03.CodDoc = Faccpedi.coddoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Vtatrack03 THEN DO:
        FIND Vtatrack01 OF Vtatrack03 NO-LOCK NO-ERROR.     /* Definicion del tracking */
    END.
    
    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
        FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
            AND Faccpedi.coddoc = Ccbcdocu.codped   /* PED */
            AND Faccpedi.nroped = Ccbcdocu.nroped
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN DO:
            MESSAGE 'NO se encontro el' Ccbcdocu.codped Ccbcdocu.nroped
                VIEW-AS ALERT-BOX ERROR.
            UNDO, RETURN 'ADM-ERROR'.
        END.
        /* ANULAMOS TRACKING */
        IF Ccbcdocu.FlgEst = 'P' THEN DO:   /* Ya pasó por control de barras */
            /* CONTROL DE BARRAS */
            RUN gn/pTracking-01 (s-CodCia,
                              s-CodDiv,
                              Faccpedi.CodDoc,
                              Faccpedi.NroPed,
                              s-User-Id,
                              'VODB',
                              'A',
                              DATETIME(TODAY, MTIME),
                              DATETIME(TODAY, MTIME),
                              Ccbcdocu.coddoc,
                              Ccbcdocu.nrodoc,
                              Ccbcdocu.coddoc,
                              Ccbcdocu.nrodoc).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        END.
        /* EMISION DE GUIA */
        RUN gn/pTracking-01 (s-CodCia,
                          s-CodDiv,
                          Faccpedi.CodDoc,
                          Faccpedi.NroPed,
                          s-User-Id,
                          'EGUI',
                          'A',
                          DATETIME(TODAY, MTIME),
                          DATETIME(TODAY, MTIME),
                          Ccbcdocu.coddoc,
                          Ccbcdocu.nrodoc,
                          Ccbcdocu.Libre_C01,
                          Ccbcdocu.Libre_C02).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        /* Extornamos Orden de Despacho */
        RUN proc_ActualizaO_D.
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        /* Borra Detalle G/R */
        FOR EACH CcbDDocu WHERE CcbDDocu.CodCia = CcbCDocu.CodCia 
            AND CcbDDocu.CodDoc = CcbCDocu.CodDoc 
            AND CcbDDocu.Nrodoc = CcbCDocu.NroDoc:
            DELETE CcbDDocu.
        END.
        /* EXTORNA STOCK DEL ALMACEN */
        IF AVAILABLE Vtatrack01 THEN DO:
            IF VtaTrack01.Libre_c01 = 'X' THEN DO:
                RUN vta\des_alm (ROWID(CcbCDocu)).
                IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
            END.
        END.
        ELSE DO:
            RUN vta\des_alm (ROWID(CcbCDocu)).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        END.

        FIND b-CDocu WHERE ROWID(b-CDocu) = ROWID(CcbCDocu) EXCLUSIVE-LOCK NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
        ASSIGN 
            b-CDocu.FlgEst = "A"
            b-CDocu.SdoAct = 0
            b-CDocu.Glosa  = "** A N U L A D O **"
            b-CDocu.FchAnu = TODAY
            b-CDocu.Usuanu = s-User-Id.
        RELEASE b-CDocu.
    END.

END PROCEDURE.

PROCEDURE proc_ActualizaO_D:

    DO ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
        /* Cabecera de O/D */
        FIND FacCPedi WHERE
            FacCPedi.CodCia = CcbCDocu.CodCia AND
            FacCPedi.CodDoc = CcbCDocu.Libre_c01 AND
            FacCPedi.NroPed = CcbCDocu.Libre_c02 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN RETURN 'ADM-ERROR'.
        FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
            /* Detalle de la O/D */
            FIND FacDPedi OF FacCPedi WHERE FacDPedi.CodMat = CcbDDocu.CodMat
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Facdpedi THEN UNDO, RETURN 'ADM-ERROR'.
            ASSIGN
                FacDPedi.CanAte = FacDPedi.CanAte - Ccbddocu.candes.
            FacDPedi.FlgEst = IF (FacDPedi.CanPed - FacDPedi.CanAte) <= 0 THEN "C" ELSE "P".
            RELEASE FacDPedi.
        END.
        /* Cabecera de Pedido */
        ASSIGN FacCPedi.FlgEst = "P".
        RELEASE FacCPedi.
    END.

END PROCEDURE.
