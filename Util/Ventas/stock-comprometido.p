DEF VAR pCodMat AS CHAR INIT '039580'.
DEF VAR pCodAlm AS CHAR INIT '45'.
DEF VAR pComprometido AS DEC.

/* CALCULO DEL STOCK COMPROMETIDO */
DEF VAR s-codcia AS INT INIT 001.

FIND FacCfgGn WHERE faccfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccfggn THEN RETURN.

/* Stock comprometido por PEDIDOS ACTIVOS MOSTRADOR */
DEF VAR TimeOut AS INTEGER NO-UNDO.
DEF VAR TimeNow AS INTEGER NO-UNDO.

/* Tiempo por defecto fuera de campaña */
TimeOut = (FacCfgGn.Dias-Res * 24 * 3600) +
          (FacCfgGn.Hora-Res * 3600) + 
          (FacCfgGn.Minu-Res * 60).
/* Tiempo dentro de campaña */
FIND FIRST FacCfgVta WHERE Faccfgvta.codcia = s-codcia
    AND Faccfgvta.coddoc = 'P/M'
    AND TODAY >= Faccfgvta.fechad
    AND TODAY <= Faccfgvta.fechah
    NO-LOCK NO-ERROR.
IF AVAILABLE FacCfgVta 
THEN TimeOut = (FacCfgVta.Dias-Res * 24 * 3600) +
                (FacCfgVta.Hora-Res * 3600) + 
                (FacCfgVta.Minu-Res * 60).
IF TimeOut > 0 THEN DO:
    FOR EACH Faccpedm NO-LOCK WHERE Faccpedm.codcia = s-codcia
        AND Faccpedm.coddoc = "P/M"
        AND Faccpedm.flgest = "P":
        TimeNow = (TODAY - FacCPedm.FchPed) * 24 * 3600.
        TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(FacCPedm.Hora, 1, 2)) * 3600) +
                  (INTEGER(SUBSTRING(FacCPedm.Hora, 4, 2)) * 60) ).
        IF TimeNow > TimeOut THEN NEXT.
        FOR EACH Facdpedm OF Faccpedm NO-LOCK WHERE Facdpedm.almdes = pCodAlm
            AND Facdpedm.codmat = pCodMat:
            pComprometido = pComprometido + FacDPedm.Factor * FacDPedm.CanPed.
            DISPLAY
                'MOstrador'
                faccpedm.coddiv
                faccpedm.nroped
                faccpedm.fchped
                facdpedm.canped * facdpedm.factor
                WITH STREAM-IO NO-BOX.
        END.
    END.
END.

    /**********   Barremos para los PEDIDOS MOSTRADOR  UTILEX ***********************/ 
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.CodCia = s-codcia 
        AND Facdpedi.coddoc = 'P/M'
        AND Facdpedi.AlmDes = pCodAlm
        AND Facdpedi.codmat = pcodmat 
        AND Facdpedi.FlgEst = "P" :
        FIND FIRST Faccpedi OF Facdpedi WHERE Faccpedi.FlgEst = "P" NO-LOCK NO-ERROR. 
        IF NOT AVAIL Faccpedi THEN NEXT.

        TimeNow = (TODAY - FacCPedi.FchPed) * 24 * 3600.
        TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(FacCPedi.Hora, 1, 2)) * 3600) +
                  (INTEGER(SUBSTRING(FacCPedi.Hora, 4, 2)) * 60) ).
        IF TimeOut > 0 THEN DO:
            IF TimeNow <= TimeOut   /* Dentro de la valides */
            THEN DO:
                /* cantidad en reservacion */
                pComprometido = pComprometido + FacDPedi.Factor * FacDPedi.CanPed.
            END.
        END.
    END.

/**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
    AND Facdpedi.almdes = pCodAlm
    AND Facdpedi.codmat = pCodMat
    AND Facdpedi.coddoc = 'PED'
    AND Facdpedi.flgest = 'P':
    /* RHC 12.12.2011 agregams los nuevos estados */
    FIND FIRST Faccpedi OF Facdpedi WHERE LOOKUP(Faccpedi.FlgEst, "X,P,W,WX,WL") > 0 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccpedi THEN NEXT.
    pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    DISPLAY
        'CRedito'
        faccpedi.flgest
        faccpedi.usuario
        faccpedi.coddiv
        faccpedi.nroped
        faccpedi.fchped
        facdpedi.canped * facdpedi.factor
        WITH STREAM-IO NO-BOX.
END.

FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
        AND Facdpedi.almdes = pCodAlm
        AND Facdpedi.codmat = pCodMat
        AND Facdpedi.coddoc = 'O/D'
        AND (Facdpedi.canped - Facdpedi.canate) > 0
        AND Facdpedi.flgest = 'P',
    FIRST Faccpedi OF Facdpedi NO-LOCK WHERE Faccpedi.flgest = 'P':
    pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    DISPLAY
        'Orden de Despacho'
        faccpedi.coddiv
        faccpedi.nroped
        faccpedi.fchped
        facdpedi.canped * facdpedi.factor
        faccpedi.usuario
        WITH STREAM-IO NO-BOX.
END.

/* Stock Comprometido por Pedidos por Reposicion Automatica */
FOR EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = s-codcia
    AND Almcrepo.TipMov = 'A'
    AND Almcrepo.AlmPed = pCodAlm
    AND Almcrepo.FlgEst = 'P'
    AND Almcrepo.FlgSit = 'A',
    EACH Almdrepo OF Almcrepo NO-LOCK WHERE Almdrepo.codmat = pCodMat
    AND almdrepo.CanApro > almdrepo.CanAten:
    pComprometido = pComprometido + (Almdrepo.CanApro - Almdrepo.CanAten).
    DISPLAY
        'Reposicion'
        almcrepo.nroser
        almcrepo.nrodoc
        almcrepo.codalm
        almcrepo.almped
        almcrepo.fchdoc
        almdrepo.canapro - almdrepo.canaten
        WITH STREAM-IO NO-BOX.
END.

MESSAGE pcomprometido.
