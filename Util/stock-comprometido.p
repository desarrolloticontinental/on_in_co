DEF VAR X AS DEC.
DEF VAR pCodMat AS CHAR INIT '005206'.
DEF VAR pCodAlm AS CHAR INIT '11'.
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

FOR EACH Facdpedm WHERE Facdpedm.CodCia = s-codcia 
                   AND  Facdpedm.AlmDes = pCODALM
                   AND  Facdpedm.codmat = pcodmat 
                   AND  Facdpedm.FlgEst = "P" :
    FIND FIRST Faccpedm OF Facdpedm WHERE 
        /*Faccpedm.CodAlm = pcodalm AND */
        Faccpedm.FlgEst = "P"  
        NO-LOCK NO-ERROR. 
    IF NOT AVAIL Faccpedm THEN NEXT.
    
    TimeNow = (TODAY - FacCPedm.FchPed) * 24 * 3600.
    TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(FacCPedm.Hora, 1, 2)) * 3600) +
              (INTEGER(SUBSTRING(FacCPedm.Hora, 4, 2)) * 60) ).
    IF TimeOut > 0 THEN DO:
        IF TimeNow <= TimeOut   /* Dentro de la valides */
        THEN DO:
            /* cantidad en reservacion */
            DISPLAY facdpedm.coddoc facdpedm.nroped.
            pComprometido = pComprometido + FacDPedm.Factor * FacDPedm.CanPed.
        END.
    END.
END.


/**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
        AND Facdpedi.almdes = pCodAlm
        AND Facdpedi.codmat = pCodMat
        AND Facdpedi.coddoc = 'PED'
        AND (Facdpedi.canped - Facdpedi.canate) > 0
        AND LOOKUP (Facdpedi.flgest, 'X,P') > 0,
        FIRST Faccpedi OF Facdpedi NO-LOCK WHERE LOOKUP(Faccpedi.FlgEst, "X,P") > 0:
    DISPLAY facdpedi.coddoc facdpedi.nroped.
    pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
END.

FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
        AND Facdpedi.almdes = pCodAlm
        AND Facdpedi.codmat = pCodMat
        AND Facdpedi.coddoc = 'O/D'
        AND (Facdpedi.canped - Facdpedi.canate) > 0
        AND  FacDPedi.FlgEst = 'P',
        FIRST Faccpedi OF Facdpedi NO-LOCK WHERE Faccpedi.flgest = 'P':
    DISPLAY facdpedi.coddoc facdpedi.nroped.
    pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
END.

/* Stock Comprometido por Pedidos por Reposicion Automatica */
FOR EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = s-codcia
    AND Almcrepo.TipMov = 'A'
    AND Almcrepo.AlmPed = pCodAlm
    AND Almcrepo.FlgEst = 'P'
    AND Almcrepo.FlgSit = 'A',
    EACH Almdrepo OF Almcrepo NO-LOCK WHERE Almdrepo.codmat = pCodMat
    AND almdrepo.CanApro > almdrepo.CanAten:
    DISPLAY almcrepo.codalm almcrepo.NroSer almcrepo.NroDoc almcrepo.FchDoc almdrepo.canapro.
    pComprometido = pComprometido + (Almdrepo.CanApro - Almdrepo.CanAten).
END.

DISPLAY pcomprometido.
