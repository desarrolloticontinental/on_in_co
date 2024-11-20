DEF VAR pNroSku     AS INT NO-UNDO.
DEF VAR pPeso       AS DEC NO-UNDO.
DEF VAR pUbigeo AS CHAR.
DEF VAR pLongitud AS DEC NO-UNDO.
DEF VAR pLatitud  AS DEC NO-UNDO.
DEF VAR pFchEnt AS DATE.
DEF VAR pmensaje AS CHAR.

FIND faccpedi WHERE codcia = 1
    AND coddoc = 'o/d'
    AND nroped = '124067180' NO-LOCK.
pFchEnt = Faccpedi.fchped.

RUN Ubigeo-Final (OUTPUT pUbigeo,
                  OUTPUT pLongitud,
                  OUTPUT pLatitud).

ASSIGN 
    pNroSku = 0 
    pPeso = 0.
FOR EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
    pPeso = pPeso + (Facdpedi.CanPed * Facdpedi.Factor * Almmmatg.PesMat).
    pNroSku = pNroSku + 1.
END.
        
RUN logis/p-fecha-entrega-ubigeo.p (
            Faccpedi.CodAlm,              /* Almacén de despacho */
            Faccpedi.FchPed,
            '20:04:05',      /* Hora base */
            Faccpedi.CodCli,              /* Cliente */
            Faccpedi.coddiv,                      /* División solicitante */
            pUbigeo,                      /* Ubigeo: CR es cuando el cliente recoje  */
            Faccpedi.CodDoc,              /* Documento actual */
            Faccpedi.NroPed,
            pNroSKU,
            pPeso,
            INPUT-OUTPUT pFchEnt,
            OUTPUT pMensaje).
MESSAGE pfchent SKIP pmensaje.
