DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF VAR x-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR x-nroped AS CHAR INIT '001105983' NO-UNDO.
DEF VAR x-codmat AS CHAR INIT '092640,092639' NO-UNDO.
DEF VAR pMensaje AS CHAR NO-UNDO.

FIND Faccpedi WHERE codcia = s-codcia AND
    coddoc = x-coddoc AND
    nroped = x-nroped
    EXCLUSIVE-LOCK.

FOR EACH Facdpedi OF Faccpedi EXCLUSIVE-LOCK WHERE LOOKUP(Facdpedi.codmat, x-codmat) > 0:
    DELETE Facdpedi.
END.
{vtagn/totales-cotizacion-sunat.i &Cabecera="FacCPedi" &Detalle="FacDPedi"}
/* ****************************************************************************************** */
/* Importes SUNAT */
/* ****************************************************************************************** */
DEF VAR hProc AS HANDLE NO-UNDO.
RUN sunat/sunat-calculo-importes PERSISTENT SET hProc.
RUN tabla-faccpedi IN hProc (INPUT Faccpedi.CodDiv,
                             INPUT Faccpedi.CodDoc,
                             INPUT Faccpedi.NroPed,
                             OUTPUT pMensaje).
IF RETURN-VALUE = "ADM-ERROR" THEN UNDO.
DELETE PROCEDURE hProc.
IF pMensaje > '' THEN MESSAGE pMensaje.
IF NOT CAN-FIND(FIRST Facdpedi OF Faccpedi NO-LOCK) THEN ASSIGN Faccpedi.FlgEst = "A".


