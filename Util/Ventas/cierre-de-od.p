DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00000'.
DEF VAR s-coddoc AS CHAR INIT 'O/D'.
DEF VAR s-flgest AS CHAR INIT 'P'.
DEF BUFFER B-CPEDI FOR Faccpedi.
DEF BUFFER B-DPEDI FOR Facdpedi.

FOR EACH FacCPedi WHERE FacCPedi.CodCia = s-codcia
    AND FacCPedi.CodDiv = s-coddiv
    AND FacCPedi.CodDoc = s-coddoc
    AND FacCPedi.FlgEst = s-flgest
    AND faccpedi.fchped <= 01/31/2012:
    RUN cierre-cotizacion.
END.


PROCEDURE cierre-cotizacion:

DEF VAR x-NroCot LIKE Faccpedi.nroped NO-UNDO.

    ASSIGN
      FacCPedi.FlgEst = 'X'.
    /* BUSCAMOS EL PEDIDO */
    FIND B-CPEDI WHERE B-CPEDI.codcia = s-codcia
        AND B-CPEDI.coddoc = 'PED'
        AND B-CPEDI.nroped = Faccpedi.nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CPEDI THEN RETURN.
    x-NroCot = B-CPEDI.NroRef.
    FOR EACH Facdpedi OF Faccpedi:
        /* BORRAMOS SALDO EN LAS COTIZACIONES */
        FIND B-DPEDI WHERE B-DPEDI.CodCia = Faccpedi.CodCia 
            AND  B-DPEDI.CodDoc = "COT" 
            AND  B-DPEDI.NroPed = x-NroCot
            AND  B-DPEDI.CodMat = Facdpedi.CodMat 
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE B-DPEDI 
            THEN ASSIGN
                  B-DPEDI.FlgEst = 'P'
                  B-DPEDI.CanAte = B-DPEDI.CanAte - (Facdpedi.CanPed - Facdpedi.CanAte).  /* << OJO << */
        Facdpedi.FlgEst = Faccpedi.FlgEst.   /* <<< OJO <<< */
    END.    
    FIND B-CPedi WHERE B-CPedi.CodCia = S-CODCIA 
        AND B-CPedi.CodDiv = S-CODDIV 
        AND B-CPedi.CodDoc = "COT"    
        AND B-CPedi.NroPed = x-NroCot
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE B-CPedi THEN B-CPedi.FlgEst = "P".

RELEASE B-CPEDI.
RELEASE B-DPEDI.

END PROCEDURE.
