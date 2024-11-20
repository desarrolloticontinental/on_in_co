/* carga costo a comprobantes */

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00015'.
DEF VAR x-fchdoc AS DATE.
DEF VAR x-cto1 AS DEC.
DEF VAR x-cto2 AS DEC.
DEF VAR x-ImpCto AS DEC.

ASSIGN
    x-fchdoc = 12/01/2009.

FOR EACH ccbcdocu WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND fchdoc >= x-fchdoc
    AND flgest <> 'a'
    AND (coddoc = 'FAC' OR coddoc = 'BOL'):
    Ccbcdocu.ImpCto = 0.
    FOR EACH CcbDDocu OF CcbCDocu,
        FIRST Almmmatg OF Ccbddocu NO-LOCK:
        IF almmmatg.monvta = 1 THEN DO:
            x-cto1 = ROUND( Almmmatg.Ctotot * ccbddocu.CanDes * ccbddocu.Factor , 2 ).
            x-cto2 = ROUND(( Almmmatg.Ctotot * ccbddocu.CanDes * ccbddocu.Factor ) /  Almmmatg.Tpocmb , 2 ).
        END.
        IF almmmatg.monvta = 2 THEN DO:
            x-cto1 = ROUND( Almmmatg.Ctotot * ccbddocu.CanDes * ccbddocu.Factor * Almmmatg.TpoCmb, 2 ).
            x-cto2 = ROUND(( Almmmatg.Ctotot * ccbddocu.CanDes * ccbddocu.Factor ) , 2 ).
        END.
        Ccbddocu.ImpCto = IF ccbcdocu.CODMON = 1 THEN x-cto1 ELSE x-cto2.
        CcbCDocu.ImpCto = CcbCDocu.ImpCto + CcbDDocu.ImpCto.    
    END.
END.

FOR EACH ccbcdocu WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND fchdoc >= x-fchdoc
    AND flgest <> 'a'
    AND coddoc = 'N/C'
    AND cndcre <> 'N':
    FIND Almcmov WHERE Almcmov.CodCia = CcbCDocu.CodCia
        AND  Almcmov.CodAlm = CcbCDocu.CodAlm 
        AND  Almcmov.TipMov = "I"
        AND  Almcmov.CodMov = CcbCDocu.CodMov 
        AND  Almcmov.NroDoc = INTEGER(CcbCDocu.NroPed)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almcmov THEN NEXT.
    FOR EACH Almdmov NO-LOCK WHERE Almdmov.CodCia = Almcmov.CodCia 
                              AND  Almdmov.CodAlm = Almcmov.CodAlm 
                              AND  Almdmov.TipMov = Almcmov.TipMov 
                              AND  Almdmov.CodMov = Almcmov.CodMov 
                              AND  Almdmov.NroDoc = Almcmov.NroDoc:
       /*************Grabando Costos ********************/
       x-ImpCto = 0.
       FIND CcbDDocu WHERE CcbDDocu.codcia = s-codcia 
                      AND  CcbDDocu.CodDiv = S-CODDIV
                      AND  CcbDDocu.CodDoc = Almcmov.CodRef 
                      AND  CcbDDocu.NroDoc = Almcmov.NroRf1 
                      AND  CcbDDocu.CodMat = Almdmov.codMat
                      USE-INDEX LLAVE04 NO-LOCK NO-ERROR.
       IF AVAILABLE CcbDDocu THEN DO: 
          x-ImpCto = CcbDDocu.ImpCto * ( ( Almdmov.Candes * Almdmov.Factor ) / (CcbDDocu.Candes * CcbDDocu.Factor) ). 
       END.
       /****************************************************/
       FIND Ccbddocu OF Ccbcdocu WHERE Ccbddocu.codmat = Almdmov.codmat.
       Ccbddocu.ImpCto = x-ImpCto.
    END.
    Ccbcdocu.ImpCto = 0.
    FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
        CcbCDocu.ImpCto = CcbCDocu.ImpCto + CcbDDocu.ImpCto.    
    END.
END.



