DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-CToTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.

FOR EACH vtalistamay WHERE codcia = 1
    AND LOOKUP(coddiv, '00018,10018') > 0:
    DELETE vtalistamay.
END.
INPUT FROM c:\tmp\todo.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = SUBSTRING(x-linea,1,6)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    RUN Graba-Datos ( '00018', 
                      DECIMAL(SUBSTRING(x-linea,7,14)) ).
    RUN Graba-Datos ( '10018', 
                      DECIMAL(SUBSTRING(x-linea,21)) ).
END.


PROCEDURE Graba-datos:
    DEF INPUT PARAMETER pCodDiv AS CHAR.
    DEF INPUT PARAMETER pPreUni AS DEC.

    CREATE Vtalistamay.
    ASSIGN
        VtaListaMay.CodCia = s-codcia
        VtaListaMay.CodDiv = pCodDiv
        VtaListaMay.codmat = SUBSTRING(x-linea,1,6)
        VtaListaMay.codfam = Almmmatg.codfam
        VtaListaMay.subfam = Almmmatg.subfam
        VtaListaMay.DesMat = Almmmatg.desmat
        VtaListaMay.DesMar = Almmmatg.desmar
        VtaListaMay.Chr__01 = Almmmatg.CHR__01
        VtaListaMay.FchIng = TODAY
        VtaListaMay.FchAct  = TODAY
        VtaListaMay.MonVta = 1
        VtaListaMay.PreOfi = pPreUni
        VtaListaMay.TpoCmb = 1
        VtaListaMay.usuario = 'ADMIN'.
    IF Almmmatg.monvta = Vtalistamay.monvta THEN x-CtoTot = Almmmatg.CtoTot.
    ELSE IF Vtalistamay.monvta = 1 THEN x-CtoTot = Almmmatg.ctotot *  Almmmatg.tpocmb.
    ELSE x-CtoTot = Almmmatg.ctotot /  Almmmatg.tpocmb.
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = VtaListaMay.Chr__01
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almtconv THEN DO:
        F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
    END.
    ASSIGN
        VtaListaMay.Dec__01 = ( (VtaListaMay.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100. 
END.
