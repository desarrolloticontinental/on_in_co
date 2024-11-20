DEF VAR x-linea AS CHAR FORMAT 'x(50)'.
DEF VAR x-CToTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.

DELETE FROM almmmatp.
INPUT FROM c:\tmp\marco.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        FIND almmmatg WHERE codcia = 1
            AND codmat = SUBSTRING(x-linea,1,6) NO-LOCK.
        CREATE almmmatp.
        BUFFER-COPY almmmatg 
            EXCEPT 
            Almmmatg.DtoVolD
            Almmmatg.DtoVolR
            Almmmatg.PromDivi
            Almmmatg.PromDto
            Almmmatg.PromFchD
            Almmmatg.PromFchH
            TO almmmatp
            ASSIGN
                almmmatp.monvta = 1
                almmmatp.CHR__01 = SUBSTRING(x-linea,11,10)
                almmmatp.preofi = DECIMAL(SUBSTRING(x-linea,21)).
        IF Almmmatg.monvta = Almmmatp.monvta 
            THEN x-CtoTot = Almmmatg.CtoTot.
        ELSE IF Almmmatp.monvta = 1 
            THEN x-CtoTot = Almmmatg.ctotot *  Almmmatg.tpocmb.
        ELSE x-CtoTot = Almmmatg.ctotot /  Almmmatg.tpocmb.

        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = Almmmatp.Chr__01
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.  
        ASSIGN
            Almmmatp.Dec__01 = ( (Almmmatp.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100. 
    END.
END.
INPUT CLOSE.
