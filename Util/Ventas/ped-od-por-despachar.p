DEF VAR pCodAlm AS CHAR INIT '40'.
DEF VAR pCodDiv AS CHAR INIT '00000'.

DEF TEMP-TABLE t-dpedi LIKE facdpedi.

DEF SHARED VAR s-codcia AS INT INIT 001.

FIND FacCfgGn WHERE faccfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccfggn THEN RETURN.

/**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
FOR EACH ALmmmatg NO-LOCK WHERE codcia = s-codcia:
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = pCodAlm
            AND Facdpedi.codmat = Almmmatg.codmat
            AND Facdpedi.coddoc = 'PED'
            AND (Facdpedi.canped - Facdpedi.canate) > 0
            AND LOOKUP (Facdpedi.flgest, 'X,P') > 0,
            FIRST Faccpedi OF Facdpedi NO-LOCK WHERE LOOKUP(Faccpedi.FlgEst, "X,P") > 0
                AND Faccpedi.coddiv = pcoddiv:
        DISPLAY facdpedi.coddoc facdpedi.nroped facdpedi.codmat faccpedi.fchped
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        CREATE t-dpedi.
        BUFFER-COPY facdpedi TO t-dpedi.
    END.

    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = pCodAlm
            AND Facdpedi.codmat = Almmmatg.codmat
            AND Facdpedi.coddoc = 'O/D'
            AND (Facdpedi.canped - Facdpedi.canate) > 0
            AND  FacDPedi.FlgEst = 'P',
            FIRST Faccpedi OF Facdpedi NO-LOCK WHERE Faccpedi.flgest = 'P'
                AND Faccpedi.coddiv = pcoddiv:
        DISPLAY facdpedi.coddoc facdpedi.nroped facdpedi.codmat faccpedi.fchped
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        CREATE t-dpedi.
        BUFFER-COPY facdpedi TO t-dpedi.
    END.
END.

OUTPUT TO c:\tmp\pendientes.txt.

FOR EACH t-dpedi, 
        FIRST faccpedi OF t-dpedi NO-LOCK,
        FIRST almmmatg OF t-dpedi NO-LOCK,
        FIRST gn-ven OF faccpedi NO-LOCK:
    DISPLAY
        faccpedi.coddiv
        '|'
        faccpedi.fchped
        '|'
        faccpedi.coddoc
        '|'
        faccpedi.nroped
        '|'
        faccpedi.codven FORMAT 'x(3)'
        ' - '
        gn-ven.NomVen
        '|'
        faccpedi.codcli
        ' - '
        faccpedi.nomcli
        '|'
        t-dpedi.codmat
        ' - '
        almmmatg.desmat
        '|'
        almmmatg.desmar
        '|'
        almmmatg.codfam
        '|'
        t-dpedi.canped * t-dpedi.factor
        '|'
        t-dpedi.canate * t-dpedi.factor
        '|'
        almmmatg.undbas
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.

OUTPUT CLOSE.

