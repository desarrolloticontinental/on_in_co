TRIGGER PROCEDURE FOR DELETE OF almssfami.

FIND FIRST Almmmatg WHERE Almmmatg.codcia = almssfami.codcia
    AND Almmmatg.codfam = almssfami.codfam
    AND Almmmatg.subfam = almssfami.subfam
    AND Almmmatg.codssfam = almssfami.codssfam
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE Almmmatg THEN DO:
    MESSAGE 'C�digo de sub-sub-familia est� siendo usado en el art�culo:' Almmmatg.codmat
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.


