TRIGGER PROCEDURE FOR DELETE OF almsfami.

FIND FIRST Almmmatg WHERE Almmmatg.codcia = Almsfami.codcia
    AND Almmmatg.codfam = Almsfami.codfam
    AND Almmmatg.subfam = Almsfami.subfam
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE Almmmatg THEN DO:
    MESSAGE 'C�digo de sub-familia est� siendo usado en el art�culo:' Almmmatg.codmat
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
/* Eliminamos Subfamilias y Subsubfamilias */
FOR EACH almssfam OF almsfami:
    DELETE almssfam.
END.

