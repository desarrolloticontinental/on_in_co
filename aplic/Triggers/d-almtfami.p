TRIGGER PROCEDURE FOR DELETE OF almtfami.


FIND FIRST Almmmatg WHERE Almmmatg.codcia = Almtfami.codcia
    AND Almmmatg.codfam = Almtfami.codfam
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE Almmmatg THEN DO:
    MESSAGE 'C�digo de familia est� siendo usado en el art�culo:' Almmmatg.codmat
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
/* Eliminamos Subfamilias y Subsubfamilias */
FOR EACH almsfami OF almtfami:
    FOR EACH almssfam OF almsfami:
        DELETE almssfam.
    END.
    DELETE almsfami.
END.
