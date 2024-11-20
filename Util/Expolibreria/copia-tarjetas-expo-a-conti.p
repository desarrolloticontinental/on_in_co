DEF TEMP-TABLE t-card LIKE gn-card.

INPUT FROM c:\tmp\expo\gn-card.d.
REPEAT :
    CREATE t-card.
    IMPORT t-card.
END.
INPUT CLOSE.

MESSAGE '1ra parte' VIEW-AS ALERT-BOX.
FOR EACH t-card WHERE t-card.nrocard <> "":
    FIND gn-card WHERE gn-card.nrocard = t-card.nrocard NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-card THEN DO:
        CREATE gn-card.
        BUFFER-COPY t-card TO gn-card.
    END.
END.


