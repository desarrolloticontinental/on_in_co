DISABLE TRIGGERS FOR LOAD OF vtalistamay.

DEF TEMP-TABLE detalle LIKE vtalistamay
    INDEX Llave01 IS PRIMARY codcia.

INPUT FROM C:\tmp\ExpoOct2012\vtalistamay.d.
REPEAT :
    CREATE detalle.
    IMPORT detalle NO-ERROR.
END.
INPUT CLOSE.

FOR EACH detalle WHERE codcia = 0:
    DELETE detalle.
END.
FOR EACH detalle WHERE codcia = 1 AND coddiv = '00015':
    CREATE vtalistamay.
    BUFFER-COPY detalle TO vtalistamay.
END.
FOR EACH detalle WHERE detalle.codcia = 1 AND detalle.coddiv = '00015':
    CREATE vtalistamay.
    BUFFER-COPY detalle TO vtalistamay ASSIGN vtalistamay.coddiv = '10018'.
END.
