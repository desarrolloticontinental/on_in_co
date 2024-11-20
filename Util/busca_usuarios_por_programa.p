
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE cPrograma AS CHARACTER   NO-UNDO.

cPrograma = "w-provee".

OUTPUT TO d:\tmp\accesos.txt.
PUT UNFORMATTED
    "MODULO|"
    "OPCIÓN|"
    "GRUPOS|"
    "USUARIO|"
    "NOMBRE|"
    SKIP.
FOR EACH pf-g002 NO-LOCK WHERE
    programa BEGINS "ADM" AND
    INDEX(programa,cPrograma) > 0:
    IF PF-G002.Seguridad-Grupos = "" THEN DO:
        PUT UNFORMATTED
            PF-G002.Aplic-Id "|"
            PF-G002.Etiqueta "|"
            "TODOS|" SKIP.
    END.
    ELSE DO i = 1 TO NUM-ENTRIES(PF-G002.Seguridad-Grupos):
        FOR EACH PF-G004 WHERE
            PF-G004.Aplic-Id = PF-G002.Aplic-Id AND
            LOOKUP(PF-G004.Seguridad,ENTRY(i,PF-G002.Seguridad-Grupos)) > 0 NO-LOCK,
            FIRST DICTDB._user WHERE
                DICTDB._user._userid = PF-G004.USER-ID NO-LOCK.
            PUT UNFORMATTED
                PF-G002.Aplic-Id "|"
                PF-G002.Etiqueta "|"
                PF-G002.Seguridad-Grupos "|"
                PF-G004.USER-ID "|"
                DICTDB._user._user-name "|"
                SKIP.
        END.
    END.
END.
OUTPUT CLOSE.

/* Accesos al módulo */
OUTPUT TO d:\tmp\accesos2.txt.
PUT UNFORMATTED
    "MODULO|"
    "USUARIO|"
    "NOMBRE|"
    SKIP.
FOR EACH PF-G004 WHERE
    PF-G004.Aplic-Id = "PLN" NO-LOCK,
    FIRST DICTDB._user WHERE
        DICTDB._user._userid = PF-G004.USER-ID NO-LOCK.
    PUT UNFORMATTED
        PF-G004.Aplic-Id "|"
        PF-G004.USER-ID "|"
        DICTDB._user._user-name "|"
        SKIP.
END.
OUTPUT CLOSE.
