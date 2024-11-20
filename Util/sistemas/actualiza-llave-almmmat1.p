DEF VAR cLlave AS CHAR NO-UNDO.
DEF VAR iItem AS INTE NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF almmmat1.

FOR EACH almmmat1 EXCLUSIVE-LOCK WHERE codcia = 1:
    DISPLAY codmat. PAUSE 0.
    cLlave = ''.
    DO iItem = 1 TO 10:
        cLLave = cLlave + 
            (IF cLlave > '' THEN '|' ELSE '') +
            TRIM(Barras[iItem]).
    END.
    almmmat1.Llave = cLlave.
END.
