DEF TEMP-TABLE detalle LIKE vtalistamingn.
DEF VAR pOptions AS CHAR.
DEF VAR pArchivo AS CHAR INIT "c:\tmp\listaprecios.slk".

FOR EACH vtalistamingn NO-LOCK WHERE codcia = 001:
    CREATE detalle.
    BUFFER-COPY vtalistamingn TO detalle.
END.

ASSIGN
    pOptions = "FileType:SLK".

RUN lib/tt-file (TEMP-TABLE Detalle:HANDLE, pArchivo, pOptions).

