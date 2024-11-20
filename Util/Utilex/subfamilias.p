DEF TEMP-TABLE detalle LIKE almsfami.
DEF VAR pOptions AS CHAR.
DEF VAR pArchivo AS CHAR INIT "c:\tmp\subfamilias.slk".

FOR EACH almsfami NO-LOCK WHERE codcia = 001:
    CREATE detalle.
    BUFFER-COPY almsfami TO detalle.
END.

ASSIGN
    pOptions = "FileType:SLK" + CHR(1) + ~
            "ExcelAlert:false" + CHR(1) + ~
            "ExcelVisible:false" + CHR(1) + ~
            "Labels:yes".

RUN lib/tt-file (TEMP-TABLE Detalle:HANDLE, pArchivo, pOptions).

