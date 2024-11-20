DEF TEMP-TABLE detalle LIKE gn-clie.
DEF VAR pOptions AS CHAR.
DEF VAR pArchivo AS CHAR INIT "c:\tmp\clientes.slk".

FOR EACH gn-clie NO-LOCK WHERE codcia = 000:
    CREATE detalle.
    BUFFER-COPY gn-clie TO detalle.
END.

ASSIGN
    pOptions = "FileType:SLK" + CHR(1) + ~
            "ExcelAlert:false" + CHR(1) + ~
            "ExcelVisible:false" + CHR(1) + ~
            "Labels:yes".

RUN lib/tt-file (TEMP-TABLE Detalle:HANDLE, pArchivo, pOptions).

