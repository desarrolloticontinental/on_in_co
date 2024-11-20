DEF TEMP-TABLE detalle LIKE gn-concp.
DEF VAR pOptions AS CHAR.
DEF VAR pArchivo AS CHAR INIT "c:\tmp\condicionvgenta.slk".

FOR EACH gn-concp NO-LOCK:
    CREATE detalle.
    BUFFER-COPY gn-concp TO detalle.
END.

ASSIGN
    pOptions = "FileType:SLK" + CHR(1) + ~
            "ExcelAlert:false" + CHR(1) + ~
            "ExcelVisible:false" + CHR(1) + ~
            "Labels:yes".

RUN lib/tt-file (TEMP-TABLE Detalle:HANDLE, pArchivo, pOptions).

