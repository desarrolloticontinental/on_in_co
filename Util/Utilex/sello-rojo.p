DEF VAR X AS CHAR.

INPUT FROM c:\tmp\sellorojo.prn.
REPEAT :
    IMPORT UNFORMATTED X.
    IF X = '' THEN LEAVE.
    CREATE vtatabla.
    ASSIGN
        codcia = 1
        tabla = "UTILEX-ROJO"
        llave_c1 = X.
END.
INPUT CLOSE.

