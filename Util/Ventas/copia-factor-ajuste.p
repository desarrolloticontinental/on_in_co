/* Copia plantilla a otras divisiones arequipa */
DEF VAR f-Divisiones AS CHAR INIT '00061,00062,00063'.
DEF VAR x-coddiv AS CHAR.
DEF VAR j AS INT.

DEF BUFFER b-tabla FOR vtatabla.

DO j = 1 TO 3:
    x-coddiv = ENTRY(j, f-Divisiones).
    FOR EACH vtatabla WHERE codcia = 1 
        AND tabla = 'DIVFACXLIN'
        AND llave_c1 = x-coddiv:
        DELETE vtatabla.
    END.
    FOR EACH vtatabla NO-LOCK WHERE codcia = 1 
        AND tabla = 'DIVFACXLIN'
        AND llave_c1 = '00060':
        CREATE b-tabla.
        BUFFER-COPY vtatabla
            TO b-tabla
            ASSIGN b-tabla.llave_c1 = x-coddiv.
    END.
    FOR EACH vtatabla WHERE codcia = 1 
        AND tabla = 'DIVFACXSLIN'
        AND llave_c1 = x-coddiv:
        DELETE vtatabla.
    END.
    FOR EACH vtatabla NO-LOCK WHERE codcia = 1 
        AND tabla = 'DIVFACXSLIN'
        AND llave_c1 = '00060':
        CREATE b-tabla.
        BUFFER-COPY vtatabla
            TO b-tabla
            ASSIGN b-tabla.llave_c1 = x-coddiv.
    END.
END.
