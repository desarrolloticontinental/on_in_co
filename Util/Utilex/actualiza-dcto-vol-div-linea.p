/* solo para compilar
DEF VAR x-tabla AS CHAR.
x-tabla = "-db integral -ld utilex -H 192.168.13.210 -S 65010 -N TCP".
CONNECT VALUE(x-tabla) NO-ERROR.
*/

DEF VAR p-ip AS CHAR.
DEF VAR x-ip AS CHAR.

DEF VAR s-Tabla AS CHAR INIT 'UTILEX-ENCARTE'.
DEF VAR k AS INT.
ASSIGN
    p-ip = '192.168.13.210,192.168.5.210,192.168.10.210,192.168.16.210,~
    192.168.6.210'.
                                          
DO k = 1 TO NUM-ENTRIES(p-ip):
    x-ip = ENTRY(k, p-ip).
    IF CONNECTED("utilex") THEN DISCONNECT utilex.
    CONNECT -db integral -ld utilex -H VALUE(x-ip) -S 65010 -N TCP NO-ERROR.
    IF CONNECTED("utilex") THEN DO:
        DISABLE TRIGGERS FOR LOAD OF utilex.vtactabla.
        DISABLE TRIGGERS FOR LOAD OF utilex.vtadtabla.
        FOR EACH utilex.vtactabla WHERE utilex.vtactabla.codcia = 001
            AND utilex.vtactabla.tabla = s-Tabla:
            DELETE utilex.vtactabla.
        END.
        FOR EACH utilex.vtadtabla WHERE utilex.vtadtabla.codcia = 001
            AND utilex.vtadtabla.tabla = s-Tabla:
            DELETE utilex.vtadtabla.
        END.
        FOR EACH integral.vtactabla NO-LOCK WHERE integral.vtactabla.codcia = 001
            AND integral.vtactabla.tabla = s-Tabla:
            CREATE utilex.vtactabla.
            BUFFER-COPY integral.vtactabla TO utilex.vtactabla.
        END.
        FOR EACH integral.vtadtabla NO-LOCK WHERE integral.vtadtabla.codcia = 001
            AND integral.vtadtabla.tabla = s-Tabla:
            CREATE utilex.vtadtabla.
            BUFFER-COPY integral.vtadtabla TO utilex.vtadtabla.
        END.
        DISPLAY x-ip FORMAT 'x(20)' SKIP.
    END.
END.

