/* Carga descuentos promocionales al nuevo formato */
DISABLE TRIGGERS FOR LOAD OF vtatabla.  /* quitar para migrar a las tiendas */
FOR EACH vtatabla WHERE codcia = 1
    AND tabla = 'DTOPROLIMA':
    DELETE vtatabla.
END.

DEF VAR k AS INT NO-UNDO.
DEF VAR F-PRECIO AS DEC NO-UNDO.

FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = 001:
    DISPLAY almmmatg.codmat.
    PAUSE 0.
    DO k = 1 TO 10:
        IF almmmatg.promdivi[k] <> ""
            AND almmmatg.promdto[k] <> 0 THEN DO:
            F-PRECIO = Almmmatg.Prevta[1].
            IF Almmmatg.MonVta = 2 THEN F-PRECIO = Almmmatg.Prevta[1] * Almmmatg.TpoCmb.
            CREATE vtatabla.
            ASSIGN
                vtatabla.codcia = 001
                vtatabla.tabla = 'DTOPROLIMA'
                vtatabla.llave_c1 = almmmatg.codmat
                vtatabla.llave_c2 = almmmatg.promdivi[k]
                VtaTabla.Rango_fecha[1] = almmmatg.promfchd[k]
                VtaTabla.Rango_fecha[2] = almmmatg.promfchh[k]
                VtaTabla.Valor[1] = almmmatg.promdto[k]
                VtaTabla.Valor[2] = ROUND(F-PRECIO * ( 1 - ( almmmatg.promdto[k] / 100 ) ),4).
        END.
    END.
END.

DEF VAR x-Divisiones AS CHAR INIT "00002,00003" NO-UNDO.
DEF BUFFER B-TABLA FOR vtatabla.
FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = 001,
    FIRST vtatabla NO-LOCK WHERE codcia = 001
    AND tabla = "DTOPROLIMA"
    AND llave_c1 = almmmatg.codmat
    AND llave_c2 = "00001":
    DISPLAY almmmatg.codmat vtatabla.llave_c2.
    PAUSE 0.
    DO k = 1 TO 2:
        FIND B-TABLA WHERE B-TABLA.codcia = vtatabla.codcia
            AND B-TABLA.tabla = vtatabla.tabla
            AND B-TABLA.llave_c1 = vtatabla.llave_c1
            AND B-TABLA.llave_c2 = ENTRY(k, x-Divisiones)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-TABLA THEN DO:
            CREATE B-TABLA.
            BUFFER-COPY vtatabla 
                TO B-TABLA
                ASSIGN B-TABLA.llave_c2 = ENTRY(k, x-Divisiones).
        END.
    END.
END.
x-Divisiones = "00061,00062".
FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = 001,
    FIRST vtatabla NO-LOCK WHERE codcia = 1
    AND tabla = "DTOPROLIMA"
    AND llave_c1 = almmmatg.codmat
    AND llave_c2 = "00060":
    DISPLAY almmmatg.codmat vtatabla.llave_c2.
    PAUSE 0.
    DO k = 1 TO 2:
        FIND B-TABLA WHERE B-TABLA.codcia = vtatabla.codcia
            AND B-TABLA.tabla = vtatabla.tabla
            AND B-TABLA.llave_c1 = vtatabla.llave_c1
            AND B-TABLA.llave_c2 = ENTRY(k, x-Divisiones)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-TABLA THEN DO:
            CREATE B-TABLA.
            BUFFER-COPY vtatabla 
                TO B-TABLA
                ASSIGN B-TABLA.llave_c2 = ENTRY(k, x-Divisiones).
        END.
    END.
END.


