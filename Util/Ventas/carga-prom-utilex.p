/*DISABLE TRIGGERS FOR LOAD OF vtatabla.*/

FOR EACH vtatabla WHERE codcia = 001 AND tabla = 'DTOPROUTILEX':
    DELETE vtatabla.
END.

DEF VAR F-PRECIO AS DEC NO-UNDO.
DEF VAR F-FACTOR AS DEC INIT 1 NO-UNDO.
DEF VAR k AS INT.
FOR EACH vtalistamingn NO-LOCK WHERE codcia = 001, FIRST Almmmatg OF Vtalistamingn NO-LOCK:
    DISPLAY vtalistamingn.codmat. PAUSE 0.
    DO k = 1 TO 10:
        IF VtaListaMinGn.PromDivi[k] <> '' THEN DO:
            FIND gn-divi WHERE gn-divi.codcia = 001
                AND gn-divi.coddiv = VtaListaMinGn.PromDivi[k]
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-div AND VtaListaMinGn.PromDto[1] <> 0 THEN DO:
                FIND vtatabla WHERE vtatabla.codcia = 001
                    AND vtatabla.tabla = 'DTOPROUTILEX'
                    AND vtatabla.llave_c1 = VtaListaMinGn.codmat
                    AND vtatabla.llave_c2 = VtaListaMinGn.PromDivi[k]
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE vtatabla THEN DO:
                    F-PRECIO = VtaListaMinGn.PreOfi.
                    IF Almmmatg.MonVta = 2 THEN F-PRECIO = VtaListaMinGn.PreOfi * Almmmatg.TpoCmb.

                    CREATE vtatabla.
                    ASSIGN
                        vtatabla.codcia = 001
                        vtatabla.tabla = 'DTOPROUTILEX'
                        vtatabla.llave_c1 = VtaListaMinGn.codmat
                        vtatabla.llave_c2 = VtaListaMinGn.PromDivi[k]
                        VtaTabla.Rango_fecha[1] = VtaListaMinGn.PromFchD[k]
                        VtaTabla.Rango_fecha[2] = VtaListaMinGn.PromFchH[k] 
                        VtaTabla.Valor[1] = VtaListaMinGn.PromDto[k]
                        VtaTabla.Valor[2] = ROUND( (F-PRECIO * F-FACTOR) * ( 1 - ( VtaListaMinGn.PromDto[k] / 100 ) ),4).
                END.
            END.
        END.
    END.
END.
