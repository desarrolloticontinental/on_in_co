/* Verifica y corrige tipo de cliente */
FOR EACH gn-clie WHERE codcia = 000 AND flgsit = "A" AND LENGTH(codcli) = 11
    AND LENGTH(ruc) = 11:
    CASE TRUE:
        WHEN LOOKUP(SUBSTRING(gn-clie.ruc,1,2),'20') > 0 THEN DO:
            /* Persona Jurídica */
            IF gn-clie.Libre_C01 <> "J" THEN DO:
                DISPLAY gn-clie.codcli gn-clie.ruc gn-clie.libre_c01 FORMAT 'x'
                    WITH STREAM-IO NO-BOX.
                PAUSE 0.
                gn-clie.Libre_c01 = "J".
            END.
        END.
        WHEN LOOKUP(SUBSTRING(gn-clie.ruc,1,2),'15,17') > 0 THEN DO:
            /* Persona Extranjera */
            IF gn-clie.Libre_C01 <> "E" THEN DO:
                DISPLAY gn-clie.codcli gn-clie.ruc gn-clie.libre_c01 FORMAT 'x'
                    WITH STREAM-IO NO-BOX.
                PAUSE 0.
                gn-clie.Libre_c01 = "E".
            END.
        END.
        WHEN LOOKUP(SUBSTRING(gn-clie.ruc,1,2),'10') > 0 THEN DO:
            /* Persona Natural */
            IF gn-clie.Libre_C01 <> "N" THEN DO:
                DISPLAY gn-clie.codcli gn-clie.ruc gn-clie.libre_c01 FORMAT 'x'
                    WITH STREAM-IO NO-BOX.
                PAUSE 0.
                gn-clie.Libre_c01 = "N".
            END.
        END.
    END CASE.
END.
