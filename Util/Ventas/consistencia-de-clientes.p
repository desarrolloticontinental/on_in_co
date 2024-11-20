DEF VAR pResultado AS CHAR NO-UNDO.
DEF VAR x-Persona AS CHAR NO-UNDO.
DEF VAR x-Situacion AS CHAR NO-UNDO.

OUTPUT TO d:\tmp\clientes-por-revisar.txt.
PUT UNFORMATTED
    'CODIGO|NOMBRE|RUC|PERSONA|SITUACION|FECHA CREACION'
    SKIP.
RLOOP:
FOR EACH gn-clie NO-LOCK WHERE codcia = 000:
    CASE gn-clie.libre_c01:
        WHEN "J" THEN DO:
            IF LENGTH(gn-clie.ruc) = 11 AND gn-clie.ruc BEGINS '20' THEN DO:
                /* dígito verificador */
                RUN lib/_ValRuc (gn-clie.Ruc, OUTPUT pResultado).
                IF pResultado <> 'ERROR' THEN NEXT RLOOP.
            END.
        END.
        WHEN "E" THEN DO:
            IF LENGTH(gn-clie.ruc) = 11 AND 
                LOOKUP(SUBSTRING(gn-clie.ruc,1,2),'15,17') > 0 THEN DO:
                /* dígito verificador */
                RUN lib/_ValRuc (gn-clie.Ruc, OUTPUT pResultado).
                IF pResultado <> 'ERROR' THEN NEXT RLOOP.
            END.
        END.
        WHEN "N" THEN DO:
            IF TRUE <> (gn-clie.ruc > '') THEN NEXT RLOOP.
            IF LENGTH(gn-clie.ruc) = 11 AND gn-clie.ruc BEGINS '10' THEN DO:
                /* dígito verificador */
                RUN lib/_ValRuc (gn-clie.Ruc, OUTPUT pResultado).
                IF pResultado <> 'ERROR' THEN NEXT RLOOP.
            END.
        END.
    END CASE.
    ASSIGN x-Persona = 'NO DEFINIDO' x-Situacion = 'NO DEFINIDO'.
    CASE x-Persona:
        WHEN "J" THEN x-Persona = "JURIDICA".
        WHEN "N" THEN x-Persona = "NATURAL".
        WHEN "E" THEN x-Persona = "EXTRANJERA".
    END CASE.
    CASE x-Situacion:
        WHEN "A" THEN x-Situacion = "ACTIVO".
        WHEN "I" THEN x-Situacion = "BLOQUEADO".
        WHEN "C" THEN x-Situacion = "CESADO".
    END CASE.
    PUT UNFORMATTED
        gn-clie.codcli '|'
        gn-clie.nomcli '|'
        gn-clie.ruc '|'
        x-Persona '|'
        x-Situacion '|'
        gn-clie.fching
        SKIP.
END.
OUTPUT CLOSE.
