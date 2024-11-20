
DEFINE VARIABLE cTipo AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tt_prov NO-UNDO
    FIELDS tt_ruc LIKE gn-prov.ruc
    .
cTipo = "1".
REPEAT:
    UPDATE cTipo.
    
    iCount = 0.

    FOR EACH tt_prov:
        DELETE tt_prov.
    END.

    CASE cTipo:
        WHEN "1" THEN DO:
            INPUT FROM D:\tmp\agentes_rete.csv.
            REPEAT:
                CREATE tt_prov.
                IMPORT DELIMITER "," tt_ruc.
            END.
            INPUT CLOSE.
            FOR EACH tt_prov WHERE tt_ruc <> "":
                FIND FIRST gn-prov WHERE
                    ruc = tt_ruc NO-ERROR.
                IF AVAILABLE gn-prov THEN DO:
                    ASSIGN Libre_c01 = "Si".
                    iCount = iCount + 1.
                END.
            END.
            MESSAGE iCount "Agentes Retenedores Encontrados"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        WHEN "2" THEN DO:
            INPUT FROM D:\tmp\buen_contrib.csv.
            REPEAT:
                CREATE tt_prov.
                IMPORT DELIMITER "," tt_ruc.
            END.
            INPUT CLOSE.
            FOR EACH tt_prov WHERE tt_ruc <> "":
                FIND FIRST gn-prov WHERE
                    ruc = tt_ruc NO-ERROR.
                IF AVAILABLE gn-prov THEN DO:
                    ASSIGN Libre_c02 = "Si".
                    iCount = iCount + 1.
                END.
            END.
            MESSAGE iCount "Buenos Contribuyentes Encontrados"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        WHEN "3" THEN DO:
            INPUT FROM D:\tmp\agentes_perc.csv.
            REPEAT:
                CREATE tt_prov.
                IMPORT DELIMITER "," tt_ruc.
            END.
            INPUT CLOSE.
            FOR EACH tt_prov WHERE tt_ruc <> "":
                FIND FIRST gn-prov WHERE
                    ruc = tt_ruc NO-ERROR.
                IF AVAILABLE gn-prov THEN DO:
                    ASSIGN Libre_c03 = "Si".
                    iCount = iCount + 1.
                END.
            END.
            MESSAGE iCount "Agentes Precepción Encontrados"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END CASE.
END.

