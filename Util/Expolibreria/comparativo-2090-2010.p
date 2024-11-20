
DEF TEMP-TABLE Resumen
    FIELD codcli LIKE faccpedi.codcli
    FIELD nomcli LIKE faccpedi.nomcli
    FIELD implin_09 AS DEC EXTENT 4
    FIELD implin_10 AS DEC EXTENT 4
    INDEX Llave01 IS PRIMARY codcli.

/* campaña 2009 */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND flgest <> 'a'
    AND coddiv = '00015'
    AND fchped >= 01/01/09
    AND fchped <= 07/31/09,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    FIND Resumen WHERE Resumen.codcli = Faccpedi.codcli NO-ERROR.
    IF NOT AVAILABLE Resumen THEN DO:
        CREATE Resumen.
        Resumen.codcli = Faccpedi.codcli.
        Resumen.nomcli = Faccpedi.nomcli.
    END.
    CASE Almmmatg.codfam:
    WHEN '010' THEN Resumen.implin_09[2] = Resumen.implin_09[2] + Facdpedi.implin.
    WHEN '011' THEN Resumen.implin_09[3] = Resumen.implin_09[3] + Facdpedi.implin.
    WHEN '012' THEN Resumen.implin_09[4] = Resumen.implin_09[4] + Facdpedi.implin.
    OTHERWISE Resumen.implin_09[1] = Resumen.implin_09[1] + Facdpedi.implin.
    END CASE.
END.

/* campaña 2010 */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND flgest <> 'a'
    AND coddiv = '00015'
    AND fchped >= 11/30/09
    AND fchped <= 07/31/10,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    FIND Resumen WHERE Resumen.codcli = Faccpedi.codcli NO-ERROR.
    IF NOT AVAILABLE Resumen THEN DO:
        CREATE Resumen.
        Resumen.codcli = Faccpedi.codcli.
        Resumen.nomcli = Faccpedi.nomcli.
    END.
    CASE Almmmatg.codfam:
    WHEN '010' THEN Resumen.implin_10[2] = Resumen.implin_10[2] + Facdpedi.implin.
    WHEN '011' THEN Resumen.implin_10[3] = Resumen.implin_10[3] + Facdpedi.implin.
    WHEN '012' THEN Resumen.implin_10[4] = Resumen.implin_10[4] + Facdpedi.implin.
    OTHERWISE Resumen.implin_10[1] = Resumen.implin_10[1] + Facdpedi.implin.
    END CASE.
END.


OUTPUT TO c:\tmp\expo-2009-2010.txt.
FOR EACH Resumen:
    DISPLAY 
        Resumen.codcli 
        '|'
        Resumen.nomcli
        '|'
        Resumen.implin_09[1]
        '|'
        Resumen.implin_09[2]
        '|'
        Resumen.implin_09[3]
        '|'
        Resumen.implin_09[4]
        '|'
        Resumen.implin_10[1]
        '|'
        Resumen.implin_10[2]
        '|'
        Resumen.implin_10[3]
        '|'
        Resumen.implin_10[4]
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

