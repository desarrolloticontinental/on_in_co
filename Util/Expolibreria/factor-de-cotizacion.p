/* campaña 2010 */
DEF TEMP-TABLE Resumen
    FIELD codcli LIKE faccpedi.codcli
    FIELD nomcli LIKE faccpedi.nomcli
    FIELD codmat LIKE facdpedi.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD desmar LIKE almmmatg.desmar
    FIELD codfam LIKE almmmatg.codfam
    FIELD undvta LIKE facdpedi.undvta
    FIELD canped LIKE facdpedi.canped
    FIELD implin LIKE facdpedi.implin
    FIELD imptot LIKE faccpedi.imptot
    INDEX LLave01 AS PRIMARY codcli codmat.
DEF BUFFER b-cpedi FOR faccpedi.
DEF VAR x-ImpTot AS DEC NO-UNDO.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND flgest <> 'a'
    AND coddiv = '00015'
    AND fchped >= 11/30/09
    AND fchped <= 07/31/10,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK
    BREAK BY Faccpedi.codcli:
    IF FIRST-OF(Faccpedi.codcli) THEN DO:
        x-imptot = 0.
        FOR EACH b-cpedi NO-LOCK WHERE codcia = 1
            AND b-cpedi.coddoc = 'cot'
            AND b-cpedi.flgest <> 'a'
            AND b-cpedi.coddiv = '00015'
            AND b-cpedi.fchped >= 11/30/09
            AND b-cpedi.fchped <= 07/31/10
            AND b-cpedi.codcli = Faccpedi.codcli:
            x-imptot = x-imptot + b-cpedi.imptot.
        END.
    END.
    FIND Resumen WHERE Resumen.codcli = Faccpedi.codcli 
        AND Resumen.codmat = Facdpedi.codmat
        NO-ERROR.
    IF NOT AVAILABLE Resumen THEN DO:
        CREATE Resumen.
        ASSIGN
            Resumen.codcli = Faccpedi.codcli
            Resumen.nomcli = Faccpedi.nomcli
            Resumen.codmat = Facdpedi.codmat
            Resumen.desmat = Almmmatg.desmat
            Resumen.desmar = Almmmatg.desmar
            Resumen.codfam = Almmmatg.codfam
            Resumen.undvta = Facdpedi.undvta
            Resumen.imptot = x-imptot.
    END.
    ASSIGN
        Resumen.implin = Resumen.implin + Facdpedi.implin
        Resumen.canped = Resumen.canped + Facdpedi.canped.
END.

OUTPUT TO c:\tmp\ratios2010.txt.
FOR EACH Resumen NO-LOCK:
    DISPLAY
        Resumen.codcli 
        '|'
        Resumen.nomcli 
        '|'
        Resumen.codmat 
        '|'
        Resumen.desmat 
        '|'
        Resumen.desmar 
        '|'
        Resumen.codfam 
        '|'
        Resumen.undvta 
        '|'
        Resumen.canped 
        '|'
        Resumen.implin 
        '|'
        Resumen.imptot 
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

