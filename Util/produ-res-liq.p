DEF VAR x-CtoMat AS DEC NO-UNDO.
DEF VAR x-CtoGas AS DEC NO-UNDO.
DEF VAR x-CtoHor AS DEC NO-UNDO.
DEF VAR x-CodMat LIKE Almmmatg.CodMat NO-UNDO.
DEF VAR x-DesMat LIKE Almmmatg.DesMat NO-UNDO.
DEF VAR x-CanFin AS DEC NO-UNDO.
DEF VAR x-NumItm AS INT NO-UNDO.

OUTPUT TO c:\tmp\conti-liq-06-07.txt.
FOR EACH PR-LIQC NO-LOCK WHERE PR-LIQC.Codcia = 001
        AND PR-LIQC.FchLiq >= 09/01/2006
        /*AND PR-LIQC.FchLiq <= 03/31/2007*/
        AND PR-LIQC.FlgEst <> 'A':
    ASSIGN
        x-CtoMat = PR-LIQC.CtoMat
        x-CtoGas = PR-LIQC.CtoGas
        x-CtoHor = PR-LIQC.CtoHor
        x-CanFin = 0
        x-NumItm = 1.
    FOR EACH PR-LIQCX OF PR-LIQC NO-LOCK,
            EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = PR-LIQCX.Codcia
                AND Almmmatg.codmat = PR-LIQCX.codart:
        x-CanFin = x-CanFin + PR-LIQCX.CanFin.
        IF x-NumItm = 1 
        THEN ASSIGN
                x-CodMat = Almmmatg.CodMat
                x-DesMat = Almmmatg.DesMat.
        x-NumItm = x-NumItm + 1.
    END.
    IF PR-LIQC.CodMon = 2
    THEN ASSIGN
            x-CtoMat = PR-LIQC.CtoMat * PR-LIQC.TpoCmb
            x-CtoGas = PR-LIQC.CtoGas * PR-LIQC.TpoCmb
            x-CtoHor = PR-LIQC.CtoHor * PR-LIQC.TpoCmb.
    DISPLAY
        PR-LIQC.FchLiq
        PR-LIQC.NumLiq
        PR-LIQC.NumOrd
        x-CanFin    COLUMN-LABEL 'Cantidad'
        x-CodMat
        x-DesMat
        x-CtoMat    COLUMN-LABEL 'Materiales'   FORMAT '->>>,>>>,>>9.99'
        x-CtoGas    COLUMN-LABEL 'Terceros'
        x-CtoHor    COLUMN-LABEL 'Mano de Obra'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

