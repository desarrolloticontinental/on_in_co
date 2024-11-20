DEF VAR x-stk10 AS DEC.
DEF VAR x-stk10a AS DEC.
DEF VAR x-stk501 AS DEC.
DEF VAR x-stk502 AS DEC.
DEF VAR x-stk27 AS DEC.

OUTPUT TO c:\tmp\utilex.txt.
FOR EACH VtaListaMinGn NO-LOCK WHERE
    VtaListaMinGn.CodCia = 001,
    FIRST Almmmatg OF VtaListaMinGn NO-LOCK:
    ASSIGN
        x-stk10 = 0
        x-stk501 = 0
        x-stk27 = 0.
    FOR EACH almmmate NO-LOCK WHERE almmmate.codcia = 1
        AND almmmate.codmat = VtaListaMinGn.CodMat
        AND LOOKUP (almmmate.codalm, '10,10a,501,502,27') > 0:
        CASE almmmate.codalm:
            WHEN '10' OR WHEN '10a' THEN x-stk10 = x-stk10 + almmmate.stkact.
            WHEN '501' OR WHEN '502' THEN x-stk501 = x-stk501 + almmmate.stkact.
            WHEN '27' THEN x-stk27 = x-stk27 + almmmate.stkact.
        END CASE.

    END.
    DISPLAY
        VtaListaMinGn.codmat 
        Almmmatg.desmat
        '|'
        Almmmatg.desmar
        '|'
        Almmmatg.codfam
        '|'
        Almmmatg.undstk
        '|'
        VtaListaMinGn.MonVta 
        '|'
        VtaListaMinGn.TpoCmb
        '|'
        VtaListaMinGn.Chr__01 
        '|'
        VtaListaMinGn.PreOfi
        '|'
        x-stk10
        '|'
        x-stk501
        '|'
        x-stk27
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

