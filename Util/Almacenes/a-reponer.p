DEF VAR pComprometido AS DEC NO-UNDO.
DEF VAR pTransito     AS DEC NO-UNDO.
DEF VAR pReposicion AS DEC NO-UNDO.
DEF VAR pAReponer   AS DEC NO-UNDO.
DEF VAR x-StkAct AS DEC NO-UNDO.
DEF VAR x-StockDisponible AS DEC NO-UNDO.

DEF BUFFER B-MATE FOR almmmate.

OUTPUT TO d:\tmp\analisis.txt.
PUT UNFORMATTED
    'CODIGO|DESCRIPCION|UNIDAD|STOCK 65|POR LLEGAR|COMPROMETIDO|SALDO|MAXIMO|EMPAQUE|A REPONER|STOCK 65S|COMPROMETIDO|SALDO'
    /*'CODIGO|DESCRIPCION|UNIDAD|STOCK 65|POR LLEGAR|COMPROMETIDO|SALDO|MAXIMO|EMPAQUE|A REPONER|STOCK 65S|MAXIMO|COMPROMETIDO|SALDO'*/
    SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 001
    AND tpoart <> 'D',
    FIRST almmmate OF almmmatg NO-LOCK WHERE almmmate.codalm = '65':
/*     AND Almmmate.StkMin > 0  */
/*     AND Almmmate.StkMax > 0: */
    x-StkAct = Almmmate.StkAct.

    /* INCREMENTAMOS LOS PEDIDOS POR REPOSICION EN TRANSITO */
    RUN alm/pedidoreposicionentransito (Almmmate.CodMat, Almmmate.CodAlm, OUTPUT pTransito).
    x-StkAct = x-StkAct + pTransito.
    /* DESCONTAMOS LO COMPROMETIDO */
    RUN vta2/Stock-Comprometido (Almmmate.CodMat, Almmmate.CodAlm, OUTPUT pComprometido).
    x-StkAct = x-StkAct - pComprometido.

    /* Se va a reponer en cantidades múltiplo del valor Almmmate.StkMax */
    pAReponer = Almmmate.StkMin - x-StkAct.
    pReposicion = ROUND(pAReponer / Almmmate.StkMax, 0) * Almmmate.StkMax.
    /*IF pReposicion <= 0 THEN NEXT.*/
    pAReponer = pReposicion.    /* OJO */

    PUT UNFORMATTED
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.undstk '|'
        almmmate.stkact '|'
        ptransito '|'
        pcomprometido '|'
        x-stkact '|'
        almmmate.stkmin '|'
        Almmmate.StkMax '|'
        preposicion '|'.

    FIND B-MATE WHERE B-MATE.codcia = 001
        AND B-MATE.codalm = '65s'
        AND B-MATE.codmat = Almmmate.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-MATE THEN DO:
        RUN vta2/Stock-Comprometido (Almmmate.CodMat, B-MATE.CodAlm, OUTPUT pComprometido).
        x-StockDisponible = B-MATE.StkAct /*- B-MATE.StkMin*/ - pComprometido.
        PUT UNFORMATTED
            B-MATE.StkAct '|'
            /*B-MATE.StkMin '|'*/
            pComprometido '|'
            x-StockDisponible
            SKIP.
    END.
    ELSE PUT SKIP.

END.
OUTPUT CLOSE.

