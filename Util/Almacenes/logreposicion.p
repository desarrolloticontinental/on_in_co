DEF VAR s-codcia AS INT INIT 001.
DEF VAR pCodFam AS CHAR INIT '001'.
DEF VAR pCodAlm AS CHAR INIT ''.
DEF VAR s-CodAlm AS CHAR INIT '05'.

DEF VAR pRowid AS ROWID.
DEF VAR pDiasUtiles AS INT.
DEF VAR pVentaDiaria AS DEC.
DEF VAR pDiasMinimo AS INT.
DEF VAR pReposicion AS DEC.
DEF VAR pComprometido AS DEC.

DEF VAR x-StockMinimo AS DEC NO-UNDO.
DEF VAR x-StockDisponible AS DEC NO-UNDO.
DEF VAR x-Item AS INT INIT 1 NO-UNDO.
DEF VAR x-CanReq AS DEC.
DEF VAR x-StkAct LIKE Almmmate.StkAct NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR x-Mensaje AS CHAR FORMAT 'x(30)'.

DEF BUFFER b-mate FOR almmmate.
    /* Buscamos los valores generales */
    FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almcfggn THEN DO:
        MESSAGE 'Debe configurar los parámetros generales' VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.
    ASSIGN
        pDiasMinimo = AlmCfgGn.DiasMinimo
        pDiasUtiles = AlmCfgGn.DiasUtiles.

DEFINE FRAME f-Salida
    almmmate.codmat
    almmmatg.desmat
    x-stkact
    pDiasMinimo 
    pVentaDiaria
    x-stockminimo
    pReposicion
    b-mate.codalm
    b-mate.stkact
    x-StockDisponible
    pComprometido
    x-item
    x-canreq
    almmmatg.canemp COLUMN-LABEL 'Empaque'
    x-mensaje
    WITH STREAM-IO NO-BOX WIDTH 320.


OUTPUT TO c:\tmp\LOG.txt.
FOR EACH almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia 
    AND Almmmate.codalm = s-codalm,
    FIRST almmmatg OF almmmate NO-LOCK WHERE Almmmatg.TpoArt <> 'D':
    /* FILTROS */
    IF pCodFam <> '' THEN DO:
        IF LOOKUP(Almmmatg.codfam, pCodFam) = 0 THEN NEXT.
    END.

    /* Venta Diaria */
    pRowid = ROWID(Almmmate).
    RUN gn/venta-diaria (pRowid, pDiasUtiles, pCodAlm, OUTPUT pVentaDiaria).

    /* Stock Minimo */
    x-StockMinimo = pDiasMinimo * pVentaDiaria.
    x-StkAct = Almmmate.StkAct.
    IF pCodAlm <> '' THEN DO:
        DO k = 1 TO NUM-ENTRIES(pCodAlm):
            IF ENTRY(k, pCodAlm) = s-codalm THEN NEXT.   /* NO del almacen activo */
            FIND B-MATE WHERE B-MATE.codcia = s-codcia
                AND B-MATE.codalm = ENTRY(k, pCodAlm)
                AND B-MATE.codmat = Almmmate.codmat
                NO-LOCK NO-ERROR.
            IF AVAILABLE B-MATE THEN x-StkAct = x-StkAct + B-MATE.StkAct.
        END.
    END.
    IF x-StkAct >= x-StockMinimo THEN DO:
        DISPLAY
            almmmate.codmat
            almmmatg.desmat
            x-stkact
            pDiasMinimo 
            pVentaDiaria
            x-stockminimo
            'error stock minimo' @ x-Mensaje
            WITH STREAM-IO NO-BOX WIDTH 320 FRAME f-Salida.
        NEXT.
    END.

    /* Cantidad de Reposicion */
    RUN gn/cantidad-de-reposicion (pRowid, pVentaDiaria, OUTPUT pReposicion).
    IF pReposicion <= 0 THEN DO:
        DISPLAY
            almmmate.codmat
            almmmatg.desmat
            x-stkact
            x-stockminimo
            pReposicion
            'error pReposicion <= 0' @ x-Mensaje
            WITH STREAM-IO NO-BOX WIDTH 320 FRAME f-Salida.
        NEXT.
    END.

    /* distribuimos el pedido entre los almacenes de despacho */
    FOR EACH Almrepos NO-LOCK WHERE  almrepos.CodCia = s-codcia
        AND almrepos.TipMat = Almmmatg.Chr__02      /* Propios o Terceros */
        AND almrepos.CodAlm = Almmmate.codalm 
        AND almrepos.AlmPed <> Almmmate.codalm
        AND pReposicion > 0 /* <<< OJO <<< */
        BY almrepos.Orden:
        FIND B-MATE WHERE B-MATE.codcia = s-codcia
            AND B-MATE.codmat = Almmmate.codmat
            AND B-MATE.codalm = Almrepos.almped
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-MATE THEN NEXT.
        RUN gn/stock-comprometido (Almmmate.codmat, Almrepos.almped, OUTPUT pComprometido).
        x-StockDisponible = B-MATE.StkAct - x-StockMinimo - pComprometido.
        IF x-StockDisponible <= 0 THEN DO:
            DISPLAY
                almmmate.codmat
                almmmatg.desmat
                x-stkact
                pDiasMinimo 
                pVentaDiaria
                x-stockminimo
                pReposicion
                pComprometido
                b-mate.codalm
                b-mate.stkact
                x-StockDisponible
                'error stock disponible <= 0' @ X-Mensaje
                WITH STREAM-IO NO-BOX WIDTH 320 FRAME f-salida.
            NEXT.
        END.
        /* Se solicitará la reposición de acuerdo al empaque del producto */
        x-CanReq = MINIMUM(x-StockDisponible, pReposicion).
        IF Almmmatg.CanEmp > 0 THEN DO:
            x-CanReq = TRUNCATE(x-CanReq / Almmmatg.CanEmp, 0) * Almmmatg.CanEmp.
        END.
        IF x-CanReq <= 0 THEN DO:
            DISPLAY
                almmmate.codmat
                almmmatg.desmat
                x-stkact
                pDiasMinimo 
                pVentaDiaria
                x-stockminimo
                pReposicion
                pComprometido
                b-mate.codalm
                b-mate.stkact
                x-StockDisponible
                x-CanReq
                almmmatg.canemp
                'error x-canreq <= 0' @ x-Mensaje
                WITH STREAM-IO NO-BOX WIDTH 320 FRAME f-Salida.
            NEXT.    /* Menos que la cantidad por empaque */
        END.
        DISPLAY
            almmmate.codmat
            almmmatg.desmat
            x-stkact
            pDiasMinimo 
            pVentaDiaria
            x-stockminimo
            pReposicion
            b-mate.codalm
            b-mate.stkact
            x-StockDisponible
            pComprometido
            x-item
            x-canreq
            almmmatg.canemp
            'OK' @ x-Mensaje
            WITH STREAM-IO NO-BOX WIDTH 320 FRAME f-Salida.
        ASSIGN
            x-Item = x-Item + 1
            pReposicion = pReposicion - x-CanReq.
    END.
END.
OUTPUT CLOSE.
