DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codmon AS INT INIT 1 NO-UNDO.
DEF VAR x-FactorFlete AS DEC NO-UNDO.
DEF VAR x-Factor      AS DEC NO-UNDO.
DEF VAR f-Factor      AS DEC NO-UNDO.
DEF VAR pCodDiv AS CHAR NO-UNDO.
DEF VAR f-FleteUnitario AS DEC DECIMALS 6 NO-UNDO.

OUTPUT TO d:\tmp\factor-flete.txt.
PUT UNFORMATTED
    'DIVISION|CODIGO|DESCRIPCION|LINEA|SUBLINEA|FACTOR FLETE|IMPORTE FLETE'
    SKIP.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
    AND LOOKUP(gn-divi.coddiv, '00065,00060') > 0:
    pCodDiv = gn-divi.coddiv.
    FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.tpoart <> "D"
        AND Almmmatg.preofi > 0
        AND Almmmatg.tpomrg <> "2":
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
            AND Almtconv.Codalter = Almmmatg.Chr__01
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN NEXT.
        /* VALORES POR DEFECTO */
        ASSIGN F-FACTOR = Almtconv.Equival.
        FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
            AND VtaTabla.Llave_c1 = pCodDiv
            AND VtaTabla.Llave_c2 = Almmmatg.codfam
            AND VtaTabla.Llave_c3 = Almmmatg.subfam
            AND VtaTabla.Tabla = "DIVFACXSLIN"
            NO-LOCK NO-ERROR.
        /* Se basa en el precio de la unidad de Oficina */
        x-FactorFlete = 0.
        IF AVAILABLE VtaTabla AND VtaTabla.Valor[1] > 0 THEN x-FactorFlete = VtaTabla.Valor[1] / 100.
        ELSE DO:
            FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
                AND VtaTabla.Llave_c1 = pCodDiv
                AND VtaTabla.Llave_c2 = Almmmatg.codfam
                AND VtaTabla.Tabla = "DIVFACXLIN"
                NO-LOCK NO-ERROR.
            IF AVAILABLE VtaTabla AND VtaTabla.Valor[1] > 0 THEN x-FactorFlete = VtaTabla.Valor[1] / 100.
        END.
        /* Factor de ajuste */
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
            AND  Almtconv.Codalter = Almmmatg.UndA
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN X-FACTOR = Almtconv.Equival.
        /* Flete ajustado a la unidad de venta */
        f-FleteUnitario = Almmmatg.PreVta[2] * x-FactorFlete / x-Factor * f-Factor.
        IF s-CodMon <> Almmmatg.MonVta 
            THEN IF s-CodMon = 1 
            THEN f-FleteUnitario = ROUND ( f-FleteUnitario * Almmmatg.TpoCmb, 6 ).
            ELSE f-FleteUnitario = ROUND ( f-FleteUnitario / Almmmatg.TpoCmb, 6 ).
        PUT UNFORMATTED
            pCodDiv '|'
            almmmatg.codmat '|'
            almmmatg.desmat '|'
            almmmatg.codfam '|'
            almmmatg.subfam '|'
            x-FactorFlete '|'
            f-FleteUnitario
            SKIP.
    END.
END.
OUTPUT CLOSE.


/*
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
    AND LOOKUP(gn-divi.coddiv, '00065,00060') > 0:
    pCodDiv = gn-divi.coddiv.
    FOR EACH VtaListaMay NO-LOCK WHERE VtaListaMay.codcia = s-codcia
        AND VtaListaMay.coddiv = pCodDiv,
        FIRST Almmmatg OF VtaListaMay NO-LOCK:
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
            AND Almtconv.Codalter = VtaListaMay.Chr__01
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN NEXT.
        /* VALORES POR DEFECTO */
        ASSIGN F-FACTOR = Almtconv.Equival.
        FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
            AND VtaTabla.Llave_c1 = pCodDiv
            AND VtaTabla.Llave_c2 = Almmmatg.codfam
            AND VtaTabla.Llave_c3 = Almmmatg.subfam
            AND VtaTabla.Tabla = "DIVFACXSLIN"
            NO-LOCK NO-ERROR.
        /* Se basa en el precio de la unidad de Oficina */
        x-FactorFlete = 0.
        IF AVAILABLE VtaTabla AND VtaTabla.Valor[1] > 0 THEN x-FactorFlete = VtaTabla.Valor[1] / 100.
        ELSE DO:
            FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
                AND VtaTabla.Llave_c1 = pCodDiv
                AND VtaTabla.Llave_c2 = Almmmatg.codfam
                AND VtaTabla.Tabla = "DIVFACXLIN"
                NO-LOCK NO-ERROR.
            IF AVAILABLE VtaTabla AND VtaTabla.Valor[1] > 0 THEN x-FactorFlete = VtaTabla.Valor[1] / 100.
        END.
        /* Factor de ajuste */
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
            AND  Almtconv.Codalter = Almmmatg.UndA
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN X-FACTOR = Almtconv.Equival.
        /* Flete ajustado a la unidad de venta */
        f-FleteUnitario = Almmmatg.PreVta[2] * x-FactorFlete / x-Factor * f-Factor.
        IF s-CodMon <> Almmmatg.MonVta 
            THEN IF s-CodMon = 1 
            THEN f-FleteUnitario = ROUND ( f-FleteUnitario * Almmmatg.TpoCmb, 6 ).
            ELSE f-FleteUnitario = ROUND ( f-FleteUnitario / Almmmatg.TpoCmb, 6 ).
        PUT UNFORMATTED
            pCodDiv '|'
            almmmatg.codmat '|'
            almmmatg.desmat '|'
            almmmatg.codfam '|'
            almmmatg.subfam '|'
            x-FactorFlete '|'
            f-FleteUnitario
            SKIP.
    END.
END.
*/



