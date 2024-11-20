DEF VAR s-codcia   AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv   AS CHAR INIT '00065' NO-UNDO.
DEF VAR s-codalm   AS CHAR INIT '65' NO-UNDO.
DEF VAR f-prevta-a AS DEC NO-UNDO.
DEF VAR f-prevta-b AS DEC NO-UNDO.
DEF VAR f-prevta-c AS DEC NO-UNDO.
DEF VAR f-factor-a AS DEC NO-UNDO.
DEF VAR f-factor-b AS DEC NO-UNDO.
DEF VAR f-factor-c AS DEC NO-UNDO.
DEF VAR f-prevta-min AS DEC NO-UNDO.
DEF VAR f-factor-min AS DEC NO-UNDO.
DEF VAR f-prevta-min-dto AS DEC NO-UNDO.
DEF VAR y-Dsctos-Min AS DEC NO-UNDO.
DEF VAR f-fleteunitario AS DEC NO-UNDO.

OUTPUT TO c:\tmp\chiclayo.txt.
PUT UNFORMATTED
    'CODIGO|DESCRICPION|LINEA|SUBLINEA|MARCA|PRECIO A|FACTOR A|UND A|PRECIO B|FACTOR B|UND B|' +
    'PRECIO C|FACTOR C|UND C|PRECIO MINIMO|FACTOR MINIMO|UND MINIMO|MINIMO DESCONTADO|'  +
    '%DESCTO|FLETE UNITARIO|' +
    'EAN 13|EAN 14 |FACTOR|EAN 14|FACTOR|EAN 14|FACTOR'
    SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 001 AND almmmatg.tpoart <> 'D'
    /*AND almmmatg.codfam = '000' AND almmmatg.subfam = '100'*/,
    FIRST almtfami OF almmmatg NO-LOCK WHERE almtfami.swcomercial = YES:
    ASSIGN
        f-prevta-a = almmmatg.prevta[2]
        f-prevta-b = almmmatg.prevta[3]
        f-prevta-c = almmmatg.prevta[4]
        f-prevta-min = almmmatg.preofi
        f-factor-a = 1
        f-factor-b = 1
        f-factor-c = 1
        f-factor-min = 1.
    IF almmmatg.monvta = 2 THEN
        ASSIGN
        f-prevta-a = f-prevta-a * almmmatg.tpocmb
        f-prevta-b = f-prevta-b * almmmatg.tpocmb
        f-prevta-c = f-prevta-c * almmmatg.tpocmb
        f-prevta-min = f-prevta-min * almmmatg.tpocmb.
    IF almmmatg.unda <> '' THEN RUN Precio_unitario (almmmatg.undA, 1, OUTPUT f-PreVta-A, OUTPUT f-Factor-A).
    IF almmmatg.undb <> '' THEN RUN Precio_unitario (almmmatg.undB, 1, OUTPUT f-PreVta-B, OUTPUT f-Factor-B).
    IF almmmatg.undc <> '' THEN RUN Precio_unitario (almmmatg.undC, 1, OUTPUT f-PreVta-C, OUTPUT f-Factor-C).
    IF almmmatg.CHR__01 <> '' THEN RUN Precio_unitario (almmmatg.CHR__01, (IF Almmmatg.PesoBruto <= 0 THEN 1 ELSE Almmmatg.Pesobruto), OUTPUT f-PreVta-Min, OUTPUT f-Factor-Min).
    f-PreVta-Min-Dto = f-PreVta-Min.
    f-FleteUnitario = 0.
    y-Dsctos-Min = 0.
    RUN Precio-Descontado.
    PUT UNFORMATTED
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almmmatg.desmar '|'
        f-prevta-a '|'
        f-factor-a '|'
        almmmatg.unda '|'
        f-prevta-b '|'
        f-factor-b '|'
        almmmatg.undb '|'
        f-prevta-c '|'
        f-factor-c '|'
        almmmatg.undc '|'
        f-prevta-min '|'
        (IF almmmatg.pesobruto <= 0 THEN 1 ELSE almmmatg.pesobruto) '|'
        almmmatg.CHR__01 '|'
        f-prevta-min-dto '|'
        y-Dsctos-Min '|'
        f-fleteunitario '|'
        almmmatg.codbrr '|'.
    FIND almmmat1 OF almmmatg NO-LOCK NO-ERROR.
    IF AVAILABLE almmmat1 THEN DO:
        IF almmmat1.barras[1] <> '' THEN
            PUT UNFORMATTED almmmat1.barras[1] '|' almmmat1.equival[1] '|'.
        ELSE PUT UNFORMATTED '||'.
        IF almmmat1.barras[2] <> '' THEN
            PUT UNFORMATTED almmmat1.barras[2] '|' almmmat1.equival[2] '|'.
        ELSE PUT UNFORMATTED '||'.
        IF almmmat1.barras[3] <> '' THEN
            PUT UNFORMATTED almmmat1.barras[3] '|' almmmat1.equival[3] ''.
        ELSE PUT UNFORMATTED '|'.
    END.
    ELSE PUT UNFORMATTED '|||||'.
    PUT UNFORMATTED
        ''
        SKIP.
END.

PROCEDURE Precio_Unitario:

    DEF INPUT PARAMETER pUndVta AS CHAR.
    DEF INPUT PARAMETER pCanPed AS DEC.
    DEF OUTPUT PARAMETER pPreVta AS DEC.
    DEF OUTPUT PARAMETER pFactor AS DEC.
    
    DEF VAR f-PreBas AS DEC NO-UNDO.
    DEF VAR f-PreVta AS DEC NO-UNDO.
    DEF VAR f-Dsctos AS DEC NO-UNDO.
    DEF VAR y-Dsctos AS DEC NO-UNDO.
    DEF VAR x-TipDto AS DEC NO-UNDO.
    DEF VAR f-FleteUnitario AS DEC NO-UNDO.

    RUN vta2/PrecioMayorista-Cont-v2 (s-CodCia,
                                      s-CodDiv,
                                      '11111111111',
                                      1,
                                      almmmatg.tpocmb,
                                      OUTPUT pFactor,
                                      almmmatg.codmat,
                                      'SINDESCUENTOS',
                                      pUndVta,
                                      pCanPed,
                                      4,
                                      s-codalm,   /* Necesario para REMATES */
                                      OUTPUT f-PreBas,
                                      OUTPUT f-PreVta,
                                      OUTPUT f-Dsctos,
                                      OUTPUT y-Dsctos,
                                      OUTPUT x-TipDto,
                                      OUTPUT f-FleteUnitario
                                      ).
    pPreVta = ROUND(f-prevta + f-fleteunitario, 4).

END PROCEDURE.

PROCEDURE  Precio-Descontado:

    DEF VAR x-PreVta1 AS DEC NO-UNDO.
    DEF VAR x-PreVta2 AS DEC NO-UNDO.
    DEF VAR f-PreVta  AS DEC NO-UNDO.
    DEF VAR f-DtoPromocional AS DEC NO-UNDO.
    DEF VAR s-CodMon AS INT INIT 1 NO-UNDO.

    DEF VAR y-Dsctos AS DEC NO-UNDO.
    /*DEF VAR f-FleteUnitario AS DEC NO-UNDO.*/
    DEF VAR x-Factor AS DEC NO-UNDO.

    DEF VAR f-PreBas AS DEC NO-UNDO.
    DEF VAR f-Dsctos AS DEC NO-UNDO.
    DEF VAR x-TipDto AS DEC NO-UNDO.

    /* Flete */
    RUN vta2/PrecioMayorista-Cont-v2 (s-CodCia,
                                      s-CodDiv,
                                      '11111111111',
                                      1,
                                      almmmatg.tpocmb,
                                      OUTPUT x-Factor,
                                      almmmatg.codmat,
                                      'SINDESCUENTOS',
                                      almmmatg.CHR__01,
                                      almmmatg.pesobruto,
                                      4,
                                      s-codalm,   /* Necesario para REMATES */
                                      OUTPUT f-PreBas,
                                      OUTPUT f-PreVta,
                                      OUTPUT f-Dsctos,
                                      OUTPUT y-Dsctos,
                                      OUTPUT x-TipDto,
                                      OUTPUT f-FleteUnitario
                                      ).


    IF Almmmatg.CodFam = "011" THEN RETURN.
    IF Almmmatg.CodFam = "013" AND Almmmatg.SubFam <> "014" THEN RETURN.
    FIND FIRST VtaTabla WHERE VtaTabla.codcia = Almmmatg.codcia
        AND VtaTabla.tabla = "DTOPROLIMA"
        AND VtaTabla.llave_c1 = Almmmatg.codmat
        AND VtaTabla.llave_c2 = s-CodDiv
        AND TODAY >= VtaTabla.Rango_Fecha[1]
        AND TODAY <= VtaTabla.Rango_Fecha[2]
        NO-LOCK NO-ERROR.
    IF AVAILABLE VtaTabla THEN DO:
        ASSIGN
            F-PREVTA = Almmmatg.Prevta[1]
            Y-DSCTOS-MIN = VtaTabla.Valor[1].
        IF Almmmatg.Monvta = 1 THEN 
          ASSIGN X-PREVTA1 = F-PREVTA
                 X-PREVTA2 = ROUND(F-PREVTA / Almmmatg.TpoCmb,6).
        ELSE
          ASSIGN X-PREVTA2 = F-PREVTA
                 X-PREVTA1 = ROUND(F-PREVTA * Almmmatg.TpoCmb,6).
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
            AND Almtconv.Codalter = Almmmatg.CHR__01
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv
        THEN x-Factor = Almtconv.Equival.
        ELSE x-Factor = 1.
        X-PREVTA1 = X-PREVTA1 * X-FACTOR.
        X-PREVTA2 = X-PREVTA2 * X-FACTOR.             
        IF S-CODMON = 1 
        THEN f-prevta-min-dto = X-PREVTA1.
        ELSE f-prevta-min-dto = X-PREVTA2.     

        f-prevta-min-dto = ROUND(f-prevta-min-dto * (1 - Y-DSCTOS-MIN / 100) + f-FleteUnitario, 4).
        /*f-prevta-min-dto = ROUND(f-prevta-min-dto + f-FleteUnitario, 4).*/
    END.


END PROCEDURE.
