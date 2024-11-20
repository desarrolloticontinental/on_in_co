DEF TEMP-TABLE detalle 
    FIELD codcia LIKE almmmatg.codcia
    FIELD codmat LIKE almmmatg.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD desmar LIKE almmmatg.desmar
    FIELD codpro AS CHAR FORMAT 'x(11)'
    FIELD nompro AS CHAR FORMAT 'x(60)'
    FIELD undbas LIKE almmmatg.undbas
    FIELD codfam LIKE almmmatg.codfam
    FIELD candes LIKE ccbddocu.candes
    FIELD implin LIKE ccbddocu.implin
    FIELD impcto LIKE ccbddocu.impcto
    FIELD tipo   AS CHAR FORMAT 'x(15)'
    FIELD codalm LIKE almcmov.codalm
    FIELD codmov AS CHAR
    FIELD nroalm AS CHAR
    FIELD coddiv LIKE ccbddocu.coddiv
    FIELD coddoc LIKE ccbddocu.coddoc
    FIELD nrodoc LIKE ccbddocu.nrodoc
    FIELD codcli LIKE ccbcdocu.codcli
    FIELD nomcli LIKE ccbcdocu.nomcli
    FIELD fchdoc LIKE ccbcdocu.fchdoc
    INDEX Llave01 AS PRIMARY codcia coddiv coddoc nrodoc.

DEF BUFFER bccbcdocu FOR ccbcdocu.
DEF VAR x-Factor AS INT NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.
DEF VAR f-Precio AS DEC NO-UNDO.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 001:
    Docs:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = gn-divi.CodCia
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND (ccbcdocu.coddoc = 'FAC' OR  
             ccbcdocu.coddoc = 'BOL' OR 
             ccbcdocu.coddoc = 'TCK' OR 
             ccbcdocu.coddoc = 'N/C')
        AND ccbcdocu.fchdoc  >= 04/01/2016
        AND ccbcdocu.fchdoc  <= 04/30/2016:
        /* FILTROS */
        IF Ccbcdocu.coddoc = "N/C" AND Ccbcdocu.CndCre <> "D" THEN NEXT Docs.
        IF LOOKUP(ccbcdocu.tpofac, 'A,S') > 0  THEN NEXT Docs.      /* NO facturas adelantadas NI servicios */ 
        IF ccbcdocu.flgest = 'A' THEN NEXT Docs.
        IF ccbcdocu.FmaPgo = '900' THEN NEXT Docs.
        /* FIN DE FILTROS */
        IF ccbcdocu.coddoc = 'N/C' THEN DO:
            FIND bccbcdocu WHERE bccbcdocu.codcia = 001
                AND bccbcdocu.coddoc = ccbcdocu.codref
                AND bccbcdocu.nrodoc = ccbcdocu.nroref NO-LOCK NO-ERROR.
            IF AVAIL bccbcdocu AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN NEXT Docs.
        END.
        x-Factor = IF ccbcdocu.coddoc = 'N/C' THEN -1 ELSE 1.

        FOR EACH ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.implin > 0,
            FIRST almmmatg OF ccbddocu NO-LOCK:
            FIND detalle OF ccbddocu NO-ERROR.
            IF NOT AVAILABLE detalle THEN DO:
                CREATE detalle.
                BUFFER-COPY Ccbddocu 
                    EXCEPT Ccbddocu.candes Ccbddocu.implin Ccbddocu.impcto
                    TO Detalle
                ASSIGN
                    detalle.codcia = almmmatg.codcia
                    detalle.codmat = almmmatg.codmat
                    detalle.desmat = almmmatg.desmat
                    detalle.undbas = almmmatg.undbas
                    detalle.desmar = almmmatg.desmar
                    detalle.codfam = almmmatg.codfam
                    detalle.codpro = almmmatg.codpr1
                    detalle.codcli = ccbcdocu.codcli
                    detalle.nomcli = ccbcdocu.nomcli
                    detalle.fchdoc = ccbcdocu.fchdoc.
                IF almmmatg.CHR__02 = 'P' THEN detalle.tipo = 'PROPIOS'.
                ELSE detalle.tipo = 'TERCEROS'.
                FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                    AND gn-prov.codpro = almmmatg.codpr1
                    NO-LOCK NO-ERROR.
                IF AVAILABLE gn-prov THEN detalle.nompro = gn-prov.nompro.
            END.
            ASSIGN
                detalle.candes = detalle.candes + ccbddocu.candes * ccbddocu.factor * x-factor.
            FIND FIRST almcmov WHERE almcmov.codcia = ccbcdocu.codcia
                AND almcmov.codref = ccbcdocu.coddoc
                AND almcmov.nroref = ccbcdocu.nrodoc
                AND almcmov.flgest <> 'A'
                NO-LOCK NO-ERROR.
            IF AVAILABLE almcmov THEN DO:
                ASSIGN
                    detalle.codalm = almcmov.codalm
                    detalle.codmov = almcmov.tipmov + STRING(almcmov.codmov)
                    detalle.nroalm = STRING(almcmov.nroser,'999') + '-' + STRING(almcmov.nrodoc).
            END.
            /*Calculando el precio de venta sin I.G.V*/
            IF ccbcdocu.codmon = 1 
            THEN detalle.implin = detalle.implin + (ccbddocu.implin - ccbddocu.impigv) * x-factor.
            ELSE DO: 
                FIND FIRST gn-tcmb WHERE gn-tcmb.fecha = ccbcdocu.fchdoc NO-LOCK NO-ERROR.
                IF AVAIL gn-tcmb THEN
                    detalle.implin = detalle.implin + (ccbddocu.implin - ccbddocu.impigv) * gn-tcmb.venta * x-factor.
                ELSE detalle.implin = detalle.implin + (ccbddocu.implin - ccbddocu.impigv) * ccbcdocu.tpocmb * x-factor.
            END.
            /* Calculando Costo Promedio */
            f-precio = 0.
            FIND LAST AlmStkGe WHERE AlmStkge.CodCia = 001
                AND AlmStkge.codmat = Ccbddocu.codmat
                AND AlmStkge.Fecha <= Ccbcdocu.fchdoc
                NO-LOCK NO-ERROR.
            IF AVAILABLE AlmStkGe THEN f-Precio = AlmStkge.CtoUni.
            IF Ccbcdocu.coddoc = "N/C" THEN DO:
                FIND Almcmov WHERE Almcmov.CodCia = CcbCDocu.CodCia
                    AND  Almcmov.CodAlm = CcbCDocu.CodAlm 
                    AND  Almcmov.TipMov = "I"
                    AND  Almcmov.CodMov = CcbCDocu.CodMov 
                    AND  Almcmov.NroSer = 000
                    AND  Almcmov.NroDoc = INTEGER(CcbCDocu.NroPed)
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Almcmov THEN DO:
                    FIND LAST AlmStkGe WHERE AlmStkge.CodCia = Almcmov.codcia
                        AND AlmStkge.codmat = Ccbddocu.codmat
                        AND AlmStkge.Fecha <= Almcmov.fchdoc
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE AlmStkGe THEN f-Precio = AlmStkge.CtoUni.
                    ASSIGN
                        detalle.codalm = almcmov.codalm
                        detalle.codmov = almcmov.tipmov + STRING(almcmov.codmov)
                        detalle.nroalm = STRING(almcmov.nroser,'999') + '-' + STRING(almcmov.nrodoc).
                END.
            END.
            ASSIGN detalle.impcto = detalle.impcto + (ccbddocu.candes * ccbddocu.factor * f-precio * x-factor).
        END.
    END.
END.
DEF VAR cadenaResultado AS CHAR.
OUTPUT TO d:\tmp\roxana.txt.
FOR EACH detalle:
    RUN lib\limpiar-texto ( detalle.nomcli, ' ', OUTPUT cadenaResultado).

    PUT UNFORMATTED
        detalle.codmat '|'
        detalle.codalm '|'
        detalle.codmov '|'
        detalle.nroalm '|'
        detalle.coddoc '|'
        detalle.nrodoc '|'
        detalle.codcli '|'
        cadenaResultado  '|'
        detalle.fchdoc '|'
        detalle.candes '|'
        detalle.impcto
        SKIP.
END.
