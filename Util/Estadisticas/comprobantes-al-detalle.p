DEF VAR x-TpoCmbCmp AS DECI INIT 1.
DEF VAR x-TpoCmbVta AS DECI INIT 1.

DEF BUFFER cotizacion FOR faccpedi.

OUTPUT TO c:\tmp\detallado.txt.
PUT UNFORMATTED
    'DESTINO|ORIGEN|EMISION|DOC|NUMERO|TIPO|PEDIDO|FECHAPED|COTIZACION|FECHACOTIZ|LISTA|CLIENTE|NOMBRE|TARJETA|IMPORTE TOTAL|ANTICIPO|' +
    'CODIGO|DESCRIPCION|UNIDAD|CANTIDAD|IMPORTE LINEA EN S/.'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND divori = "00015"
    AND LOOKUP(coddoc, 'fac,bol') > 0
    AND flgest <> 'A'
    AND fchdoc >= 10/01/2013
    AND fchdoc <= TODAY,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 0
    AND gn-clie.codcli = ccbcdocu.codcli,
    FIRST faccpedi NO-LOCK WHERE faccpedi.codcia = 001
    AND faccpedi.coddoc = ccbcdocu.codped
    AND faccpedi.nroped = ccbcdocu.nroped,
    FIRST cotizacion NO-LOCK WHERE cotizacion.codcia = 001
    AND cotizacion.coddoc = faccpedi.codref
    AND cotizacion.nroped = faccpedi.nroref:
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.
    IF Ccbcdocu.codmon = 1 THEN x-TpoCmbVta = 1.
    FOR EACH ccbddocu OF ccbcdocu:
        PUT UNFORMATTED
            ccbcdocu.coddiv '|'
            ccbcdocu.divori '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.tpofac '|'
            faccpedi.coddoc + ' ' + faccpedi.nroped '|'
            faccpedi.fchped '|' 
            cotizacion.coddoc + ' ' + cotizacion.nroped '|'
            cotizacion.fchped '|'
            cotizacion.libre_c01 '|'
            ccbcdocu.codcli '|'
            ccbcdocu.nomcli '|'.
        IF CcbCDocu.NroCard = '' THEN PUT UNFORMATTED gn-clie.NroCard '|'.
        ELSE PUT UNFORMATTED CcbCDocu.NroCard '|'.
        PUT UNFORMATTED
            (ccbcdocu.imptot * x-TpoCmbVta) '|'
            (ccbcdocu.imptot2 * x-TpoCmbVta) '|'
            ccbddocu.codmat '|'.
        FIND almmmatg OF ccbddocu NO-LOCK NO-ERROR.
        FIND ccbtabla WHERE CcbTabla.CodCia = 001
            AND CcbTabla.Tabla = ccbcdocu.coddoc
            AND CcbTabla.Codigo = ccbddocu.codmat
            NO-LOCK NO-ERROR.
        FIND Almmserv WHERE almmserv.CodCia = 001
            AND almmserv.codmat = ccbddocu.codmat
            NO-LOCK NO-ERROR.
        IF LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,TCK') > 0 THEN DO:
            IF LOOKUP(ccbcdocu.tpofac, 'A,S') = 0 THEN DO:
                IF AVAILABLE almmmatg THEN PUT UNFORMATTED almmmatg.desmat '|'.
                ELSE PUT '|'.
            END.
            ELSE DO:
                IF ccbcdocu.tpofac = 'S' THEN DO:
                    IF AVAILABLE almmserv THEN PUT UNFORMATTED  almmserv.DesMat '|'.
                    ELSE PUT '|'.
                END.
                ELSE PUT '|'.
            END.
        END.
        ELSE DO:
            IF CcbCdocu.CndCre = "N" THEN DO:
                IF AVAILABLE ccbtabla THEN PUT UNFORMATTED CcbTabla.Nombre '|'.
                ELSE PUT '|'.
            END.
            ELSE DO:
                IF AVAILABLE almmmatg THEN PUT UNFORMATTED almmmatg.desmat '|'.
                ELSE PUT '|'.
            END.
        END.
        PUT UNFORMATTED
            ccbddocu.undvta '|'
            ccbddocu.candes '|'
            (ccbddocu.implin * x-TpoCmbVta)
            SKIP.
    END.
END.
