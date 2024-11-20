DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-codfchi AS DATE.
DEF VAR x-codfchf AS DATE.
DEFINE VAR pCodDiv AS CHAR NO-UNDO.
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.
DEF VAR x-codven    AS CHAR.
DEF VAR x-fmapgo    as char.
DEF VAR x-canal     as char.
DEF VAR x-CodUnico  LIKE Gn-clie.CodUnico.
DEF VAR x-NroCard   LIKE GN-card.NroCard.
DEF VAR x-Sede      LIKE Gn-ClieD.Sede.
DEF VAR cl-CodCia   AS INT NO-UNDO.
DEF VAR x-CodCli    LIKE Gn-clie.codcli.
DEF VAR x-Zona      AS CHAR NO-UNDO.
DEF VAR x-coe       AS DECI INIT 0.
DEF VAR x-can       AS DECI INIT 0.
DEF VAR f-factor    AS DECI INIT 0.
DEF VAR x-AlmDes    AS CHAR.
DEF VAR x-signo1 AS INT INIT 1.
DEF VAR x-ImpLin AS DEC NO-UNDO.
DEF VAR x-ImpTot AS DEC NO-UNDO.     /* IMporte NETO de venta */
DEF VAR x-TpoCmbCmp AS DECI INIT 1.
DEF VAR x-TpoCmbVta AS DECI INIT 1.
DEF VAR x-PorIgv AS DEC DECIMALS 4.
DEF VAR s-clivar    AS CHAR FORMAT 'x(11)'.
DEF VAR s-CliUni    AS CHAR FORMAT 'x(11)' INIT '99999999999' NO-UNDO.

DEF BUFFER B-CDOCU FOR ccbcdocu.
DEF BUFFER B-DDOCU FOR ccbddocu.
DEF BUFFER B-DIVI  FOR Gn-Divi.

ASSIGN
    x-codfchi = 01/01/2009
    x-codfchf = 12/31/2011.

FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
    /* Barremos las ventas */
    ESTADISTICAS:
    FOR EACH CcbCdocu NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
            AND CcbCdocu.CodDiv = Gn-Divi.CodDiv
            AND CcbCdocu.FchDoc >= x-CodFchI
            AND CcbCdocu.FchDoc <= x-CodFchF
            AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0      /* NO facturas adelantadas NI servicios */
            USE-INDEX llave10:
        /* ***************** FILTROS ********************************** */
        IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
        IF CcbCDocu.FlgEst = "A"  THEN NEXT.
        IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
        DISPLAY ccbcdocu.coddiv ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc.
        PAUSE 0.
        IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
            FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
                AND B-CDOCU.CodDoc = CcbCdocu.Codref 
                AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE B-CDOCU THEN NEXT.
            IF LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN NEXT.     /* NO SERVICIOS NI ADELANTADAS */
            IF Ccbcdocu.CndCre = "N" THEN DO:       /* OTRAS */
                /* NO por APLICACION DE ANTICIPO */
                FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
                    FIND FIRST Ccbtabla WHERE Ccbtabla.codcia = s-codcia
                        AND Ccbtabla.tabla = "N/C"
                        AND Ccbtabla.codigo = Ccbddocu.codmat
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE Ccbtabla AND CcbTabla.Libre_L01 = NO THEN NEXT ESTADISTICAS.
                END.
            END.
        END.

        ASSIGN
            pCodDiv = IF Ccbcdocu.DivOri <> '' THEN Ccbcdocu.DivOri ELSE Ccbcdocu.CodDiv
            x-CodVen = Ccbcdocu.codven.
        IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
            FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
                AND B-CDOCU.CodDoc = CcbCdocu.Codref 
                AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
                NO-LOCK NO-ERROR.
            IF AVAILABLE B-CDOCU THEN DO:
                ASSIGN
                    pCodDiv = IF B-CDOCU.DivOri <> '' THEN B-CDOCU.DivOri ELSE B-CDOCU.CodDiv
                    x-CodVen = B-CDOCU.codven.
            END.
        END.
        /* Ajuste de la division en los valores historicos */
        IF pCodDiv <> '00017' AND x-codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
        IF pCodDiv <> '00018' AND LOOKUP(x-codven, '015,173,900,901,902,017') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
        IF pCodDiv <> '00019' AND x-codven = '081' THEN pCodDiv = '00019'.   /* Mesa redonda */
        IF pCodDiv <> '00099' AND x-codven = '998' THEN pCodDiv = '00099'.   /* Exportaciones */
        IF pCodDiv <> '00098' AND x-codven = '157' THEN pCodDiv = '00098'.   /* Refiles */
        /* *********************************************************** */
        FIND B-DIVI WHERE B-DIVI.codcia = s-codcia AND B-DIVI.coddiv = pCodDiv NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-DIVI THEN NEXT.
        /* *********************************************************** */
        ASSIGN
            x-signo1 = ( IF CcbCdocu.Coddoc = "N/C" THEN -1 ELSE 1 )
            x-ImpTot = Ccbcdocu.ImpTot.     /* <<< OJO <<< */

        /* buscamos si hay una aplicación de fact adelantada */
        FIND FIRST Ccbddocu OF Ccbcdocu WHERE Ccbddocu.implin < 0 NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbddocu THEN x-ImpTot = x-ImpTot + ABSOLUTE(Ccbddocu.ImpLin).
        /* ************************************************* */

        FIND dwh_ventas_cab WHERE dwh_ventas_cab.codcia = s-codcia
            AND dwh_ventas_cab.coddiv = pcoddiv
            AND dwh_ventas_cab.coddoc = ccbcdocu.coddoc
            AND dwh_ventas_cab.nrodoc = ccbcdocu.nrodoc
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dwh_ventas_cab THEN NEXT.
        
        /* NOTAS DE CREDITO por OTROS conceptos */
        /* RHC 15.03.10 considerar los rEbates */
       IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac <> "E" THEN DO:
           RUN PROCESA-NOTA.
           NEXT.
       END.
       IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac = "E" THEN DO:
           RUN PROCESA-NOTA-REBADE.
           NEXT.
       END.

       FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
           x-almdes = ccbddocu.almdes.
           IF ccbcdocu.codref = "N/C" THEN DO:
               FIND FIRST B-DDOCU OF B-CDOCU WHERE B-DDOCU.codmat = ccbddocu.codmat NO-LOCK NO-ERROR.
               IF AVAILABLE B-DDOCU THEN x-almdes = B-DDOCU.almdes.
           END.
           FIND dwh_ventas_det OF dwh_ventas_cab WHERE dwh_ventas_det.codmat = ccbddocu.codmat NO-ERROR.
           IF AVAILABLE dwh_ventas_det THEN dwh_ventas_det.almdes = x-almdes.
       END.
    END.
END.

PROCEDURE procesa-nota:

FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:
    /* ***************** FILTROS ********************************* */
    FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
        AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    IF Ccbddocu.implin <= 0 THEN NEXT.       /* <<< OJO <<< */
    /* ************************************************************ */
    IF Ccbddocu.ImpCto = ? THEN NEXT.
    /* **************************************************** */
    x-AlmDes = CcbDdocu.AlmDes.
    FIND dwh_ventas_det OF dwh_ventas_cab WHERE dwh_ventas_det.codmat = ccbddocu.codmat NO-ERROR.
    IF AVAILABLE dwh_ventas_det THEN dwh_ventas_det.almdes = x-almdes.
END.  

END.


PROCEDURE procesa-nota-rebade:

    FIND B-CDOCU WHERE B-CDOCU.codcia = Ccbcdocu.codcia
        AND B-CDOCU.coddoc = Ccbcdocu.codref
        AND B-CDOCU.nrodoc = Ccbcdocu.nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDOCU THEN RETURN.
    ASSIGN
        x-Can = 0                       /* ¿¿¿ OJO ??? */
        x-ImpTot = 0.                   /* <<< OJO <<< */
    FOR EACH CcbDdocu OF B-CDOCU NO-LOCK WHERE Ccbddocu.ImpLin > 0,
        FIRST Almmmatg OF Ccbddocu NO-LOCK WHERE LOOKUP(Almmmatg.codfam , '010,012') > 0:
        x-ImpTot = x-ImpTot + Ccbddocu.ImpLin.
    END.  
    IF x-ImpTot <= 0 THEN RETURN.
    /* ************************************************* */
    FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:
        /* ***************** FILTROS ********************************* */
        FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
            AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almmmatg THEN NEXT.
        IF LOOKUP(Almmmatg.codfam , '010,012') = 0 THEN NEXT.
        IF Ccbddocu.implin <= 0 THEN NEXT.       /* <<< OJO <<< */
        /* ************************************************************ */
        IF Ccbddocu.ImpCto = ? THEN NEXT.
        /* **************************************************** */

        x-AlmDes = CcbDdocu.AlmDes.
        FIND dwh_ventas_det OF dwh_ventas_cab WHERE dwh_ventas_det.codmat = ccbddocu.codmat NO-ERROR.
        IF AVAILABLE dwh_ventas_det THEN dwh_ventas_det.almdes = x-almdes.
    END.  

END.
