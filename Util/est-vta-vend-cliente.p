DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR cl-codcia AS INT.
Def var x-CodFchI   as date format '99/99/9999' init TODAY.
Def var x-CodFchF   as date format '99/99/9999' init TODAY.
Def var x-signo1    as inte init 1.
Def var x-Day       as inte format '99'   init 1.
Def var x-Month     as inte format '99'   init 1.
Def var x-Year      as inte format '9999' init 1.
DEF VAR x-ImpAde    AS DEC NO-UNDO.     /* Importe aplicado de la factura adelantada */
DEF VAR x-ImpTot    AS DEC NO-UNDO.     /* IMporte NETO de venta */
Def var x-TpoCmbCmp as deci init 1.
Def var x-TpoCmbVta as deci init 1.
def var x-fmapgo    as char.
def var x-canal     as char.
Def var f-factor    as deci init 0.
DEF VAR x-codven LIKE gn-ven.codven.

Def BUFFER B-CDOCU FOR CcbCdocu.

DEF TEMP-TABLE detalle
    FIELD codcia AS INT
    FIELD codven LIKE gn-ven.codven
    FIELD codcli LIKE gn-clie.codcli
    FIELD nomcli LIKE gn-clie.nomcli
    FIELD impmn AS DEC
    FIELD impme AS DEC
    INDEX llave01 codcia codven codcli.

ASSIGN
    x-CodFchI = 05/01/2009
    x-CodFchF = 05/15/2009.

    FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia use-index Idx01 no-lock :
        /* Barremos las ventas */
        FOR EACH CcbCdocu WHERE CcbCdocu.CodCia = S-CODCIA 
                            AND CcbCdocu.CodDiv = Gn-Divi.CodDiv
                            AND CcbCdocu.FchDoc >= x-CodFchI
                            AND CcbCdocu.FchDoc <= x-CodFchF
                            AND CcbCdocu.TpoFac <> 'A'      /* NO facturas adelantadas */
                            USE-INDEX llave10
                            BREAK BY CcbCdocu.CodCia
                                  BY CcbCdocu.CodDiv
                                  BY CcbCdocu.FchDoc:
            /* ***************** FILTROS ********************************** */
            IF Lookup(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
            IF CcbCDocu.FlgEst = "A"  THEN NEXT.
            IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
            /* *********************************************************** */
            ASSIGN
            x-Day   = DAY(CcbCdocu.FchDoc)
            x-Month = MONTH(CcbCdocu.FchDoc)
            x-Year  = YEAR(CcbCdocu.FchDoc)
            x-ImpTot = Ccbcdocu.ImpTot.     /* <<< OJO <<< */
            x-CodVen = Ccbcdocu.codven.
            IF Ccbcdocu.coddoc = 'N/C' THEN DO:
                x-signo1 = -1.
                FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia AND
                                   B-CDOCU.CodDoc = CcbCdocu.Codref AND
                                   B-CDOCU.NroDoc = CcbCdocu.Nroref 
                                   NO-LOCK NO-ERROR.
                IF AVAILABLE B-CDOCU THEN x-CodVen = B-CDOCU.codven.
            END.
            ELSE x-signo1 = 1.

            /* buscamos si hay una aplicación de fact adelantada */
            FIND FIRST Ccbddocu OF Ccbcdocu WHERE Ccbddocu.implin < 0 NO-LOCK NO-ERROR.
            IF AVAILABLE Ccbddocu THEN x-ImpTot = x-ImpTot + ABSOLUTE(Ccbddocu.ImpLin).
            /* ************************************************* */
            FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc
                              USE-INDEX Cmb01
                              NO-LOCK NO-ERROR.
            IF NOT AVAIL Gn-Tcmb THEN 
                FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc
                                   USE-INDEX Cmb01
                                   NO-LOCK NO-ERROR.
            IF AVAIL Gn-Tcmb THEN 
                ASSIGN
                x-TpoCmbCmp = Gn-Tcmb.Compra
                x-TpoCmbVta = Gn-Tcmb.Venta.

            FIND detalle WHERE detalle.codcia = ccbcdocu.codcia
                AND detalle.codven = x-codven
                AND detalle.codcli = ccbcdocu.codcli
                NO-ERROR.
            IF NOT AVAILABLE detalle THEN DO:
                CREATE detalle.
                ASSIGN
                    detalle.codcia = ccbcdocu.codcia
                    detalle.codven = x-codven
                    detalle.codcli = ccbcdocu.codcli.
            END.
            IF Ccbcdocu.CodMon = 1 THEN 
                ASSIGN
                detalle.impmn = detalle.impmn + x-signo1 * x-ImpTot
                detalle.impme = detalle.impme + x-signo1 * x-ImpTot / x-TpoCmbCmp.
            IF Ccbcdocu.CodMon = 2 THEN 
                ASSIGN
                detalle.impme = detalle.impme + x-signo1 * x-ImpTot
                detalle.impmn = detalle.impmn + x-signo1 * x-ImpTot * x-TpoCmbVta.

        END.
    END.

    OUTPUT TO m:\tmp\vendedores.txt.
    FOR EACH detalle, FIRST gn-ven OF detalle NO-LOCK, FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = detalle.codcli:
        DISPLAY
            detalle.codven
            gn-ven.nomven
            detalle.codcli
            gn-clie.nomcli
            detalle.impmn FORMAT '(>>>,>>>,>>>,>>9.99)'
            WITH STREAM-IO NO-BOX WIDTH 200.
    END.
    OUTPUT CLOSE.


RETURN.



