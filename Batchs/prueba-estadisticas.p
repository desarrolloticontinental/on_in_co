DEF VAR s-codcia AS INT INIT 001.
DEF BUFFER b-cdocu FOR ccbcdocu.
DEF BUFFER b-ddocu FOR ccbddocu.
DEF VAR pcoddiv AS CHAR.
DEF VAR pdivdes AS CHAR.
DEF VAR x-signo AS INT.
DEF VAR x-signo1 AS INT.
DEF VAR x-imptot AS DEC.
DEF VAR x-implin AS DEC.
DEF VAR x-porigv AS DEC.
DEF VAR x-coe AS DEC.
DEF VAR x-can AS DEC.
DEF VAR f-factor AS DEC.
DEF VAR x-TpoCmbVta AS DEC.
DEF VAR x-TpoCmbCmp AS DEC.

ESTADISTICAS:
FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK,
    EACH CcbCdocu USE-INDEX Llave10 NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
        AND CcbCdocu.CodDiv = Gn-Divi.CodDiv
        AND CcbCdocu.FchDoc = 03/12/2013:
    /* ***************** FILTROS ********************************** */
    IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C") = 0 THEN NEXT.
    IF CcbCDocu.FlgEst = "A"  THEN NEXT.
    IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
    /* ************************************ */
    IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
        FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
            AND B-CDOCU.CodDoc = CcbCdocu.Codref 
            AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CDOCU THEN NEXT.
        /* *************************** */
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
        pCodDiv     = IF Ccbcdocu.DivOri <> '' THEN Ccbcdocu.DivOri ELSE Ccbcdocu.CodDiv
        pDivDes     = Ccbcdocu.CodDiv.
    IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
        FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
            AND B-CDOCU.CodDoc = CcbCdocu.Codref 
            AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-CDOCU THEN DO:
            ASSIGN
                pCodDiv     = IF B-CDOCU.DivOri <> '' THEN B-CDOCU.DivOri ELSE B-CDOCU.CodDiv
                pDivDes     = B-CDOCU.CodDiv.
        END.
    END.
    /* *********************************************************** */
    ASSIGN
        x-signo1 = ( IF CcbCdocu.Coddoc = "N/C" THEN -1 ELSE 1 )
        x-ImpTot = Ccbcdocu.ImpTot.     /* <<< OJO <<< */

    /* buscamos si hay una aplicación de fact adelantada */
    FIND FIRST Ccbddocu OF Ccbcdocu WHERE Ccbddocu.implin < 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbddocu THEN x-ImpTot = x-ImpTot + ABSOLUTE(Ccbddocu.ImpLin).
    /* ************************************************* */

    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.


    IF pCodDiv <> '00024' THEN NEXT.


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

   ASSIGN
       x-Coe = 1
       x-Can = 1.
   /* FACTURAS ADELANTADAS O DE SERVICIO */
   IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0 AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 
       THEN DO:
       RUN PROCESA-OTRAS-FACTURAS.
       NEXT.
   END.
   FOR EACH CcbDdocu OF CcbCdocu NO-LOCK:
       /* ****************** Filtros ************************* */
       FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
           AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
       IF NOT AVAILABLE Almmmatg THEN NEXT.
       IF Ccbddocu.implin < 0 THEN NEXT.       /* <<< OJO <<< */
       /* **************************************************** */
       /*IF Ccbddocu.ImpCto = ? THEN NEXT.*/
       /* **************************************************** */
       FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
           AND Almtconv.Codalter = Ccbddocu.UndVta
           NO-LOCK NO-ERROR.
       F-FACTOR  = 1. 
       IF AVAILABLE Almtconv THEN DO:
          F-FACTOR = Almtconv.Equival.
          IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
       END.
       IF CcbCDocu.CodDoc = "N/C" THEN DO:
           FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
               AND B-CDOCU.CodDoc = CcbCdocu.Codref 
               AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
               NO-LOCK.
           FIND FIRST B-DDOCU WHERE B-DDOCU.codcia = B-CDOCU.codcia
               AND B-DDOCU.coddoc = B-CDOCU.coddoc
               AND B-DDOCU.nrodoc = B-CDOCU.nrodoc
               AND B-DDOCU.codmat = CcbDdocu.codmat 
               NO-LOCK NO-ERROR.
       END.
       /* si aún no tiene código de almacén despacho */
       RUN Carga-Ventas-Detalle.
   END.  
END.


PROCEDURE procesa-nota:

    ASSIGN
        x-Can = 0                       /* ¿¿¿ OJO ??? */
        x-ImpTot = B-CDOCU.ImpTot.      /* <<< OJO <<< */

    /* buscamos si hay una aplicación de fact adelantada */
    FIND FIRST Ccbddocu OF B-CDOCU WHERE Ccbddocu.implin < 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbddocu THEN x-ImpTot = x-ImpTot + ABSOLUTE(Ccbddocu.ImpLin).
    /* ************************************************* */
    x-Coe = Ccbcdocu.ImpTot / x-ImpTot.     /* OJO */
    FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:
        /* ***************** FILTROS ********************************* */
        FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
            AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almmmatg THEN NEXT.
        IF Ccbddocu.implin <= 0 THEN NEXT.       /* <<< OJO <<< */
        /* ************************************************************ */
        IF Ccbddocu.ImpCto = ? THEN NEXT.
        /* **************************************************** */
        F-FACTOR  = 1. 
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = Ccbddocu.UndVta
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
           F-FACTOR = Almtconv.Equival.
           IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.
        RUN Carga-Ventas-Detalle.    
    END.  

END PROCEDURE.

PROCEDURE carga-ventas-detalle:

    DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
    ASSIGN
        x-ImpLin = Ccbddocu.ImpLin - Ccbddocu.ImpDto2
        x-ImpCto = Ccbddocu.ImpCto.
    IF x-PorIgv <= 0 THEN x-PorIgv = Ccbddocu.ImpIgv / ( Ccbddocu.ImpLin - Ccbddocu.ImpIgv) * 100.
    IF x-PorIgv = ? THEN x-PorIgv = 0.
    IF x-ImpCto = ? THEN x-ImpCto = 0.
    /* ************************************************************************ */
    DISPLAY
        pcoddiv 
        pdivdes 
        ccbcdocu.coddoc 
        ccbcdocu.nrodoc 
        ccbddocu.codmat 
        ccbddocu.candes 
        f-factor 
        x-can 
        x-signo1
        x-implin
        x-coe
        WITH 1 COL.

END PROCEDURE.

PROCEDURE procesa-nota-rebade:

    /* EL REBATE ES APLICADO SOLO A PRODUCTOS DE LA FAMILIA 010 Y 012 */
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
    x-Coe = Ccbcdocu.ImpTot / x-ImpTot.     /* OJO */
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
        F-FACTOR  = 1. 
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = Ccbddocu.UndVta
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
           F-FACTOR = Almtconv.Equival.
           IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.
        RUN Carga-Ventas-Detalle.
    END.  

END PROCEDURE.

PROCEDURE procesa-otras-facturas:

DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

/* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */

ASSIGN
    x-ImpLin = Ccbcdocu.ImpTot
    x-ImpCto = 0.
IF x-PorIgv <= 0 THEN x-PorIgv = Ccbcdocu.ImpIgv /(Ccbcdocu.ImpTot - Ccbcdocu.ImpIgv) * 100.
IF x-ImpCto = ? THEN x-ImpCto = 0.
/* ************************************************************************ */
    DISPLAY
        pcoddiv 
        pdivdes 
        ccbcdocu.coddoc 
        ccbcdocu.nrodoc 
        "999999"
        0
        f-factor 
        x-can 
        x-signo1
        x-implin
        x-coe
        WITH 1 COL.


END PROCEDURE.
