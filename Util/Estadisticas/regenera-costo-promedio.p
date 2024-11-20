Def var s-codcia    as inte init 1.         /* OJO */
DEF VAR s-clivar    AS CHAR FORMAT 'x(11)'.
DEF VAR s-CliUni    AS CHAR FORMAT 'x(11)' INIT '99999999999' NO-UNDO.

Def var x-signo1    as inte init 1.
Def var x-fin       as inte init 0.
Def var f-factor    as deci init 0.
Def var x-NroFchI   as inte init 0.
Def var x-NroFchF   as inte init 0.
Def var x-CodFchI   as date format '99/99/9999' init TODAY.
Def var x-CodFchF   as date format '99/99/9999' init TODAY.
Def var i           as inte init 0.
Def var x-TpoCmbCmp as deci init 1.
Def var x-TpoCmbVta as deci init 1.
Def var x-Day       as inte format '99'   init 1.
Def var x-Month     as inte format '99'   init 1.
Def var x-Year      as inte format '9999' init 1.
Def var x-coe       as deci init 0.
Def var x-can       as deci init 0.

DEF VAR x-ImpAde    AS DEC NO-UNDO.     /* Importe aplicado de la factura adelantada */
DEF VAR x-ImpTot    AS DEC NO-UNDO.     /* IMporte NETO de venta */

Def BUFFER B-CDOCU FOR CcbCdocu.
DEF BUFFER B-DIVI  FOR Gn-Divi.

DEF VAR x-codven    AS CHAR.
DEF VAR x-fmapgo    as char.
DEF VAR x-canal     as char.
DEF VAR x-CodUnico  LIKE Gn-clie.CodUnico.
DEF VAR x-NroCard   LIKE GN-card.NroCard.
DEF VAR x-Sede      LIKE Gn-ClieD.Sede.
DEF VAR cl-CodCia   AS INT NO-UNDO.
DEF VAR x-CodCli    LIKE Gn-clie.codcli.
DEF VAR x-Zona      AS CHAR NO-UNDO.

FIND FIRST Empresas WHERE Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
    IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
END.
FIND FIRST FacCfgGn WHERE FacCFgGn.codcia = s-codcia NO-LOCK NO-ERROR.
s-CliVar = FacCfgGn.CliVar.

x-CodFchF = TODAY - 1.
x-CodFchI = DATE(01, 01, YEAR(TODAY)).      /* Por defecto */

/* Fecha de Cierre */
DEF VAR dFchCie AS DATE NO-UNDO.
IF DAY(TODAY) < 15 
THEN dFchCie = TODAY - DAY(TODAY).
ELSE dFchCie = TODAY.
dFchCie = dFchCie - DAY(dFchCie) + 1. 

x-CodFchI = dFchCie.        /* OJO */

/* DESCOMENTAR SI QUIERES CALCULAR POR UN RANGO DE FECHAS FIJO */

x-CodFchI = 12/01/2009.
/*
x-CodFchF = 12/31/2010.
*/

x-NroFchI = INTEGER(STRING(YEAR(x-CodFchI),"9999") + STRING(MONTH(x-CodFchI),"99")).      
x-NroFchF = INTEGER(STRING(YEAR(x-CodFchF),"9999") + STRING(MONTH(x-CodFchF),"99")).                           

DEFINE VAR pCodDiv AS CHAR NO-UNDO.
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.

DISPLAY x-CodFchI x-CodFchF x-NroFchI x-NroFchF.
PAUSE 0.
    
FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
    DISPLAY gn-divi.coddiv STRING(TIME,'HH:MM') TODAY.
    PAUSE 0.
    /* Barremos las ventas */
    FOR EACH CcbCdocu NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
            AND CcbCdocu.CodDiv = Gn-Divi.CodDiv
            AND CcbCdocu.FchDoc >= x-CodFchI
            AND CcbCdocu.FchDoc <= x-CodFchF
            AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0      /* NO facturas adelantadas NI servicios */
            USE-INDEX llave10
            BREAK BY CcbCdocu.CodCia BY CcbCdocu.CodDiv BY CcbCdocu.FchDoc:

        pCodDiv = Ccbcdocu.coddiv.      /* VALOR POR DEFECTO */
        pCanalVenta = GN-DIVI.CanalVenta.

        IF Ccbcdocu.coddiv <> '00017' 
            AND Ccbcdocu.codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
        IF Ccbcdocu.coddiv <> '00018' 
            AND LOOKUP(Ccbcdocu.codven, '015,173,901,902') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
        /* ***************** FILTROS ********************************** */
        IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
        IF CcbCDocu.FlgEst = "A"  THEN NEXT.
        IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
        IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
            /* 27.08.10 */
            FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
                AND B-CDOCU.CodDoc = CcbCdocu.Codref 
                AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE B-CDOCU THEN NEXT.
            IF LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN NEXT.     /* NO SERVICIOS NI ADELANTADAS */
            IF B-CDOCU.coddiv <> '00017' 
                AND B-CDOCU.codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
            IF B-CDOCU.coddiv <> '00018' 
                AND LOOKUP(B-CDOCU.codven, '015,173,901,902') > 0 THEN pCodDiv = '00018'.    /* Provincias */
            IF B-CDOCU.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
        END.
        /* *********************************************************** */
        FIND B-DIVI WHERE B-DIVI.codcia = s-codcia
            AND B-DIVI.coddiv = pCodDiv NO-LOCK.
        pCanalVenta = B-DIVI.CanalVenta.
        /* *********************************************************** */
 
        ASSIGN
            x-signo1 = ( IF CcbCdocu.Coddoc = "N/C" THEN -1 ELSE 1 )
            x-Day   = DAY(CcbCdocu.FchDoc)
            x-Month = MONTH(CcbCdocu.FchDoc)
            x-Year  = YEAR(CcbCdocu.FchDoc)
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

        /* VARIABLES DE VENTAS */
        ASSIGN
            x-codven = Ccbcdocu.CodVen
            X-FMAPGO = CcbCdocu.FmaPgo
            x-NroCard = Ccbcdocu.nrocard
            x-Sede = Ccbcdocu.Sede 
            x-Canal = ''
            x-CodCli = Ccbcdocu.CodCli
            x-CodUnico = Ccbcdocu.codcli
            x-Zona = ''.

        IF CcbCdocu.Coddoc = "N/C" THEN DO:
           FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
               AND B-CDOCU.Coddoc = CcbCdocu.Codref 
               AND B-CDOCU.NroDoc = CcbCdocu.Nroref
               NO-LOCK NO-ERROR.
           IF AVAILABLE B-CDOCU THEN DO:
               ASSIGN
                   x-CodVen = B-CDOCU.CodVen
                   X-FMAPGO = B-CDOCU.FmaPgo
                   x-NroCard = B-CDOCU.NroCard
                   x-Sede = B-CDOCU.Sede.
           END.
        END.

        /* CANAL */
        FIND gn-clie WHERE gn-clie.codcia = cl-codcia     /* OJO */
            AND gn-clie.codcli = Ccbcdocu.codcli
            NO-LOCK NO-ERROR.
        IF AVAILABLE gn-clie AND gn-clie.codunico <> '' THEN x-codunico = gn-clie.codunico.
        IF AVAILABLE Gn-clie THEN x-canal = gn-clie.Canal.
        /* ZONA */
        IF AVAILABLE gn-clie THEN DO:
            FIND TabDepto WHERE TabDepto.CodDepto = gn-clie.CodDept NO-LOCK NO-ERROR.
            IF AVAILABLE TabDepto THEN x-Zona = TabDepto.Zona.
            /* 20.07.10 */
            IF gn-clie.CodDept = '15' AND gn-clie.CodProv = '01' THEN x-Zona = 'LMC'.
        END.

        /* EN CASO DE SER UN CLIENTE VARIOS */
        IF Ccbcdocu.codcli = s-CliVar THEN DO:
            ASSIGN
                x-CodCli = s-CliVar
                x-CodUnico = s-CliVar.
            IF x-NroCard <> '' THEN DO:
                FIND FIRST Gn-Clie WHERE  Gn-clie.codcia = cl-codcia
                    AND Gn-clie.nrocard = x-NroCard
                    AND Gn-clie.flgsit = 'A'        /* ACTIVOS */
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Gn-clie 
                THEN FIND FIRST Gn-Clie WHERE  Gn-clie.codcia = cl-codcia
                        AND Gn-clie.nrocard = x-NroCard
                        NO-LOCK NO-ERROR.
                IF AVAILABLE Gn-Clie THEN x-CodUnico = Gn-Clie.CodUnico.
            END.
        END.
        /* ******************************** */
        
        /* NOTAS DE CREDITO por OTROS conceptos */
        /* RHC 15.03.10 considerar los rEbates */
       IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac <> "E" THEN DO:
           RUN PROCESA-NOTA.
           NEXT.
       END.
       IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac = "E" THEN DO:
           RUN PROCESA-NOTA-REBATE.
           NEXT.
       END.

       ASSIGN
           x-Coe = 1
           x-Can = 1.
       FOR EACH CcbDdocu OF CcbCdocu NO-LOCK:
           /* ****************** Filtros ************************* */
           FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
               AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
           IF NOT AVAILABLE Almmmatg THEN NEXT.
           IF Ccbddocu.implin <= 0 THEN NEXT.       /* <<< OJO <<< */
           /* **************************************************** */
           FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
               AND Almtconv.Codalter = Ccbddocu.UndVta
               NO-LOCK NO-ERROR.
           F-FACTOR  = 1. 
           IF AVAILABLE Almtconv THEN DO:
              F-FACTOR = Almtconv.Equival.
              IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
           END.

           RUN Carga-Estadisticas-2.    /* Estadísticas CON materiales */
       END.  
    END.
END.
/* Estadisticas Resumidas */
DISPLAY 'RESUMEN' STRING(TIME,'HH:MM') TODAY.
PAUSE 0.
RUN Resumen-Estadisticas.

DISPLAY 'FINALIZADO' STRING(TIME,'HH:MM') TODAY.
PAUSE 0.

RETURN.


PROCEDURE PROCESA-NOTA.
/* ******************* */

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
    F-FACTOR  = 1. 
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = Ccbddocu.UndVta
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almtconv THEN DO:
       F-FACTOR = Almtconv.Equival.
       IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
    END.

    RUN Carga-Estadisticas-2.    /* Estadísticas CON materiales */

END.  

END.
/* **************************************************** */

PROCEDURE PROCESA-NOTA-REBATE.
/* ************************** */

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
        F-FACTOR  = 1. 
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = Ccbddocu.UndVta
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
           F-FACTOR = Almtconv.Equival.
           IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.

        RUN Carga-Estadisticas-2.    /* Estadísticas CON materiales */

    END.  
END.
/* **************************************************** */

PROCEDURE Carga-Estadisticas-2:
/* **************************** */

    FIND estavtas.EvtAll01 WHERE estavtas.EvtAll01.Codcia = CcbCdocu.Codcia 
        AND estavtas.EvtAll01.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        AND estavtas.EvtAll01.CodDiv = pCoddiv 
        AND estavtas.EvtAll01.CanalVenta = pCanalVenta
        AND estavtas.EvtAll01.CodUnico = x-CodUnico
        AND estavtas.EvtAll01.CodCli = x-CodCli
        AND estavtas.EvtAll01.Zona = x-Zona
        AND estavtas.EvtAll01.NroCard = x-NroCard
        AND estavtas.EvtAll01.Sede = x-Sede 
        AND estavtas.EvtAll01.CodMat = CcbDdocu.CodMat 
        AND estavtas.EvtAll01.CodPro = Almmmatg.CodPr1
        AND estavtas.EvtAll01.CodVen = x-codven     /* Ccbcdocu.codven */
        AND estavtas.EvtAll01.FmaPgo = x-fmapgo
        NO-ERROR.     
    IF NOT AVAILABLE estavtas.EvtAll01 THEN RETURN.
    /* 11.09.10 INCORPORAMOS EL COSTO PROMEDIO */
    FIND LAST AlmStkGe WHERE Almstkge.codcia = Ccbcdocu.codcia
        AND Almstkge.codmat = Ccbddocu.codmat
        AND Almstkge.fecha <= Ccbcdocu.fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almstkge THEN DO:
        ASSIGN
            estavtas.EvtAll01.ProxDiaMn[x-Day] = estavtas.EvtAll01.ProxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe)
            estavtas.EvtAll01.ProxDiaMe[x-Day] = estavtas.EvtAll01.ProxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe / x-TpoCmbCmp)
            estavtas.EvtAll01.ProxMesMn = estavtas.EvtAll01.ProxMesMn + x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe
            estavtas.EvtAll01.ProxMesMe = estavtas.EvtAll01.ProxMesMe + x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe / x-TpoCmbCmp.
    END.
END.
/* ************************* */


PROCEDURE Resumen-Estadisticas:
/* ************************* */

    FOR EACH estavtas.EvtAll01 NO-LOCK WHERE estavtas.EvtAll01.codcia = s-codcia
        AND estavtas.EvtAll01.NroFch >= x-NroFchI
        AND estavtas.EvtAll01.NroFch <= x-NroFchF:
        /* VENTAS POR DIVISION */
        FIND estavtas.EvtDivi WHERE estavtas.EvtDivi.codcia = s-codcia
            AND estavtas.EvtDivi.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtDivi.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtDivi.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtDivi.Zona = estavtas.EvtAll01.Zona
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE estavtas.EvtDivi THEN NEXT.
        ASSIGN
            estavtas.EvtDivi.ProxMesMe = estavtas.EvtDivi.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtDivi.ProxMesMn = estavtas.EvtDivi.ProxMesMn + estavtas.EvtAll01.ProxMesMn.                  
        DO I = 1 TO 31 :
            estavtas.EvtDivi.ProxDiaMn[I] = estavtas.EvtDivi.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtDivi.ProxDiaMe[I] = estavtas.EvtDivi.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR CLIENTE */
        FIND estavtas.EvtClie WHERE estavtas.EvtClie.codcia = s-codcia
            AND estavtas.EvtClie.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtClie.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtClie.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtClie.CodUnico = estavtas.EvtAll01.CodUnico
            AND estavtas.EvtClie.CodCli = estavtas.EvtAll01.CodCli
            AND estavtas.EvtClie.Zona = estavtas.EvtAll01.Zona
            AND estavtas.EvtClie.Sede = estavtas.EvtAll01.Sede
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE estavtas.EvtClie THEN NEXT.
        ASSIGN
            estavtas.EvtClie.ProxMesMe = estavtas.EvtClie.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtClie.ProxMesMn = estavtas.EvtClie.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtClie.ProxDiaMn[I] = estavtas.EvtClie.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtClie.ProxDiaMe[I] = estavtas.EvtClie.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR ARTICULO */
        FIND estavtas.EvtArti WHERE estavtas.EvtArti.codcia = s-codcia
            AND estavtas.EvtArti.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtArti.CodMat = estavtas.EvtAll01.CodMat
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE estavtas.EvtArti THEN NEXT.
        ASSIGN
            estavtas.EvtArti.ProxMesMe = estavtas.EvtArti.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtArti.ProxMesMn = estavtas.EvtArti.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtArti.ProxDiaMn[I] = estavtas.EvtArti.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtArti.ProxDiaMe[I] = estavtas.EvtArti.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR VENDEDOR */
        FIND  estavtas.EvtVen WHERE  estavtas.EvtVen.codcia = s-codcia
            AND estavtas.EvtVen.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtVen.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtVen.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtVen.CodVen = estavtas.EvtAll01.CodVen
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtVen THEN NEXT.
        ASSIGN
            estavtas.EvtVen.ProxMesMe = estavtas.EvtVen.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtVen.ProxMesMn = estavtas.EvtVen.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtVen.ProxDiaMn[I] = estavtas.EvtVen.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtVen.ProxDiaMe[I] = estavtas.EvtVen.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR PROVEEDOR */
        FIND  estavtas.EvtProv WHERE  estavtas.EvtProv.codcia = s-codcia
            AND estavtas.EvtProv.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtProv.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtProv.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtProv.CodPro = estavtas.EvtAll01.CodPro
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtProv THEN NEXT.
        ASSIGN
            estavtas.EvtProv.ProxMesMe =  estavtas.EvtProv.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtProv.ProxMesMn =  estavtas.EvtProv.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtProv.ProxDiaMn[I] = estavtas.EvtProv.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtProv.ProxDiaMe[I] = estavtas.EvtProv.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR CONDICION DE VENTA */
        FIND estavtas.EvtFpgo WHERE estavtas.EvtFpgo.codcia = s-codcia
            AND estavtas.EvtFpgo.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtFpgo.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtFpgo.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtFpgo.FmaPgo = estavtas.EvtAll01.FmaPgo
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtFpgo THEN NEXT.
        ASSIGN
            estavtas.EvtFpgo.ProxMesMe = estavtas.EvtFpgo.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtFpgo.ProxMesMn = estavtas.EvtFpgo.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtFpgo.ProxDiaMn[I] = estavtas.EvtFpgo.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtFpgo.ProxDiaMe[I] = estavtas.EvtFpgo.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR ARTICULO Y DIVISION */
        FIND estavtas.EvtArtDv WHERE estavtas.EvtArtDv.codcia = s-codcia
            AND estavtas.EvtArtDv.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtArtDv.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtArtDv.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtArtDv.CodMat = estavtas.EvtAll01.CodMat
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtArtDv THEN NEXT.
        ASSIGN
            estavtas.EvtArtDv.ProxMesMe = estavtas.EvtArtDv.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtArtDv.ProxMesMn = estavtas.EvtArtDv.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtArtDv.ProxDiaMn[I] = estavtas.EvtArtDv.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtArtDv.ProxDiaMe[I] = estavtas.EvtArtDv.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR CANAL DE VENTA */
        FIND estavtas.EvtCnVta WHERE estavtas.EvtCnVta.codcia = s-codcia
            AND estavtas.EvtCnVta.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtCnVta.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtCnVta.CodDiv = estavtas.EvtAll01.CodDiv
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtCnVta THEN NEXT.
        ASSIGN
            estavtas.EvtCnVta.ProxMesMe = estavtas.EvtCnVta.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtCnVta.ProxMesMn = estavtas.EvtCnVta.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtCnVta.ProxDiaMn[I] = estavtas.EvtCnVta.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtCnVta.ProxDiaMe[I] = estavtas.EvtCnVta.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR CLIENTE Y ARTICULO */
        FIND estavtas.EvtClArti WHERE estavtas.EvtClArti.codcia = s-codcia
            AND estavtas.EvtClArti.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtClArti.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtClArti.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtClArti.CodCli = estavtas.EvtAll01.CodCli
            AND estavtas.EvtClArti.CodUnico = estavtas.EvtAll01.CodUnico
            AND estavtas.EvtClArti.CodMat = estavtas.EvtAll01.CodMat
            AND estavtas.EvtClArti.Zona = estavtas.EvtAll01.Zona
            AND estavtas.EvtClArti.Sede = estavtas.EvtAll01.Sede
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtClArti THEN NEXT.
        ASSIGN
            estavtas.EvtClArti.ProxMesMe = estavtas.EvtClArti.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtClArti.ProxMesMn = estavtas.EvtClArti.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtClArti.ProxDiaMn[I] = estavtas.EvtClArti.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtClArti.ProxDiaMe[I] = estavtas.EvtClArti.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR CLIENTE */
        FIND estavtas.EvtCard WHERE estavtas.EvtCard.codcia = s-codcia
            AND estavtas.EvtCard.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtCard.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtCard.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtCard.CodUnico = estavtas.EvtAll01.CodUnico
            AND estavtas.EvtCard.CodCli = estavtas.EvtAll01.CodCli
            AND estavtas.EvtCard.Zona = estavtas.EvtAll01.Zona
            AND estavtas.EvtCard.Sede = estavtas.EvtAll01.Sede
            AND estavtas.EvtCard.NroCard = estavtas.EvtAll01.NroCard
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE estavtas.EvtCard THEN NEXT.
        ASSIGN
            estavtas.EvtCard.ProxMesMe = estavtas.EvtCard.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtCard.ProxMesMn = estavtas.EvtCard.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtCard.ProxDiaMn[I] = estavtas.EvtCard.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtCard.ProxDiaMe[I] = estavtas.EvtCard.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR VENDEDOR Y ARTICULO */
        FIND estavtas.EvtVenArti WHERE estavtas.EvtVenArti.codcia = s-codcia
            AND estavtas.EvtVenArti.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtVenArti.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtVenArti.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtVenArti.CodVen = estavtas.EvtAll01.CodVen
            AND estavtas.EvtVenArti.CodMat = estavtas.EvtAll01.CodMat
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtVenArti THEN NEXT.
        ASSIGN
            estavtas.EvtVenArti.ProxMesMe = estavtas.EvtVenArti.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtVenArti.ProxMesMn = estavtas.EvtVenArti.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtVenArti.ProxDiaMn[I] = estavtas.EvtVenArti.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtVenArti.ProxDiaMe[I] = estavtas.EvtVenArti.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR VENDEDOR Y CLIENTE */
        FIND estavtas.EvtVenCli WHERE estavtas.EvtVenCli.codcia = s-codcia
            AND estavtas.EvtVenCli.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtVenCli.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtVenCli.CodDiv = estavtas.EvtAll01.CodDiv
            AND estavtas.EvtVenCli.CodVen = estavtas.EvtAll01.CodVen
            AND estavtas.EvtVenCli.CodCli = estavtas.EvtAll01.CodCli
            AND estavtas.EvtVenCli.CodUnico = estavtas.EvtAll01.CodUnico
            AND estavtas.EvtVenCli.Zona = estavtas.EvtAll01.Zona
            AND estavtas.EvtVenCli.Sede = estavtas.EvtAll01.Sede
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtVenCli THEN NEXT.
        ASSIGN
            estavtas.EvtVenCli.ProxMesMe = estavtas.EvtVenCli.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtVenCli.ProxMesMn = estavtas.EvtVenCli.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtVenCli.ProxDiaMn[I] = estavtas.EvtVenCli.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtVenCli.ProxDiaMe[I] = estavtas.EvtVenCli.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
    END.
END.
/* ************************* */


