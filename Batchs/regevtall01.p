/* REGULARIZAR LA INFORMACION DE LAS ESTADISTICA EVTALL01..04 */

DISABLE TRIGGERS FOR LOAD OF EvtAll01.
DISABLE TRIGGERS FOR LOAD OF EvtAll02.
DISABLE TRIGGERS FOR LOAD OF EvtAll03.

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

DEF VAR x-fmapgo    as char.
DEF VAR x-canal     as char.
DEF VAR x-CodUnico  LIKE Gn-clie.CodUnico.
DEF VAR x-NroCard   LIKE GN-card.NroCard.
DEF VAR x-Sede      LIKE Gn-ClieD.Sede.
DEF VAR cl-CodCia   AS INT NO-UNDO.
DEF VAR x-CodCli    LIKE Gn-clie.codcli.

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
x-CodFchI = 01/01/2009.
/* *********************************************************** */

x-NroFchI = INTEGER(STRING(YEAR(x-CodFchI),"9999") + STRING(MONTH(x-CodFchI),"99")).      
x-NroFchF = INTEGER(STRING(YEAR(x-CodFchF),"9999") + STRING(MONTH(x-CodFchF),"99")).                           

DEFINE VAR pCodDiv AS CHAR NO-UNDO.

DISPLAY x-CodFchI x-CodFchF x-NroFchI x-NroFchF.
PAUSE 0.
    
RUN Borra-Estadisticas.         /* BORRAMOS LA INFORMACION HISTORICA */

FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
    /* Barremos las ventas */
    FOR EACH CcbCdocu NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
            AND CcbCdocu.CodDiv = Gn-Divi.CodDiv
            AND CcbCdocu.FchDoc >= x-CodFchI
            AND CcbCdocu.FchDoc <= x-CodFchF
            AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0      /* NO facturas adelantadas NI servicios */
            USE-INDEX llave10
            BREAK BY CcbCdocu.CodCia BY CcbCdocu.CodDiv BY CcbCdocu.FchDoc:
        pCodDiv = Ccbcdocu.coddiv.      /* VALOR POR DEFECTO */
        IF Ccbcdocu.coddiv <> '00017' 
            AND Ccbcdocu.codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
        IF Ccbcdocu.coddiv <> '00018' 
            AND LOOKUP(Ccbcdocu.codven, '015,179,901,902') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.
        /* ***************** FILTROS ********************************** */
        IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
        IF CcbCDocu.FlgEst = "A"  THEN NEXT.
        IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
        IF CcbCDocu.ImpCto = ? THEN DO: 
            RUN Corrige-Costo.
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        END.
        IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
            /* RHC 15.03.10 NO por rEbate */
            IF Ccbcdocu.TpoFac <> "E" THEN DO:
                FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
                    AND B-CDOCU.CodDoc = CcbCdocu.Codref 
                    AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE B-CDOCU THEN NEXT.
                IF LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN NEXT.     /* NO SERVICIOS NI ADELANTADAS */
            END.
        END.
        /* *********************************************************** */
        DISPLAY CcbCdocu.Codcia CcbCdocu.Coddiv CcbCdocu.FchDoc  CcbCdocu.CodDoc CcbCdocu.NroDoc
                STRING(TIME,'HH:MM') TODAY.
        PAUSE 0.
 
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
            X-FMAPGO = CcbCdocu.FmaPgo
            x-NroCard = Ccbcdocu.nrocard
            x-Sede = Ccbcdocu.Sede 
            x-Canal = ''
            x-CodCli = Ccbcdocu.CodCli
            x-CodUnico = Ccbcdocu.codcli.

        IF CcbCdocu.Coddoc = "N/C" THEN DO:
           FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
               AND B-CDOCU.Coddoc = CcbCdocu.Codref 
               AND B-CDOCU.NroDoc = CcbCdocu.Nroref
               NO-LOCK NO-ERROR.
           IF AVAILABLE B-CDOCU THEN DO:
               X-FMAPGO = B-CDOCU.FmaPgo.
               x-NroCard = B-CDOCU.NroCard.
               x-Sede = B-CDOCU.Sede.
           END.
        END.
        
        FIND gn-clie WHERE gn-clie.codcia = cl-codcia     /* OJO */
            AND gn-clie.codcli = Ccbcdocu.codcli
            NO-LOCK NO-ERROR.
        IF AVAILABLE gn-clie AND gn-clie.codunico <> '' THEN x-codunico = gn-clie.codunico.
        IF AVAILABLE Gn-clie THEN x-canal = gn-clie.Canal.

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

/* TOP 100 */
RUN Top-100.

RETURN.

PROCEDURE Corrige-Costo:
/* ******************** */
    FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(Ccbcdocu) EXCLUSIVE-LOCK NO-WAIT.
    IF ERROR-STATUS:ERROR THEN RETURN 'ADM-ERROR'.
    B-CDOCU.ImpCto = 0.        /* <<< OJO <<< */
    RELEASE B-CDOCU.

END.
/* ******************** */

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


PROCEDURE Borra-Estadisticas:
/* ************************* */

    FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
        FOR EACH EvtAll01 WHERE EvtAll01.Codcia = S-CODCIA
                            AND EvtAll01.CodDiv = Gn-Divi.CodDiv
                            AND EvtAll01.NroFch >= x-NroFchI
                            AND EvtAll01.NroFch <= x-NroFchF:
             DELETE EvtAll01.
        END.
    END.
    FOR EACH EvtAll02 WHERE EvtAll02.Codcia = S-CODCIA:
         DELETE EvtAll02.
    END.
    FOR EACH EvtAll03 WHERE EvtAll03.Codcia = S-CODCIA:
         DELETE EvtAll03.
    END.
    FOR EACH EvtAll04 WHERE EvtAll04.Codcia = S-CODCIA:
         DELETE EvtAll04.
    END.

END.

PROCEDURE Carga-Estadisticas-2:
/* **************************** */

    FIND EvtAll01 WHERE EvtAll01.Codcia = CcbCdocu.Codcia 
        AND EvtAll01.CodDiv = pCoddiv 
        AND EvtAll01.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        AND EvtAll01.CodUnico = x-CodUnico
        AND EvtAll01.NroCard  = x-NroCard
        AND EvtAll01.CodCli = x-CodCli
        AND EvtAll01.Sede = x-Sede 
        AND EvtAll01.CodVen = Ccbcdocu.codven
        AND EvtAll01.FmaPgo = x-fmapgo
        AND EvtAll01.CodMat = CcbDdocu.CodMat 
        NO-ERROR.     
    IF NOT AVAILABLE EvtAll01 THEN DO:
         CREATE EvtAll01.
         ASSIGN
             EvtAll01.Codcia = CcbCdocu.Codcia 
             EvtAll01.CodDiv = pCoddiv 
             EvtAll01.CodAno = x-Year 
             EvtAll01.CodMes = x-Month
             EvtAll01.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
             EvtAll01.CodUnico = x-CodUnico
             EvtAll01.NroCard  = x-NroCard
             EvtAll01.CodCli = x-CodCli
             EvtAll01.Sede   = x-Sede
             EvtAll01.CodVen = Ccbcdocu.codven
             EvtAll01.FmaPgo = x-fmapgo
             EvtAll01.CodMat = CcbDdocu.CodMat
             EvtALL01.Canal  = x-Canal
             EvtALL01.CodDept = gn-clie.CodDept
             EvtALL01.CodDist = gn-clie.CodDist
             EvtALL01.codfam = Almmmatg.codfam
             EvtALL01.CodPais = gn-clie.CodPais
             EvtALL01.CodPro = Almmmatg.codpr1
             EvtALL01.CodProv = gn-clie.CodProv
             EvtALL01.subfam = Almmmatg.subfam
             EvtALL01.DesMar = Almmmatg.desmar.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtAll01.CtoxDiaMn[x-Day] = EvtAll01.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtAll01.VtaxDiaMn[x-Day] = EvtAll01.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtAll01.CtoxDiaMe[x-Day] = EvtAll01.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp)
            EvtAll01.VtaxDiaMe[x-Day] = EvtAll01.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp)
            EvtAll01.CtoxMesMn = EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtAll01.VtaxMesMn = EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            EvtAll01.CtoxMesMe = EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            EvtAll01.VtaxMesMe = EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtAll01.CtoxDiaMe[x-Day] = EvtAll01.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtAll01.VtaxDiaMe[x-Day] = EvtAll01.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtAll01.CtoxDiaMn[x-Day] = EvtAll01.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe * x-TpoCmbVta)
            EvtAll01.VtaxDiaMn[x-Day] = EvtAll01.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe * x-TpoCmbVta)
            EvtAll01.CtoxMesMn = EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            EvtAll01.VtaxMesMn = EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            EvtAll01.CtoxMesMe = EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtAll01.VtaxMesMe = EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        EvtAll01.CanxDia[x-Day] = EvtAll01.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        EvtAll01.CanxMes = EvtAll01.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can ).
END.
/* ************************* */


PROCEDURE TOP-100:
/* ************** */

    DEF VAR k AS INT NO-UNDO.

    /* CALCULAMOS las ventas de los 3 últimos años */
    ASSIGN
        x-NroFchI = INTEGER(STRING(YEAR(TODAY) - 3,"9999") + STRING(MONTH(TODAY),"99"))
        x-NroFchF = INTEGER(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")).                           

    FOR EACH Gn-divi NO-LOCK WHERE Gn-divi.codcia = s-codcia:
        FOR EACH Evtall01 NO-LOCK WHERE Evtall01.codcia = s-codcia
                AND Evtall01.nrofch >= x-NroFchI
                AND Evtall01.nrofch <= x-NroFchF
                AND Evtall01.coddiv = GN-divi.coddiv:
            ASSIGN
                x-CodUnico = Evtall01.CodUnico
                x-CodCli   = Evtall01.CodUnico.
            FIND Evtall03 WHERE Evtall03.codcia = Evtall01.codcia
                AND Evtall03.codunico = x-CodUnico
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Evtall03 THEN CREATE EvtALL03.
            ASSIGN
                EvtALL03.CodCia = Evtall01.codcia
                EvtALL03.CodCli = x-CodCli
                EvtALL03.CodUnico = x-CodUnico
                EvtALL03.VtaxMesMe = EvtALL03.VtaxMesMe + Evtall01.VtaxMesMe
                EvtALL03.VtaxMesMn = EvtALL03.VtaxMesMn + Evtall01.VtaxMesMn.
            RELEASE Evtall03.
            FIND Evtall04 WHERE Evtall04.codcia = Evtall01.codcia
                AND Evtall04.codunico = x-CodUnico
                AND Evtall04.CodCli   = Evtall01.CodCli
                AND Evtall04.NroFch   = Evtall01.NroFch
                AND Evtall04.coddiv   = Evtall01.coddiv
                AND Evtall04.CodFam   = Evtall01.CodFam
                AND Evtall04.CodVen   = Evtall01.CodVen
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Evtall04 THEN CREATE Evtall04.
            ASSIGN
                Evtall04.CodCia    = Evtall01.codcia
                Evtall04.CodUnico  = x-CodUnico
                Evtall04.CodCli    = Evtall01.CodCli
                Evtall04.NroFch    = Evtall01.NroFch
                EvtALL04.CodDiv    = Evtall01.CodDiv
                EvtALL04.codfam    = Evtall01.CodFam
                EvtAll04.CodVen    = Evtall01.CodVen
                EvtALL04.Codano    = evtall01.codano               
                EvtALL04.Codmes    = Evtall01.codmes
                EvtALL04.CanxMes   = EvtALL04.CanxMes + EvtALL01.CanxMes 
                Evtall04.VtaxMesMe = Evtall04.VtaxMesMe + Evtall01.VtaxMesMe
                Evtall04.VtaxMesMn = Evtall04.VtaxMesMn + Evtall01.VtaxMesMn.
            RELEASE Evtall04.
        END.
    END.

    /* CREAMOS EL ARCHIVO RESUMEN DE LOS TOP 100 */
    DEF VAR x-Cuenta AS INT NO-UNDO.

    FOR EACH EvtALL03 USE-INDEX Indice02 NO-LOCK WHERE Evtall03.codcia = s-codcia
            BY Evtall03.VtaxMesMn DESC:
        x-Cuenta = x-Cuenta + 1.
        /* Definimos si va al detalle o unificados */
        ASSIGN
            x-CodUnico = Evtall03.codunico
            x-CodCli   = Evtall03.codcli.
        IF x-Cuenta > 200           /* Resumimos por el cliente unificado */
        THEN ASSIGN
                x-CodUnico = s-CliUni
                x-CodCli   = s-CliUni.
        DISPLAY 'top 200' x-cuenta evtall03.codunico x-codunico STRING(TIME,'hh:mm')
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        /* Acumulamos la información detallada por cada año */
        x-Month = MONTH(TODAY).     /* Mes de partida */
        DO k = YEAR(TODAY) - 3 TO YEAR(TODAY):
            x-Year = k.
            REPEAT WHILE x-Month <= 12:
                x-NroFchI = x-Year * 100 + x-Month.
                FOR EACH Gn-divi NO-LOCK WHERE Gn-divi.codcia = s-codcia:
                    FOR EACH Evtall01 NO-LOCK WHERE Evtall01.codcia = Evtall03.codcia
                            AND Evtall01.nrofch = x-NroFchI
                            AND Evtall01.coddiv = Gn-divi.CodDiv
                            AND Evtall01.codunico = EvtAll03.CodUnico:
                        FIND EvtAll02 USE-INDEX Indice05 WHERE EvtAll02.Codcia = Evtall01.Codcia
                            AND EvtAll02.CodUnico = x-CodUnico
                            AND EvtAll02.CodDiv = Evtall01.Coddiv
                            AND EvtAll02.Nrofch = Evtall01.NroFch
                            /*AND EvtAll02.FmaPgo = Evtall01.FmaPgo*/
                            AND EvtAll02.CodFam = Evtall01.CodFam
                            /*AND EvtAll02.SubFam = Evtall01.SubFam*/
                            AND EvtAll02.DesMar = Evtall01.DesMar
                            EXCLUSIVE-LOCK
                            NO-ERROR.
                        IF NOT AVAILABLE EvtAll02 THEN DO:
                             CREATE EvtAll02.
                             BUFFER-COPY EvtAll01
                                 EXCEPT
                                    EvtAll01.CodCli
                                    EvtAll01.CodUnico
                                    EvtAll01.NroCard
                                    EvtAll01.Sede
                                    EvtAll01.CodVen
                                    EvtAll01.CodMat
                                    EvtAll01.SubFam
                                    EvtAll01.FmaPgo
                                    EvtAll01.Canal
                                    EvtAll01.CodPais
                                    EvtAll01.CodDept
                                    EvtAll01.CodProv
                                    EvtAll01.CodDist
                                    EvtAll01.CtoxDiaMn
                                    EvtAll01.VtaxDiaMn
                                    EvtAll01.CtoxMesMn
                                    EvtAll01.VtaxMesMn
                                    EvtAll01.CtoxDiaMe
                                    EvtAll01.VtaxDiaMe
                                    EvtAll01.CtoxMesMe
                                    EvtAll01.VtaxMesMe
                                    EvtAll01.CanxDia
                                    EvtAll01.CanxMes
                                 TO EvtAll02
                                 ASSIGN
                                    /*EvtAll02.CodCli = x-CodCli*/
                                    EvtAll02.CodUnico = x-CodUnico
                                    EvtAll02.Ranking = x-Cuenta.
                        END.
                        ASSIGN
                            EvtAll02.CtoxMesMn = EvtAll02.CtoxMesMn + EvtAll01.CtoxMesMn
                            EvtAll02.VtaxMesMn = EvtAll02.VtaxMesMn + EvtAll01.VtaxMesMn
                            EvtAll02.CtoxMesMe = EvtAll02.CtoxMesMe + EvtAll01.CtoxMesMe
                            EvtAll02.VtaxMesMe = EvtAll02.VtaxMesMe + EvtAll01.VtaxMesMe
                            EvtAll02.CanxMes = EvtAll02.CanxMes + EvtAll01.CanxMes.
                    END.
                END.    /* Gn-Divi */
                x-Month = x-Month + 1.
            END.    /* REPEAT */
            x-Month = 01.
        END.
        /* FIN ciclo de acumulacion */
    END.    /* EvtALL03 */

END.
/* ************** */


    /* CALCULAMOS las ventas de los 3 últimos años */
    ASSIGN
        x-NroFchI = INTEGER(STRING(YEAR(TODAY) - 3,"9999") + STRING(MONTH(TODAY),"99"))
        x-NroFchF = INTEGER(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")).                           

    FOR EACH Gn-divi NO-LOCK WHERE Gn-divi.codcia = s-codcia:
        FOR EACH Evtall01 NO-LOCK WHERE Evtall01.codcia = s-codcia
                AND Evtall01.nrofch >= x-NroFchI
                AND Evtall01.nrofch <= x-NroFchF
                AND Evtall01.coddiv = GN-divi.coddiv:
            ASSIGN
                x-CodUnico = Evtall01.CodUnico
                x-CodCli   = Evtall01.CodUnico.
            FIND Evtall04 WHERE Evtall04.codcia = Evtall01.codcia
                AND Evtall04.codunico = x-CodUnico
                AND Evtall04.CodCli   = Evtall01.CodCli
                AND Evtall04.NroFch   = Evtall01.NroFch
                AND Evtall04.coddiv   = Evtall01.coddiv
                AND Evtall04.CodFam   = Evtall01.CodFam
                AND Evtall04.CodVen   = Evtall01.CodVen
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Evtall04 THEN CREATE Evtall04.
            ASSIGN
                Evtall04.CodCia    = Evtall01.codcia
                Evtall04.CodUnico  = x-CodUnico
                Evtall04.CodCli    = x-CodCli
                Evtall04.NroFch    = Evtall01.NroFch
                EvtALL04.CodDiv    = Evtall01.CodDiv
                EvtALL04.codfam    = Evtall01.CodFam
                EvtAll04.CodVen    = Evtall01.CodVen
                EvtALL04.Codano    = evtall01.codano               
                EvtALL04.Codmes    = Evtall01.codmes
                EvtALL04.CanxMes   = EvtALL04.CanxMes + EvtALL01.CanxMes 
                Evtall04.VtaxMesMe = Evtall04.VtaxMesMe + Evtall01.VtaxMesMe
                Evtall04.VtaxMesMn = Evtall04.VtaxMesMn + Evtall01.VtaxMesMn.
            RELEASE Evtall04.
        END.
    END. 
