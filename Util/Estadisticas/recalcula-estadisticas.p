DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

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

DEF BUFFER B-CDOCU FOR CcbCdocu.
DEF BUFFER B-DDOCU FOR CcbDdocu.
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
/*
x-CodFchI = 01/01/2010.
x-CodFchF = 12/31/2010.
*/

x-NroFchI = INTEGER(STRING(YEAR(x-CodFchI),"9999") + STRING(MONTH(x-CodFchI),"99")).      
x-NroFchF = INTEGER(STRING(YEAR(x-CodFchF),"9999") + STRING(MONTH(x-CodFchF),"99")).                           

DEFINE VAR pCodDiv AS CHAR NO-UNDO.
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.

DISPLAY x-CodFchI x-CodFchF x-NroFchI x-NroFchF STRING(TIME,'HH:MM') TODAY SKIP.
PAUSE 0.
    
RUN Borra-Estadisticas.         /* BORRAMOS LA INFORMACION HISTORICA */

FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
    DISPLAY gn-divi.coddiv STRING(TIME,'HH:MM') TODAY SKIP.
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
            AND LOOKUP(Ccbcdocu.codven, '015,173,901,902,017') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
        /* ***************** FILTROS ********************************** */
        IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
        IF CcbCDocu.FlgEst = "A"  THEN NEXT.
        IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
/*         IF CcbCDocu.ImpCto = ? THEN DO:              */
/*             RUN Corrige-Costo.                       */
/*             IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT. */
/*         END.                                         */
        IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
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
DISPLAY 'RESUMEN' STRING(TIME,'HH:MM') TODAY SKIP.
PAUSE 0.
RUN Resumen-Estadisticas.

/* TOP 100 */
DISPLAY 'TOP 100' STRING(TIME,'HH:MM') TODAY SKIP.
PAUSE 0.
RUN Top-100.

DISPLAY 'FINALIZADO' STRING(TIME,'HH:MM') TODAY SKIP.
PAUSE 0.

RETURN.

PROCEDURE Corrige-Costo:
 /* ******************** */
    FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(Ccbcdocu) EXCLUSIVE-LOCK NO-WAIT.
    IF ERROR-STATUS:ERROR THEN RETURN 'ADM-ERROR'.
    B-CDOCU.ImpCto = 0.        /* <<< OJO <<< */
    FOR EACH B-DDOCU OF B-CDOCU NO-LOCK:
        B-CDOCU.ImpCto = B-CDOCU.ImpCto + B-DDOCU.ImpCto.
    END.
    IF B-CDOCU.ImpCto = ? THEN B-CDOCU.ImpCto = 0.
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

    FOR EACH estavtas.EvtAll01 WHERE estavtas.EvtAll01.Codcia = S-CODCIA
        AND estavtas.EvtAll01.NroFch >= x-NroFchI
        AND estavtas.EvtAll01.NroFch <= x-NroFchF:
        DELETE estavtas.EvtAll01.
    END.
    FOR EACH estavtas.EvtAll03 WHERE estavtas.EvtAll03.Codcia = S-CODCIA:
         DELETE estavtas.EvtAll03.
    END.
    FOR EACH estavtas.EvtDivi USE-INDEX Indice02 WHERE estavtas.EvtDivi.Codcia = S-CODCIA
        AND estavtas.EvtDivi.NroFch >= x-NroFchI
        AND estavtas.EvtDivi.NroFch <= x-NroFchF:
        DELETE estavtas.EvtDivi.
    END.
    FOR EACH estavtas.EvtClie USE-INDEX Indice02 WHERE estavtas.EvtClie.Codcia = S-CODCIA
        AND estavtas.EvtClie.NroFch >= x-NroFchI
        AND estavtas.EvtClie.NroFch <= x-NroFchF:
        DELETE estavtas.EvtClie.
    END.
    FOR EACH estavtas.EvtArti USE-INDEX Indice02 WHERE estavtas.EvtArti.Codcia = S-CODCIA
        AND estavtas.EvtArti.NroFch >= x-NroFchI
        AND estavtas.EvtArti.NroFch <= x-NroFchF:
        DELETE estavtas.EvtArti.
    END.
    FOR EACH estavtas.EvtVen USE-INDEX Indice02 WHERE estavtas.EvtVen.Codcia = S-CODCIA
        AND estavtas.EvtVen.NroFch >= x-NroFchI
        AND estavtas.EvtVen.NroFch <= x-NroFchF:
        DELETE estavtas.EvtVen.
    END.
    FOR EACH estavtas.EvtProv USE-INDEX Indice02 WHERE estavtas.EvtProv.Codcia = S-CODCIA
        AND estavtas.EvtProv.NroFch >= x-NroFchI
        AND estavtas.EvtProv.NroFch <= x-NroFchF:
        DELETE estavtas.EvtProv.
    END.
    FOR EACH estavtas.EvtFpgo USE-INDEX Indice02 WHERE estavtas.EvtFpgo.Codcia = S-CODCIA
        AND estavtas.EvtFpgo.NroFch >= x-NroFchI
        AND estavtas.EvtFpgo.NroFch <= x-NroFchF:
        DELETE estavtas.EvtFpgo.
    END.
    FOR EACH estavtas.EvtArtDv USE-INDEX Indice02 WHERE estavtas.EvtArtDv.Codcia = S-CODCIA
        AND estavtas.EvtArtDv.NroFch >= x-NroFchI
        AND estavtas.EvtArtDv.NroFch <= x-NroFchF:
        DELETE estavtas.EvtArtDv.
    END.
    FOR EACH estavtas.EvtCnVta USE-INDEX Indice02 WHERE estavtas.EvtCnVta.Codcia = S-CODCIA
        AND estavtas.EvtCnVta.NroFch >= x-NroFchI
        AND estavtas.EvtCnVta.NroFch <= x-NroFchF:
        DELETE estavtas.EvtCnVta.
    END.
    FOR EACH estavtas.EvtClArti USE-INDEX Indice02 WHERE estavtas.EvtClArti.Codcia = S-CODCIA
        AND estavtas.EvtClArti.NroFch >= x-NroFchI
        AND estavtas.EvtClArti.NroFch <= x-NroFchF:
        DELETE estavtas.EvtClArti.
    END.
    FOR EACH estavtas.EvtCard USE-INDEX Indice02 WHERE estavtas.EvtCard.Codcia = S-CODCIA
        AND estavtas.EvtCard.NroFch >= x-NroFchI
        AND estavtas.EvtCard.NroFch <= x-NroFchF:
        DELETE estavtas.EvtCard.
    END.
    FOR EACH estavtas.EvtVenArti USE-INDEX Indice02 WHERE estavtas.EvtVenArti.Codcia = S-CODCIA
        AND estavtas.EvtVenArti.NroFch >= x-NroFchI
        AND estavtas.EvtVenArti.NroFch <= x-NroFchF:
        DELETE estavtas.EvtVenArti.
    END.
    FOR EACH estavtas.EvtVenCli USE-INDEX Indice02 WHERE estavtas.EvtVenCli.Codcia = S-CODCIA
        AND estavtas.EvtVenCli.NroFch >= x-NroFchI
        AND estavtas.EvtVenCli.NroFch <= x-NroFchF:
        DELETE estavtas.EvtVenCli.
    END.
END.

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
    IF NOT AVAILABLE estavtas.EvtAll01 THEN DO:
         CREATE estavtas.EvtAll01.
         ASSIGN
             estavtas.EvtAll01.Codcia = CcbCdocu.Codcia 
             estavtas.EvtAll01.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
             estavtas.EvtAll01.CodDiv = pCoddiv 
             estavtas.EvtAll01.CodAno = x-Year 
             estavtas.EvtAll01.CodMes = x-Month
             estavtas.EvtAll01.CanalVenta = pCanalVenta
             estavtas.EvtAll01.CodUnico = x-CodUnico
             estavtas.EvtAll01.CodCli = x-CodCli
             estavtas.EvtAll01.Zona = x-Zona
             estavtas.EvtAll01.NroCard  = x-NroCard
             estavtas.EvtAll01.Sede   = x-Sede
             estavtas.EvtAll01.CodMat = CcbDdocu.CodMat
             estavtas.EvtAll01.CodPro = Almmmatg.CodPr1
             estavtas.EvtAll01.CodVen = x-codven        /* Ccbcdocu.codven */
             estavtas.EvtAll01.FmaPgo = x-fmapgo.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            estavtas.EvtAll01.CtoxDiaMn[x-Day] = estavtas.EvtAll01.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            estavtas.EvtAll01.VtaxDiaMn[x-Day] = estavtas.EvtAll01.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            estavtas.EvtAll01.CtoxDiaMe[x-Day] = estavtas.EvtAll01.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp)
            estavtas.EvtAll01.VtaxDiaMe[x-Day] = estavtas.EvtAll01.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp)
            estavtas.EvtAll01.CtoxMesMn = estavtas.EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            estavtas.EvtAll01.VtaxMesMn = estavtas.EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            estavtas.EvtAll01.CtoxMesMe = estavtas.EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            estavtas.EvtAll01.VtaxMesMe = estavtas.EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            estavtas.EvtAll01.CtoxDiaMe[x-Day] = estavtas.EvtAll01.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            estavtas.EvtAll01.VtaxDiaMe[x-Day] = estavtas.EvtAll01.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            estavtas.EvtAll01.CtoxDiaMn[x-Day] = estavtas.EvtAll01.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe * x-TpoCmbVta)
            estavtas.EvtAll01.VtaxDiaMn[x-Day] = estavtas.EvtAll01.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe * x-TpoCmbVta)
            estavtas.EvtAll01.CtoxMesMn = estavtas.EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            estavtas.EvtAll01.VtaxMesMn = estavtas.EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            estavtas.EvtAll01.CtoxMesMe = estavtas.EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            estavtas.EvtAll01.VtaxMesMe = estavtas.EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
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
    /* *************************************** */
    ASSIGN            
        estavtas.EvtAll01.CanxDia[x-Day] = estavtas.EvtAll01.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        estavtas.EvtAll01.CanxMes = estavtas.EvtAll01.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can ).
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
        IF NOT AVAILABLE estavtas.EvtDivi THEN DO:
            CREATE estavtas.EvtDivi.
            ASSIGN
                estavtas.EvtDivi.codcia = s-codcia
                estavtas.EvtDivi.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtDivi.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtDivi.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtDivi.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtDivi.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtDivi.Zona = estavtas.EvtAll01.Zona.
        END.
        ASSIGN
            estavtas.EvtDivi.CanxMes   = estavtas.EvtDivi.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtDivi.VtaxMesMe = estavtas.EvtDivi.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtDivi.VtaxMesMn = estavtas.EvtDivi.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtDivi.CtoxMesMe = estavtas.EvtDivi.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtDivi.CtoxMesMn = estavtas.EvtDivi.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtDivi.ProxMesMe = estavtas.EvtDivi.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtDivi.ProxMesMn = estavtas.EvtDivi.ProxMesMn + estavtas.EvtAll01.ProxMesMn.                  
        DO I = 1 TO 31 :
            estavtas.EvtDivi.CanxDia[I]   = estavtas.EvtDivi.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtDivi.VtaxDiaMn[I] = estavtas.EvtDivi.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtDivi.VtaxDiaMe[I] = estavtas.EvtDivi.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtDivi.CtoxDiaMn[I] = estavtas.EvtDivi.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtDivi.CtoxDiaMe[I] = estavtas.EvtDivi.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE estavtas.EvtClie THEN DO:
            CREATE estavtas.EvtClie.
            ASSIGN
                estavtas.EvtClie.codcia = s-codcia
                estavtas.EvtClie.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtClie.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtClie.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtClie.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtClie.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtClie.CodUnico = estavtas.EvtAll01.CodUnico
                estavtas.EvtClie.CodCli = estavtas.EvtAll01.CodCli
                estavtas.EvtClie.Zona = estavtas.EvtAll01.Zona
                estavtas.EvtClie.Sede = estavtas.EvtAll01.Sede.
        END.
        ASSIGN
            estavtas.EvtClie.CanxMes   = estavtas.EvtClie.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtClie.VtaxMesMe = estavtas.EvtClie.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtClie.VtaxMesMn = estavtas.EvtClie.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtClie.CtoxMesMe = estavtas.EvtClie.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtClie.CtoxMesMn = estavtas.EvtClie.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtClie.ProxMesMe = estavtas.EvtClie.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtClie.ProxMesMn = estavtas.EvtClie.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtClie.CanxDia[I]   = estavtas.EvtClie.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtClie.VtaxDiaMn[I] = estavtas.EvtClie.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtClie.VtaxDiaMe[I] = estavtas.EvtClie.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtClie.CtoxDiaMn[I] = estavtas.EvtClie.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtClie.CtoxDiaMe[I] = estavtas.EvtClie.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
            estavtas.EvtClie.ProxDiaMn[I] = estavtas.EvtClie.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtClie.ProxDiaMe[I] = estavtas.EvtClie.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR ARTICULO */
        FIND estavtas.EvtArti WHERE estavtas.EvtArti.codcia = s-codcia
            AND estavtas.EvtArti.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtArti.CodMat = estavtas.EvtAll01.CodMat
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE estavtas.EvtArti THEN DO:
            CREATE estavtas.EvtArti.
            ASSIGN
                estavtas.EvtArti.codcia = s-codcia
                estavtas.EvtArti.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtArti.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtArti.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtArti.CodMat = estavtas.EvtAll01.CodMat.
        END.
        ASSIGN
            estavtas.EvtArti.CanxMes   = estavtas.EvtArti.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtArti.VtaxMesMe = estavtas.EvtArti.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtArti.VtaxMesMn = estavtas.EvtArti.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtArti.CtoxMesMe = estavtas.EvtArti.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtArti.CtoxMesMn = estavtas.EvtArti.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtArti.ProxMesMe = estavtas.EvtArti.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtArti.ProxMesMn = estavtas.EvtArti.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtArti.CanxDia[I]   = estavtas.EvtArti.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtArti.VtaxDiaMn[I] = estavtas.EvtArti.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtArti.VtaxDiaMe[I] = estavtas.EvtArti.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtArti.CtoxDiaMn[I] = estavtas.EvtArti.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtArti.CtoxDiaMe[I] = estavtas.EvtArti.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE  estavtas.EvtVen THEN DO:
            CREATE  estavtas.EvtVen.
            ASSIGN
                estavtas.EvtVen.codcia = s-codcia
                estavtas.EvtVen.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtVen.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtVen.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtVen.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtVen.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtVen.CodVen = estavtas.EvtAll01.CodVen.
        END.
        ASSIGN
            estavtas.EvtVen.CanxMes   = estavtas.EvtVen.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtVen.VtaxMesMe = estavtas.EvtVen.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtVen.VtaxMesMn = estavtas.EvtVen.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtVen.CtoxMesMe = estavtas.EvtVen.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtVen.CtoxMesMn = estavtas.EvtVen.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtVen.ProxMesMe = estavtas.EvtVen.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtVen.ProxMesMn = estavtas.EvtVen.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtVen.CanxDia[I]   = estavtas.EvtVen.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtVen.VtaxDiaMn[I] = estavtas.EvtVen.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtVen.VtaxDiaMe[I] = estavtas.EvtVen.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtVen.CtoxDiaMn[I] = estavtas.EvtVen.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtVen.CtoxDiaMe[I] = estavtas.EvtVen.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE  estavtas.EvtProv THEN DO:
            CREATE  estavtas.EvtProv.
            ASSIGN
                estavtas.EvtProv.codcia = s-codcia
                estavtas.EvtProv.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtProv.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtProv.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtProv.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtProv.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtProv.CodPro = estavtas.EvtAll01.CodPro.
        END.
        ASSIGN
            estavtas.EvtProv.CanxMes   = estavtas.EvtProv.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtProv.VtaxMesMe =  estavtas.EvtProv.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtProv.VtaxMesMn =  estavtas.EvtProv.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtProv.CtoxMesMe =  estavtas.EvtProv.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtProv.CtoxMesMn =  estavtas.EvtProv.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtProv.ProxMesMe =  estavtas.EvtProv.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtProv.ProxMesMn =  estavtas.EvtProv.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtProv.CanxDia[I]   = estavtas.EvtProv.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtProv.VtaxDiaMn[I] = estavtas.EvtProv.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtProv.VtaxDiaMe[I] = estavtas.EvtProv.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtProv.CtoxDiaMn[I] = estavtas.EvtProv.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtProv.CtoxDiaMe[I] = estavtas.EvtProv.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE  estavtas.EvtFpgo THEN DO:
            CREATE  estavtas.EvtFpgo.
            ASSIGN
                estavtas.EvtFpgo.codcia = s-codcia
                estavtas.EvtFpgo.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtFpgo.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtFpgo.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtFpgo.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtFpgo.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtFpgo.FmaPgo = estavtas.EvtAll01.FmaPgo.
        END.
        ASSIGN
            estavtas.EvtFpgo.CanxMes   = estavtas.EvtFpgo.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtFpgo.VtaxMesMe = estavtas.EvtFpgo.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtFpgo.VtaxMesMn = estavtas.EvtFpgo.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtFpgo.CtoxMesMe = estavtas.EvtFpgo.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtFpgo.CtoxMesMn = estavtas.EvtFpgo.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtFpgo.ProxMesMe = estavtas.EvtFpgo.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtFpgo.ProxMesMn = estavtas.EvtFpgo.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtFpgo.CanxDia[I]   = estavtas.EvtFpgo.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtFpgo.VtaxDiaMn[I] = estavtas.EvtFpgo.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtFpgo.VtaxDiaMe[I] = estavtas.EvtFpgo.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtFpgo.CtoxDiaMn[I] = estavtas.EvtFpgo.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtFpgo.CtoxDiaMe[I] = estavtas.EvtFpgo.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE  estavtas.EvtArtDv THEN DO:
            CREATE  estavtas.EvtArtDv.
            ASSIGN
                estavtas.EvtArtDv.codcia = s-codcia
                estavtas.EvtArtDv.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtArtDv.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtArtDv.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtArtDv.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtArtDv.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtArtDv.CodMat = estavtas.EvtAll01.CodMat.
        END.
        ASSIGN
            estavtas.EvtArtDv.CanxMes   = estavtas.EvtArtDv.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtArtDv.VtaxMesMe = estavtas.EvtArtDv.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtArtDv.VtaxMesMn = estavtas.EvtArtDv.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtArtDv.CtoxMesMe = estavtas.EvtArtDv.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtArtDv.CtoxMesMn = estavtas.EvtArtDv.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtArtDv.ProxMesMe = estavtas.EvtArtDv.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtArtDv.ProxMesMn = estavtas.EvtArtDv.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtArtDv.CanxDia[I]   = estavtas.EvtArtDv.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtArtDv.VtaxDiaMn[I] = estavtas.EvtArtDv.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtArtDv.VtaxDiaMe[I] = estavtas.EvtArtDv.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtArtDv.CtoxDiaMn[I] = estavtas.EvtArtDv.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtArtDv.CtoxDiaMe[I] = estavtas.EvtArtDv.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
            estavtas.EvtArtDv.ProxDiaMn[I] = estavtas.EvtArtDv.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtArtDv.ProxDiaMe[I] = estavtas.EvtArtDv.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
        /* VENTAS POR CANAL DE VENTA */
        FIND estavtas.EvtCnVta WHERE estavtas.EvtCnVta.codcia = s-codcia
            AND estavtas.EvtCnVta.NroFch = estavtas.EvtAll01.NroFch
            AND estavtas.EvtCnVta.CanalVenta = estavtas.EvtAll01.CanalVenta
            AND estavtas.EvtCnVta.CodDiv = estavtas.EvtAll01.CodDiv
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE  estavtas.EvtCnVta THEN DO:
            CREATE  estavtas.EvtCnVta.
            ASSIGN
                estavtas.EvtCnVta.codcia = s-codcia
                estavtas.EvtCnVta.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtCnVta.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtCnVta.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtCnVta.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtCnVta.CodDiv = estavtas.EvtAll01.CodDiv.
        END.
        ASSIGN
            estavtas.EvtCnVta.CanxMes   = estavtas.EvtCnVta.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtCnVta.VtaxMesMe = estavtas.EvtCnVta.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtCnVta.VtaxMesMn = estavtas.EvtCnVta.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtCnVta.CtoxMesMe = estavtas.EvtCnVta.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtCnVta.CtoxMesMn = estavtas.EvtCnVta.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtCnVta.ProxMesMe = estavtas.EvtCnVta.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtCnVta.ProxMesMn = estavtas.EvtCnVta.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtCnVta.CanxDia[I]   = estavtas.EvtCnVta.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtCnVta.VtaxDiaMn[I] = estavtas.EvtCnVta.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtCnVta.VtaxDiaMe[I] = estavtas.EvtCnVta.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtCnVta.CtoxDiaMn[I] = estavtas.EvtCnVta.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtCnVta.CtoxDiaMe[I] = estavtas.EvtCnVta.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE  estavtas.EvtClArti THEN DO:
            CREATE  estavtas.EvtClArti.
            ASSIGN
                estavtas.EvtClArti.codcia = s-codcia
                estavtas.EvtClArti.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtClArti.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtClArti.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtClArti.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtClArti.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtClArti.CodCli = estavtas.EvtAll01.CodCli
                estavtas.EvtClArti.CodUnico = estavtas.EvtAll01.CodUnico
                estavtas.EvtClArti.Zona = estavtas.EvtAll01.Zona
                estavtas.EvtClArti.Sede = estavtas.EvtAll01.Sede
                estavtas.EvtClArti.CodMat = estavtas.EvtAll01.CodMat.
        END.
        ASSIGN
            estavtas.EvtClArti.CanxMes   = estavtas.EvtClArti.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtClArti.VtaxMesMe = estavtas.EvtClArti.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtClArti.VtaxMesMn = estavtas.EvtClArti.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtClArti.CtoxMesMe = estavtas.EvtClArti.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtClArti.CtoxMesMn = estavtas.EvtClArti.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtClArti.ProxMesMe = estavtas.EvtClArti.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtClArti.ProxMesMn = estavtas.EvtClArti.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtClArti.CanxDia[I]   = estavtas.EvtClArti.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtClArti.VtaxDiaMn[I] = estavtas.EvtClArti.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtClArti.VtaxDiaMe[I] = estavtas.EvtClArti.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtClArti.CtoxDiaMn[I] = estavtas.EvtClArti.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtClArti.CtoxDiaMe[I] = estavtas.EvtClArti.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE estavtas.EvtCard THEN DO:
            CREATE estavtas.EvtCard.
            ASSIGN
                estavtas.EvtCard.codcia = s-codcia
                estavtas.EvtCard.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtCard.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtCard.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtCard.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtCard.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtCard.CodUnico = estavtas.EvtAll01.CodUnico
                estavtas.EvtCard.CodCli = estavtas.EvtAll01.CodCli
                estavtas.EvtCard.Zona = estavtas.EvtAll01.Zona
                estavtas.EvtCard.Sede = estavtas.EvtAll01.Sede
                estavtas.EvtCard.NroCard = estavtas.EvtAll01.NroCard.
        END.
        ASSIGN
            estavtas.EvtCard.CanxMes   = estavtas.EvtCard.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtCard.VtaxMesMe = estavtas.EvtCard.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtCard.VtaxMesMn = estavtas.EvtCard.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtCard.CtoxMesMe = estavtas.EvtCard.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtCard.CtoxMesMn = estavtas.EvtCard.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtCard.ProxMesMe = estavtas.EvtCard.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtCard.ProxMesMn = estavtas.EvtCard.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtCard.CanxDia[I]   = estavtas.EvtCard.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtCard.VtaxDiaMn[I] = estavtas.EvtCard.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtCard.VtaxDiaMe[I] = estavtas.EvtCard.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtCard.CtoxDiaMn[I] = estavtas.EvtCard.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtCard.CtoxDiaMe[I] = estavtas.EvtCard.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE  estavtas.EvtVenArti THEN DO:
            CREATE  estavtas.EvtVenArti.
            ASSIGN
                estavtas.EvtVenArti.codcia = s-codcia
                estavtas.EvtVenArti.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtVenArti.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtVenArti.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtVenArti.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtVenArti.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtVenArti.CodVen = estavtas.EvtAll01.CodVen
                estavtas.EvtVenArti.CodMat = estavtas.EvtAll01.CodMat.
        END.
        ASSIGN
            estavtas.EvtVenArti.CanxMes   = estavtas.EvtVenArti.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtVenArti.VtaxMesMe = estavtas.EvtVenArti.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtVenArti.VtaxMesMn = estavtas.EvtVenArti.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtVenArti.CtoxMesMe = estavtas.EvtVenArti.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtVenArti.CtoxMesMn = estavtas.EvtVenArti.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtVenArti.ProxMesMe = estavtas.EvtVenArti.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtVenArti.ProxMesMn = estavtas.EvtVenArti.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtVenArti.CanxDia[I]   = estavtas.EvtVenArti.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtVenArti.VtaxDiaMn[I] = estavtas.EvtVenArti.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtVenArti.VtaxDiaMe[I] = estavtas.EvtVenArti.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtVenArti.CtoxDiaMn[I] = estavtas.EvtVenArti.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtVenArti.CtoxDiaMe[I] = estavtas.EvtVenArti.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
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
        IF NOT AVAILABLE  estavtas.EvtVenCli THEN DO:
            CREATE  estavtas.EvtVenCli.
            ASSIGN
                estavtas.EvtVenCli.codcia = s-codcia
                estavtas.EvtVenCli.CodAno = estavtas.EvtAll01.CodAno
                estavtas.EvtVenCli.CodMes = estavtas.EvtAll01.CodMes
                estavtas.EvtVenCli.NroFch = estavtas.EvtAll01.NroFch
                estavtas.EvtVenCli.CanalVenta = estavtas.EvtAll01.CanalVenta
                estavtas.EvtVenCli.CodDiv = estavtas.EvtAll01.CodDiv
                estavtas.EvtVenCli.CodVen = estavtas.EvtAll01.CodVen
                estavtas.EvtVenCli.CodCli = estavtas.EvtAll01.CodCli
                estavtas.EvtVenCli.CodUnico = estavtas.EvtAll01.CodUnico
                estavtas.EvtVenCli.Zona = estavtas.EvtAll01.Zona
                estavtas.EvtVenCli.Sede = estavtas.EvtAll01.Sede.
        END.
        ASSIGN
            estavtas.EvtVenCli.CanxMes   = estavtas.EvtVenCli.CanxMes   + estavtas.EvtAll01.CanxMes
            estavtas.EvtVenCli.VtaxMesMe = estavtas.EvtVenCli.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
            estavtas.EvtVenCli.VtaxMesMn = estavtas.EvtVenCli.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
            estavtas.EvtVenCli.CtoxMesMe = estavtas.EvtVenCli.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
            estavtas.EvtVenCli.CtoxMesMn = estavtas.EvtVenCli.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
            estavtas.EvtVenCli.ProxMesMe = estavtas.EvtVenCli.ProxMesMe + estavtas.EvtAll01.ProxMesMe
            estavtas.EvtVenCli.ProxMesMn = estavtas.EvtVenCli.ProxMesMn + estavtas.EvtAll01.ProxMesMn.
        DO I = 1 TO 31 :
            estavtas.EvtVenCli.CanxDia[I]   = estavtas.EvtVenCli.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
            estavtas.EvtVenCli.VtaxDiaMn[I] = estavtas.EvtVenCli.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
            estavtas.EvtVenCli.VtaxDiaMe[I] = estavtas.EvtVenCli.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
            estavtas.EvtVenCli.CtoxDiaMn[I] = estavtas.EvtVenCli.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
            estavtas.EvtVenCli.CtoxDiaMe[I] = estavtas.EvtVenCli.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
            estavtas.EvtVenCli.ProxDiaMn[I] = estavtas.EvtVenCli.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
            estavtas.EvtVenCli.ProxDiaMe[I] = estavtas.EvtVenCli.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
        END.
    END.
END.
/* ************************* */


PROCEDURE TOP-100:
/* ************** */

    DEF VAR k AS INT NO-UNDO.

    /* CALCULAMOS las ventas de los 3 últimos años */
    ASSIGN
        x-NroFchI = INTEGER(STRING(YEAR(TODAY) - 3,"9999") + STRING(MONTH(TODAY),"99"))
        x-NroFchF = INTEGER(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")).                           

    FOR EACH estavtas.EvtClie NO-LOCK USE-INDEX Indice02 WHERE estavtas.EvtClie.codcia = s-codcia
            AND estavtas.EvtClie.nrofch >= x-NroFchI
            AND estavtas.EvtClie.nrofch <= x-NroFchF:
        ASSIGN
            x-CodUnico = estavtas.EvtClie.CodUnico
            x-CodCli   = estavtas.EvtClie.CodUnico.
        FIND estavtas.Evtall03 WHERE estavtas.Evtall03.codcia = EvtClie.codcia
            AND estavtas.Evtall03.codunico = x-CodUnico
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE estavtas.Evtall03 THEN CREATE estavtas.EvtALL03.
        ASSIGN
            estavtas.EvtALL03.CodCia = estavtas.EvtClie.codcia
            estavtas.EvtALL03.CodCli = x-CodCli
            estavtas.EvtALL03.CodUnico = x-CodUnico
            estavtas.EvtALL03.VtaxMesMe = estavtas.EvtALL03.VtaxMesMe + estavtas.EvtClie.VtaxMesMe
            estavtas.EvtALL03.VtaxMesMn = estavtas.EvtALL03.VtaxMesMn + estavtas.EvtClie.VtaxMesMn.
    END.

END.
