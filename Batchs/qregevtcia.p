DISABLE TRIGGERS FOR LOAD OF integral.EvtDivi.
DISABLE TRIGGERS FOR LOAD OF integral.EvtArti.
DISABLE TRIGGERS FOR LOAD OF integral.EvtClie.
DISABLE TRIGGERS FOR LOAD OF integral.EvtFpgo.
DISABLE TRIGGERS FOR LOAD OF integral.EvtClFpgo.
DISABLE TRIGGERS FOR LOAD OF integral.EvtVend.
DISABLE TRIGGERS FOR LOAD OF integral.EvtClArti.
DISABLE TRIGGERS FOR LOAD OF integral.EvtAll01.
DISABLE TRIGGERS FOR LOAD OF integral.EvtAll02.
DISABLE TRIGGERS FOR LOAD OF integral.EvtAll03.
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.

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
/*
x-CodFchI = 07/01/2010.
*/

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
        /* BLOQUEADO POR AHORA
        IF Ccbcdocu.coddiv <> '00017' 
            AND Ccbcdocu.codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
        IF Ccbcdocu.coddiv <> '00018' 
            AND LOOKUP(Ccbcdocu.codven, '015,179,901,902') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.
        ************************ */
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
        
/*         RUN Carga-Estadisticas.     /* Estadísticas SIN materiales */ */

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

           RUN Carga-Estadisticas.     /* Estadísticas SIN materiales */
           RUN Carga-Estadisticas-2.    /* Estadísticas CON materiales */

       END.  
    END.
END.

/* Estadisticas Resumidas */
RUN Resumen-Estadisticas.

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

    RUN Carga-Estadisticas.     /* Estadísticas SIN materiales */
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

        RUN Carga-Estadisticas.     /* Estadísticas SIN materiales */
        RUN Carga-Estadisticas-2.    /* Estadísticas CON materiales */

    END.  
END.
/* **************************************************** */


PROCEDURE Borra-Estadisticas:
/* ************************* */

    FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
        FOR EACH integral.EvtDivi WHERE integral.EvtDivi.Codcia = S-CODCIA
                           AND integral.EvtDivi.CodDiv = Gn-Divi.CodDiv
                           AND integral.EvtDivi.NroFch >= x-NroFchI
                           AND integral.EvtDivi.NroFch <= x-NroFchF
                           USE-INDEX llave01 :
            DELETE integral.EvtDivi.
        END.
        FOR EACH integral.EvtArti WHERE integral.EvtArti.Codcia = S-CODCIA
                           AND integral.EvtArti.CodDiv = Gn-Divi.CodDiv
                           AND integral.EvtArti.NroFch >= x-NroFchI
                           AND integral.EvtArti.NroFch <= x-NroFchF
                           USE-INDEX llave02 :
            DELETE integral.EvtArti.
        END.
        FOR EACH integral.EvtClie WHERE integral.EvtClie.Codcia = S-CODCIA
                           AND integral.EvtClie.CodDiv = Gn-Divi.CodDiv
                           AND integral.EvtClie.NroFch >= x-NroFchI
                           AND integral.EvtClie.NroFch <= x-NroFchF
                           USE-INDEX llave02 :
            DELETE integral.EvtClie.
        END.
        FOR EACH integral.EvtFpgo WHERE integral.EvtFpgo.Codcia = S-CODCIA
                           AND integral.EvtFpgo.CodDiv = Gn-Divi.CodDiv
                           AND integral.EvtFpgo.NroFch >= x-NroFchI
                           AND integral.EvtFpgo.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE integral.EvtFpgo.
        END.
        FOR EACH integral.EvtClFpgo WHERE integral.EvtClFpgo.Codcia = S-CODCIA
                             AND integral.EvtClFpgo.CodDiv = Gn-Divi.CodDiv
                             AND integral.EvtClFpgo.NroFch >= x-NroFchI
                             AND integral.EvtClFpgo.NroFch <= x-NroFchF
                             USE-INDEX llave02:
            DELETE integral.EvtClFpgo.
        END.
        FOR EACH integral.EvtVen WHERE integral.EvtVen.Codcia = S-CODCIA
                           AND integral.EvtVen.CodDiv = Gn-Divi.CodDiv
                           AND integral.EvtVen.NroFch >= x-NroFchI
                           AND integral.EvtVen.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE integral.EvtVen.
        END.
        FOR EACH integral.Evtcan WHERE integral.Evtcan.Codcia = S-CODCIA
                           AND integral.Evtcan.CodDiv = Gn-Divi.CodDiv
                           AND integral.Evtcan.NroFch >= x-NroFchI
                           AND integral.Evtcan.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE integral.Evtcan.
        END.
        FOR EACH integral.EvtVend WHERE integral.EvtVend.Codcia = S-CODCIA
                           AND integral.EvtVend.CodDiv = Gn-Divi.CodDiv
                           AND integral.EvtVend.NroFch >= x-NroFchI
                           AND integral.EvtVend.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE integral.EvtVend.
        END.
        FOR EACH integral.EvtClArti WHERE integral.EvtClArti.Codcia = S-CODCIA
                             AND integral.EvtClArti.CodDiv = Gn-Divi.CodDiv
                             AND integral.EvtClArti.NroFch >= x-NroFchI
                             AND integral.EvtClArti.NroFch <= x-NroFchF
                             USE-INDEX llave04:
            DELETE integral.EvtClArti.
        END.
        FOR EACH integral.EvtProvF WHERE integral.EvtProvF.Codcia = S-CODCIA
                            AND integral.EvtProvF.CodDiv = Gn-Divi.CodDiv
                            AND integral.EvtProvF.NroFch >= x-NroFchI
                            AND integral.EvtProvF.NroFch <= x-NroFchF
                            USE-INDEX llave02 :
             DELETE integral.EvtProvF.
        END.
        FOR EACH integral.EvtLine WHERE integral.Evtline.Codcia = S-CODCIA
            AND integral.Evtline.CodDiv = Gn-Divi.CodDiv
            AND integral.Evtline.NroFch >= x-NroFchI
            AND integral.Evtline.NroFch <= x-NroFchF
            USE-INDEX llave02 :
            DELETE integral.EvtLine.
        END.
        FOR EACH integral.EvtProv WHERE integral.EvtProv.Codcia = S-CODCIA
            AND integral.EvtProv.CodDiv = Gn-Divi.CodDiv
            AND integral.EvtProv.NroFch >= x-NroFchI
            AND integral.EvtProv.NroFch <= x-NroFchF
            USE-INDEX llave02 :
            DELETE integral.EvtProv.
        END.
        FOR EACH integral.EvtProvL WHERE integral.EvtProvL.Codcia = S-CODCIA
            AND integral.EvtProvL.CodDiv = Gn-Divi.CodDiv
            AND integral.EvtProvL.NroFch >= x-NroFchI
            AND integral.EvtProvL.NroFch <= x-NroFchF
            USE-INDEX llave02 :
            DELETE integral.EvtProvL.
        END.
        FOR EACH integral.EvtAll01 WHERE integral.EvtAll01.Codcia = S-CODCIA
                            AND integral.EvtAll01.CodDiv = Gn-Divi.CodDiv
                            AND integral.EvtAll01.NroFch >= x-NroFchI
                            AND integral.EvtAll01.NroFch <= x-NroFchF:
             DELETE integral.EvtAll01.
        END.
    END.
    FOR EACH integral.EvtAll02 WHERE integral.EvtAll02.Codcia = S-CODCIA:
         DELETE integral.EvtAll02.
    END.
    FOR EACH integral.EvtAll03 WHERE integral.EvtAll03.Codcia = S-CODCIA:
         DELETE integral.EvtAll03.
    END.
    FOR EACH integral.EvtAll04 WHERE integral.EvtAll04.Codcia = S-CODCIA:
         DELETE integral.EvtAll04.
    END.

END.

PROCEDURE Carga-Estadisticas:
/* ************************* */
    FIND integral.EvtDivi WHERE integral.EvtDivi.Codcia = CcbCdocu.Codcia 
        AND integral.EvtDivi.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtDivi.NroFch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE integral.EvtDivi THEN DO:
        CREATE integral.EvtDivi.
        ASSIGN
            integral.EvtDivi.Codcia = CcbCdocu.Codcia 
            integral.EvtDivi.CodDiv = CcbCdocu.Coddiv 
            integral.EvtDivi.CodAno = x-Year 
            integral.EvtDivi.CodMes = x-Month
            integral.EvtDivi.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")).
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtDivi.CtoxDiaMn[x-Day] = integral.EvtDivi.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto *  x-coe)
            integral.EvtDivi.VtaxDiaMn[x-Day] = integral.EvtDivi.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtDivi.CtoxMesMn = integral.EvtDivi.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtDivi.VtaxMesMn = integral.EvtDivi.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtDivi.CtoxMesMe = integral.EvtDivi.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto / x-TpoCmbCmp * x-coe
            integral.EvtDivi.VtaxMesMe = integral.EvtDivi.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin / x-TpoCmbCmp * x-coe.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtDivi.CtoxDiaMe[x-Day] = integral.EvtDivi.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtDivi.VtaxDiaMe[x-Day] = integral.EvtDivi.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtDivi.CtoxMesMn = integral.EvtDivi.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtDivi.VtaxMesMn = integral.EvtDivi.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtDivi.CtoxMesMe = integral.EvtDivi.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtDivi.VtaxMesMe = integral.EvtDivi.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.

    FIND integral.EvtClie WHERE integral.EvtClie.Codcia = CcbCdocu.Codcia 
        AND integral.EvtClie.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtClie.Codcli = CcbCdocu.Codcli 
        AND integral.EvtClie.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE integral.EvtClie THEN DO:
        CREATE integral.EvtClie.
        ASSIGN
            integral.EvtClie.Codcia = CcbCdocu.Codcia 
            integral.EvtClie.CodDiv = CcbCdocu.Coddiv 
            integral.EvtClie.CodAno = x-Year 
            integral.EvtClie.CodMes = x-Month
            integral.EvtClie.Codcli = CcbCdocu.Codcli
            integral.EvtClie.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))  .
    END.                   
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtClie.CtoxDiaMn[x-Day] = integral.EvtClie.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtClie.VtaxDiaMn[x-Day] = integral.EvtClie.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtClie.CtoxMesMn = integral.EvtClie.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtClie.VtaxMesMn = integral.EvtClie.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtClie.CtoxMesMe = integral.EvtClie.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto / x-TpoCmbCmp * x-coe
            integral.EvtClie.VtaxMesMe = integral.EvtClie.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin / x-TpoCmbCmp * x-coe.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtClie.CtoxDiaMe[x-Day] = integral.EvtClie.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtClie.VtaxDiaMe[x-Day] = integral.EvtClie.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtClie.CtoxMesMn = integral.EvtClie.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtClie.VtaxMesMn = integral.EvtClie.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtClie.CtoxMesMe = integral.EvtClie.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtClie.VtaxMesMe = integral.EvtClie.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.

    FIND integral.EvtFpgo WHERE integral.EvtFpgo.Codcia = CcbCdocu.Codcia 
        AND integral.EvtFpgo.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtFpgo.FmaPgo = X-FMAPGO        
        AND integral.EvtFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))                           
        NO-ERROR.
    IF NOT AVAILABLE integral.EvtFpgo THEN DO:
        CREATE integral.EvtFpgo.
        ASSIGN
            integral.EvtFpgo.Codcia = CcbCdocu.Codcia 
            integral.EvtFpgo.CodDiv = CcbCdocu.Coddiv 
            integral.EvtFpgo.CodAno = x-Year 
            integral.EvtFpgo.CodMes = x-Month
            integral.EvtFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
            integral.EvtFpgo.FmaPgo = X-FMAPGO.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtFpgo.CtoxDiaMn[x-Day] = integral.EvtFpgo.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtFpgo.VtaxDiaMn[x-Day] = integral.EvtFpgo.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtFpgo.CtoxMesMn = integral.EvtFpgo.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtFpgo.VtaxMesMn = integral.EvtFpgo.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtFpgo.CtoxMesMe = integral.EvtFpgo.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto / x-TpoCmbCmp * x-coe
            integral.EvtFpgo.VtaxMesMe = integral.EvtFpgo.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin / x-TpoCmbCmp * x-coe.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtFpgo.CtoxDiaMe[x-Day] = integral.EvtFpgo.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtFpgo.VtaxDiaMe[x-Day] = integral.EvtFpgo.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtFpgo.CtoxMesMn = integral.EvtFpgo.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtFpgo.VtaxMesMn = integral.EvtFpgo.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtFpgo.CtoxMesMe = integral.EvtFpgo.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtFpgo.VtaxMesMe = integral.EvtFpgo.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.

    FIND integral.EvtClFpgo WHERE integral.EvtClFpgo.Codcia = CcbCdocu.Codcia 
        AND integral.EvtClFpgo.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtClFpgo.FmaPgo = X-FMAPGO 
        AND integral.EvtClFpgo.Codcli = CcbCdocu.Codcli 
        AND integral.EvtClFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))                           
        NO-ERROR.
    IF NOT AVAILABLE integral.EvtClFpgo THEN DO:
        CREATE integral.EvtClFpgo.
        ASSIGN
            integral.EvtClFpgo.Codcia = CcbCdocu.Codcia 
            integral.EvtClFpgo.CodDiv = CcbCdocu.Coddiv 
            integral.EvtClFpgo.CodAno = x-Year 
            integral.EvtClFpgo.CodMes = x-Month
            integral.EvtClFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
            integral.EvtClFpgo.Codcli = CcbCdocu.Codcli
            integral.EvtClFpgo.FmaPgo = X-FMAPGO.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtClFpgo.CtoxDiaMn[x-Day] = integral.EvtClFpgo.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtClFpgo.VtaxDiaMn[x-Day] = integral.EvtClFpgo.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtClFpgo.CtoxMesMn = integral.EvtClFpgo.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtClFpgo.VtaxMesMn = integral.EvtClFpgo.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtClFpgo.CtoxMesMe = integral.EvtClFpgo.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto / x-TpoCmbCmp * x-coe
            integral.EvtClFpgo.VtaxMesMe = integral.EvtClFpgo.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin / x-TpoCmbCmp * x-coe.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtClFpgo.CtoxDiaMe[x-Day] = integral.EvtClFpgo.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtClFpgo.VtaxDiaMe[x-Day] = integral.EvtClFpgo.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtClFpgo.CtoxMesMn = integral.EvtClFpgo.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtClFpgo.VtaxMesMn = integral.EvtClFpgo.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtClFpgo.CtoxMesMe = integral.EvtClFpgo.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtClFpgo.VtaxMesMe = integral.EvtClFpgo.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.

    FIND integral.EvtVen WHERE integral.EvtVen.Codcia = CcbCdocu.Codcia 
        AND integral.EvtVen.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtVen.CodVen = CcbCdocu.CodVen 
        AND integral.EvtVen.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE integral.EvtVen THEN DO:
       CREATE integral.EvtVen.
       ASSIGN
           integral.EvtVen.Codcia = CcbCdocu.Codcia 
           integral.EvtVen.CodDiv = CcbCdocu.Coddiv 
           integral.EvtVen.CodAno = x-Year 
           integral.EvtVen.CodMes = x-Month
           integral.EvtVen.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
           integral.EvtVen.CodVen = CcbCdocu.CodVen.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtVen.CtoxDiaMn[x-Day] = integral.EvtVen.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtVen.VtaxDiaMn[x-Day] = integral.EvtVen.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtVen.CtoxMesMn = integral.EvtVen.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtVen.VtaxMesMn = integral.EvtVen.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtVen.CtoxMesMe = integral.EvtVen.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto / x-TpoCmbCmp * x-coe
            integral.EvtVen.VtaxMesMe = integral.EvtVen.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin / x-TpoCmbCmp * x-coe.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtVen.CtoxDiaMe[x-Day] = integral.EvtVen.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtVen.VtaxDiaMe[x-Day] = integral.EvtVen.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtVen.CtoxMesMn = integral.EvtVen.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtVen.VtaxMesMn = integral.EvtVen.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtVen.CtoxMesMe = integral.EvtVen.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtVen.VtaxMesMe = integral.EvtVen.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.

    FIND integral.Evtcan WHERE integral.Evtcan.Codcia = CcbCdocu.Codcia 
        AND integral.Evtcan.CodDiv = CcbCdocu.Coddiv 
        AND integral.Evtcan.Canal  = x-canal         
        AND integral.Evtcan.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE integral.Evtcan THEN DO:
       CREATE integral.Evtcan.
       ASSIGN
           integral.Evtcan.Codcia = CcbCdocu.Codcia 
           integral.Evtcan.CodDiv = CcbCdocu.Coddiv 
           integral.Evtcan.CodAno = x-Year 
           integral.Evtcan.CodMes = x-Month
           integral.Evtcan.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
           integral.Evtcan.Canal  = x-canal.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.Evtcan.CtoxDiaMn[x-Day] = integral.Evtcan.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.Evtcan.VtaxDiaMn[x-Day] = integral.Evtcan.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.Evtcan.CtoxMesMn = integral.Evtcan.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.Evtcan.VtaxMesMn = integral.Evtcan.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.Evtcan.CtoxMesMe = integral.Evtcan.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto / x-TpoCmbCmp * x-coe
            integral.Evtcan.VtaxMesMe = integral.Evtcan.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin / x-TpoCmbCmp * x-coe.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.Evtcan.CtoxDiaMe[x-Day] = integral.Evtcan.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.Evtcan.VtaxDiaMe[x-Day] = integral.Evtcan.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.Evtcan.CtoxMesMn = integral.Evtcan.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.Evtcan.VtaxMesMn = integral.Evtcan.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.Evtcan.CtoxMesMe = integral.Evtcan.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.Evtcan.VtaxMesMe = integral.Evtcan.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
END.
/* ************************* */

PROCEDURE Carga-Estadisticas-2:
/* **************************** */

    FIND integral.EvtArti WHERE integral.EvtArti.Codcia = CcbCdocu.Codcia 
        AND integral.EvtArti.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtArti.CodMat = CcbDdocu.CodMat 
        AND integral.EvtArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        NO-ERROR.     
    IF NOT AVAILABLE integral.EvtArti THEN DO:
         CREATE integral.EvtArti.
         ASSIGN
             integral.EvtArti.Codcia = CcbCdocu.Codcia 
             integral.EvtArti.CodDiv = CcbCdocu.Coddiv
             integral.EvtArti.CodAno = x-Year 
             integral.EvtArti.CodMes = x-Month
             integral.EvtArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
             integral.EvtArti.CodMat = CcbDdocu.CodMat.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtArti.CtoxDiaMn[x-Day] = integral.EvtArti.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtArti.VtaxDiaMn[x-Day] = integral.EvtArti.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtArti.CtoxMesMn = integral.EvtArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtArti.VtaxMesMn = integral.EvtArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtArti.CtoxMesMe = integral.EvtArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            integral.EvtArti.VtaxMesMe = integral.EvtArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtArti.CtoxDiaMe[x-Day] = integral.EvtArti.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtArti.VtaxDiaMe[x-Day] = integral.EvtArti.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtArti.CtoxMesMn = integral.EvtArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtArti.VtaxMesMn = integral.EvtArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtArti.CtoxMesMe = integral.EvtArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtArti.VtaxMesMe = integral.EvtArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        integral.EvtArti.CanxDia[x-Day] = integral.EvtArti.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        integral.EvtArti.CanxMes = integral.EvtArti.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can ).

    FIND integral.EvtVend WHERE integral.EvtVend.Codcia = CcbCdocu.Codcia 
        AND integral.EvtVend.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtVend.CodVen = CcbCdocu.CodVen 
        AND integral.EvtVend.CodMat = CcbDdocu.CodMat 
        AND integral.EvtVend.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE integral.EvtVend THEN DO:
       CREATE integral.EvtVend.
       ASSIGN
           integral.EvtVend.Codcia = CcbCdocu.Codcia 
           integral.EvtVend.CodDiv = CcbCdocu.Coddiv 
           integral.EvtVend.CodAno = x-Year 
           integral.EvtVend.CodMes = x-Month
           integral.EvtVend.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
           integral.EvtVend.CodVen = CcbCdocu.CodVen             
           integral.EvtVend.CodMat = CcbDdocu.CodMat.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtVend.CtoxDiaMn[x-Day] = integral.EvtVend.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtVend.VtaxDiaMn[x-Day] = integral.EvtVend.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtVend.CtoxMesMn = integral.EvtVend.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtVend.VtaxMesMn = integral.EvtVend.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtVend.CtoxMesMe = integral.EvtVend.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            integral.EvtVend.VtaxMesMe = integral.EvtVend.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtVend.CtoxDiaMe[x-Day] = integral.EvtVend.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto  * x-coe)
            integral.EvtVend.VtaxDiaMe[x-Day] = integral.EvtVend.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin  * x-coe)
            integral.EvtVend.CtoxMesMn = integral.EvtVend.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtVend.VtaxMesMn = integral.EvtVend.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtVend.CtoxMesMe = integral.EvtVend.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtVend.VtaxMesMe = integral.EvtVend.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        integral.EvtVend.CanxDia[x-Day] = integral.EvtVend.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can)
        integral.EvtVend.CanxMes = integral.EvtVend.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can).

    FIND integral.EvtClArti WHERE integral.EvtClArti.Codcia = CcbCDocu.Codcia 
        AND integral.EvtClArti.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtClArti.CodCli = CcbCDocu.CodCli 
        AND integral.EvtClArti.CodMat = CcbDDocu.CodMat 
        AND integral.EvtClArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        USE-INDEX Llave01
        NO-ERROR.
    IF NOT AVAILABLE integral.EvtClArti THEN DO:
       CREATE integral.EvtClArti.
       ASSIGN
           integral.EvtClArti.Codcia = CcbCdocu.Codcia 
           integral.EvtClArti.CodDiv = CcbCdocu.Coddiv 
           integral.EvtClArti.CodAno = x-Year 
           integral.EvtClArti.CodMes = x-Month
           integral.EvtClArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
           integral.EvtClArti.CodCli = CcbCdocu.CodCli             
           integral.EvtClArti.CodMat = CcbDdocu.CodMat.            
    END.
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtClArti.CtoxDiaMn[x-Day] = integral.EvtClArti.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtClArti.VtaxDiaMn[x-Day] = integral.EvtClArti.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtClArti.CtoxMesMn = integral.EvtClArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtClArti.VtaxMesMn = integral.EvtClArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtClArti.CtoxMesMe = integral.EvtClArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto  * x-coe /  x-TpoCmbCmp
            integral.EvtClArti.VtaxMesMe = integral.EvtClArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin  * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtClArti.CtoxDiaMe[x-Day] = integral.EvtClArti.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtClArti.VtaxDiaMe[x-Day] = integral.EvtClArti.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtClArti.CtoxMesMn = integral.EvtClArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtClArti.VtaxMesMn = integral.EvtClArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtClArti.CtoxMesMe = integral.EvtClArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtClArti.VtaxMesMe = integral.EvtClArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        integral.EvtClArti.CanxDia[x-Day] = integral.EvtClArti.CanxDia[x-Day] + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can)
        integral.EvtClArti.CanxMes = integral.EvtClArti.CanxMes + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can).

    FIND integral.EvtProvF WHERE integral.EvtProvF.Codcia = CcbCdocu.Codcia 
        AND integral.EvtProvF.CodDiv = CcbCdocu.Coddiv 
        AND integral.EvtProvF.CodProv = Almmmatg.CodPr1 
        AND integral.EvtProvF.FmaPgo = X-FMAPGO 
        AND integral.EvtProvF.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        NO-ERROR.     
    IF NOT AVAILABLE integral.EvtProvF THEN DO:
         CREATE integral.EvtProvF.
         ASSIGN
             integral.EvtProvF.Codcia = CcbCdocu.Codcia 
             integral.EvtProvF.CodDiv = CcbCdocu.Coddiv 
             integral.EvtProvF.CodAno = x-Year 
             integral.EvtProvF.CodMes = x-Month
             integral.EvtProvF.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
             integral.EvtProvF.FmaPgo = X-FMAPGO 
             integral.EvtProvF.CodProv = Almmmatg.CodPr1.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtProvF.CtoxDiaMn[x-Day] = integral.EvtProvF.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtProvF.VtaxDiaMn[x-Day] = integral.EvtProvF.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtProvF.CtoxMesMn = integral.EvtProvF.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtProvF.VtaxMesMn = integral.EvtProvF.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtProvF.CtoxMesMe = integral.EvtProvF.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto  * x-coe / x-TpoCmbCmp
            integral.EvtProvF.VtaxMesMe = integral.EvtProvF.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin  * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtProvF.CtoxDiaMe[x-Day] = integral.EvtProvF.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtProvF.VtaxDiaMe[x-Day] = integral.EvtProvF.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtProvF.CtoxMesMn = integral.EvtProvF.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtProvF.VtaxMesMn = integral.EvtProvF.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtProvF.CtoxMesMe = integral.EvtProvF.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtProvF.VtaxMesMe = integral.EvtProvF.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        integral.EvtProvF.CanxDia[x-Day] = integral.EvtProvF.CanxDia[x-Day] + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can)
        integral.EvtProvF.CanxMes = integral.EvtProvF.CanxMes + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can).

    FIND integral.EvtAll01 WHERE integral.EvtAll01.Codcia = CcbCdocu.Codcia 
        AND integral.EvtAll01.CodDiv = pCoddiv 
        AND integral.EvtAll01.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        AND integral.EvtAll01.CodUnico = x-CodUnico
        AND integral.EvtAll01.NroCard  = x-NroCard
        AND integral.EvtAll01.CodCli = x-CodCli
        AND integral.EvtAll01.Sede = x-Sede 
        AND integral.EvtAll01.CodVen = Ccbcdocu.codven
        AND integral.EvtAll01.FmaPgo = x-fmapgo
        AND integral.EvtAll01.CodMat = CcbDdocu.CodMat 
        NO-ERROR.     
    IF NOT AVAILABLE integral.EvtAll01 THEN DO:
         CREATE integral.EvtAll01.
         ASSIGN
             integral.EvtAll01.Codcia = CcbCdocu.Codcia 
             integral.EvtAll01.CodDiv = pCoddiv 
             integral.EvtAll01.CodAno = x-Year 
             integral.EvtAll01.CodMes = x-Month
             integral.EvtAll01.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
             integral.EvtAll01.CodUnico = x-CodUnico
             integral.EvtAll01.NroCard  = x-NroCard
             integral.EvtAll01.CodCli = x-CodCli
             integral.EvtAll01.Sede   = x-Sede
             integral.EvtAll01.CodVen = Ccbcdocu.codven
             integral.EvtAll01.FmaPgo = x-fmapgo
             integral.EvtAll01.CodMat = CcbDdocu.CodMat
             integral.EvtALL01.Canal  = x-Canal
             integral.EvtALL01.codfam = Almmmatg.codfam
             integral.EvtALL01.CodPro = Almmmatg.codpr1
             integral.EvtALL01.subfam = Almmmatg.subfam
             integral.EvtALL01.DesMar = Almmmatg.desmar.
         IF AVAILABLE gn-clie THEN
             ASSIGN
             integral.EvtALL01.CodDept = gn-clie.CodDept
             integral.EvtALL01.CodDist = gn-clie.CodDist
             integral.EvtALL01.CodPais = gn-clie.CodPais
             integral.EvtALL01.CodProv = gn-clie.CodProv.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            integral.EvtAll01.CtoxDiaMn[x-Day] = integral.EvtAll01.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtAll01.VtaxDiaMn[x-Day] = integral.EvtAll01.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtAll01.CtoxDiaMe[x-Day] = integral.EvtAll01.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp)
            integral.EvtAll01.VtaxDiaMe[x-Day] = integral.EvtAll01.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp)
            integral.EvtAll01.CtoxMesMn = integral.EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtAll01.VtaxMesMn = integral.EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            integral.EvtAll01.CtoxMesMe = integral.EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            integral.EvtAll01.VtaxMesMe = integral.EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            integral.EvtAll01.CtoxDiaMe[x-Day] = integral.EvtAll01.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            integral.EvtAll01.VtaxDiaMe[x-Day] = integral.EvtAll01.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            integral.EvtAll01.CtoxDiaMn[x-Day] = integral.EvtAll01.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe * x-TpoCmbVta)
            integral.EvtAll01.VtaxDiaMn[x-Day] = integral.EvtAll01.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe * x-TpoCmbVta)
            integral.EvtAll01.CtoxMesMn = integral.EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            integral.EvtAll01.VtaxMesMn = integral.EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            integral.EvtAll01.CtoxMesMe = integral.EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            integral.EvtAll01.VtaxMesMe = integral.EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        integral.EvtAll01.CanxDia[x-Day] = integral.EvtAll01.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        integral.EvtAll01.CanxMes = integral.EvtAll01.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can ).
END.
/* ************************* */

PROCEDURE Resumen-Estadisticas:
/* ************************* */

    FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
        FOR EACH integral.EvtArti NO-LOCK WHERE integral.EvtArti.Codcia = S-CODCIA
                AND integral.EvtArti.CodDiv = Gn-Divi.CodDiv
                AND integral.EvtArti.NroFch >= x-NroFchI
                AND integral.EvtArti.NroFch <= x-NroFchF
                USE-INDEX llave01,
                EACH Almmmatg WHERE Almmmatg.Codcia = integral.Evtarti.Codcia 
                    AND Almmmatg.CodMat = integral.EvtArti.Codmat:
            DISPLAY integral.Evtarti.Codcia 
                     integral.EvtArti.Coddiv
                     integral.EvtArti.CodMat
                     integral.EvtArti.CodAno
                     integral.EvtArti.CodMes.
            PAUSE 0.
            FIND integral.EvtLine WHERE integral.EvtLine.Codcia = integral.EvtArti.Codcia 
                AND integral.EvtLine.CodDiv = integral.EvtArti.CodDiv 
                AND integral.EvtLine.CodFam = Almmmatg.CodFam 
                AND integral.EvtLine.NroFch = integral.EvtArti.NroFch
                NO-ERROR.
            IF NOT AVAILABLE integral.EvtLine THEN DO:
                CREATE integral.EvtLine.
                ASSIGN  
                     integral.EvtLine.Codano = integral.EvtArti.Codano 
                     integral.EvtLine.CodCia = integral.EvtArti.Codcia
                     integral.EvtLine.CodDiv = integral.EvtArti.CodDiv
                     integral.EvtLine.Codmes = integral.EvtArti.CodMes
                     integral.EvtLine.CodFam = Almmmatg.CodFam 
                     integral.EvtLine.Nrofch = integral.EvtArti.NroFch.
            END.
            ASSIGN
                 integral.EvtLine.VtaxMesMe = integral.EvtLine.VtaxMesMe + integral.EvtArti.VtaxMesMe 
                 integral.EvtLine.VtaxMesMn = integral.EvtLine.VtaxMesMn + integral.EvtArti.VtaxMesMn
                 integral.EvtLine.CtoxMesMe = integral.EvtLine.CtoxMesMe + integral.EvtArti.CtoxMesMe
                 integral.EvtLine.CtoxMesMn = integral.EvtLine.CtoxMesMn + integral.EvtArti.CtoxMesMn.                  
             DO I = 1 TO 31 :
                 integral.EvtLine.VtaxDiaMn[I] = integral.EvtLine.VtaxDiaMn[I] + integral.EvtArti.VtaxDiaMn[I].
                 integral.EvtLine.VtaxDiaMe[I] = integral.EvtLine.VtaxDiaMe[I] + integral.EvtArti.VtaxDiaMe[I].
                 integral.EvtLine.CtoxDiaMn[I] = integral.EvtLine.CtoxDiaMn[I] + integral.EvtArti.CtoxDiaMn[I].
                 integral.EvtLine.CtoxDiaMe[I] = integral.EvtLine.CtoxDiaMe[I] + integral.EvtArti.CtoxDiaMe[I].
             END.

             FIND integral.EvtProv WHERE integral.EvtProv.Codcia = integral.EvtArti.Codcia AND
                                integral.EvtProv.CodDiv = integral.EvtArti.CodDiv AND
                                integral.EvtProv.CodProv = Almmmatg.CodPr1 AND
                                integral.EvtProv.NroFch = integral.EvtArti.NroFch
                                NO-ERROR.
             IF NOT AVAILABLE integral.EvtProv THEN DO:
                CREATE integral.EvtProv.
                ASSIGN  
                     integral.EvtProv.Codano = integral.EvtArti.Codano 
                     integral.EvtProv.CodCia = integral.EvtArti.Codcia
                     integral.EvtProv.CodDiv = integral.EvtArti.CodDiv
                     integral.EvtProv.Codmes = integral.EvtArti.CodMes
                     integral.EvtProv.CodProv = Almmmatg.CodPr1
                     integral.EvtProv.Nrofch = integral.EvtArti.NroFch.
             END.
             ASSIGN
                 integral.EvtProv.VtaxMesMe = integral.EvtProv.VtaxMesMe + integral.EvtArti.VtaxMesMe 
                 integral.EvtProv.VtaxMesMn = integral.EvtProv.VtaxMesMn + integral.EvtArti.VtaxMesMn
                 integral.EvtProv.CtoxMesMe = integral.EvtProv.CtoxMesMe + integral.EvtArti.CtoxMesMe
                 integral.EvtProv.CtoxMesMn = integral.EvtProv.CtoxMesMn + integral.EvtArti.CtoxMesMn.                  
             DO I = 1 TO 31 :
                integral.EvtProv.VtaxDiaMn[I] = integral.EvtProv.VtaxDiaMn[I] + integral.EvtArti.VtaxDiaMn[I].
                integral.EvtProv.VtaxDiaMe[I] = integral.EvtProv.VtaxDiaMe[I] + integral.EvtArti.VtaxDiaMe[I].
                integral.EvtProv.CtoxDiaMn[I] = integral.EvtProv.CtoxDiaMn[I] + integral.EvtArti.CtoxDiaMn[I].
                integral.EvtProv.CtoxDiaMe[I] = integral.EvtProv.CtoxDiaMe[I] + integral.EvtArti.CtoxDiaMe[I].
             END.

             FIND integral.EvtProvL WHERE integral.EvtProvL.Codcia = integral.EvtArti.Codcia 
                 AND integral.EvtProvL.CodDiv = integral.EvtArti.CodDiv 
                 AND integral.EvtProvL.CodProv = Almmmatg.CodPr1 
                 AND integral.EvtProvL.CodFam = Almmmatg.CodFam 
                 AND integral.EvtProvL.NroFch = integral.EvtArti.NroFch
                 NO-ERROR.
             IF NOT AVAILABLE integral.EvtProvL THEN DO:
                CREATE integral.EvtProvL.
                ASSIGN  
                     integral.EvtProvL.Codano = integral.EvtArti.Codano 
                     integral.EvtProvL.CodCia = integral.EvtArti.Codcia
                     integral.EvtProvL.CodDiv = integral.EvtArti.CodDiv
                     integral.EvtProvL.Codmes = integral.EvtArti.CodMes
                     integral.EvtProvL.CodProv = Almmmatg.CodPr1
                     integral.EvtProvL.CodFam  = Almmmatg.CodFam
                     integral.EvtProvL.Nrofch = integral.EvtArti.NroFch.
             END.
             ASSIGN
                 integral.EvtProvL.VtaxMesMe = integral.EvtProvL.VtaxMesMe + integral.EvtArti.VtaxMesMe 
                 integral.EvtProvL.VtaxMesMn = integral.EvtProvL.VtaxMesMn + integral.EvtArti.VtaxMesMn
                 integral.EvtProvL.CtoxMesMe = integral.EvtProvL.CtoxMesMe + integral.EvtArti.CtoxMesMe
                 integral.EvtProvL.CtoxMesMn = integral.EvtProvL.CtoxMesMn + integral.EvtArti.CtoxMesMn.                  
             DO I = 1 TO 31 :
                integral.EvtProvL.VtaxDiaMn[I] = integral.EvtProvL.VtaxDiaMn[I] + integral.EvtArti.VtaxDiaMn[I].
                integral.EvtProvL.VtaxDiaMe[I] = integral.EvtProvL.VtaxDiaMe[I] + integral.EvtArti.VtaxDiaMe[I].
                integral.EvtProvL.CtoxDiaMn[I] = integral.EvtProvL.CtoxDiaMn[I] + integral.EvtArti.CtoxDiaMn[I].
                integral.EvtProvL.CtoxDiaMe[I] = integral.EvtProvL.CtoxDiaMe[I] + integral.EvtArti.CtoxDiaMe[I].
             END.
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

    FOR EACH Gn-divi NO-LOCK WHERE Gn-divi.codcia = s-codcia:
        FOR EACH integral.Evtall01 NO-LOCK WHERE integral.Evtall01.codcia = s-codcia
                AND integral.Evtall01.nrofch >= x-NroFchI
                AND integral.Evtall01.nrofch <= x-NroFchF
                AND integral.Evtall01.coddiv = GN-divi.coddiv:
            ASSIGN
                x-CodUnico = integral.Evtall01.CodUnico
                x-CodCli   = integral.Evtall01.CodUnico.
            FIND integral.Evtall03 WHERE integral.Evtall03.codcia = integral.Evtall01.codcia
                AND integral.Evtall03.codunico = x-CodUnico
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE integral.Evtall03 THEN CREATE integral.EvtALL03.
            ASSIGN
                integral.EvtALL03.CodCia = integral.Evtall01.codcia
                integral.EvtALL03.CodCli = x-CodCli
                integral.EvtALL03.CodUnico = x-CodUnico
                integral.EvtALL03.VtaxMesMe = integral.EvtALL03.VtaxMesMe + integral.Evtall01.VtaxMesMe
                integral.EvtALL03.VtaxMesMn = integral.EvtALL03.VtaxMesMn + integral.Evtall01.VtaxMesMn.
            RELEASE integral.Evtall03.
            FIND integral.Evtall04 WHERE integral.Evtall04.codcia = integral.Evtall01.codcia
                AND integral.Evtall04.codunico = x-CodUnico
                AND integral.Evtall04.CodCli   = integral.Evtall01.CodCli
                AND integral.Evtall04.NroFch   = integral.Evtall01.NroFch
                AND integral.Evtall04.coddiv   = integral.Evtall01.coddiv
                AND integral.Evtall04.CodFam   = integral.Evtall01.CodFam
                AND integral.Evtall04.CodVen   = integral.Evtall01.CodVen
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE integral.Evtall04 THEN CREATE integral.Evtall04.
            ASSIGN
                integral.Evtall04.CodCia    = integral.Evtall01.codcia
                integral.Evtall04.CodUnico  = x-CodUnico
                integral.Evtall04.CodCli    = integral.Evtall01.CodCli
                integral.Evtall04.NroFch    = integral.Evtall01.NroFch
                integral.EvtALL04.CodDiv    = integral.Evtall01.CodDiv
                integral.EvtALL04.codfam    = integral.Evtall01.CodFam
                integral.EvtAll04.CodVen    = integral.Evtall01.CodVen
                integral.EvtALL04.Codano    = integral.Evtall01.codano               
                integral.EvtALL04.Codmes    = integral.Evtall01.codmes
                integral.EvtALL04.CanxMes   = integral.EvtALL04.CanxMes + integral.EvtALL01.CanxMes 
                integral.Evtall04.VtaxMesMe = integral.Evtall04.VtaxMesMe + integral.Evtall01.VtaxMesMe
                integral.Evtall04.VtaxMesMn = integral.Evtall04.VtaxMesMn + integral.Evtall01.VtaxMesMn.
            RELEASE integral.Evtall04.
        END.
    END.

    /* CREAMOS EL ARCHIVO RESUMEN DE LOS TOP 100 */
    DEF VAR x-Cuenta AS INT NO-UNDO.

    FOR EACH integral.EvtALL03 USE-INDEX Indice02 NO-LOCK WHERE integral.Evtall03.codcia = s-codcia
            BY integral.Evtall03.VtaxMesMn DESC:
        x-Cuenta = x-Cuenta + 1.
        /* Definimos si va al detalle o unificados */
        ASSIGN
            x-CodUnico = integral.Evtall03.codunico
            x-CodCli   = integral.Evtall03.codcli.
        IF x-Cuenta > 200           /* Resumimos por el cliente unificado */
        THEN ASSIGN
                x-CodUnico = s-CliUni
                x-CodCli   = s-CliUni.
        DISPLAY 'top 200' x-cuenta integral.Evtall03.codunico x-codunico STRING(TIME,'hh:mm')
            WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        /* Acumulamos la información detallada por cada año */
        x-Month = MONTH(TODAY).     /* Mes de partida */
        DO k = YEAR(TODAY) - 3 TO YEAR(TODAY):
            x-Year = k.
            REPEAT WHILE x-Month <= 12:
                x-NroFchI = x-Year * 100 + x-Month.
                FOR EACH Gn-divi NO-LOCK WHERE Gn-divi.codcia = s-codcia:
                    FOR EACH integral.Evtall01 NO-LOCK WHERE integral.Evtall01.codcia = integral.Evtall03.codcia
                            AND integral.Evtall01.nrofch = x-NroFchI
                            AND integral.Evtall01.coddiv = Gn-divi.CodDiv
                            AND integral.Evtall01.codunico = integral.EvtAll03.CodUnico:
                        FIND integral.EvtAll02 USE-INDEX Indice05 WHERE integral.EvtAll02.Codcia = integral.Evtall01.Codcia
                            AND integral.EvtAll02.CodUnico = x-CodUnico
                            AND integral.EvtAll02.CodDiv = integral.Evtall01.Coddiv
                            AND integral.EvtAll02.Nrofch = integral.Evtall01.NroFch
                            /*AND integral.EvtAll02.FmaPgo = integral.Evtall01.FmaPgo*/
                            AND integral.EvtAll02.CodFam = integral.Evtall01.CodFam
                            /*AND integral.EvtAll02.SubFam = integral.Evtall01.SubFam*/
                            AND integral.EvtAll02.DesMar = integral.Evtall01.DesMar
                            EXCLUSIVE-LOCK
                            NO-ERROR.
                        IF NOT AVAILABLE integral.EvtAll02 THEN DO:
                             CREATE integral.EvtAll02.
                             BUFFER-COPY integral.EvtAll01
                                 EXCEPT
                                    integral.EvtAll01.CodCli
                                    integral.EvtAll01.CodUnico
                                    integral.EvtAll01.NroCard
                                    integral.EvtAll01.Sede
                                    integral.EvtAll01.CodVen
                                    integral.EvtAll01.CodMat
                                    integral.EvtAll01.SubFam
                                    integral.EvtAll01.FmaPgo
                                    integral.EvtAll01.Canal
                                    integral.EvtAll01.CodPais
                                    integral.EvtAll01.CodDept
                                    integral.EvtAll01.CodProv
                                    integral.EvtAll01.CodDist
                                    integral.EvtAll01.CtoxDiaMn
                                    integral.EvtAll01.VtaxDiaMn
                                    integral.EvtAll01.CtoxMesMn
                                    integral.EvtAll01.VtaxMesMn
                                    integral.EvtAll01.CtoxDiaMe
                                    integral.EvtAll01.VtaxDiaMe
                                    integral.EvtAll01.CtoxMesMe
                                    integral.EvtAll01.VtaxMesMe
                                    integral.EvtAll01.CanxDia
                                    integral.EvtAll01.CanxMes
                                 TO integral.EvtAll02
                                 ASSIGN
                                    /*integral.EvtAll02.CodCli = x-CodCli*/
                                    integral.EvtAll02.CodUnico = x-CodUnico
                                    integral.EvtAll02.Ranking = x-Cuenta.
                        END.
                        ASSIGN
                            integral.EvtAll02.CtoxMesMn = integral.EvtAll02.CtoxMesMn + integral.EvtAll01.CtoxMesMn
                            integral.EvtAll02.VtaxMesMn = integral.EvtAll02.VtaxMesMn + integral.EvtAll01.VtaxMesMn
                            integral.EvtAll02.CtoxMesMe = integral.EvtAll02.CtoxMesMe + integral.EvtAll01.CtoxMesMe
                            integral.EvtAll02.VtaxMesMe = integral.EvtAll02.VtaxMesMe + integral.EvtAll01.VtaxMesMe
                            integral.EvtAll02.CanxMes = integral.EvtAll02.CanxMes + integral.EvtAll01.CanxMes.
                    END.
                END.    /* Gn-Divi */
                x-Month = x-Month + 1.
            END.    /* REPEAT */
            x-Month = 01.
        END.
        /* FIN ciclo de acumulacion */
    END.    /* integral.EvtALL03 */

END.
/* ************** */


    /* CALCULAMOS las ventas de los 3 últimos años */
    ASSIGN
        x-NroFchI = INTEGER(STRING(YEAR(TODAY) - 3,"9999") + STRING(MONTH(TODAY),"99"))
        x-NroFchF = INTEGER(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")).                           

    FOR EACH Gn-divi NO-LOCK WHERE Gn-divi.codcia = s-codcia:
        FOR EACH integral.Evtall01 NO-LOCK WHERE integral.Evtall01.codcia = s-codcia
                AND integral.Evtall01.nrofch >= x-NroFchI
                AND integral.Evtall01.nrofch <= x-NroFchF
                AND integral.Evtall01.coddiv = GN-divi.coddiv:
            ASSIGN
                x-CodUnico = integral.Evtall01.CodUnico
                x-CodCli   = integral.Evtall01.CodUnico.
            FIND integral.Evtall04 WHERE integral.Evtall04.codcia = integral.Evtall01.codcia
                AND integral.Evtall04.codunico = x-CodUnico
                AND integral.Evtall04.CodCli   = integral.Evtall01.CodCli
                AND integral.Evtall04.NroFch   = integral.Evtall01.NroFch
                AND integral.Evtall04.coddiv   = integral.Evtall01.coddiv
                AND integral.Evtall04.CodFam   = integral.Evtall01.CodFam
                AND integral.Evtall04.CodVen   = integral.Evtall01.CodVen
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE integral.Evtall04 THEN CREATE integral.Evtall04.
            ASSIGN
                integral.Evtall04.CodCia    = integral.Evtall01.codcia
                integral.Evtall04.CodUnico  = x-CodUnico
                integral.Evtall04.CodCli    = x-CodCli
                integral.Evtall04.NroFch    = integral.Evtall01.NroFch
                integral.EvtALL04.CodDiv    = integral.Evtall01.CodDiv
                integral.EvtALL04.codfam    = integral.Evtall01.CodFam
                integral.EvtAll04.CodVen    = integral.Evtall01.CodVen
                integral.EvtALL04.Codano    = integral.Evtall01.codano               
                integral.EvtALL04.Codmes    = integral.Evtall01.codmes
                integral.EvtALL04.CanxMes   = integral.EvtALL04.CanxMes + integral.EvtALL01.CanxMes 
                integral.Evtall04.VtaxMesMe = integral.Evtall04.VtaxMesMe + integral.Evtall01.VtaxMesMe
                integral.Evtall04.VtaxMesMn = integral.Evtall04.VtaxMesMn + integral.Evtall01.VtaxMesMn.
            RELEASE integral.Evtall04.
        END.
    END. 
