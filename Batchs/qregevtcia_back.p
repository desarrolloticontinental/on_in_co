DISABLE TRIGGERS FOR LOAD OF EvtDivi.
DISABLE TRIGGERS FOR LOAD OF EvtArti.
DISABLE TRIGGERS FOR LOAD OF EvtClie.
DISABLE TRIGGERS FOR LOAD OF EvtFpgo.
DISABLE TRIGGERS FOR LOAD OF EvtClFpgo.
DISABLE TRIGGERS FOR LOAD OF EvtVend.
DISABLE TRIGGERS FOR LOAD OF EvtClArti.
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
/* 30.06.10 RECALCULO PARA PARCHAR LAS ESTADISTICAS CON NUEVAS DIVISIONES */
x-CodFchI = 01/01/2009.

x-NroFchI = INTEGER(STRING(YEAR(x-CodFchI),"9999") + STRING(MONTH(x-CodFchI),"99")).      
x-NroFchF = INTEGER(STRING(YEAR(x-CodFchF),"9999") + STRING(MONTH(x-CodFchF),"99")).                           

DEFINE VAR pCodCia AS CHAR NO-UNDO.

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
        
        RUN Carga-Estadisticas.     /* Estadísticas SIN materiales */

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

/* FOR EACH CcbDdocu OF CcbCdocu:                          */
/*     x-can = IF CcbDdocu.CodMat = "00005" THEN 1 ELSE 0. */
/* END.  
                                                  */

/* FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia                                    */
/*     AND B-CDOCU.CodDoc = CcbCdocu.Codref                                               */
/*     AND B-CDOCU.NroDoc = CcbCdocu.Nroref                                               */
/*     NO-LOCK NO-ERROR.                                                                  */
/* IF NOT AVAILABLE B-CDOCU THEN RETURN.                                                  */
/* IF LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN RETURN.    /* NO SERVICIOS NI ADELANTADAS */ */

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

/* FOR EACH Ccbdmvto OF Ccbcdocu NO-LOCK:                                                 */
/*     FIND B-CDOCU WHERE B-CDOCU.codcia = Ccbdmvto.codcia                                */
/*         AND B-CDOCU.coddoc = Ccbdmvto.codref                                           */
/*         AND B-CDOCU.nrodoc = Ccbdmvto.nroref                                           */
/*         NO-LOCK NO-ERROR.                                                              */
/*     IF NOT AVAILABLE B-CDOCU THEN NEXT.                                                */
/*     ASSIGN                                                                             */
/*         x-Can = 0                       /* ¿¿¿ OJO ??? */                              */
/*         x-ImpTot = B-CDOCU.ImpTot.      /* <<< OJO <<< */                              */
/*     /* ************************************************* */                            */
/*     x-Coe = Ccbdmvto.ImpTot / x-ImpTot.     /* OJO */                                  */
/*     FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:                                              */
/*         /* ***************** FILTROS ********************************* */              */
/*         FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia                          */
/*             AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.                    */
/*         IF NOT AVAILABLE Almmmatg THEN NEXT.                                           */
/*         IF Ccbddocu.implin <= 0 THEN NEXT.       /* <<< OJO <<< */                     */
/*         /* ************************************************************ */             */
/*         F-FACTOR  = 1.                                                                 */
/*         FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas                        */
/*             AND Almtconv.Codalter = Ccbddocu.UndVta                                    */
/*             NO-LOCK NO-ERROR.                                                          */
/*         IF AVAILABLE Almtconv THEN DO:                                                 */
/*            F-FACTOR = Almtconv.Equival.                                                */
/*            IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu. */
/*         END.                                                                           */
/*                                                                                        */
/*         RUN Carga-Estadisticas-2.    /* Estadísticas CON materiales */                 */
/*                                                                                        */
/*     END.                                                                               */
/* END.                                                                                   */

END.
/* **************************************************** */


PROCEDURE Borra-Estadisticas:
/* ************************* */

    FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
        FOR EACH EvtDivi WHERE EvtDivi.Codcia = S-CODCIA
                           AND EvtDivi.CodDiv = Gn-Divi.CodDiv
                           AND EvtDivi.NroFch >= x-NroFchI
                           AND EvtDivi.NroFch <= x-NroFchF
                           USE-INDEX llave01 :
            DELETE EvtDivi.
        END.
        FOR EACH EvtArti WHERE EvtArti.Codcia = S-CODCIA
                           AND EvtArti.CodDiv = Gn-Divi.CodDiv
                           AND EvtArti.NroFch >= x-NroFchI
                           AND EvtArti.NroFch <= x-NroFchF
                           USE-INDEX llave02 :
            DELETE EvtArti.
        END.
        FOR EACH EvtClie WHERE EvtClie.Codcia = S-CODCIA
                           AND EvtClie.CodDiv = Gn-Divi.CodDiv
                           AND EvtClie.NroFch >= x-NroFchI
                           AND EvtClie.NroFch <= x-NroFchF
                           USE-INDEX llave02 :
            DELETE EvtClie.
        END.
        FOR EACH EvtFpgo WHERE EvtFpgo.Codcia = S-CODCIA
                           AND EvtFpgo.CodDiv = Gn-Divi.CodDiv
                           AND EvtFpgo.NroFch >= x-NroFchI
                           AND EvtFpgo.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE EvtFpgo.
        END.
        FOR EACH EvtClFpgo WHERE EvtClFpgo.Codcia = S-CODCIA
                             AND EvtClFpgo.CodDiv = Gn-Divi.CodDiv
                             AND EvtClFpgo.NroFch >= x-NroFchI
                             AND EvtClFpgo.NroFch <= x-NroFchF
                             USE-INDEX llave02:
            DELETE EvtClFpgo.
        END.
        FOR EACH EvtVen WHERE EvtVen.Codcia = S-CODCIA
                           AND EvtVen.CodDiv = Gn-Divi.CodDiv
                           AND EvtVen.NroFch >= x-NroFchI
                           AND EvtVen.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE EvtVen.
        END.
        FOR EACH Evtcan WHERE Evtcan.Codcia = S-CODCIA
                           AND Evtcan.CodDiv = Gn-Divi.CodDiv
                           AND Evtcan.NroFch >= x-NroFchI
                           AND Evtcan.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE Evtcan.
        END.
        FOR EACH EvtVend WHERE EvtVend.Codcia = S-CODCIA
                           AND EvtVend.CodDiv = Gn-Divi.CodDiv
                           AND EvtVend.NroFch >= x-NroFchI
                           AND EvtVend.NroFch <= x-NroFchF
                           USE-INDEX llave02:
            DELETE EvtVend.
        END.
        FOR EACH EvtClArti WHERE EvtClArti.Codcia = S-CODCIA
                             AND EvtClArti.CodDiv = Gn-Divi.CodDiv
                             AND EvtClArti.NroFch >= x-NroFchI
                             AND EvtClArti.NroFch <= x-NroFchF
                             USE-INDEX llave04:
            DELETE EvtClArti.
        END.
        FOR EACH EvtProvF WHERE EvtProvF.Codcia = S-CODCIA
                            AND EvtProvF.CodDiv = Gn-Divi.CodDiv
                            AND EvtProvF.NroFch >= x-NroFchI
                            AND EvtProvF.NroFch <= x-NroFchF
                            USE-INDEX llave02 :
             DELETE EvtProvF.
        END.
        FOR EACH EvtLine WHERE Evtline.Codcia = S-CODCIA
            AND Evtline.CodDiv = Gn-Divi.CodDiv
            AND Evtline.NroFch >= x-NroFchI
            AND Evtline.NroFch <= x-NroFchF
            USE-INDEX llave02 :
            DELETE EvtLine.
        END.
        FOR EACH EvtProv WHERE EvtProv.Codcia = S-CODCIA
            AND EvtProv.CodDiv = Gn-Divi.CodDiv
            AND EvtProv.NroFch >= x-NroFchI
            AND EvtProv.NroFch <= x-NroFchF
            USE-INDEX llave02 :
            DELETE EvtProv.
        END.
        FOR EACH EvtProvL WHERE EvtProvL.Codcia = S-CODCIA
            AND EvtProvL.CodDiv = Gn-Divi.CodDiv
            AND EvtProvL.NroFch >= x-NroFchI
            AND EvtProvL.NroFch <= x-NroFchF
            USE-INDEX llave02 :
            DELETE EvtProvL.
        END.
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

PROCEDURE Carga-Estadisticas:
/* ************************* */
    FIND EvtDivi WHERE EvtDivi.Codcia = CcbCdocu.Codcia 
        AND EvtDivi.CodDiv = CcbCdocu.Coddiv 
        AND EvtDivi.NroFch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE EvtDivi THEN DO:
        CREATE EvtDivi.
        ASSIGN
            EvtDivi.Codcia = CcbCdocu.Codcia 
            EvtDivi.CodDiv = CcbCdocu.Coddiv 
            EvtDivi.CodAno = x-Year 
            EvtDivi.CodMes = x-Month
            EvtDivi.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")).
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtDivi.CtoxDiaMn[x-Day] = EvtDivi.CtoxDiaMn[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtDivi.VtaxDiaMn[x-Day] = EvtDivi.VtaxDiaMn[x-Day] + ( x-signo1 * x-ImpTot)
            EvtDivi.CtoxMesMn = EvtDivi.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto
            EvtDivi.VtaxMesMn = EvtDivi.VtaxMesMn + x-signo1 * x-ImpTot
            EvtDivi.CtoxMesMe = EvtDivi.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto / x-TpoCmbCmp
            EvtDivi.VtaxMesMe = EvtDivi.VtaxMesMe + x-signo1 * x-ImpTot / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtDivi.CtoxDiaMe[x-Day] = EvtDivi.CtoxDiaMe[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtDivi.VtaxDiaMe[x-Day] = EvtDivi.VtaxDiaMe[x-Day] + ( x-signo1 * x-ImpTot)
            EvtDivi.CtoxMesMn = EvtDivi.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto * x-TpoCmbVta
            EvtDivi.VtaxMesMn = EvtDivi.VtaxMesMn + x-signo1 * x-ImpTot * x-TpoCmbVta
            EvtDivi.CtoxMesMe = EvtDivi.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto
            EvtDivi.VtaxMesMe = EvtDivi.VtaxMesMe + x-signo1 * x-ImpTot.

    FIND EvtClie WHERE EvtClie.Codcia = CcbCdocu.Codcia 
        AND EvtClie.CodDiv = CcbCdocu.Coddiv 
        AND EvtClie.Codcli = CcbCdocu.Codcli 
        AND EvtClie.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE EvtClie THEN DO:
        CREATE EvtClie.
        ASSIGN
            EvtClie.Codcia = CcbCdocu.Codcia 
            EvtClie.CodDiv = CcbCdocu.Coddiv 
            EvtClie.CodAno = x-Year 
            EvtClie.CodMes = x-Month
            EvtClie.Codcli = CcbCdocu.Codcli
            EvtClie.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))  .
    END.                   
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtClie.CtoxDiaMn[x-Day] = EvtClie.CtoxDiaMn[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtClie.VtaxDiaMn[x-Day] = EvtClie.VtaxDiaMn[x-Day] + ( x-signo1 * x-ImpTot)
            EvtClie.CtoxMesMn = EvtClie.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto
            EvtClie.VtaxMesMn = EvtClie.VtaxMesMn + x-signo1 * x-ImpTot
            EvtClie.CtoxMesMe = EvtClie.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto / x-TpoCmbCmp
            EvtClie.VtaxMesMe = EvtClie.VtaxMesMe + x-signo1 * x-ImpTot / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtClie.CtoxDiaMe[x-Day] = EvtClie.CtoxDiaMe[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtClie.VtaxDiaMe[x-Day] = EvtClie.VtaxDiaMe[x-Day] + ( x-signo1 * x-ImpTot)
            EvtClie.CtoxMesMn = EvtClie.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto * x-TpoCmbVta
            EvtClie.VtaxMesMn = EvtClie.VtaxMesMn + x-signo1 * x-ImpTot * x-TpoCmbVta
            EvtClie.CtoxMesMe = EvtClie.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto
            EvtClie.VtaxMesMe = EvtClie.VtaxMesMe + x-signo1 * x-ImpTot.

    FIND EvtFpgo WHERE EvtFpgo.Codcia = CcbCdocu.Codcia 
        AND EvtFpgo.CodDiv = CcbCdocu.Coddiv 
        AND EvtFpgo.FmaPgo = X-FMAPGO        
        AND EvtFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))                           
        NO-ERROR.
    IF NOT AVAILABLE EvtFpgo THEN DO:
        CREATE EvtFpgo.
        ASSIGN
            EvtFpgo.Codcia = CcbCdocu.Codcia 
            EvtFpgo.CodDiv = CcbCdocu.Coddiv 
            EvtFpgo.CodAno = x-Year 
            EvtFpgo.CodMes = x-Month
            EvtFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
            EvtFpgo.FmaPgo = X-FMAPGO.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtFpgo.CtoxDiaMn[x-Day] = EvtFpgo.CtoxDiaMn[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtFpgo.VtaxDiaMn[x-Day] = EvtFpgo.VtaxDiaMn[x-Day] + ( x-signo1 * x-ImpTot)
            EvtFpgo.CtoxMesMn = EvtFpgo.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto
            EvtFpgo.VtaxMesMn = EvtFpgo.VtaxMesMn + x-signo1 * x-ImpTot
            EvtFpgo.CtoxMesMe = EvtFpgo.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto / x-TpoCmbCmp
            EvtFpgo.VtaxMesMe = EvtFpgo.VtaxMesMe + x-signo1 * x-ImpTot / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtFpgo.CtoxDiaMe[x-Day] = EvtFpgo.CtoxDiaMe[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtFpgo.VtaxDiaMe[x-Day] = EvtFpgo.VtaxDiaMe[x-Day] + ( x-signo1 * x-ImpTot)
            EvtFpgo.CtoxMesMn = EvtFpgo.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto * x-TpoCmbVta
            EvtFpgo.VtaxMesMn = EvtFpgo.VtaxMesMn + x-signo1 * x-ImpTot * x-TpoCmbVta
            EvtFpgo.CtoxMesMe = EvtFpgo.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto
            EvtFpgo.VtaxMesMe = EvtFpgo.VtaxMesMe + x-signo1 * x-ImpTot.

    FIND EvtClFpgo WHERE EvtClFpgo.Codcia = CcbCdocu.Codcia 
        AND EvtClFpgo.CodDiv = CcbCdocu.Coddiv 
        AND EvtClFpgo.FmaPgo = X-FMAPGO 
        AND EvtClFpgo.Codcli = CcbCdocu.Codcli 
        AND EvtClFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))                           
        NO-ERROR.
    IF NOT AVAILABLE EvtClFpgo THEN DO:
        CREATE EvtClFpgo.
        ASSIGN
            EvtClFpgo.Codcia = CcbCdocu.Codcia 
            EvtClFpgo.CodDiv = CcbCdocu.Coddiv 
            EvtClFpgo.CodAno = x-Year 
            EvtClFpgo.CodMes = x-Month
            EvtClFpgo.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
            EvtClFpgo.Codcli = CcbCdocu.Codcli
            EvtClFpgo.FmaPgo = X-FMAPGO.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtClFpgo.CtoxDiaMn[x-Day] = EvtClFpgo.CtoxDiaMn[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtClFpgo.VtaxDiaMn[x-Day] = EvtClFpgo.VtaxDiaMn[x-Day] + ( x-signo1 * x-ImpTot)
            EvtClFpgo.CtoxMesMn = EvtClFpgo.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto
            EvtClFpgo.VtaxMesMn = EvtClFpgo.VtaxMesMn + x-signo1 * x-ImpTot
            EvtClFpgo.CtoxMesMe = EvtClFpgo.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto / x-TpoCmbCmp
            EvtClFpgo.VtaxMesMe = EvtClFpgo.VtaxMesMe + x-signo1 * x-ImpTot / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtClFpgo.CtoxDiaMe[x-Day] = EvtClFpgo.CtoxDiaMe[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtClFpgo.VtaxDiaMe[x-Day] = EvtClFpgo.VtaxDiaMe[x-Day] + ( x-signo1 * x-ImpTot)
            EvtClFpgo.CtoxMesMn = EvtClFpgo.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto * x-TpoCmbVta
            EvtClFpgo.VtaxMesMn = EvtClFpgo.VtaxMesMn + x-signo1 * x-ImpTot * x-TpoCmbVta
            EvtClFpgo.CtoxMesMe = EvtClFpgo.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto
            EvtClFpgo.VtaxMesMe = EvtClFpgo.VtaxMesMe + x-signo1 * x-ImpTot.

    FIND EvtVen WHERE EvtVen.Codcia = CcbCdocu.Codcia 
        AND EvtVen.CodDiv = CcbCdocu.Coddiv 
        AND EvtVen.CodVen = CcbCdocu.CodVen 
        AND EvtVen.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE EvtVen THEN DO:
       CREATE EvtVen.
       ASSIGN
           EvtVen.Codcia = CcbCdocu.Codcia 
           EvtVen.CodDiv = CcbCdocu.Coddiv 
           EvtVen.CodAno = x-Year 
           EvtVen.CodMes = x-Month
           EvtVen.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
           EvtVen.CodVen = CcbCdocu.CodVen.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtVen.CtoxDiaMn[x-Day] = EvtVen.CtoxDiaMn[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtVen.VtaxDiaMn[x-Day] = EvtVen.VtaxDiaMn[x-Day] + ( x-signo1 * x-ImpTot)
            EvtVen.CtoxMesMn = EvtVen.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto
            EvtVen.VtaxMesMn = EvtVen.VtaxMesMn + x-signo1 * x-ImpTot
            EvtVen.CtoxMesMe = EvtVen.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto / x-TpoCmbCmp
            EvtVen.VtaxMesMe = EvtVen.VtaxMesMe + x-signo1 * x-ImpTot / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtVen.CtoxDiaMe[x-Day] = EvtVen.CtoxDiaMe[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            EvtVen.VtaxDiaMe[x-Day] = EvtVen.VtaxDiaMe[x-Day] + ( x-signo1 * x-ImpTot)
            EvtVen.CtoxMesMn = EvtVen.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto * x-TpoCmbVta
            EvtVen.VtaxMesMn = EvtVen.VtaxMesMn + x-signo1 * x-ImpTot * x-TpoCmbVta
            EvtVen.CtoxMesMe = EvtVen.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto
            EvtVen.VtaxMesMe = EvtVen.VtaxMesMe + x-signo1 * x-ImpTot.

    FIND Evtcan WHERE Evtcan.Codcia = CcbCdocu.Codcia 
        AND Evtcan.CodDiv = CcbCdocu.Coddiv 
        AND Evtcan.Canal  = x-canal         
        AND Evtcan.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE Evtcan THEN DO:
       CREATE Evtcan.
       ASSIGN
           Evtcan.Codcia = CcbCdocu.Codcia 
           Evtcan.CodDiv = CcbCdocu.Coddiv 
           Evtcan.CodAno = x-Year 
           Evtcan.CodMes = x-Month
           Evtcan.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
           Evtcan.Canal  = x-canal.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            Evtcan.CtoxDiaMn[x-Day] = Evtcan.CtoxDiaMn[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            Evtcan.VtaxDiaMn[x-Day] = Evtcan.VtaxDiaMn[x-Day] + ( x-signo1 * x-ImpTot)
            Evtcan.CtoxMesMn = Evtcan.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto
            Evtcan.VtaxMesMn = Evtcan.VtaxMesMn + x-signo1 * x-ImpTot
            Evtcan.CtoxMesMe = Evtcan.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto / x-TpoCmbCmp
            Evtcan.VtaxMesMe = Evtcan.VtaxMesMe + x-signo1 * x-ImpTot / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            Evtcan.CtoxDiaMe[x-Day] = Evtcan.CtoxDiaMe[x-Day] + ( x-signo1 * CcbCdocu.ImpCto)
            Evtcan.VtaxDiaMe[x-Day] = Evtcan.VtaxDiaMe[x-Day] + ( x-signo1 * x-ImpTot)
            Evtcan.CtoxMesMn = Evtcan.CtoxMesMn + x-signo1 * CcbCdocu.ImpCto * x-TpoCmbVta
            Evtcan.VtaxMesMn = Evtcan.VtaxMesMn + x-signo1 * x-ImpTot * x-TpoCmbVta
            Evtcan.CtoxMesMe = Evtcan.CtoxMesMe + x-signo1 * CcbCdocu.ImpCto
            Evtcan.VtaxMesMe = Evtcan.VtaxMesMe + x-signo1 * x-ImpTot.
END.
/* ************************* */

PROCEDURE Carga-Estadisticas-2:
/* **************************** */

    FIND EvtArti WHERE EvtArti.Codcia = CcbCdocu.Codcia 
        AND EvtArti.CodDiv = CcbCdocu.Coddiv 
        AND EvtArti.CodMat = CcbDdocu.CodMat 
        AND EvtArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        NO-ERROR.     
    IF NOT AVAILABLE EvtArti THEN DO:
         CREATE EvtArti.
         ASSIGN
             EvtArti.Codcia = CcbCdocu.Codcia 
             EvtArti.CodDiv = CcbCdocu.Coddiv 
             EvtArti.CodAno = x-Year 
             EvtArti.CodMes = x-Month
             EvtArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
             EvtArti.CodMat = CcbDdocu.CodMat.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtArti.CtoxDiaMn[x-Day] = EvtArti.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtArti.VtaxDiaMn[x-Day] = EvtArti.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtArti.CtoxMesMn = EvtArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtArti.VtaxMesMn = EvtArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            EvtArti.CtoxMesMe = EvtArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            EvtArti.VtaxMesMe = EvtArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtArti.CtoxDiaMe[x-Day] = EvtArti.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtArti.VtaxDiaMe[x-Day] = EvtArti.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtArti.CtoxMesMn = EvtArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            EvtArti.VtaxMesMn = EvtArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            EvtArti.CtoxMesMe = EvtArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtArti.VtaxMesMe = EvtArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        EvtArti.CanxDia[x-Day] = EvtArti.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        EvtArti.CanxMes = EvtArti.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can ).

    FIND EvtVend WHERE EvtVend.Codcia = CcbCdocu.Codcia 
        AND EvtVend.CodDiv = CcbCdocu.Coddiv 
        AND EvtVend.CodVen = CcbCdocu.CodVen 
        AND EvtVend.CodMat = CcbDdocu.CodMat 
        AND EvtVend.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        NO-ERROR.
    IF NOT AVAILABLE EvtVend THEN DO:
       CREATE EvtVend.
       ASSIGN
           EvtVend.Codcia = CcbCdocu.Codcia 
           EvtVend.CodDiv = CcbCdocu.Coddiv 
           EvtVend.CodAno = x-Year 
           EvtVend.CodMes = x-Month
           EvtVend.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
           EvtVend.CodVen = CcbCdocu.CodVen             
           EvtVend.CodMat = CcbDdocu.CodMat.            
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtVend.CtoxDiaMn[x-Day] = EvtVend.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtVend.VtaxDiaMn[x-Day] = EvtVend.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtVend.CtoxMesMn = EvtVend.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtVend.VtaxMesMn = EvtVend.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            EvtVend.CtoxMesMe = EvtVend.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            EvtVend.VtaxMesMe = EvtVend.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtVend.CtoxDiaMe[x-Day] = EvtVend.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto  * x-coe)
            EvtVend.VtaxDiaMe[x-Day] = EvtVend.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin  * x-coe)
            EvtVend.CtoxMesMn = EvtVend.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            EvtVend.VtaxMesMn = EvtVend.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            EvtVend.CtoxMesMe = EvtVend.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtVend.VtaxMesMe = EvtVend.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        EvtVend.CanxDia[x-Day] = EvtVend.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can)
        EvtVend.CanxMes = EvtVend.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can).

    FIND EvtClArti WHERE EvtClArti.Codcia = CcbCDocu.Codcia 
        AND EvtClArti.CodDiv = CcbCDocu.Coddiv 
        AND EvtClArti.CodCli = CcbCDocu.CodCli 
        AND EvtClArti.CodMat = CcbDDocu.CodMat 
        AND EvtClArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        USE-INDEX Llave01
        NO-ERROR.
    IF NOT AVAILABLE EvtClArti THEN DO:
       CREATE EvtClArti.
       ASSIGN
           EvtClArti.Codcia = CcbCdocu.Codcia 
           EvtClArti.CodDiv = CcbCdocu.Coddiv 
           EvtClArti.CodAno = x-Year 
           EvtClArti.CodMes = x-Month
           EvtClArti.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
           EvtClArti.CodCli = CcbCdocu.CodCli             
           EvtClArti.CodMat = CcbDdocu.CodMat.            
    END.
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtClArti.CtoxDiaMn[x-Day] = EvtClArti.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtClArti.VtaxDiaMn[x-Day] = EvtClArti.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtClArti.CtoxMesMn = EvtClArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtClArti.VtaxMesMn = EvtClArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            EvtClArti.CtoxMesMe = EvtClArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto  * x-coe /  x-TpoCmbCmp
            EvtClArti.VtaxMesMe = EvtClArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin  * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtClArti.CtoxDiaMe[x-Day] = EvtClArti.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtClArti.VtaxDiaMe[x-Day] = EvtClArti.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtClArti.CtoxMesMn = EvtClArti.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            EvtClArti.VtaxMesMn = EvtClArti.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            EvtClArti.CtoxMesMe = EvtClArti.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtClArti.VtaxMesMe = EvtClArti.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        EvtClArti.CanxDia[x-Day] = EvtClArti.CanxDia[x-Day] + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can)
        EvtClArti.CanxMes = EvtClArti.CanxMes + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can).

    FIND EvtProvF WHERE EvtProvF.Codcia = CcbCdocu.Codcia 
        AND EvtProvF.CodDiv = CcbCdocu.Coddiv 
        AND EvtProvF.CodProv = Almmmatg.CodPr1 
        AND EvtProvF.FmaPgo = X-FMAPGO 
        AND EvtProvF.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        NO-ERROR.     
    IF NOT AVAILABLE EvtProvF THEN DO:
         CREATE EvtProvF.
         ASSIGN
             EvtProvF.Codcia = CcbCdocu.Codcia 
             EvtProvF.CodDiv = CcbCdocu.Coddiv 
             EvtProvF.CodAno = x-Year 
             EvtProvF.CodMes = x-Month
             EvtProvF.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
             EvtProvF.FmaPgo = X-FMAPGO 
             EvtProvF.CodProv = Almmmatg.CodPr1.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtProvF.CtoxDiaMn[x-Day] = EvtProvF.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtProvF.VtaxDiaMn[x-Day] = EvtProvF.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtProvF.CtoxMesMn = EvtProvF.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtProvF.VtaxMesMn = EvtProvF.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            EvtProvF.CtoxMesMe = EvtProvF.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto  * x-coe / x-TpoCmbCmp
            EvtProvF.VtaxMesMe = EvtProvF.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin  * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtProvF.CtoxDiaMe[x-Day] = EvtProvF.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtProvF.VtaxDiaMe[x-Day] = EvtProvF.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtProvF.CtoxMesMn = EvtProvF.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            EvtProvF.VtaxMesMn = EvtProvF.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            EvtProvF.CtoxMesMe = EvtProvF.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtProvF.VtaxMesMe = EvtProvF.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        EvtProvF.CanxDia[x-Day] = EvtProvF.CanxDia[x-Day] + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can)
        EvtProvF.CanxMes = EvtProvF.CanxMes + ( x-signo1 * CcbdDocu.CanDes * F-FACTOR * x-can).

    FIND EvtAll01 WHERE EvtAll01.Codcia = CcbCdocu.Codcia 
        AND EvtAll01.CodDiv = CcbCdocu.Coddiv 
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
             EvtAll01.CodDiv = CcbCdocu.Coddiv 
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

PROCEDURE Resumen-Estadisticas:
/* ************************* */

    FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
        FOR EACH EvtArti NO-LOCK WHERE EvtArti.Codcia = S-CODCIA
                AND EvtArti.CodDiv = Gn-Divi.CodDiv
                AND EvtArti.NroFch >= x-NroFchI
                AND EvtArti.NroFch <= x-NroFchF
                USE-INDEX llave01,
                EACH Almmmatg WHERE Almmmatg.Codcia = Evtarti.Codcia 
                    AND Almmmatg.CodMat = EvtArti.Codmat:
            DISPLAY Evtarti.Codcia 
                     EvtArti.Coddiv
                     EvtArti.CodMat
                     EvtArti.CodAno
                     EvtArti.CodMes.
            PAUSE 0.
            FIND EvtLine WHERE EvtLine.Codcia = EvtArti.Codcia 
                AND EvtLine.CodDiv = EvtArti.CodDiv 
                AND EvtLine.CodFam = Almmmatg.CodFam 
                AND EvtLine.NroFch = EvtArti.NroFch
                NO-ERROR.
            IF NOT AVAILABLE EvtLine THEN DO:
                CREATE EvtLine.
                ASSIGN  
                     EvtLine.Codano = EvtArti.Codano 
                     EvtLine.CodCia = EvtArti.Codcia
                     EvtLine.CodDiv = EvtArti.CodDiv
                     EvtLine.Codmes = EvtArti.CodMes
                     EvtLine.CodFam = Almmmatg.CodFam 
                     EvtLine.Nrofch = EvtArti.NroFch.
            END.
            ASSIGN
                 EvtLine.VtaxMesMe = EvtLine.VtaxMesMe + EvtArti.VtaxMesMe 
                 EvtLine.VtaxMesMn = EvtLine.VtaxMesMn + EvtArti.VtaxMesMn
                 EvtLine.CtoxMesMe = EvtLine.CtoxMesMe + EvtArti.CtoxMesMe
                 EvtLine.CtoxMesMn = EvtLine.CtoxMesMn + EvtArti.CtoxMesMn.                  
             DO I = 1 TO 31 :
                 EvtLine.VtaxDiaMn[I] = EvtLine.VtaxDiaMn[I] + EvtArti.VtaxDiaMn[I].
                 EvtLine.VtaxDiaMe[I] = EvtLine.VtaxDiaMe[I] + EvtArti.VtaxDiaMe[I].
                 EvtLine.CtoxDiaMn[I] = EvtLine.CtoxDiaMn[I] + EvtArti.CtoxDiaMn[I].
                 EvtLine.CtoxDiaMe[I] = EvtLine.CtoxDiaMe[I] + EvtArti.CtoxDiaMe[I].
             END.

             FIND EvtProv WHERE EvtProv.Codcia = EvtArti.Codcia AND
                                EvtProv.CodDiv = EvtArti.CodDiv AND
                                EvtProv.CodProv = Almmmatg.CodPr1 AND
                                EvtProv.NroFch = EvtArti.NroFch
                                NO-ERROR.
             IF NOT AVAILABLE EvtProv THEN DO:
                CREATE EvtProv.
                ASSIGN  
                     EvtProv.Codano = EvtArti.Codano 
                     EvtProv.CodCia = EvtArti.Codcia
                     EvtProv.CodDiv = EvtArti.CodDiv
                     EvtProv.Codmes = EvtArti.CodMes
                     EvtProv.CodProv = Almmmatg.CodPr1
                     EvtProv.Nrofch = EvtArti.NroFch.
             END.
             ASSIGN
                 EvtProv.VtaxMesMe = EvtProv.VtaxMesMe + EvtArti.VtaxMesMe 
                 EvtProv.VtaxMesMn = EvtProv.VtaxMesMn + EvtArti.VtaxMesMn
                 EvtProv.CtoxMesMe = EvtProv.CtoxMesMe + EvtArti.CtoxMesMe
                 EvtProv.CtoxMesMn = EvtProv.CtoxMesMn + EvtArti.CtoxMesMn.                  
             DO I = 1 TO 31 :
                EvtProv.VtaxDiaMn[I] = EvtProv.VtaxDiaMn[I] + EvtArti.VtaxDiaMn[I].
                EvtProv.VtaxDiaMe[I] = EvtProv.VtaxDiaMe[I] + EvtArti.VtaxDiaMe[I].
                EvtProv.CtoxDiaMn[I] = EvtProv.CtoxDiaMn[I] + EvtArti.CtoxDiaMn[I].
                EvtProv.CtoxDiaMe[I] = EvtProv.CtoxDiaMe[I] + EvtArti.CtoxDiaMe[I].
             END.

             FIND EvtProvL WHERE EvtProvL.Codcia = EvtArti.Codcia 
                 AND EvtProvL.CodDiv = EvtArti.CodDiv 
                 AND EvtProvL.CodProv = Almmmatg.CodPr1 
                 AND EvtProvL.CodFam = Almmmatg.CodFam 
                 AND EvtProvL.NroFch = EvtArti.NroFch
                 NO-ERROR.
             IF NOT AVAILABLE EvtProvL THEN DO:
                CREATE EvtProvL.
                ASSIGN  
                     EvtProvL.Codano = EvtArti.Codano 
                     EvtProvL.CodCia = EvtArti.Codcia
                     EvtProvL.CodDiv = EvtArti.CodDiv
                     EvtProvL.Codmes = EvtArti.CodMes
                     EvtProvL.CodProv = Almmmatg.CodPr1
                     EvtProvL.CodFam  = Almmmatg.CodFam
                     EvtProvL.Nrofch = EvtArti.NroFch.
             END.
             ASSIGN
                 EvtProvL.VtaxMesMe = EvtProvL.VtaxMesMe + EvtArti.VtaxMesMe 
                 EvtProvL.VtaxMesMn = EvtProvL.VtaxMesMn + EvtArti.VtaxMesMn
                 EvtProvL.CtoxMesMe = EvtProvL.CtoxMesMe + EvtArti.CtoxMesMe
                 EvtProvL.CtoxMesMn = EvtProvL.CtoxMesMn + EvtArti.CtoxMesMn.                  
             DO I = 1 TO 31 :
                EvtProvL.VtaxDiaMn[I] = EvtProvL.VtaxDiaMn[I] + EvtArti.VtaxDiaMn[I].
                EvtProvL.VtaxDiaMe[I] = EvtProvL.VtaxDiaMe[I] + EvtArti.VtaxDiaMe[I].
                EvtProvL.CtoxDiaMn[I] = EvtProvL.CtoxDiaMn[I] + EvtArti.CtoxDiaMn[I].
                EvtProvL.CtoxDiaMe[I] = EvtProvL.CtoxDiaMe[I] + EvtArti.CtoxDiaMe[I].
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
