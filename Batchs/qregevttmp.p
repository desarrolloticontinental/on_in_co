DISABLE TRIGGERS FOR LOAD OF EvtDivi.
DISABLE TRIGGERS FOR LOAD OF EvtArti.
DISABLE TRIGGERS FOR LOAD OF EvtClie.
DISABLE TRIGGERS FOR LOAD OF EvtFpgo.
DISABLE TRIGGERS FOR LOAD OF EvtClFpgo.
DISABLE TRIGGERS FOR LOAD OF EvtVend.
DISABLE TRIGGERS FOR LOAD OF EvtClArti.
DISABLE TRIGGERS FOR LOAD OF EvtAll01.

Def var s-codcia    as inte init 1.
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
def var x-fmapgo    as char.
def var x-canal     as char.
DEF VAR x-CodUnico  LIKE Gn-clie.CodUnico.
DEF VAR x-NroCard   LIKE GN-card.NroCard.

DEF VAR x-ImpAde    AS DEC NO-UNDO.     /* Importe aplicado de la factura adelantada */
DEF VAR x-ImpTot    AS DEC NO-UNDO.     /* IMporte NETO de venta */

Def BUFFER B-CDOCU FOR CcbCdocu.
 
x-codfchi = DATE(MONTH(TODAY - 30), 01, YEAR(TODAY - 30)).
x-codfchf = TODAY - 1.

/* DESCOMENTAR SI QUIERES CALCULAR POR UN RANGO DE FECHAS FIJO */ 
x-CodFchI = 08/01/2009.


X-CODFCHI = DATE(01,01,2006).
X-CODFCHF = DATE(12,31,2009).


x-NroFchI = INTEGER(STRING(YEAR(x-CodFchI),"9999") + STRING(MONTH(x-CodFchI),"99")).      
x-NroFchF = INTEGER(STRING(YEAR(x-CodFchF),"9999") + STRING(MONTH(x-CodFchF),"99")).                           

DISPLAY x-CodFchI x-CodFchF x-NroFchI x-NroFchF.
PAUSE 0.
    
FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
    RUN Borra-Estadisticas.
END.

FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK :
    /* Barremos las ventas */
    FOR EACH CcbCdocu WHERE CcbCdocu.CodCia = S-CODCIA 
            AND CcbCdocu.CodDiv = Gn-Divi.CodDiv
            AND CcbCdocu.FchDoc >= x-CodFchI
            AND CcbCdocu.FchDoc <= x-CodFchF
            AND CcbCdocu.TpoFac <> 'A'      /* NO facturas adelantadas */
            USE-INDEX llave10
            BREAK BY CcbCdocu.CodCia BY CcbCdocu.CodDiv BY CcbCdocu.FchDoc:
        /* ***************** FILTROS ********************************** */
        IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
        IF CcbCDocu.FlgEst = "A"  THEN NEXT.
        IF CcbCDocu.ImpCto = ? THEN CcbCDocu.ImpCto = 0.        /* <<< OJO <<< */
        IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
        /* *********************************************************** */
        DISPLAY CcbCdocu.Codcia
                CcbCdocu.Coddiv
                CcbCdocu.FchDoc 
                CcbCdocu.CodDoc
                CcbCdocu.NroDoc
                STRING(TIME,'HH:MM')
                TODAY .
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

        FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc
            USE-INDEX Cmb01 NO-LOCK NO-ERROR.
        IF NOT AVAIL Gn-Tcmb THEN 
            FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc
                USE-INDEX Cmb01 NO-LOCK NO-ERROR.
        IF AVAIL Gn-Tcmb THEN 
            ASSIGN
                x-TpoCmbCmp = Gn-Tcmb.Compra
                x-TpoCmbVta = Gn-Tcmb.Venta.

        /* VARIABLES DE VENTAS */
        ASSIGN
            X-FMAPGO = CcbCdocu.FmaPgo
            x-NroCard = Ccbcdocu.nrocard
            x-Canal = ''.
        IF CcbCdocu.Coddoc = "N/C" THEN DO:
           FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
               AND B-CDOCU.Coddoc = CcbCdocu.Codref 
               AND B-CDOCU.NroDoc = CcbCdocu.Nroref
               NO-LOCK NO-ERROR.
           IF AVAILABLE B-CDOCU THEN DO:
               X-FMAPGO = B-CDOCU.FmaPgo.
               IF x-NroCard = '' THEN x-NroCard = B-CDOCU.NroCard.
           END.
        END.
        x-CodUnico = Ccbcdocu.codcli.
        FIND gn-clie WHERE gn-clie.codcia = 000     /* OJO */
            AND gn-clie.codcli = Ccbcdocu.codcli
            NO-LOCK NO-ERROR.
        IF AVAILABLE gn-clie AND gn-clie.codunico <> '' THEN x-codunico = gn-clie.codunico.
        IF x-NroCard = '' AND AVAILABLE gn-clie THEN x-NroCard = Gn-clie.NroCard.
        IF AVAILABLE Gn-clie THEN x-canal = gn-clie.Canal.
        
        /* NOTAS DE CREDITO por OTROS conceptos */
       IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" THEN DO:
           RUN PROCESA-NOTA.
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


PROCEDURE PROCESA-NOTA.
/* ******************* */

/* FOR EACH CcbDdocu OF CcbCdocu:                          */
/*     x-can = IF CcbDdocu.CodMat = "00005" THEN 1 ELSE 0. */
/* END.  
                                                  */

FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
    AND B-CDOCU.CodDoc = CcbCdocu.Codref 
    AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CDOCU THEN RETURN.

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

PROCEDURE Borra-Estadisticas:
/* ************************* */
    FOR EACH EvtAll01 WHERE EvtAll01.Codcia = S-CODCIA
                        AND EvtAll01.CodDiv = Gn-Divi.CodDiv
                        AND EvtAll01.NroFch >= x-NroFchI
                        AND EvtAll01.NroFch <= x-NroFchF:
         DELETE EvtAll01.
    END.

END.


PROCEDURE Carga-Estadisticas-2:
/* **************************** */

    /* CONSOLIDADO GENERAL DE ESTADISTICAS */
    FIND EvtAll01 WHERE EvtAll01.Codcia = CcbCdocu.Codcia 
        AND EvtAll01.CodDiv = CcbCdocu.Coddiv 
        AND EvtAll01.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        AND EvtAll01.CodUnico = x-CodUnico
        AND EvtAll01.NroCard  = x-NroCard
        AND EvtAll01.CodCli = Ccbcdocu.codcli
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
             EvtAll01.CodCli = Ccbcdocu.codcli
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
             EvtALL01.subfam = Almmmatg.subfam.
    END.                    
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            EvtAll01.CtoxDiaMn[x-Day] = EvtAll01.CtoxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtAll01.VtaxDiaMn[x-Day] = EvtAll01.VtaxDiaMn[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtAll01.CtoxMesMn = EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtAll01.VtaxMesMn = EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-coe
            EvtAll01.CtoxMesMe = EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            EvtAll01.VtaxMesMe = EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe / x-TpoCmbCmp.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            EvtAll01.CtoxDiaMe[x-Day] = EvtAll01.CtoxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpCto * x-coe)
            EvtAll01.VtaxDiaMe[x-Day] = EvtAll01.VtaxDiaMe[x-Day] + ( x-signo1 * CcbDdocu.ImpLin * x-coe)
            EvtAll01.CtoxMesMn = EvtAll01.CtoxMesMn + x-signo1 * CcbDdocu.ImpCto * x-TpoCmbVta * x-coe
            EvtAll01.VtaxMesMn = EvtAll01.VtaxMesMn + x-signo1 * CcbDdocu.ImpLin * x-TpoCmbVta * x-coe
            EvtAll01.CtoxMesMe = EvtAll01.CtoxMesMe + x-signo1 * CcbDdocu.ImpCto * x-coe
            EvtAll01.VtaxMesMe = EvtAll01.VtaxMesMe + x-signo1 * CcbDdocu.ImpLin * x-coe.
    ASSIGN            
        EvtAll01.CanxDia[x-Day] = EvtAll01.CanxDia[x-Day] + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        EvtAll01.CanxMes = EvtAll01.CanxMes + ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can ).
END.
/* ************************* */
