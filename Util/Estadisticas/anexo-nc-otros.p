DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

DEF TEMP-TABLE detalle LIKE estavtas.evtall01
    FIELD coddoc LIKE ccbcdocu.coddoc
    FIELD nrodoc LIKE ccbcdocu.nrodoc
    FIELD fchdoc LIKE ccbcdocu.fchdoc
    FIELD implin LIKE ccbddocu.implin
    INDEX llave01 IS PRIMARY coddiv coddoc nrodoc.

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

x-CodFchI = 11/01/2010.
x-CodFchF = 11/30/2010.


x-NroFchI = INTEGER(STRING(YEAR(x-CodFchI),"9999") + STRING(MONTH(x-CodFchI),"99")).      
x-NroFchF = INTEGER(STRING(YEAR(x-CodFchF),"9999") + STRING(MONTH(x-CodFchF),"99")).                           

DEFINE VAR pCodDiv AS CHAR NO-UNDO.
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.

DISPLAY x-CodFchI x-CodFchF x-NroFchI x-NroFchF STRING(TIME,'HH:MM') TODAY SKIP.
PAUSE 0.
    
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
            AND LOOKUP(Ccbcdocu.codven, '015,173,900,901,902,017') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
        /* ***************** FILTROS ********************************** */
        IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
        IF CcbCDocu.FlgEst = "A"  THEN NEXT.
        IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
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
    END.
END.

/* informacion final */
OUTPUT TO c:\tmp\nc-otros.txt APPEND.
FOR EACH detalle, FIRST Almmmatg OF detalle NO-LOCK, FIRST gn-divi OF detalle NO-LOCK,
    FIRST gn-clie WHERE gn-clie.codcia = 0 AND gn-clie.codcli = detalle.codcli NO-LOCK,
    FIRST gn-ven OF detalle NO-LOCK:
    DISPLAY
        detalle.coddiv 
        GN-DIVI.DesDiv
        '|'
        detalle.fchdoc
        '|'
        detalle.coddoc
        '|'
        detalle.nrodoc
        '|'
        detalle.codcli
        gn-clie.nomcli
        '|'
        detalle.codven
        gn-ven.NomVen
        '|'
        detalle.fmapgo
        '|'
        detalle.codmat
        almmmatg.desmat
        '|'
        detalle.implin
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.


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

    CREATE detalle.
    ASSIGN
        detalle.Codcia = CcbCdocu.Codcia 
        detalle.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) 
        detalle.CodDiv = pCoddiv 
        detalle.coddoc = ccbcdocu.coddoc
        detalle.nrodoc = ccbcdocu.nrodoc
        detalle.fchdoc = ccbcdocu.fchdoc
        detalle.CodAno = x-Year 
        detalle.CodMes = x-Month
        detalle.CanalVenta = pCanalVenta
        detalle.CodUnico = x-CodUnico
        detalle.CodCli = x-CodCli
        detalle.Zona = x-Zona
        detalle.NroCard  = x-NroCard
        detalle.Sede   = x-Sede
        detalle.CodMat = CcbDdocu.CodMat
        detalle.CodPro = Almmmatg.CodPr1
        detalle.CodVen = x-codven        /* Ccbcdocu.codven */
        detalle.FmaPgo = x-fmapgo.
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            detalle.ImpLin = CcbDdocu.ImpLin * x-coe.
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            detalle.ImpLin = CcbDdocu.ImpLin * x-TpoCmbVta * x-coe.
END.
/* ************************* */
