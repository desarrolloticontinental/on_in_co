&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR x-CodFchI AS DATE.
DEF VAR x-CodFchF AS DATE.
DEF VAR FechaD AS INT.
DEF VAR FechaH AS INT.
DEF VAR s-CodCia AS INT INIT 001.
DEF VAR x-signo1 AS INT INIT 1.
DEF VAR x-ImpLin AS DEC NO-UNDO.
DEF VAR x-ImpTot AS DEC NO-UNDO.     /* IMporte NETO de venta */
DEF VAR x-TpoCmbCmp AS DECI INIT 1.
DEF VAR x-TpoCmbVta AS DECI INIT 1.
DEF VAR x-PorIgv AS DEC DECIMALS 4.

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

DEF VAR s-clivar    AS CHAR FORMAT 'x(11)'.
DEF VAR s-CliUni    AS CHAR FORMAT 'x(11)' INIT '99999999999' NO-UNDO.

DEFINE VAR pCodDiv AS CHAR NO-UNDO.
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.

DEF BUFFER B-CDOCU FOR CcbCdocu.
DEF BUFFER B-DIVI  FOR Gn-Divi.

ASSIGN
    x-CodFchF = TODAY - 1
    x-CodFchI = DATE(01, 01, YEAR(TODAY)).      /* Por defecto */

/* Fecha de Cierre */
DEF VAR dFchCie AS DATE NO-UNDO.

IF DAY(TODAY) < 15 
THEN dFchCie = TODAY - DAY(TODAY).
ELSE dFchCie = TODAY.
dFchCie = dFchCie - DAY(dFchCie) + 1. 

x-CodFchI = dFchCie.        /* OJO */
/* RHC 27.07.2011 un mes mas */
IF MONTH (x-CodFchI) = 01
    THEN x-CodFchI = DATE(12, 01, YEAR(x-CodFchI) - 1 ).
    ELSE x-CodFchI = DATE( MONTH(x-CodFchI) - 1, 01, YEAR(x-CodFchI) ).
    
ASSIGN
    FechaD = YEAR(x-CodFchI) * 10000 + MONTH(x-CodFchI) * 100 + DAY(x-CodFchI)
    FechaH = YEAR(x-CodFchF) * 10000 + MONTH(x-CodFchF) * 100 + DAY(x-CodFchF).

FIND FIRST Empresas WHERE Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
    IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
END.
FIND FIRST FacCfgGn WHERE FacCFgGn.codcia = s-codcia NO-LOCK NO-ERROR.
s-CliVar = FacCfgGn.CliVar.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 12.73
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF TEMP-TABLE t-cab LIKE dwh_ventas_cab.
DEF TEMP-TABLE t-det LIKE dwh_ventas_det.

/* INFORMACION DETALLADA Y DEPURADA */
DEF VAR s-coddiv AS CHAR INIT '00023'.

ASSIGN
    x-CodFchI = 02/01/2011
    x-CodFchF = 02/28/2011
    FechaD = YEAR(x-CodFchI) * 10000 + MONTH(x-CodFchI) * 100 + DAY(x-CodFchI)
    FechaH = YEAR(x-CodFchF) * 10000 + MONTH(x-CodFchF) * 100 + DAY(x-CodFchF).


RUN Carga-Ventas-Basicas.

/* comparamos */
MESSAGE 'inicio de consistencia'.
FOR EACH t-cab NO-LOCK:
    FIND dwh_ventas_cab OF t-cab NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_cab THEN DO:
        MESSAGE 'cabecera no encontrada' t-cab.coddoc t-cab.nrodoc.
        NEXT.
    END.
    FOR EACH t-det OF t-cab NO-LOCK:
        FIND dwh_ventas_det OF dwh_ventas_cab WHERE dwh_ventas_det.codmat = t-det.codmat
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dwh_ventas_cab THEN DO:
            MESSAGE 'detalle no encontrado' t-cab.coddoc t-cab.nrodoc t-det.codmat.
            NEXT.
        END.
        IF t-det.impnaccigv <> dwh_ventas_det.ImpNacCIGV 
            OR t-det.cantidad <> dwh_ventas_det.Cantidad THEN DO:
            MESSAGE 'detalle no conincide' t-cab.coddoc t-cab.nrodoc t-det.codmat.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-Ventas-Basicas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Ventas-Basicas Procedure 
PROCEDURE Carga-Ventas-Basicas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


FOR EACH Gn-Divi NO-LOCK WHERE Gn-Divi.Codcia = S-CodCia 
    AND gn-divi.coddiv BEGINS s-coddiv:
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
        IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
            FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
                AND B-CDOCU.CodDoc = CcbCdocu.Codref 
                AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE B-CDOCU THEN DO:
                MESSAGE 'ref no encontrada' ccbcdocu.codref ccbcdocu.nroref.
                NEXT.
            END.
            IF LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN DO:
                MESSAGE 'servicios o adelanto' b-cdocu.coddoc b-cdocu.nrodoc.
                NEXT.     /* NO SERVICIOS NI ADELANTADAS */
            END.
            IF Ccbcdocu.CndCre = "N" THEN DO:       /* OTRAS */
                /* NO por APLICACION DE ANTICIPO */
                FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
                    FIND FIRST Ccbtabla WHERE Ccbtabla.codcia = s-codcia
                        AND Ccbtabla.tabla = "N/C"
                        AND Ccbtabla.codigo = Ccbddocu.codmat
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE Ccbtabla AND CcbTabla.Libre_L01 = NO THEN DO:
                        MESSAGE 'anticpo de campañ' ccbcdocu.coddoc ccbcdocu.nrodoc.
                        NEXT ESTADISTICAS.
                    END.
                END.
            END.
        END.

        ASSIGN
            pCodDiv = IF Ccbcdocu.DivOri <> '' THEN Ccbcdocu.DivOri ELSE Ccbcdocu.CodDiv.

        /* Ajuste de la division en los valores historicos */
        IF pCodDiv <> '00017' AND Ccbcdocu.codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
        IF pCodDiv <> '00018' AND LOOKUP(Ccbcdocu.codven, '015,173,900,901,902,017') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
        IF pCodDiv <> '00019' AND Ccbcdocu.codven = '081' THEN pCodDiv = '00019'.   /* Mesa redonda */
        IF pCodDiv <> '00099' AND Ccbcdocu.codven = '998' THEN pCodDiv = '00099'.   /* Exportaciones */
        IF pCodDiv <> '00098' AND Ccbcdocu.codven = '157' THEN pCodDiv = '00098'.   /* Refiles */
        IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
            FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
                AND B-CDOCU.CodDoc = CcbCdocu.Codref 
                AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE B-CDOCU THEN DO:
                MESSAGE 'error, no encontrado' ccbcdocu.codref ccbcdocu.nroref.
                NEXT estadisticas.
            END.
            IF B-CDOCU.DivOri <> '00017' AND B-CDOCU.codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
            IF B-CDOCU.DivOri <> '00018' AND LOOKUP(B-CDOCU.codven, '015,173,901,902,017') > 0 THEN pCodDiv = '00018'.    /* Provincias */
            IF B-CDOCU.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
            IF B-CDOCU.DivOri <> '00019' AND B-CDOCU.codven = '081' THEN pCodDiv = '00019'.   /* Mesa redonda */
            IF B-CDOCU.DivOri <> '00099' AND B-CDOCU.codven = '998' THEN pCodDiv = '00099'.   /* Exportaciones */
            IF B-CDOCU.DivOri <> '00098' AND B-CDOCU.codven = '157' THEN pCodDiv = '00098'.   /* Refiles */
        END.
        /* *********************************************************** */
        FIND B-DIVI WHERE B-DIVI.codcia = s-codcia AND B-DIVI.coddiv = pCodDiv NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-DIVI THEN NEXT.
        pCanalVenta = B-DIVI.CanalVenta.
        /* *********************************************************** */
/*         DISPLAY ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc. */
/*         PAUSE 0.                                                                 */
 
        ASSIGN
            x-signo1 = ( IF CcbCdocu.Coddoc = "N/C" THEN -1 ELSE 1 )
            /*x-Day   = DAY(CcbCdocu.FchDoc)*/
            /*-Month = MONTH(CcbCdocu.FchDoc)*/
            /*x-Year  = YEAR(CcbCdocu.FchDoc)*/
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

        RUN Carga-Ventas-Cabecera.
        
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
       FOR EACH CcbDdocu OF CcbCdocu NO-LOCK:
           /* ****************** Filtros ************************* */
           FIND Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
               AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK NO-ERROR.
           IF NOT AVAILABLE Almmmatg THEN DO:
               MESSAGE 'cod no encontra' ccbddocu.coddoc ccbddocu.nrodoc ccbddocu.codmat.
               NEXT.
           END.
           IF Ccbddocu.implin <= 0 THEN DO:
               MESSAGE 'importe negat' ccbddocu.coddoc ccbddocu.nrodoc ccbddocu.codmat.
               NEXT.       /* <<< OJO <<< */
           END.
           /* **************************************************** */
           IF Ccbddocu.ImpCto = ? THEN DO:
               MESSAGE 'costo mal' ccbddocu.coddoc ccbddocu.nrodoc ccbddocu.codmat.
               NEXT.
           END.
           /* **************************************************** */
           FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
               AND Almtconv.Codalter = Ccbddocu.UndVta
               NO-LOCK NO-ERROR.
           F-FACTOR  = 1. 
           IF AVAILABLE Almtconv THEN DO:
              F-FACTOR = Almtconv.Equival.
              IF Almmmatg.FacEqu <> 0 THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
           END.
           RUN Carga-Ventas-Detalle.
       END.  
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Ventas-Cabecera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Ventas-Cabecera Procedure 
PROCEDURE Carga-Ventas-Cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE t-cab.
ASSIGN
    t-cab.CodCia = s-codcia
    t-cab.CodCli = x-codcli        /* x-codunico */
    t-cab.CodDiv = pcoddiv
    t-cab.CodDoc = ccbcdocu.coddoc
    t-cab.CodVen = x-codven
    t-cab.Fecha  = YEAR(ccbcdocu.fchdoc) * 10000 + MONTH(ccbcdocu.fchdoc) * 100 + DAY(ccbcdocu.fchdoc)
    t-cab.FmaPgo = x-fmapgo
    t-cab.NroDoc = ccbcdocu.nrodoc
    t-cab.TpoCmb = ccbcdocu.tpocmb
    t-cab.TpoCmbVta = x-tpocmbvta
    t-cab.TpoCmbCmp = x-tpocmbcmp.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Ventas-Detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Ventas-Detalle Procedure 
PROCEDURE Carga-Ventas-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */

    ASSIGN
        x-ImpLin = Ccbddocu.ImpLin - Ccbddocu.ImpDto2.
    IF x-PorIgv <= 0 THEN x-PorIgv = Ccbddocu.ImpIgv / ( Ccbddocu.ImpLin - Ccbddocu.ImpIgv) * 100.
    /* ************************************************************************ */
    CREATE t-det.
    ASSIGN
        t-det.CodCia = t-cab.codcia
        t-det.CodDiv = t-cab.coddiv
        t-det.CodDoc = t-cab.coddoc
        t-det.NroDoc = t-cab.nrodoc
        t-det.CodMat = ccbddocu.codmat
        t-det.Cantidad = ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can ).
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            t-det.CostoExtCIGV = x-signo1 * CcbDdocu.ImpCto * x-coe / x-TpoCmbCmp
            t-det.CostoExtSIGV = t-det.CostoExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-det.CostoNacCIGV = x-signo1 * CcbDdocu.ImpCto * x-coe
            t-det.CostoNacSIGV = t-det.CostoNacSIGV / ( 1 + ( x-PorIgv / 100) )
            t-det.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            t-det.ImpExtSIGV   = t-det.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-det.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
            t-det.ImpNacSIGV   = t-det.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            t-det.CostoExtCIGV = x-signo1 * CcbDdocu.ImpCto * x-coe 
            t-det.CostoExtSIGV = t-det.CostoExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-det.CostoNacCIGV = x-signo1 * CcbDdocu.ImpCto * x-coe * x-TpoCmbVta
            t-det.CostoNacSIGV = t-det.CostoNacSIGV / ( 1 + ( x-PorIgv / 100) )
            t-det.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            t-det.ImpExtSIGV   = t-det.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-det.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
            t-det.ImpNacSIGV   = t-det.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Procesa-Nota) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Nota Procedure 
PROCEDURE Procesa-Nota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Procesa-Nota-Rebade) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Nota-Rebade Procedure 
PROCEDURE Procesa-Nota-Rebade :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

