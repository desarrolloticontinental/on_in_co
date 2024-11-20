&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-DPEDI FOR FacDPedi.
DEFINE BUFFER BufferCotizacion FOR FacCPedi.
DEFINE BUFFER COTIZACION FOR FacCPedi.



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

DEF NEW SHARED VAR s-CodCia AS INT INIT 001.
DEF NEW SHARED VAR cl-CodCia AS INT INIT 000.
DEF NEW SHARED VAR pv-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-CodDiv AS CHAR INIT '00506'.
DEF NEW SHARED VAR s-User-Id AS CHAR INIT 'SYSTEM'.

DEF VAR s-codalm AS CHAR NO-UNDO.
DEF VAR s-CodDoc AS CHAR NO-UNDO.
DEF VAR s-NroSer AS INTE NO-UNDO.
DEF VAR s-codmon AS INT NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-fmapgo AS CHAR NO-UNDO.
DEF VAR s-tpocmb AS DEC NO-UNDO.
DEF VAR s-nrodec AS INT NO-UNDO.
DEF VAR s-PorIgv AS DEC NO-UNDO.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE VAR x-ClientesVarios AS CHAR.
x-ClientesVarios = FacCfgGn.CliVar.     /* 11 digitos */

DEF VAR x-Articulo-ICBPer AS CHAR INIT '099268'.

DEF NEW SHARED VAR s-DiasVtoCot     AS INTE INIT 7.
DEF NEW SHARED VAR s-CodVen         AS CHAR INIT '021'.
DEF NEW SHARED VAR s-FlgEmpaque     LIKE GN-DIVI.FlgEmpaque INIT NO.
DEF NEW SHARED VAR s-FlgRotacion    LIKE gn-divi.flgrotacion.
DEF NEW SHARED VAR s-FlgMinVenta    LIKE GN-DIVI.FlgMinVenta INIT NO.
DEF NEW SHARED VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.
DEF NEW SHARED VAR s-FlgTipoVenta   LIKE GN-DIVI.FlgPreVta.
DEF NEW SHARED VAR s-DiasVtoPed     LIKE GN-DIVI.DiasVtoPed INIT 7.
DEF NEW SHARED VAR s-TpoPed         AS CHAR INIT "LF".

FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = s-CodDiv
    NO-LOCK NO-ERROR.
IF AVAILABLE gn-divi THEN 
    ASSIGN
        s-DiasVtoCot = GN-DIVI.DiasVtoCot
        s-DiasVtoPed = GN-DIVI.DiasVtoPed
        s-FlgEmpaque = GN-DIVI.FlgEmpaque
        s-VentaMayorista = GN-DIVI.VentaMayorista.

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
   Temp-Tables and Buffers:
      TABLE: B-DPEDI B "?" ? INTEGRAL FacDPedi
      TABLE: BufferCotizacion B "?" ? INTEGRAL FacCPedi
      TABLE: COTIZACION B "?" ? INTEGRAL FacCPedi
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 12.23
         WIDTH              = 51.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF Faccpedi.
DISABLE TRIGGERS FOR LOAD OF Facdpedi.
DISABLE TRIGGERS FOR LOAD OF gn-clie.
DISABLE TRIGGERS FOR LOAD OF gn-clied.
DISABLE TRIGGERS FOR LOAD OF logtabla.
DISABLE TRIGGERS FOR LOAD OF vtadtrkped.
DISABLE TRIGGERS FOR LOAD OF vtactrkped.
DISABLE TRIGGERS FOR LOAD OF faccorre.

/* Barremos las COTIZACIONES con FlgEst = "E" */

DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR x-CuentaItems AS INTE NO-UNDO.
DEF VAR pRowidCOT AS ROWID NO-UNDO.
DEF VAR pRowidPED AS ROWID NO-UNDO.
DEF VAR pComprobante AS CHAR NO-UNDO.
DEF VAR pSede AS CHAR NO-UNDO.

/*  */
DEFINE VAR x-tabla AS CHAR.
DEFINE VAR x-llave_c1 AS CHAR.
DEFINE VAR x-llave_c2 AS CHAR.
DEFINE VAR x-llave_c3 AS CHAR.

x-tabla = "CONFIG-VTAS-EXTERNA".
x-llave_c1 = "VTAS-WEB".
x-llave_c2 = "DIVISIONES".

FOR EACH vtatabla WHERE vtatabla.codcia = s-codcia AND vtatabla.tabla = x-tabla AND
                        vtatabla.llave_c1 = x-llave_c1 AND vtatabla.llave_c2 = x-llave_c2 NO-LOCK:

    s-coddiv = vtatabla.llave_c3.    

    FIND FIRST gn-divi WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = s-CodDiv
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-divi THEN DO:
        ASSIGN
            s-DiasVtoCot = GN-DIVI.DiasVtoCot
            s-DiasVtoPed = GN-DIVI.DiasVtoPed
            s-FlgEmpaque = GN-DIVI.FlgEmpaque
            s-VentaMayorista = GN-DIVI.VentaMayorista.
        PRINCIPAL:
        /* Por cada Pedido del Cliente se va a genera una COT, un PED, una O/D y una HPK */
        FOR EACH BufferCotizacion NO-LOCK WHERE BufferCotizacion.CodCia = s-CodCia AND
            BufferCotizacion.CodDiv = s-CodDiv AND 
            BufferCotizacion.CodDoc = 'COT' AND
            BufferCotizacion.FlgEst = 'E':
            
            RUN MASTER-TRANSACTION (OUTPUT pMensaje).
            /*
            MESSAGE s-coddiv SKIP
                    pMensaje SKIP
                    BufferCotizacion.CodDoc SKIP
                    BufferCotizacion.nroped.
            */
            IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
                PUT UNFORMATTED BufferCotizacion.CodDoc ' ' BufferCotizacion.NroPed ' ERROR: ' pMensaje SKIP.
            END.
            ELSE PUT UNFORMATTED BufferCotizacion.CodDoc ' ' BufferCotizacion.NroPed SKIP.
        END.

    END.
    ELSE DO:
        pMensaje = "La division " + s-coddiv + " no existe en GN-DIVI".
        PUT UNFORMATTED ' ERROR: ' pMensaje SKIP.
    END.

END.

/*
PRINCIPAL:
/* Por cada Pedido del Cliente se va a genera una COT, un PED, una O/D y una HPK */
FOR EACH BufferCotizacion NO-LOCK WHERE BufferCotizacion.CodCia = s-CodCia AND
    BufferCotizacion.CodDiv = s-CodDiv AND 
    BufferCotizacion.CodDoc = 'COT' AND
    BufferCotizacion.FlgEst = 'E':
    RUN MASTER-TRANSACTION (OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        PUT UNFORMATTED BufferCotizacion.CodDoc ' ' BufferCotizacion.NroPed ' ERROR: ' pMensaje SKIP.
    END.
    ELSE PUT UNFORMATTED BufferCotizacion.CodDoc ' ' BufferCotizacion.NroPed SKIP.
END.
*/

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-Detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Detalle Procedure 
PROCEDURE Carga-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-codmat AS CHAR NO-UNDO.
DEF VAR x-nroitm AS INTE NO-UNDO.
DEF VAR x-CanPed AS DECI NO-UNDO.
DEF VAR x-ImpLin AS DECI NO-UNDO.
DEF VAR x-ImpIgv AS DECI NO-UNDO.
DEF VAR x-precio AS DECI NO-UNDO.
DEF VAR x-precio-sin-igv AS DECI NO-UNDO.

FOR EACH Facdpedi OF Faccpedi EXCLUSIVE-LOCK:
    ASSIGN
        Facdpedi.CanPedWeb = Facdpedi.Canped
        Facdpedi.CodMatWeb = Facdpedi.CodMat
        Facdpedi.ImpLinWeb = Facdpedi.ImpLin
        Facdpedi.PreUniWeb = Facdpedi.PreUni .
END.

/* ********************************* */
/* OJO: Ya el flete viene desde Open */
/* ********************************* */
/* RETURN.                               RUBEN   NO SE COORDINO NADAAAAAAAA       */
/* ********************************* */
/* ********************************* */

/* ************************************************************************************************** */
/* Descuentos*/
/* ************************************************************************************************** */
/* IF Facdpedi.ImpDto2 > 0 AND Facdpedi.PorDto2 = 0 THEN DO:                  */
/*     Facdpedi.PorDto2 = ROUND(Facdpedi.ImpDto2 / Facdpedi.ImpLin * 100, 2). */
/* END.                                                                       */
/* ************************************************************************************************** */
/* ************************************************************************************************** */
/* Ic - 26Dic2017 , Flete (Costo de envio) */
/* ************************************************************************************************** */
IF Faccpedi.impfle > 0 THEN DO:
    x-NroItm = 0.
    FOR EACH Facdpedi OF Faccpedi NO-LOCK:
        x-NroItm = x-NroItm + 1.
    END.
    /* Buscar el codigo como interno del FLETE */
    x-CodMat = '044939'.
    /* Se ubico el Codigo Interno  */
    FIND Almmmatg WHERE almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = x-codmat
        AND Almmmatg.tpoart <> 'D'
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg THEN DO:
        ASSIGN
            x-CanPed = 1
            x-Precio = Faccpedi.ImpFle.     /* Con IGV */
        ASSIGN
            x-ImpLin = x-CanPed * x-Precio.
        /* Items */
        x-NroItm = x-NroItm + 1.
        CREATE Facdpedi.
        ASSIGN 
            Facdpedi.CodCia = s-codcia
            FacDPedi.CodDiv = Faccpedi.coddiv
            FacDPedi.CodDoc = Faccpedi.coddoc
            FacDPedi.NroPed = Faccpedi.nroped
            FacDPedi.CodCli = Faccpedi.codcli
            FacDPedi.FchPed = Faccpedi.fchped
            .
        ASSIGN
            Facdpedi.codmat = x-CodMat
            Facdpedi.libre_c05 = "FLETE"
            Facdpedi.Factor = 1 
            Facdpedi.NroItm = x-NroItm 
            Facdpedi.UndVta = (IF AVAILABLE Almmmatg THEN Almmmatg.Chr__01 ELSE '')
            Facdpedi.ALMDES = ENTRY(1,S-CODALM)     /* Almacén por defecto */
            Facdpedi.AftIgv = Almmmatg.AftIgv   /*(IF x-ImpIgv > 0 THEN YES ELSE NO)*/
            Facdpedi.CanPed = x-CanPed
            Facdpedi.PreUni = x-Precio
            Facdpedi.ImpLin = x-ImpLin
            .
        IF Facdpedi.AftIgv 
            THEN Facdpedi.ImpIgv = Facdpedi.ImpLin - ROUND( Facdpedi.ImpLin  / ( 1 + (Faccpedi.PorIgv / 100) ), 4 ).
            ELSE Facdpedi.ImpIgv = 0.
        /* DATOS LISTA EXPRESS */
        ASSIGN
            Facdpedi.CanPedWeb = x-CanPed
            Facdpedi.CodMatWeb = x-CodMat
            Facdpedi.DesMatWeb = "FLETE"
            Facdpedi.ImpLinWeb = x-ImpLin
            Facdpedi.PreUniWeb = x-Precio .


        /*
        NO VAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        
        /* ************************************************************************************************** */
        /* ************************************************************************************************** */
        /* Cálculo SUNAT */
        /* ************************************************************************************************** */
        /* ************************************************************************************************** */
        DEF VAR x-TasaIGV AS DECI NO-UNDO.
        DEF VAR x-factor-descuento AS DECI NO-UNDO.
        x-TasaIGV = Faccpedi.PorIgv / 100.
        /* Tipo de Afectación */
        ASSIGN Facdpedi.cTipoAfectacion = "GRAVADA".
        IF Facdpedi.aftigv = NO THEN ASSIGN Facdpedi.cTipoAfectacion = "EXONERADA".        
        /* Precio Unitario SIN impuestos */
        IF Facdpedi.cTipoAfectacion = "EXONERADA" THEN DO:
            ASSIGN Facdpedi.cPreUniSinImpuesto = Facdpedi.preuni.
        END.
        ELSE DO:
            ASSIGN Facdpedi.cPreUniSinImpuesto = ROUND(Facdpedi.preuni / (1 + x-TasaIGV ),10).
        END.
        /* Factor del Descuento: en decimales */
        x-factor-descuento = 0.
        IF Facdpedi.Por_Dsctos[1] > 0 
            OR Facdpedi.Por_Dsctos[2] > 0 
            OR Facdpedi.Por_Dsctos[3] > 0 /*OR t-Facdpedi.dcto_otros_factor > 0*/ 
            OR Facdpedi.Pordto2 > 0 THEN DO:
            x-factor-descuento = ( 1 -  ( 1 - Facdpedi.Por_Dsctos[1] / 100 ) *
                                   ( 1 - Facdpedi.Por_Dsctos[2] / 100 ) *
                                   ( 1 - Facdpedi.Por_Dsctos[3] / 100 ) * 
                                   ( 1 - Facdpedi.Pordto2 / 100 ) ) * 100.                        
            x-factor-descuento = ROUND(x-factor-descuento / 100 , 5).   /* Puede ser hasta 5 decimales */
        END.
        ASSIGN 
            Facdpedi.FactorDescuento = x-factor-descuento.
        /* Tasa de IGV */
        IF LOOKUP(Facdpedi.cTipoAfectacion,"EXONERADA,INAFECTA") = 0 THEN DO:
            ASSIGN Facdpedi.TasaIGV = x-tasaIGV.
        END.
        /* Importes Unitarios */
        ASSIGN Facdpedi.ImporteUnitarioSinImpuesto = Facdpedi.cPreUniSinImpuesto.
        /* Importe Base del Descuento */
        IF Facdpedi.FactorDescuento > 0 THEN DO:
            ASSIGN 
                Facdpedi.ImporteBaseDescuento = ROUND(Facdpedi.ImporteUnitarioSinImpuesto * Facdpedi.CanPed,2)
                Facdpedi.ImporteDescuento = ROUND(Facdpedi.FactorDescuento * Facdpedi.ImporteBaseDescuento,2).
        END.
        /* Importe total SIN impuestos */
        ASSIGN 
            Facdpedi.ImporteTotalSinImpuesto = ROUND(Facdpedi.ImporteUnitarioSinImpuesto * Facdpedi.CanPed,2) - Facdpedi.ImporteDescuento
            Facdpedi.MontoBaseIGV = ROUND(Facdpedi.ImporteUnitarioSinImpuesto * Facdpedi.CanPed,2) - Facdpedi.ImporteDescuento.
        /* Importes con y sin impuestos */
        ASSIGN 
            Facdpedi.ImporteIGV = ROUND(Facdpedi.MontoBaseIGV * Facdpedi.TasaIGV,2)
            Facdpedi.ImporteTotalImpuesto = Facdpedi.ImporteIGV.
        ASSIGN
            Facdpedi.ImporteUnitarioConImpuesto = ROUND((Facdpedi.ImporteTotalSinImpuesto + Facdpedi.ImporteTotalImpuesto) / Facdpedi.CanPed,4).
        IF Facdpedi.cTipoAfectacion = "EXONERADA" THEN DO:
            ASSIGN Facdpedi.cImporteVentaExonerado = ROUND(Facdpedi.ImporteUnitarioSinImpuesto * Facdpedi.CanPed,4) - Facdpedi.ImporteDescuento.
        END.
        IF LOOKUP(Facdpedi.cTipoAfectacion,"EXONERADA,GRATUITA") = 0 THEN DO:
            ASSIGN  
                Facdpedi.cSumaImpteTotalSinImpuesto = Facdpedi.ImporteTotalSinImpuesto
                Facdpedi.cMontoBaseIGV = Facdpedi.MontoBaseIGV.
        END.
        ASSIGN 
            Facdpedi.cSumaIGV = Facdpedi.ImporteIGV.
        /* Importe Total Con Impuestos */
        ASSIGN
            Facdpedi.cImporteTotalConImpuesto = Facdpedi.MontoBaseIGV + Facdpedi.ImporteIGV.
        /* ************************************************************************************************** */
        /* ************************************************************************************************** */
        /* Control Drop Shipping */
/*         IF FacDPedi.UseInDropShipment = "si" THEN DO:          */
/*             /* Sí es DropShipping */                           */
/*             RUN Registra-DropShipping (INPUT Facdpedi.codmat). */
/*         END.                                                   */
        */

    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-Cotizacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Cotizacion Procedure 
PROCEDURE Graba-Cotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pRowidCOT AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    ASSIGN 
        FacCPedi.FlgEst = "P".    /* APROBADO */
    ASSIGN
        pRowidCOT = ROWID(FacCPedi).   /* Guardamos el puntero de la COT */
    ASSIGN 
        FacCPedi.Hora = STRING(TIME,"HH:MM:SS")
        FacCPedi.Libre_c01 = FacCPedi.CodDiv
        FacCPedi.Libre_d01  = s-NroDec
        FacCPedi.TpoCmb = s-tpocmb
        FacCPedi.FchVen = TODAY + s-DiasVtoCot
        Faccpedi.ImpDto2 = 0.
    FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = Faccpedi.codcli
        NO-LOCK NO-ERROR.
    ASSIGN 
        Faccpedi.Atencion = Faccpedi.DNICli
        FacCPedi.NroCard = ""
        Faccpedi.FaxCli = IF (AVAILABLE gn-clie) THEN SUBSTRING(TRIM(gn-clie.clfcli) + "00",1,2) +
            SUBSTRING(TRIM(gn-clie.clfcli2) + "00",1,2) ELSE "".
    /* ********************************************************************************************** */
    /* UBIGEO POR DEFECTO */
    /* ********************************************************************************************** */
    IF FacCPedi.CodCli = x-ClientesVarios THEN
        ASSIGN
            FacCPedi.Sede      = ''
            FacCPedi.CodPos    = Faccpedi.CodPos       /* Código Postal */
            FacCPedi.Ubigeo[2] = Faccpedi.CodDept 
            FacCPedi.Ubigeo[3] = Faccpedi.CodProv 
            FacCPedi.Ubigeo[4] = Faccpedi.CodDist.
    ELSE DO:
        /* ********************************************************************************************** */
        /* CONTROL DE SEDE Y UBIGEO: POR CLIENTE */
        /* ********************************************************************************************** */
        FIND gn-clied WHERE gn-clied.codcia = cl-codcia
            AND gn-clied.codcli = Faccpedi.codcli
            AND gn-clied.sede = pSede       /* "@@@" */
            NO-LOCK NO-ERROR.
        IF AVAILABLE gn-clied THEN
            ASSIGN
            FacCPedi.Sede      = Gn-ClieD.Sede
            FacCPedi.CodPos    = Gn-ClieD.CodPos
            FacCPedi.Ubigeo[1] = Gn-ClieD.Sede
            FacCPedi.Ubigeo[2] = "@CL"
            FacCPedi.Ubigeo[3] = Gn-ClieD.CodCli.
    END.
    /* ********************************************************************************************** */
    /* ********************************************************************************************** */
    /* Ic 10Feb2016 - Metodo de Pago Lista Express */
    DISABLE TRIGGERS FOR LOAD OF vtatabla.
    DEFINE BUFFER i-vtatabla FOR vtatabla.
  
    DEFINE VAR lxDescuentos AS DEC.    
    DEFINE VAR lxdsctosinigv AS DEC.
    /* Ic - 17Ene2018, ListaExpress desde IVERSA */
    CREATE i-vtatabla.
    ASSIGN 
        i-vtatabla.codcia = s-codcia
        i-vtatabla.tabla = 'MTPGLSTEXPRS'
        i-vtatabla.llave_c1 = FacCPedi.NroPed
        i-vtatabla.llave_c2 = FacCPedi.NroPed /*tt-MetodPagoListaExpress.tt-pedidoweb*/
        i-vtatabla.llave_c3 = FacCPedi.OrdCmp /*tt-MetodPagoListaExpress.tt-metodopago*/
        i-vtatabla.llave_c5 = ""  /*tt-MetodPagoListaExpress.tt-tipopago                        */
        i-vtatabla.llave_c4 = ""  /*tt-MetodPagoListaExpress.tt-nombreclie*/
        i-vtatabla.valor[1] = 0   /*tt-MetodPagoListaExpress.tt-preciopagado*/
        i-vtatabla.valor[2] = 0   /*tt-MetodPagoListaExpress.tt-preciounitario*/
        i-vtatabla.valor[3] = 0   /*tt-MetodPagoListaExpress.tt-costoenvio                          */
        i-vtatabla.valor[4] = 0.   /*tt-MetodPagoListaExpress.tt-descuento.  Importe*/  
    lxDescuentos = i-vtatabla.valor[4].
    lxDescuentos = 0.
    lxdsctosinigv = 0.
    IF lxDescuentos > 0  THEN DO:
          lxdsctosinigv = (lxDescuentos * 100) / 118.
    END.
    ASSIGN 
        faccpedi.impdto2 = lxDescuentos
        faccpedi.importe[3] = lxdsctosinigv.
    RELEASE i-vtatabla.
  
    /* ****************************************************************************************** */
    /* ****************************************************************************************** */
    /* ARTIMETICA SUNAT */
    /* ****************************************************************************************** */
    /*{vtagn/totales-cotizacion-sunat.i &Cabecera="FacCPedi" &Detalle="FacDPedi"}*/
    /* ****************************************************************************************** */
    /* Importes SUNAT */
    /* ****************************************************************************************** */
/*     DEF VAR hProc AS HANDLE NO-UNDO.                             */
/*     RUN sunat/sunat-calculo-importes PERSISTENT SET hProc.       */
/*     RUN tabla-faccpedi IN hProc (INPUT Faccpedi.CodDiv,          */
/*                                  INPUT Faccpedi.CodDoc,          */
/*                                  INPUT Faccpedi.NroPed,          */
/*                                  OUTPUT pMensaje).               */
/*     IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, RETURN 'ADM-ERROR'. */
/*     DELETE PROCEDURE hProc.                                      */
    
    /* ****************************************************************************************** */
    /* ****************************************************************************************** */

    /* ************************ */
    /* TOTAL GENERAL COTIZACION */
    /* ************************ */
    /*{vtagn/totales-cotizacion-unificada.i &Cabecera="FacCPedi" &Detalle="FacDPedi"}*/
    /*
    {vtagn/totales-cotizacion-sunat.i &Cabecera="FacCPedi" &Detalle="FacDPedi"}
    /* ************************************************************************* */
    /* GRABACION DE IMPORTES EN FACCPEDI O CCBCDOCU PARA MANTENER COMPATIBILIDAD */
    /* ************************************************************************* */
    IF Faccpedi.TotalVenta > 0 THEN DO:
        ASSIGN
            Faccpedi.ImpExo = Faccpedi.TotalValorVentaNetoOpExoneradas
            Faccpedi.ImpTot = Faccpedi.TotalVenta
            Faccpedi.ImpIgv = Faccpedi.TotalIGV
            Faccpedi.ImpVta = Faccpedi.TotalValorVentaNetoOpGravadas. 
        ASSIGN
            Faccpedi.ImpBrt = Faccpedi.ImpVta + Faccpedi.ImpDto.
        ASSIGN
            Faccpedi.AcuBon[10] = 0.
        FOR EACH Facdpedi OF Faccpedi NO-LOCK:
            Faccpedi.AcuBon[10] = Faccpedi.AcuBon[10] + Facdpedi.montoTributoBolsaPlastico.
        END.
    END.
    */
    /* Trigger */
    RUN w-faccpedi.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-OD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-OD Procedure 
PROCEDURE Graba-OD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowidPED AS ROWID.
DEF OUTPUT PARAMETER pComprobante AS CHAR NO-UNDO.      /* O/D 123123456 */
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN vtagn/ventas-library PERSISTENT SET hProc.

pMensaje = "".
RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i ~
        &Tabla="Faccpedi" ~
        &Condicion="ROWID(Faccpedi) = pRowidPED" ~
        &Bloqueo="EXCLUSIVE-LOCK" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'" }
    IF Faccpedi.flgest <> "G" THEN DO:
        pMensaje = 'El Pedido ya fue aprobado por:' + CHR(10) +
            'Usuario: ' + FacCPedi.UsrAprobacion + CHR(10) +
            'Fecha: ' + STRING(FacCPedi.FchAprobacion) + CHR(10) + CHR(10) +
            'Proceso abortado'.
        UNDO, RETURN 'ADM-ERROR'.  
    END.
    /* ****************************** */
    /* POR DEFECTO TODO ESTA APROBADO */
    /* ****************************** */
    ASSIGN 
        Faccpedi.FlgEst = "P"
        FacCPedi.FchAprobacion = TODAY
        FacCPedi.UsrAprobacion = s-user-id.

    FOR EACH FacDPedi OF FacCPedi EXCLUSIVE-LOCK:
        ASSIGN  FacDPedi.Flgest = FacCPedi.Flgest.   /* <<< OJO <<< */
    END.
    /* TRACKING */
    FIND Almacen OF Faccpedi NO-LOCK.
    RUN vtagn/pTracking-04 (s-CodCia,
                      Almacen.CodDiv,
                      Faccpedi.CodDoc,
                      Faccpedi.NroPed,
                      s-User-Id,
                      'ANP',
                      'P',
                      DATETIME(TODAY, MTIME),
                      DATETIME(TODAY, MTIME),
                      Faccpedi.coddoc,
                      Faccpedi.nroped,
                      Faccpedi.coddoc,
                      Faccpedi.nroped).
    /* CREAMOS LA ORDEN */
    pMensaje = "".
    RUN VTA_Genera-OD IN hProc ( ROWID(Faccpedi), OUTPUT pComprobante, OUTPUT pMensaje ).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.

    /* Buscamos O/D */
    FIND Faccpedi WHERE Faccpedi.codcia = s-codcia AND
        Faccpedi.coddoc = ENTRY(1,pComprobante,' ') AND
        Faccpedi.nroped = ENTRY(2,pComprobante,' ')
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    /* Trigger */
    IF AVAILABLE Faccpedi THEN RUN w-faccpedi.

END.
DELETE PROCEDURE hProc.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Pedido Procedure 
PROCEDURE Graba-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowidCOT AS ROWID.
DEF INPUT PARAMETER s-CodDoc AS CHAR.       /* PED */
DEF OUTPUT PARAMETER pRowidPED AS ROWID NO-UNDO.   /* PED */
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF VAR x-CodAlm AS CHAR NO-UNDO.     
DEF VAR pFchEnt AS DATE NO-UNDO.
DEF VAR i-Cuenta AS INT NO-UNDO.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND COTIZACION WHERE ROWID(COTIZACION) = pRowidCOT NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        {lib/mensaje-de-error.i &MensajeError="pMensaje" &CuentaError="i-Cuenta"}
        UNDO, RETURN 'ADM-ERROR'.
    END.
    {lib/lock-genericov3.i ~
        &Tabla="FacCorre" ~
        &Alcance="FIRST" ~
        &Condicion="FacCorre.CodCia = S-CODCIA ~
        AND FacCorre.CodDoc = s-CodDoc ~
        AND FacCorre.CodDiv = s-CodDiv ~
        AND FacCorre.FlgEst = YES" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'"}
    IF FacCorre.FlgCic = NO THEN DO:
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            pMensaje = 'Se ha llegado al límite del correlativo: ' + string(FacCorre.NroFin) + CHR(10) +
                'No se puede generar el documento ' + FacCorre.CodDoc + ' serie ' + string(FacCorre.NroSer,'999').
            UNDO, RETURN "ADM-ERROR".
        END.
    END.
    IF FacCorre.FlgCic = YES THEN DO:
        /* REGRESAMOS AL NUMERO 1 */
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            IF FacCorre.NroIni > 0 THEN FacCorre.Correlativo = FacCorre.NroIni.
            ELSE FacCorre.Correlativo = 1.
        END.
    END.
    REPEAT:
        IF NOT CAN-FIND(FIRST FacCPedi WHERE FacCPedi.codcia = s-codcia
                        AND FacCPedi.coddiv = FacCorre.coddiv
                        AND FacCPedi.coddoc = FacCorre.coddoc
                        AND FacCPedi.nroped = STRING(FacCorre.nroser, '999') + 
                        STRING(FacCorre.correlativo, '999999')
                        NO-LOCK)
            THEN LEAVE.
        ASSIGN
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
    END.

    CREATE FacCPedi.
    BUFFER-COPY COTIZACION TO Faccpedi
        ASSIGN
        Faccpedi.CodDoc = s-coddoc 
        Faccpedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        Faccpedi.CodRef = COTIZACION.CodDoc
        Faccpedi.NroRef = COTIZACION.NroPed
        FacCPedi.Libre_f02 = FacCPedi.FchEnt
        Faccpedi.FlgEst = "G"       /* FLAG TEMPORAL POR APROBAR */
        FacCPedi.Libre_c02 = ""  /* PCO o NORMAL */
        Faccpedi.Usuario = S-USER-ID
        Faccpedi.Hora   = STRING(TIME,"HH:MM:SS")
        NO-ERROR
        .
    IF ERROR-STATUS:ERROR THEN DO:
        {lib/mensaje-de-error.i &MensajeError="pMensaje" &CuentaError="i-Cuenta"}
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        pRowidPED = ROWID(FacCPedi).   /* Guardamos el puntero de la COT */
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    /* TRACKING */
    RUN vtagn/pTracking-04 (s-CodCia,
                            s-CodDiv,
                            Faccpedi.CodDoc,
                            Faccpedi.NroPed,
                            s-User-Id,
                            'GNP',
                            'P',
                            DATETIME(TODAY, MTIME),
                            DATETIME(TODAY, MTIME),
                            Faccpedi.CodDoc,
                            Faccpedi.NroPed,
                            Faccpedi.CodRef,
                            Faccpedi.NroRef).
    /* ********************************************************************************************** */
    /* Definimos cuantos almacenes hay de despacho */
    /* Cuando se modifica un pedido hay solo un almacén */
    /* ********************************************************************************************** */
    x-CodAlm = ENTRY(1, s-CodAlm).        /* El primer almacén por defecto */
    ASSIGN 
        FacCPedi.CodAlm = x-CodAlm.               /* <<<< OJO <<<< : Almacén del PEDIDO */
    /* ********************************************************************************************** */
    /* Division destino */
    /* ********************************************************************************************** */
    FIND Almacen OF Faccpedi NO-LOCK NO-ERROR.
    IF AVAILABLE Almacen THEN FacCPedi.DivDes = Almacen.CodDiv.
    /* ********************************************************************************************** */
    FOR EACH B-DPEDI OF COTIZACION NO-LOCK, FIRST Almmmatg OF B-DPEDI NO-LOCK:
        CREATE Facdpedi.
        BUFFER-COPY B-DPEDI
            EXCEPT B-DPEDI.TipVta    /* Campo con valor A, B, C o D */
            TO Facdpedi
            ASSIGN
            Facdpedi.CodCia = Faccpedi.CodCia
            Facdpedi.CodDiv = Faccpedi.CodDiv
            Facdpedi.coddoc = Faccpedi.coddoc
            Facdpedi.NroPed = Faccpedi.NroPed
            Facdpedi.FchPed = Faccpedi.FchPed
            Facdpedi.Hora   = Faccpedi.Hora 
            Facdpedi.FlgEst = Faccpedi.FlgEst.
    END.
    /* ********************************************************************************************** */
    /* Reactualizamos la Fecha de Entrega                                             */
    /* ********************************************************************************************** */
    IF COTIZACION.FchPed = COTIZACION.FchEnt 
        THEN FacCPedi.Cliente_Recoge = YES.     /* CR lo puede despachar cualquier día */
    ELSE FacCPedi.Cliente_Recoge = NO.          /* Por calendario logístico */
    /* ********************************************************************************************** */
    /* Reactualizamos la Fecha de Entrega                                             */
    /* ********************************************************************************************** */
    /* LA RUTINA VA A DECIDIR SI EL CALCULO ES POR UBIGEO O POR GPS */
    RUN logis/p-fecha-de-entrega (
        FacCPedi.CodDoc,              /* Documento actual */
        FacCPedi.NroPed,
        INPUT-OUTPUT pFchEnt,
        OUTPUT pMensaje).
    IF pMensaje > '' THEN UNDO, RETURN 'ADM-ERROR'.
    ASSIGN
        FacCPedi.FchEnt = pFchEnt.
    /* Trigger */
    RUN w-faccpedi.
    /* ********************************************************************************************** */
    /* Actualizamos la cotizacion */
    /* ********************************************************************************************** */
    RUN vta2/pactualizacotizacion ( ROWID(Faccpedi), "C", OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    /* ********************************************************************************************** */
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MASTER-TRANSACTION) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MASTER-TRANSACTION Procedure 
PROCEDURE MASTER-TRANSACTION :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i ~
        &Tabla="Faccpedi" ~
        &Alcance="FIRST" ~
        &Condicion="ROWID(Faccpedi) = ROWID(BufferCotizacion)"~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR NO-WAIT" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'"}
    /* ***************************************************************************** */
    /* Deben estar todos los items */
    /* ***************************************************************************** */
    x-CuentaItems = 0.
    FOR EACH Facdpedi OF Faccpedi NO-LOCK:
        x-CuentaItems = x-CuentaItems + 1.
    END.
    IF Faccpedi.Items <> x-CuentaItems THEN DO:
        pMensaje = "Items incompletos".
        RETURN 'ADM-ERROR'.
    END.
    /* ***************************************************************************** */
    /* Actualizamos Control de Migración */
    /* ***************************************************************************** */
    ASSIGN
        Faccpedi.FlgEst = "P"
        /* 
            IC - En reunion 23Feb2022, coordinacion con Brayan y Daniel Llican
            estos FecAct, HorAct los graba brayan al realizar el INSERT        
        Faccpedi.FecAct = TODAY
        Faccpedi.HorAct = STRING(TIME, 'HH:MM:SS')
        */
        Faccpedi.UsrAct = s-User-Id
        FacCPedi.flagmigracion = '1'
        FacCPedi.MigFecha = TODAY
        FacCPedi.MigHora = STRING(TIME,"HH:MM:SS")
        FacCPedi.MigUsuario = s-user-id.
    /* ***************************************************************************** */
    /* Creamos el cliente si no existe */
    /* ***************************************************************************** */
    RUN Registra-Cliente (OUTPUT pSede, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        pMensaje = "NO se pudo crear el cliente".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        s-CodAlm = FacCPedi.CodAlm
        s-PorIgv = FacCfgGn.PorIgv
        s-CodMon = FacCPedi.CodMon
        s-CodCli = FacCPedi.CodCli
        s-FmaPgo = FacCPedi.FmaPgo      /*'000'*/
        s-TpoCmb = 1
        s-NroDec = 4.
    FIND FIRST TcmbCot WHERE  TcmbCot.Codcia = 0
        AND  (TcmbCot.Rango1 <= TODAY - TODAY + 1
              AND   TcmbCot.Rango2 >= TODAY - TODAY + 1)
        NO-LOCK NO-ERROR.
    IF AVAIL TcmbCot THEN S-TPOCMB = TcmbCot.TpoCmb.  
    /* RHC 11.08.2014 TC Caja Compra */
    FOR EACH gn-tccja NO-LOCK BY Fecha:
        IF TODAY >= Fecha THEN s-TpoCmb = Gn-TCCja.Compra.
    END.
    /* ***************************************************************************** */
    /* CARGAMOS DETALLE */
    /* ***************************************************************************** */
    RUN Carga-Detalle.
    /* ***************************************************************************** */
    /* COTIZACION */
    /* ***************************************************************************** */
    RUN Graba-Cotizacion (OUTPUT pRowidCOT, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "NO pudo grabar la cotización".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* ***************************************************************************** */
    /* RHC 20/06/2020 Si el cliente no es válido NO genera ni PED ni O/D */
    /* ***************************************************************************** */
    FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = BufferCotizacion.codcli
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie AND gn-clie.FlgSit = "C" THEN RETURN 'OK'.
    /* ***************************************************************************** */
    /* SOLO SI LOS DOS SWITCH (ALM e INV ESTAN ACTIVOS Y VIGENTES SE CONTINÚA */
    /* ***************************************************************************** */
    FIND FacTabla WHERE FacTabla.CodCia = s-CodCia AND
        FacTabla.Tabla = "LF" AND
        FacTabla.Codigo = "ALM" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FacTabla THEN RETURN 'OK'.
    IF FacTabla.Campo-L[1] = NO THEN RETURN 'OK'.
    IF FacTabla.Campo-D[1] <> ? AND FacTabla.Campo-D[1] < TODAY THEN RETURN 'OK'.
    FIND FacTabla WHERE FacTabla.CodCia = s-CodCia AND
        FacTabla.Tabla = "LF" AND
        FacTabla.Codigo = "INV" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FacTabla THEN RETURN 'OK'.
    IF FacTabla.Campo-L[1] = NO THEN RETURN 'OK'.
    IF FacTabla.Campo-D[1] <> ? AND FacTabla.Campo-D[1] < TODAY THEN RETURN 'OK'.
    /* ***************************************************************************** */
    /* PEDIDO LOGISTICO */
    /* ***************************************************************************** */
    RUN Graba-Pedido (INPUT pRowidCOT, INPUT "PED", OUTPUT pRowidPED, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "NO pudo grabar el pedido".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    
    /* ***************************************************************************** */
    /* ORDEN DE DESPACHO */
    /* ***************************************************************************** */
    pComprobante = "".
    RUN Graba-OD (INPUT pRowidPED, OUTPUT pComprobante, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "NO pudo grabar la Orden".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Registra-Cliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registra-Cliente Procedure 
PROCEDURE Registra-Cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pSede AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF VAR cEvento AS CHAR INIT "CREATE" NO-UNDO.
DEF VAR x-Cuenta AS INT NO-UNDO.

IF FacCPedi.CodCli = x-ClientesVarios THEN RETURN 'OK'.

FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia 
    AND gn-clie.codcli = FacCPedi.CodCli
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-clie THEN DO:
    /* Nuevo Cliente */
    /* Genera automáticamente un registro Sede = "@@@" */
    CREATE gn-clie.
    ASSIGN
        gn-clie.CodCia      = cl-codcia
        gn-clie.CodCli      = FacCPedi.CodCli
        gn-clie.Libre_C01   = "N"
        gn-clie.NomCli      = CAPS(FacCPedi.NomCli)
        gn-clie.DirCli      = CAPS(FacCPedi.DirCli)
        gn-clie.Ruc         = FacCPedi.RucCli
        gn-clie.DNI         = FacCPedi.DNICli
        gn-clie.clfCli      = "C" 
        gn-clie.CodPais     = "01" 
        gn-clie.CodVen      = FacCPedi.CodVen
        gn-clie.CndVta      = FacCPedi.FmaPgo
        gn-clie.Fching      = TODAY 
        gn-clie.usuario     = S-USER-ID 
        gn-clie.TpoCli      = "1"
        gn-clie.CodDiv      = FacCPedi.CodDiv
        gn-clie.Rucold      = "NO"
        gn-clie.Libre_L01   = NO
        gn-clie.FlgSit      = 'A'    /* Activo */
        gn-clie.FlagAut     = 'A'   /* Autorizado */
        gn-clie.CodDept     = FacCPedi.CodDept 
        gn-clie.CodProv     = FacCPedi.CodProv 
        gn-clie.CodDist     = FacCPedi.CodDist
        gn-clie.E-Mail      = FacCPedi.E-Mail
        gn-clie.Telfnos[1]  = FacCPedi.TelephoneContactReceptor
        gn-clie.SwCargaSunat = "N"
        NO-ERROR
        .
    IF ERROR-STATUS:ERROR THEN DO:
        {lib/mensaje-de-error.i &CuentaError="x-Cuenta" &MensajeError="pMensaje"}
        UNDO, RETURN 'ADM-ERROR'.
    END.
    cEvento = "CREATE".
    /* ****************************************************************************** */
    /* Datos Adicionales */
    /* ****************************************************************************** */
    CASE TRUE:
        WHEN gn-clie.codcli BEGINS '20' THEN gn-clie.Libre_C01 = "J".
        WHEN gn-clie.codcli BEGINS '10' THEN gn-clie.Libre_C01 = "N".
        WHEN gn-clie.codcli BEGINS '15' THEN gn-clie.Libre_C01 = "N".
        WHEN gn-clie.codcli BEGINS '17' THEN gn-clie.Libre_C01   = "E".
        /* 03/11/2023: Actualmente el carnet de extranjeria tiene 9 dígitos Giuliana Chirinos S.Leon */
        WHEN LENGTH(gn-clie.DNI) <> 8 THEN gn-clie.Libre_C01 = "E".
    END CASE.
    /* ****************************************************************************** */
    /* RHC 24/06/2020 Agregar información a la dirección */
    /* ****************************************************************************** */
    /* Armado de la dirección */
    DEF VAR pAddress AS CHAR NO-UNDO.
    pAddress = CAPS(FacCPedi.DirCli).
    FIND TabDepto WHERE TabDepto.CodDepto = FacCPedi.CodDept NO-LOCK NO-ERROR.
    FIND TabProvi WHERE TabProvi.CodDepto = FacCPedi.CodDept AND
        TabProvi.CodProvi = FacCPedi.CodProv NO-LOCK NO-ERROR.
    FIND TabDistr WHERE TabDistr.CodDepto = FacCPedi.CodDept AND
        TabDistr.CodProvi = FacCPedi.CodProv AND
        TabDistr.CodDistr = FacCPedi.CodDist NO-LOCK NO-ERROR.
    IF AVAILABLE TabDepto AND AVAILABLE TabProvi AND AVAILABLE TabDistr THEN DO:
        pAddress = TRIM(pAddress) + ' ' + 
                    CAPS(TRIM(TabDepto.NomDepto)) + ' - ' +
                    CAPS(TRIM(TabProvi.NomProvi)) + ' - ' +
                    CAPS(TabDistr.NomDistr).
    END.
    ASSIGN
        Gn-Clie.DirCli = pAddress.
    /* Actualizamos datos COTIZACION */
    ASSIGN
        FacCPedi.NomCli = Gn-Clie.NomCli
        FacCPedi.DirCli = Gn-Clie.DirCli.
    /* ****************************************************************************** */
    /* Triggers */
    /* ****************************************************************************** */
    RUN w-gn-clie (INPUT cEvento).
    /* Réplica */
    RUN rpl-gn-clie.
    /* Log */
    RUN lib/logtabla ("gn-clie", STRING(gn-clie.codcia, '999') + '|' +
                      STRING(gn-clie.codcli, 'x(11)'), cEvento).
END.
ELSE DO:
    FIND CURRENT gn-clie EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR THEN DO:
        {lib/mensaje-de-error.i &CuentaError="x-Cuenta" &MensajeError="pMensaje"}
        UNDO, RETURN 'ADM-ERROR'.
    END.
/*     ASSIGN                                          */
/*         gn-clie.NomCli      = CAPS(FacCPedi.NomCli) */
/*         gn-clie.DirCli      = CAPS(FacCPedi.DirCli) */
/*         gn-clie.Ruc         = FacCPedi.RucCli       */
/*         gn-clie.DNI         = FacCPedi.DNICli       */
/*         gn-clie.FchAct = TODAY                      */
/*         gn-clie.UsrAut = s-user-id                  */
/*         gn-clie.CodDept     = FacCPedi.CodDept      */
/*         gn-clie.CodProv     = FacCPedi.CodProv      */
/*         gn-clie.CodDist     = FacCPedi.CodDist.     */
    cEvento = "UPDATE".
END.
/* ****************************************************************************** */
/* RHC Control de Sede del Cliente */
/* ****************************************************************************** */
IF TRUE <> (FacCPedi.IDCustomer > '') THEN RETURN 'OK'.

FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia 
    AND gn-clie.codcli = FacCPedi.CodCli
    NO-LOCK NO-ERROR.
FIND FIRST gn-clied OF gn-clie WHERE gn-clied.sedeclie = FacCPedi.IDCustomer
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-clied THEN DO:
    DEFINE VAR x-correlativo AS INT INIT 0.

    DEF BUFFER x-gn-clied FOR gn-clied.

    FOR EACH x-gn-clieD OF gn-clie NO-LOCK:
        IF x-gn-clieD.sede <> '@@@' THEN DO:
            ASSIGN 
                x-correlativo = MAXIMUM(INTEGER(TRIM(x-gn-clieD.sede)),x-correlativo)
                NO-ERROR.
        END.
    END.
    
    CREATE gn-clied.
    ASSIGN
        Gn-ClieD.CodCia = gn-clie.codcia
        Gn-ClieD.CodCli = gn-clie.codcli
        Gn-ClieD.FchCreacion = TODAY
        Gn-ClieD.Sede = STRING(x-correlativo + 1,"9999")
        Gn-ClieD.SedeClie = FacCPedi.IDCustomer
        Gn-ClieD.UsrCreacion = s-user-id
        Gn-ClieD.DomFiscal = NO
        Gn-ClieD.SwSedeSunat = "M"   /* MANUAL */
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        {lib/mensaje-de-error.i &CuentaError="x-Cuenta" &MensajeError="pMensaje"}
        UNDO, RETURN 'ADM-ERROR'.
    END.
END.
ELSE DO:
    FIND CURRENT gn-clied EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR THEN DO:
        {lib/mensaje-de-error.i &CuentaError="x-Cuenta" &MensajeError="pMensaje"}
        UNDO, RETURN 'ADM-ERROR'.
    END.
END.
/* Armado de la dirección */
pAddress = CAPS(FacCPedi.DirCli).
FIND TabDepto WHERE TabDepto.CodDepto = FacCPedi.CodDept NO-LOCK NO-ERROR.
FIND TabProvi WHERE TabProvi.CodDepto = FacCPedi.CodDept AND
    TabProvi.CodProvi = FacCPedi.CodProv NO-LOCK NO-ERROR.
FIND TabDistr WHERE TabDistr.CodDepto = FacCPedi.CodDept AND
    TabDistr.CodProvi = FacCPedi.CodProv AND
    TabDistr.CodDistr = FacCPedi.CodDist NO-LOCK NO-ERROR.
IF AVAILABLE TabDepto AND AVAILABLE TabProvi AND AVAILABLE TabDistr THEN DO:
    pAddress = TRIM(pAddress) + ' ' + 
                CAPS(TRIM(TabDepto.NomDepto)) + ' - ' +
                CAPS(TRIM(TabProvi.NomProvi)) + ' - ' +
                CAPS(TabDistr.NomDistr).
END.
ASSIGN
    Gn-ClieD.CodDept = FacCPedi.CodDept
    Gn-ClieD.CodProv = FacCPedi.CodProv
    Gn-ClieD.CodDist = FacCPedi.CodDist
    Gn-ClieD.DirCli  = pAddress
    Gn-ClieD.Referencias = FacCPedi.ReferenceAddress
    Gn-ClieD.Libre_c04 = FacCPedi.TelephoneContactReceptor 
    Gn-ClieD.Libre_c05 = FacCPedi.ContactReceptorName.
FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
    TabDistr.CodProvi = Gn-ClieD.CodProv AND
    TabDistr.CodDistr = Gn-ClieD.CodDist
    NO-LOCK NO-ERROR.
IF AVAILABLE TabDistr THEN DO:
    ASSIGN
        Gn-ClieD.Codpos = TabDistr.CodPos NO-ERROR.
END.
ASSIGN
    pSede = Gn-ClieD.Sede.      /* OJO */

/* ******* */
/* Réplica */
/* ******* */
RUN rpl-gn-clied.
/* ******* */

RELEASE gn-clied.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Registra-DropShipping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registra-DropShipping Procedure 
PROCEDURE Registra-DropShipping :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.

DEF VAR s-coddiv AS CHAR INIT '00519' NO-UNDO.
DEF VAR s-Tabla AS CHAR INIT 'DROPSHIPPING' NO-UNDO.

FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-codcia 
    AND VtaTabla.Tabla  = s-Tabla
    AND VtaTabla.Llave_c1 = pcodmat
    NO-LOCK NO-ERROR.
IF AVAILABLE VtaTabla THEN RETURN.
CREATE VtaTabla.
ASSIGN
    VtaTabla.CodCia = s-codcia 
    VtaTabla.Tabla  = s-Tabla
    VtaTabla.Llave_c1 = pcodmat
    NO-ERROR.
IF ERROR-STATUS:ERROR THEN UNDO, RETURN.

RUN Replica-DropShipping.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-DropShipping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-DropShipping Procedure 
PROCEDURE Replica-DropShipping :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = vtatabla
    &Key    =  "string(vtatabla.codcia,'999') + string(vtatabla.tabla,'x(20)') + ~
    string(vtatabla.llave_c1,'x(30)') + string(vtatabla.llave_c2,'x(20)') + ~
        string(vtatabla.llave_c3,'x(20)') + string(vtatabla.llave_c4,'x(20)') + ~
        string(vtatabla.llave_c5,'x(20)')"
    &Prg    = r-vtatabla
    &Event  = WRITE
    &FlgDB0 = TRUE  /* Replicar de la sede remota a la base principal */
    &FlgDB1 = NO    /* Plaza Lima Norte 00501 */
    &FlgDB2 = NO    /* Surquillo 00023 */
    &FlgDB3 = NO    /* Chorrillos 00027 */
    &FlgDB4 = NO    /* San Borja 00502 */
    &FlgDB5 = NO    /* La Molina 00503 */
    &FlgDB6 = NO    /* Beneficiencia 00504 */
    &FlgDB7 = NO    /* Plaza Norte 00505 */
    &FlgDB8 = NO    /* La Rambla 00507 */
    &FlgDB9 = NO    /* San Isidro 00508 */
    &FlgDB10 = NO   /* 00065 */
    &FlgDB11 = NO   /* Atocongo 00510 */
    &FlgDB12 = NO   /* Angamos 00511 */
    &FlgDB13 = NO   /* Salaverry 00512 */
    &FlgDB14 = NO   /* Centro Civico 00513 */
    &FlgDB15 = NO   /* Primavera 00514 */
    &FlgDB16 = NO   /* Bellavista 00516 */
    &FlgDB17 = NO
    &FlgDB18 = NO   /* AREQUIPA*/
    &FlgDB19 = NO       /* EXPOLIBRERIA */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL */
    &FlgDB30 = NO   /* SERVIDOR ATE */
    }


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rpl-gn-clie) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rpl-gn-clie Procedure 
PROCEDURE rpl-gn-clie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = gn-clie
    &Key    =  "string(gn-clie.codcia,'999') + string(gn-clie.codcli, 'x(11)')"
    &Prg    = r-gn-clie
    &Event  = WRITE
    &FlgDb0 = NO    /* Replicar sede remota a la base principal */
    &FlgDB1 = NO    /* Replicar a la División 00501 (Plaza Norte) */
    &FlgDB2 = NO    /* Replicar a la División 00023 (Surquillo) */
    &FlgDB3 = NO
    &FlgDB4 = NO
    &FlgDB5 = NO
    &FlgDB6 = NO
    &FlgDB7 = NO
    &FlgDB8 = NO    /* La Rambla 00507 */
    &FlgDB9 = NO    /* San Isidro 00508 */
    &FlgDB10 = NO   /* 00065 */
    &FlgDB11 = NO   /* Atocongo 00510 */
    &FlgDB12 = NO   /* Angamos 00511 */
    &FlgDB13 = NO   /* Salaverry 00512 */
    &FlgDB14 = NO   /* Centro Civico 00513 */
    &FlgDB15 = NO   /* Primavera 00514 */
    &FlgDB16 = NO   /* Bellavista 00516 */
    &FlgDB17 = NO
    &FlgDB18 = NO   /* AREQUIPA */
    &FlgDB19 = NO       /* EXPOLIBRERIA */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL */
    &FlgDB30 = NO   /* SERVIDOR ATE */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rpl-gn-clied) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rpl-gn-clied Procedure 
PROCEDURE rpl-gn-clied :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = gn-clied
    &Key    =  "string(gn-clied.codcia,'999') + string(gn-clied.codcli, 'x(11)') + string(gn-clied.sede)"
    &Prg    = r-gn-clied
    &Event  = WRITE
    &FlgDb0 = NO    /* Replicar sede remota a la base principal */
    &FlgDB1 = YES    /* Replicar a la División 00501 (Plaza Norte) */
    &FlgDB2 = YES    /* Replicar a la División 00023 (Surquillo) */
    &FlgDB3 = YES
    &FlgDB4 = YES
    &FlgDB5 = YES
    &FlgDB6 = YES
    &FlgDB7 = YES
    &FlgDB8 = YES    /* La Rambla 00507 */
    &FlgDB9 = YES    /* San Isidro 00508 */
    &FlgDB10 = YES   /* 00065 */
    &FlgDB11 = YES   /* Atocongo 00510 */
    &FlgDB12 = YES   /* Angamos 00511 */
    &FlgDB13 = YES   /* Salaverry 00512 */
    &FlgDB14 = YES   /* Centro Civico 00513 */
    &FlgDB15 = YES   /* Primavera 00514 */
    &FlgDB16 = YES   /* Bellavista 00516 */
    &FlgDB17 = YES
    &FlgDB18 = YES   /* AREQUIPA */
    &FlgDB19 = NO       /* EXPOLIBRERIA */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL */
    &FlgDB30 = NO   /* SERVIDOR DESARROLLO */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-w-faccpedi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE w-faccpedi Procedure 
PROCEDURE w-faccpedi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ************************************************************** */
/* COMPLETAR DATOS */
IF Faccpedi.DivDes = "" THEN Faccpedi.DivDes = Faccpedi.CodDiv.
IF Faccpedi.FchVen = ? THEN Faccpedi.FchVen = TODAY.

/* ******************************************** */
/* RHC 28/04/2020 NO tomar en cuenta los fletes */
/* ******************************************** */
ASSIGN
    Faccpedi.Items = 0          /* Items */
    Faccpedi.Peso = 0           /* Peso */
    Faccpedi.Volumen = 0.       /* Volumen */
FOR EACH Facdpedi OF Faccpedi NO-LOCK,
    FIRST Almmmatg OF Facdpedi NO-LOCK,
    FIRST Almtfami OF Almmmatg NO-LOCK WHERE Almtfami.Libre_c01 <> "SV":
    ASSIGN
        Faccpedi.Items = Faccpedi.Items + 1
        Faccpedi.Peso = Faccpedi.Peso + (Facdpedi.CanPed * Facdpedi.Factor * Almmmatg.PesMat)
        Faccpedi.Volumen = Faccpedi.Volumen + (Facdpedi.CanPed * Facdpedi.Factor * Almmmatg.Libre_d02 / 1000000).
END.
/* IMPORTES en S/ */
ASSIGN
    Faccpedi.AcuBon[8] = Faccpedi.ImpTot * (IF Faccpedi.CodMon = 2 THEN Faccpedi.TpoCmb ELSE 1).   /* Importes */
/* ************************************************************** */
/* LISTA DE PRECIOS */
IF LOOKUP(Faccpedi.CodDoc, 'COT,P/M') > 0 THEN DO:
    IF TRUE <> (Faccpedi.Lista_de_Precios > '') THEN DO:
        IF Faccpedi.CodDoc = 'COT'  AND Faccpedi.Libre_c01 > ''
            THEN Faccpedi.Lista_de_Precios = Faccpedi.Libre_c01.
        IF Faccpedi.CodDoc = 'P/M' 
            THEN Faccpedi.Lista_de_Precios = Faccpedi.CodDiv.
        IF TRUE <> (Faccpedi.Lista_de_Precios > '') 
            THEN Faccpedi.Lista_de_Precios = Faccpedi.CodDiv.
    END.
END.

IF Faccpedi.coddoc = "COT" THEN DO:
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = Faccpedi.codcli
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN DO:
        ASSIGN
            FacCPedi.FaxCli = SUBSTRING(TRIM(gn-clie.clfcli) + "00",1,2) +
                            SUBSTRING(TRIM(gn-clie.clfcli2) + "00",1,2).
        FOR EACH Facdpedi OF Faccpedi NO-LOCK:
            IF Facdpedi.codaux = "*" THEN DO:
                Faccpedi.FaxCli = Faccpedi.FaxCli + Facdpedi.CodAux.
                LEAVE.
            END.
        END.
    END.
END.

/* RHC 15/10/2018 STATUS DE PEDIDOS: Renato Lira */
/* SE_ALM Nuevo Documento */
IF LOOKUP(Faccpedi.CodDoc, 'O/D,OTR,O/M') > 0 THEN DO:
    RUN gn/p-log-status-pedidos (Faccpedi.CodDoc, 
                                 Faccpedi.NroPed, 
                                 'TRCKPED', 
                                 'SE_ALM',
                                 '',
                                 ?).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-w-gn-clie) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE w-gn-clie Procedure 
PROCEDURE w-gn-clie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pEvento AS CHAR.

FIND FIRST gn-cliecyg OF gn-clie NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-cliecyg THEN DO:
    CREATE gn-cliecyg.
    BUFFER-COPY gn-clie 
        EXCEPT gn-clie.ApeMat gn-clie.ApePat gn-clie.Nombre
        TO gn-cliecyg NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN DELETE gn-cliecyg.
    RELEASE gn-cliecyg.
END.
/* CONTROL DE MIGRACION AL OPENORANGE */
CICLO:
DO ON ERROR UNDO, LEAVE:
    IF pEvento = "CREATE" THEN DO:
        ASSIGN
            gn-clie.FlagFecha = STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM:SS')
            gn-clie.FlagMigracion = "N"
            gn-clie.FlagTipo      = "I"
            gn-clie.FlagUsuario   = s-user-id
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.
    END.
    ELSE DO:
        ASSIGN
            gn-clie.FlagFecha = STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM:SS')
            gn-clie.FlagMigracion = "N"
            gn-clie.FlagTipo      = "U"
            gn-clie.FlagUsuario   = s-user-id.
    END.
END.
/* RHC 21/10/2019 */
/* RHC 31/10/18 Se crea un refgistro en gn-clied automaticamente */
FIND FIRST gn-clied OF gn-clie WHERE gn-clied.sede = "@@@" NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-clied THEN DO:
    CREATE gn-clied.
    BUFFER-COPY gn-clie TO gn-clied
        ASSIGN
        Gn-ClieD.sedeclie = FacCPedi.IDCustomer
        Gn-ClieD.DomFiscal = YES
        Gn-ClieD.Sede = "@@@"   /* Valor que no se puede anular */
        NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN UNDO, RETURN ERROR.
    IF gn-clie.SwCargaSunat = "S" THEN  Gn-ClieD.SwSedeSunat = "S".
    FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
        TabDistr.CodProvi = Gn-ClieD.CodProv AND
        TabDistr.CodDistr = Gn-ClieD.CodDist
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN DO:
        ASSIGN
            Gn-ClieD.Codpos = TabDistr.CodPos NO-ERROR.
    END.
    /* ******* */
    /* Réplica */
    /* ******* */
    RUN rpl-gn-clied.
    /* ******* */
    RELEASE gn-clied.
END.
/* ************************************************************** */

/* Parche */
IF gn-clie.FchIng <> ? AND gn-clie.FchAct = ? 
    THEN gn-clie.FchAct = gn-clie.FchIng.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

