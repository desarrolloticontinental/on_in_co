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

DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR FechaD AS INT NO-UNDO.
DEF VAR FechaH AS INT NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
DEF VAR x-signo1 AS INT INIT 1 NO-UNDO.
DEF VAR x-ImpLin AS DEC NO-UNDO.
DEF VAR x-ImpTot AS DEC NO-UNDO.     /* IMporte NETO de venta */
DEF VAR x-TpoCmbCmp AS DECI INIT 1 NO-UNDO.
DEF VAR x-TpoCmbVta AS DECI INIT 1 NO-UNDO.
DEF VAR x-PorIgv AS DEC DECIMALS 4 NO-UNDO.

DEF VAR x-codven    AS CHAR NO-UNDO.
DEF VAR x-fmapgo    as CHAR NO-UNDO.
DEF VAR x-canal     as CHAR NO-UNDO.
DEF VAR x-CodUnico  LIKE Gn-clie.CodUnico NO-UNDO.
DEF VAR x-NroCard   LIKE GN-card.NroCard NO-UNDO.
DEF VAR x-Sede      LIKE Gn-ClieD.Sede NO-UNDO.
DEF VAR cl-CodCia   AS INT NO-UNDO.
DEF VAR pv-CodCia   AS INT NO-UNDO.
DEF VAR x-CodCli    LIKE Gn-clie.codcli NO-UNDO.
DEF VAR x-Zona      AS CHAR NO-UNDO.
DEF VAR x-coe       AS DECI INIT 0 NO-UNDO.
DEF VAR x-can       AS DECI INIT 0 NO-UNDO.
DEF VAR f-factor    AS DECI INIT 0 NO-UNDO.
DEF VAR x-AlmDes    AS CHAR NO-UNDO.
DEF VAR x-Tipo      AS CHAR NO-UNDO.        /* MOSTRADOR o CREDITO */
DEF VAR x-Delivery  AS CHAR NO-UNDO.

DEF VAR s-clivar    AS CHAR FORMAT 'x(11)' NO-UNDO.
DEF VAR s-CliUni    AS CHAR FORMAT 'x(11)' INIT '99999999999' NO-UNDO.

DEFINE VAR pCodDiv AS CHAR NO-UNDO.     /* Quién lo vendió */
DEFINE VAR pDivDes AS CHAR NO-UNDO.     /* Dónde se despachó */
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.

DEF BUFFER B-CDOCU FOR CcbCdocu.
DEF BUFFER B-DDOCU FOR CcbDdocu.
DEF BUFFER B-CPEDI FOR FacCPedi.
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
x-CodFchI = ADD-INTERVAL(dFchCie, -2, "months").
/* RHC 27.07.2011 un mes mas */
/* IF MONTH (x-CodFchI) = 01                                           */
/*     THEN x-CodFchI = DATE(12, 01, YEAR(x-CodFchI) - 1 ).            */
/* ELSE x-CodFchI = DATE( MONTH(x-CodFchI) - 1, 01, YEAR(x-CodFchI) ). */
/* /* RHC 27.07.2011 un mes mas */                                     */
/* IF MONTH (x-CodFchI) = 01                                           */
/*     THEN x-CodFchI = DATE(12, 01, YEAR(x-CodFchI) - 1 ).            */
/* ELSE x-CodFchI = DATE( MONTH(x-CodFchI) - 1, 01, YEAR(x-CodFchI) ). */
/* ************************ */
ASSIGN
    x-CodFchI = 01/01/2014
    x-CodFchF = 03/31/2015.
ASSIGN
    FechaD = YEAR(x-CodFchI) * 10000 + MONTH(x-CodFchI) * 100 + DAY(x-CodFchI)
    FechaH = YEAR(x-CodFchF) * 10000 + MONTH(x-CodFchF) * 100 + DAY(x-CodFchF).
                                    
FIND FIRST Empresas WHERE Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
    IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
    IF NOT Empresas.Campo-CodPro THEN pv-codcia = s-codcia.
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
         HEIGHT             = 16.62
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR s-coddiv AS CHAR INIT '00500' NO-UNDO.

/* INFORMACION DETALLADA Y DEPURADA */
DISPLAY 'inicio del proceso: ' FechaD FechaH SKIP.
PAUSE 0.

DISPLAY 'borra ventas: ' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.
RUN Borra-Ventas-Basicas.

DISPLAY 'carga ventas: ' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.
RUN Carga-Ventas-Basicas.

/* ESTADISTICAS */
DISPLAY 'borra estadisticas: ' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.
RUN Borra-Estadisticas.

DISPLAY 'carga estadistica: ' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.
RUN Carga-Estadisticas.

DISPLAY '** fin del proceso **' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Estadisticas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Estadisticas Procedure 
PROCEDURE Borra-Estadisticas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Ventas WHERE Ventas.DateKey >= x-CodFchI
    AND Ventas.DateKey <= x-CodFchF
    AND Ventas.CodDiv = s-CodDiv:
    DELETE Ventas.
END.
FOR EACH VentasxCliente WHERE VentasxCliente.DateKey >= x-CodFchI
    AND VentasxCliente.DateKey <= x-CodFchF
    AND VentasxCliente.CodDiv = s-CodDiv:
    DELETE VentasxCliente.
END.
FOR EACH VentasxProducto WHERE VentasxProducto.DateKey >= x-CodFchI
    AND VentasxProducto.DateKey <= x-CodFchF
    AND VentasxProducto.CodDiv = s-CodDiv:
    DELETE VentasxProducto.
END.
FOR EACH VentasxVendedor WHERE VentasxVendedor.DateKey >= x-CodFchI
    AND VentasxVendedor.DateKey <= x-CodFchF
    AND VentasxVendedor.CodDiv = s-CodDiv:
    DELETE VentasxVendedor.
END.
FOR EACH VentasxClienteLinea WHERE VentasxClienteLinea.DateKey >= x-CodFchI
    AND VentasxClienteLinea.DateKey <= x-CodFchF
    AND VentasxClienteLinea.CodDiv = s-CodDiv:
    DELETE VentasxClienteLinea.
END.
FOR EACH VentasxVendCliente WHERE VentasxVendCliente.DateKey >= x-CodFchI
    AND VentasxVendCliente.DateKey <= x-CodFchF
    AND VentasxVendCliente.CodDiv = s-CodDiv:
    DELETE VentasxVendCliente.
END.
FOR EACH VentasxLinea WHERE VentasxLinea.DateKey >= x-CodFchI
    AND VentasxLinea.DateKey <= x-CodFchF
    AND VentasxLinea.CodDiv = s-CodDiv:
    DELETE VentasxLinea.
END.
/* FOR EACH VentasxAlmacen WHERE VentasxAlmacen.DateKey >= x-CodFchI */
/*     AND VentasxAlmacen.DateKey <= x-CodFchF:                      */
/*     DELETE VentasxAlmacen.                                        */
/* END.                                                              */
IF AVAILABLE Ventas THEN RELEASE Ventas.
IF AVAILABLE VentasxCliente THEN RELEASE VentasxCliente.
IF AVAILABLE VentasxProducto THEN RELEASE VentasxProducto.
IF AVAILABLE VentasxVendedor THEN RELEASE VentasxVendedor.
IF AVAILABLE VentasxClienteLinea THEN RELEASE VentasxClienteLinea.
IF AVAILABLE VentasxVendCliente THEN RELEASE VentasxVendCliente.
IF AVAILABLE VentasxLinea THEN RELEASE VentasxLinea.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Borra-Ventas-Basicas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Ventas-Basicas Procedure 
PROCEDURE Borra-Ventas-Basicas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Ventas_Cabecera WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv:
    FOR EACH Ventas_Detalle OF Ventas_Cabecera:
        DELETE Ventas_Detalle.
    END.
    DELETE Ventas_Cabecera.
END.
IF AVAILABLE Ventas_Cabecera THEN RELEASE Ventas_Cabecera.
IF AVAILABLE Ventas_Detalle THEN RELEASE Ventas_Detalle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-cli) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-cli Procedure 
PROCEDURE Carga-cli :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST VentasxCliente WHERE VentasxCliente.DateKey = Ventas_Cabecera.DateKey
        AND VentasxCliente.coddiv = Ventas_Cabecera.coddiv
        AND VentasxCliente.divdes = Ventas_Cabecera.divdes
        AND VentasxCliente.codcli = Ventas_Cabecera.codcli
        AND VentasxCliente.tipo   = Ventas_Cabecera.tipo
        AND VentasxCliente.delivery = Ventas_Cabecera.delivery
        AND VentasxCliente.ListaBase = Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxCliente THEN CREATE VentasxCliente.
    ASSIGN
        VentasxCliente.DateKey      = Ventas_Cabecera.DateKey
        VentasxCliente.CodDiv       = Ventas_Cabecera.coddiv
        VentasxCliente.DivDes       = Ventas_Cabecera.divdes
        VentasxCliente.CodCli       = Ventas_Cabecera.codcli
        VentasxCliente.Tipo         = Ventas_Cabecera.tipo
        VentasxCliente.delivery     = Ventas_Cabecera.delivery
        VentasxCliente.ListaBase     = Ventas_Cabecera.ListaBase
        VentasxCliente.ImpExtCIGV   = VentasxCliente.impextcigv + Ventas_Detalle.impextcigv
        VentasxCliente.ImpExtSIGV   = VentasxCliente.impextsigv + Ventas_Detalle.impextsigv
        VentasxCliente.ImpNacCIGV   = VentasxCliente.impnaccigv + Ventas_Detalle.impnaccigv
        VentasxCliente.ImpNacSIGV   = VentasxCliente.impnacsigv + Ventas_Detalle.impnacsigv
        VentasxCliente.CostoExtCIGV = VentasxCliente.costoextcigv + Ventas_Detalle.costoextcigv
        VentasxCliente.CostoExtSIGV = VentasxCliente.costoextsigv + Ventas_Detalle.costoextsigv
        VentasxCliente.CostoNacCIGV = VentasxCliente.costonaccigv + Ventas_Detalle.costonaccigv
        VentasxCliente.CostoNacSIGV = VentasxCliente.costonacsigv + Ventas_Detalle.costonacsigv
        VentasxCliente.PromExtCIGV  = VentasxCliente.promextcigv + Ventas_Detalle.promextcigv
        VentasxCliente.PromExtSIGV  = VentasxCliente.promextsigv + Ventas_Detalle.promextsigv
        VentasxCliente.PromNacCIGV  = VentasxCliente.promnaccigv + Ventas_Detalle.promnaccigv
        VentasxCliente.PromNacSIGV  = VentasxCliente.promnacsigv + Ventas_Detalle.promnacsigv.

    RELEASE VentasxCliente.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-climat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-climat Procedure 
PROCEDURE Carga-climat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST VentasxClienteLinea WHERE VentasxClienteLinea.DateKey = Ventas_Cabecera.DateKey
        AND VentasxClienteLinea.coddiv = Ventas_Cabecera.coddiv
        AND VentasxClienteLinea.divdes = Ventas_Cabecera.divdes
        AND VentasxClienteLinea.codcli = Ventas_Cabecera.codcli
        AND VentasxClienteLinea.CodFam = DimProducto.CodFam
        AND VentasxClienteLinea.SubFam = DimProducto.SubFam
        AND VentasxClienteLinea.DesMar = DimProducto.DesMar
        AND VentasxClienteLinea.Licencia = DimProducto.Licencia
        AND VentasxClienteLinea.Tipo = Ventas_Cabecera.Tipo
        AND VentasxClienteLinea.Delivery = Ventas_Cabecera.Delivery
        AND VentasxClienteLinea.ListaBase = Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxClienteLinea THEN CREATE VentasxClienteLinea.
    ASSIGN
        VentasxClienteLinea.DateKey = Ventas_Cabecera.DateKey
        VentasxClienteLinea.CodDiv = Ventas_Cabecera.coddiv
        VentasxClienteLinea.DivDes = Ventas_Cabecera.divdes
        VentasxClienteLinea.CodCli = Ventas_Cabecera.codcli
        VentasxClienteLinea.CodFam = DimProducto.CodFam
        VentasxClienteLinea.SubFam = DimProducto.SubFam
        VentasxClienteLinea.DesMar = DimProducto.DesMar
        VentasxClienteLinea.Licencia = DimProducto.Licencia
        VentasxClienteLinea.Tipo     = Ventas_Cabecera.tipo
        VentasxClienteLinea.Delivery = Ventas_Cabecera.Delivery
        VentasxClienteLinea.ListaBase = Ventas_Cabecera.ListaBase
        VentasxClienteLinea.ImpExtCIGV = VentasxClienteLinea.impextcigv + Ventas_Detalle.impextcigv
        VentasxClienteLinea.ImpExtSIGV = VentasxClienteLinea.impextsigv + Ventas_Detalle.impextsigv
        VentasxClienteLinea.ImpNacCIGV = VentasxClienteLinea.impnaccigv + Ventas_Detalle.impnaccigv
        VentasxClienteLinea.ImpNacSIGV = VentasxClienteLinea.impnacsigv + Ventas_Detalle.impnacsigv
        VentasxClienteLinea.CostoExtCIGV = VentasxClienteLinea.costoextcigv + Ventas_Detalle.costoextcigv
        VentasxClienteLinea.CostoExtSIGV = VentasxClienteLinea.costoextsigv + Ventas_Detalle.costoextsigv
        VentasxClienteLinea.CostoNacCIGV = VentasxClienteLinea.costonaccigv + Ventas_Detalle.costonaccigv
        VentasxClienteLinea.CostoNacSIGV = VentasxClienteLinea.costonacsigv + Ventas_Detalle.costonacsigv
        VentasxClienteLinea.PromExtCIGV = VentasxClienteLinea.promextcigv + Ventas_Detalle.promextcigv
        VentasxClienteLinea.PromExtSIGV = VentasxClienteLinea.promextsigv + Ventas_Detalle.promextsigv
        VentasxClienteLinea.PromNacCIGV = VentasxClienteLinea.promnaccigv + Ventas_Detalle.promnaccigv
        VentasxClienteLinea.PromNacSIGV = VentasxClienteLinea.promnacsigv + Ventas_Detalle.promnacsigv.

    RELEASE VentasxClienteLinea.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-despachos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-despachos Procedure 
PROCEDURE Carga-despachos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*     FIND FIRST VentasxAlmacen WHERE VentasxAlmacen.DateKey = Ventas_Cabecera.DateKey            */
/*         AND VentasxAlmacen.codmat = Ventas_Detalle.codmat                                       */
/*         AND VentasxAlmacen.almdes = Ventas_Detalle.almdes                                       */
/*         AND VentasxAlmacen.coddiv = Ventas_Cabecera.divdes                                      */
/*         NO-ERROR.                                                                               */
/*     IF NOT AVAILABLE VentasxAlmacen THEN CREATE VentasxAlmacen.                                 */
/*     ASSIGN                                                                                      */
/*         VentasxAlmacen.DateKey = Ventas_Cabecera.DateKey                                        */
/*         VentasxAlmacen.CodMat = Ventas_Detalle.codmat                                           */
/*         VentasxAlmacen.AlmDes = Ventas_Detalle.almdes                                           */
/*         VentasxAlmacen.CodDiv = Ventas_Cabecera.divdes                                          */
/*         VentasxAlmacen.Cantidad = VentasxAlmacen.Cantidad + Ventas_Detalle.cantidad             */
/*         VentasxAlmacen.ImpExtCIGV = VentasxAlmacen.impextcigv + Ventas_Detalle.impextcigv       */
/*         VentasxAlmacen.ImpExtSIGV = VentasxAlmacen.impextsigv + Ventas_Detalle.impextsigv       */
/*         VentasxAlmacen.ImpNacCIGV = VentasxAlmacen.impnaccigv + Ventas_Detalle.impnaccigv       */
/*         VentasxAlmacen.ImpNacSIGV = VentasxAlmacen.impnacsigv + Ventas_Detalle.impnacsigv       */
/*         VentasxAlmacen.CostoExtCIGV = VentasxAlmacen.costoextcigv + Ventas_Detalle.costoextcigv */
/*         VentasxAlmacen.CostoExtSIGV = VentasxAlmacen.costoextsigv + Ventas_Detalle.costoextsigv */
/*         VentasxAlmacen.CostoNacCIGV = VentasxAlmacen.costonaccigv + Ventas_Detalle.costonaccigv */
/*         VentasxAlmacen.CostoNacSIGV = VentasxAlmacen.costonacsigv + Ventas_Detalle.costonacsigv */
/*         VentasxAlmacen.PromExtCIGV = VentasxAlmacen.promextcigv + Ventas_Detalle.promextcigv    */
/*         VentasxAlmacen.PromExtSIGV = VentasxAlmacen.promextsigv + Ventas_Detalle.promextsigv    */
/*         VentasxAlmacen.PromNacCIGV = VentasxAlmacen.promnaccigv + Ventas_Detalle.promnaccigv    */
/*         VentasxAlmacen.PromNacSIGV = VentasxAlmacen.promnacsigv + Ventas_Detalle.promnacsigv.   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Estadisticas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Estadisticas Procedure 
PROCEDURE Carga-Estadisticas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* VENTAS PRINCIPAL */
    RUN Carga-ventas.
END.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* VENTAS POR CLIENTE */
    RUN Carga-cli.
END.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* VENTAS POR CLIENTE */
    RUN Carga-mat.
END.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* VENTAS POR VENDEDOR */
    RUN Carga-vend.
END.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* VENTAS POR CLIENTE Y PRODUCTO */
    RUN Carga-climat.
END.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* VENTAS POR VENDEDOR Y CLIENTE */
    RUN Carga-vendcli.
END.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* VENTAS POR PRODUCTO RESUMIDO */
    RUN Carga-resmat.

END.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI
    AND Ventas_Cabecera.DateKey <= x-CodFchF
    AND Ventas_Cabecera.CodDiv = s-CodDiv,
    EACH Ventas_Detalle OF Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF Ventas_Detalle NO-LOCK:
    /* DESPACHOS POR PRODUCTO RESUMIDO */
    RUN Carga-despachos.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-mat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-mat Procedure 
PROCEDURE Carga-mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST VentasxProducto WHERE VentasxProducto.DateKey = Ventas_Cabecera.DateKey
        AND VentasxProducto.coddiv = Ventas_Cabecera.coddiv
        AND VentasxProducto.divdes = Ventas_Cabecera.divdes
        AND VentasxProducto.CodMat = Ventas_Detalle.codmat
        AND VentasxProducto.Tipo = Ventas_Cabecera.tipo
        AND VentasxProducto.Delivery = Ventas_Cabecera.Delivery
        AND VentasxProducto.ListaBase = Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxProducto THEN CREATE VentasxProducto.
    ASSIGN
        VentasxProducto.DateKey = Ventas_Cabecera.DateKey
        VentasxProducto.CodDiv = Ventas_Cabecera.coddiv
        VentasxProducto.DivDes = Ventas_Cabecera.divdes
        VentasxProducto.CodMat = TRIM(Ventas_Detalle.codmat)
        VentasxProducto.Tipo = Ventas_Cabecera.tipo
        VentasxProducto.Delivery = Ventas_Cabecera.Delivery
        VentasxProducto.ListaBase = Ventas_Cabecera.ListaBase
        VentasxProducto.Cantidad = VentasxProducto.cantidad + Ventas_Detalle.cantidad
        VentasxProducto.ImpExtCIGV = VentasxProducto.impextcigv + Ventas_Detalle.impextcigv
        VentasxProducto.ImpExtSIGV = VentasxProducto.impextsigv + Ventas_Detalle.impextsigv
        VentasxProducto.ImpNacCIGV = VentasxProducto.impnaccigv + Ventas_Detalle.impnaccigv
        VentasxProducto.ImpNacSIGV = VentasxProducto.impnacsigv + Ventas_Detalle.impnacsigv
        VentasxProducto.CostoExtCIGV = VentasxProducto.costoextcigv + Ventas_Detalle.costoextcigv
        VentasxProducto.CostoExtSIGV = VentasxProducto.costoextsigv + Ventas_Detalle.costoextsigv
        VentasxProducto.CostoNacCIGV = VentasxProducto.costonaccigv + Ventas_Detalle.costonaccigv
        VentasxProducto.CostoNacSIGV = VentasxProducto.costonacsigv + Ventas_Detalle.costonacsigv
        VentasxProducto.PromExtCIGV = VentasxProducto.promextcigv + Ventas_Detalle.promextcigv
        VentasxProducto.PromExtSIGV = VentasxProducto.promextsigv + Ventas_Detalle.promextsigv
        VentasxProducto.PromNacCIGV = VentasxProducto.promnaccigv + Ventas_Detalle.promnaccigv
        VentasxProducto.PromNacSIGV = VentasxProducto.promnacsigv + Ventas_Detalle.promnacsigv.

    RELEASE VentasxProducto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-resmat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-resmat Procedure 
PROCEDURE Carga-resmat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST VentasxLinea WHERE VentasxLinea.DateKey = Ventas_Cabecera.DateKey
        AND VentasxLinea.coddiv = Ventas_Cabecera.coddiv
        AND VentasxLinea.divdes = Ventas_Cabecera.divdes
        AND VentasxLinea.codfam = DimProducto.codfam
        AND VentasxLinea.subfam = DimProducto.subfam
        AND VentasxLinea.desmar = DimProducto.desmar
        AND VentasxLinea.licencia = DimProducto.licencia
        AND VentasxLinea.codpro = DimProducto.codpro[1]
        AND VentasxLinea.codven = Ventas_Cabecera.codven
        AND VentasxLinea.tipo = Ventas_Cabecera.tipo
        AND VentasxLinea.delivery = Ventas_Cabecera.delivery
        AND VentasxLinea.ListaBase = Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxLinea THEN CREATE VentasxLinea.
    ASSIGN
        VentasxLinea.DateKey = Ventas_Cabecera.DateKey
        VentasxLinea.CodDiv = Ventas_Cabecera.coddiv
        VentasxLinea.DivDes = Ventas_Cabecera.divdes
        VentasxLinea.codfam = DimProducto.codfam
        VentasxLinea.subfam = DimProducto.subfam
        VentasxLinea.desmar = DimProducto.desmar
        VentasxLinea.licencia = DimProducto.licencia
        VentasxLinea.codpro = DimProducto.codpro[1]
        VentasxLinea.codven = Ventas_Cabecera.codven
        VentasxLinea.tipo = Ventas_Cabecera.tipo
        VentasxLinea.delivery = Ventas_Cabecera.delivery
        VentasxLinea.ListaBase = Ventas_Cabecera.ListaBase
        VentasxLinea.ImpExtCIGV = VentasxLinea.impextcigv + Ventas_Detalle.impextcigv
        VentasxLinea.ImpExtSIGV = VentasxLinea.impextsigv + Ventas_Detalle.impextsigv
        VentasxLinea.ImpNacCIGV = VentasxLinea.impnaccigv + Ventas_Detalle.impnaccigv
        VentasxLinea.ImpNacSIGV = VentasxLinea.impnacsigv + Ventas_Detalle.impnacsigv
        VentasxLinea.CostoExtCIGV = VentasxLinea.costoextcigv + Ventas_Detalle.costoextcigv
        VentasxLinea.CostoExtSIGV = VentasxLinea.costoextsigv + Ventas_Detalle.costoextsigv
        VentasxLinea.CostoNacCIGV = VentasxLinea.costonaccigv + Ventas_Detalle.costonaccigv
        VentasxLinea.CostoNacSIGV = VentasxLinea.costonacsigv + Ventas_Detalle.costonacsigv
        VentasxLinea.PromExtCIGV = VentasxLinea.promextcigv + Ventas_Detalle.promextcigv
        VentasxLinea.PromExtSIGV = VentasxLinea.promextsigv + Ventas_Detalle.promextsigv
        VentasxLinea.PromNacCIGV = VentasxLinea.promnaccigv + Ventas_Detalle.promnaccigv
        VentasxLinea.PromNacSIGV = VentasxLinea.promnacsigv + Ventas_Detalle.promnacsigv.

    RELEASE VentasxLinea.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-vend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-vend Procedure 
PROCEDURE Carga-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST VentasxVendedor WHERE VentasxVendedor.DateKey = Ventas_Cabecera.DateKey
        AND VentasxVendedor.coddiv = Ventas_Cabecera.coddiv
        AND VentasxVendedor.divdes = Ventas_Cabecera.divdes
        AND VentasxVendedor.codven = Ventas_Cabecera.codven
        AND VentasxVendedor.tipo = Ventas_Cabecera.tipo
        AND VentasxVendedor.delivery = Ventas_Cabecera.delivery
        AND VentasxVendedor.ListaBase = Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxVendedor THEN CREATE VentasxVendedor.
    ASSIGN
        VentasxVendedor.DateKey = Ventas_Cabecera.DateKey
        VentasxVendedor.CodDiv = Ventas_Cabecera.coddiv
        VentasxVendedor.DivDes = Ventas_Cabecera.divdes
        VentasxVendedor.CodVen = Ventas_Cabecera.codven
        VentasxVendedor.Tipo = Ventas_Cabecera.tipo
        VentasxVendedor.delivery = Ventas_Cabecera.delivery
        VentasxVendedor.ListaBase = Ventas_Cabecera.ListaBase
        VentasxVendedor.ImpExtCIGV = VentasxVendedor.impextcigv + Ventas_Detalle.impextcigv
        VentasxVendedor.ImpExtSIGV = VentasxVendedor.impextsigv + Ventas_Detalle.impextsigv
        VentasxVendedor.ImpNacCIGV = VentasxVendedor.impnaccigv + Ventas_Detalle.impnaccigv
        VentasxVendedor.ImpNacSIGV = VentasxVendedor.impnacsigv + Ventas_Detalle.impnacsigv
        VentasxVendedor.CostoExtCIGV = VentasxVendedor.costoextcigv + Ventas_Detalle.costoextcigv
        VentasxVendedor.CostoExtSIGV = VentasxVendedor.costoextsigv + Ventas_Detalle.costoextsigv
        VentasxVendedor.CostoNacCIGV = VentasxVendedor.costonaccigv + Ventas_Detalle.costonaccigv
        VentasxVendedor.CostoNacSIGV = VentasxVendedor.costonacsigv + Ventas_Detalle.costonacsigv
        VentasxVendedor.PromExtCIGV = VentasxVendedor.promextcigv + Ventas_Detalle.promextcigv
        VentasxVendedor.PromExtSIGV = VentasxVendedor.promextsigv + Ventas_Detalle.promextsigv
        VentasxVendedor.PromNacCIGV = VentasxVendedor.promnaccigv + Ventas_Detalle.promnaccigv
        VentasxVendedor.PromNacSIGV = VentasxVendedor.promnacsigv + Ventas_Detalle.promnacsigv.

    RELEASE VentasxVendedor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-vendcli) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-vendcli Procedure 
PROCEDURE Carga-vendcli :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST VentasxVendCliente WHERE VentasxVendCliente.DateKey = Ventas_Cabecera.DateKey
        AND VentasxVendCliente.coddiv = Ventas_Cabecera.coddiv
        AND VentasxVendCliente.divdes = Ventas_Cabecera.divdes
        AND VentasxVendCliente.codcli = Ventas_Cabecera.codcli
        AND VentasxVendCliente.codven = Ventas_Cabecera.codven
        AND VentasxVendCliente.tipo   = Ventas_Cabecera.tipo
        AND VentasxVendCliente.delivery = Ventas_Cabecera.delivery
        AND VentasxVendCliente.ListaBase = Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxVendCliente THEN CREATE VentasxVendCliente.
    ASSIGN
        VentasxVendCliente.DateKey = Ventas_Cabecera.DateKey
        VentasxVendCliente.CodDiv = Ventas_Cabecera.coddiv
        VentasxVendCliente.DivDes = Ventas_Cabecera.divdes
        VentasxVendCliente.CodCli = Ventas_Cabecera.codcli
        VentasxVendCliente.CodVen = Ventas_Cabecera.codven
        VentasxVendCliente.Tipo   = Ventas_Cabecera.tipo
        VentasxVendCliente.delivery = Ventas_Cabecera.delivery
        VentasxVendCliente.ListaBase = Ventas_Cabecera.ListaBase
        VentasxVendCliente.ImpExtCIGV = VentasxVendCliente.impextcigv + Ventas_Detalle.impextcigv
        VentasxVendCliente.ImpExtSIGV = VentasxVendCliente.impextsigv + Ventas_Detalle.impextsigv
        VentasxVendCliente.ImpNacCIGV = VentasxVendCliente.impnaccigv + Ventas_Detalle.impnaccigv
        VentasxVendCliente.ImpNacSIGV = VentasxVendCliente.impnacsigv + Ventas_Detalle.impnacsigv
        VentasxVendCliente.CostoExtCIGV = VentasxVendCliente.costoextcigv + Ventas_Detalle.costoextcigv
        VentasxVendCliente.CostoExtSIGV = VentasxVendCliente.costoextsigv + Ventas_Detalle.costoextsigv
        VentasxVendCliente.CostoNacCIGV = VentasxVendCliente.costonaccigv + Ventas_Detalle.costonaccigv
        VentasxVendCliente.CostoNacSIGV = VentasxVendCliente.costonacsigv + Ventas_Detalle.costonacsigv
        VentasxVendCliente.PromExtCIGV = VentasxVendCliente.promextcigv + Ventas_Detalle.promextcigv
        VentasxVendCliente.PromExtSIGV = VentasxVendCliente.promextsigv + Ventas_Detalle.promextsigv
        VentasxVendCliente.PromNacCIGV = VentasxVendCliente.promnaccigv + Ventas_Detalle.promnaccigv
        VentasxVendCliente.PromNacSIGV = VentasxVendCliente.promnacsigv + Ventas_Detalle.promnacsigv.

    RELEASE VentasxVendCliente.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-ventas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-ventas Procedure 
PROCEDURE Carga-ventas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST Ventas WHERE Ventas.DateKey = Ventas_Cabecera.DateKey
        AND Ventas.coddiv = Ventas_Cabecera.coddiv
        AND Ventas.divdes = Ventas_Cabecera.divdes
        AND Ventas.codcli = Ventas_Cabecera.codcli
        AND Ventas.codven = Ventas_Cabecera.codven
        AND Ventas.CodMat = Ventas_Detalle.codmat
        AND Ventas.Tipo   = Ventas_Cabecera.tipo
        AND Ventas.Delivery = Ventas_Cabecera.Delivery
        AND Ventas.ListaBase = Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE Ventas THEN CREATE Ventas.
    ASSIGN
        Ventas.DateKey = Ventas_Cabecera.DateKey
        Ventas.CodDiv = Ventas_Cabecera.coddiv
        Ventas.DivDes = Ventas_Cabecera.divdes
        Ventas.CodCli = Ventas_Cabecera.codcli
        Ventas.CodVen = Ventas_Cabecera.codven
        Ventas.CodMat = Ventas_Detalle.codmat
        Ventas.Tipo   = Ventas_Cabecera.tipo
        Ventas.Delivery = Ventas_Cabecera.Delivery
        Ventas.ListaBase = Ventas_Cabecera.ListaBase
        Ventas.Cantidad = Ventas.cantidad + Ventas_Detalle.cantidad
        Ventas.ImpExtCIGV = Ventas.impextcigv + Ventas_Detalle.impextcigv
        Ventas.ImpExtSIGV = Ventas.impextsigv + Ventas_Detalle.impextsigv
        Ventas.ImpNacCIGV = Ventas.impnaccigv + Ventas_Detalle.impnaccigv
        Ventas.ImpNacSIGV = Ventas.impnacsigv + Ventas_Detalle.impnacsigv
        Ventas.CostoExtCIGV = Ventas.costoextcigv + Ventas_Detalle.costoextcigv
        Ventas.CostoExtSIGV = Ventas.costoextsigv + Ventas_Detalle.costoextsigv
        Ventas.CostoNacCIGV = Ventas.costonaccigv + Ventas_Detalle.costonaccigv
        Ventas.CostoNacSIGV = Ventas.costonacsigv + Ventas_Detalle.costonacsigv
        Ventas.PromExtCIGV = Ventas.promextcigv + Ventas_Detalle.promextcigv
        Ventas.PromExtSIGV = Ventas.promextsigv + Ventas_Detalle.promextsigv
        Ventas.PromNacCIGV = Ventas.promnaccigv + Ventas_Detalle.promnaccigv
        Ventas.PromNacSIGV = Ventas.promnacsigv + Ventas_Detalle.promnacsigv.

    RELEASE Ventas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Ventas-Basicas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Ventas-Basicas Procedure 
PROCEDURE Carga-Ventas-Basicas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Barremos las ventas */
ESTADISTICAS:
FOR EACH DimDivision NO-LOCK WHERE DimDivision.CodDiv = s-CodDiv,
    EACH CcbCdocu USE-INDEX Llave10 NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
        AND CcbCdocu.CodDiv = DimDivision.CodDiv
        AND CcbCdocu.FchDoc >= x-CodFchI
        AND CcbCdocu.FchDoc <= x-CodFchF:
    /* ***************** FILTROS ********************************** */
    /*IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,A/C") = 0 THEN NEXT.*/
    IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C") = 0 THEN NEXT.
    IF LOOKUP(CcbCDocu.FlgEst, "A,X") > 0 THEN NEXT.
    IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
    /* NO facturas adelantadas NI servicios */
    /*IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL") > 0  AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN NEXT.*/
    IF LOOKUP(Ccbcdocu.CodDoc, 'N/C,A/C') > 0 THEN DO:
        FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia
            AND B-CDOCU.CodDoc = CcbCdocu.Codref
            AND B-CDOCU.NroDoc = CcbCdocu.Nroref
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE B-CDOCU THEN NEXT.
/*          IF LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN NEXT.     /* NO SERVICIOS NI ADELANTADAS */ */
/*         IF Ccbcdocu.CndCre = "N" THEN DO:       /* OTRAS */                               */
/*             /* NO por APLICACION DE ANTICIPO */                                           */
/*             FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:                                        */
/*                 FIND FIRST Ccbtabla WHERE Ccbtabla.codcia = s-codcia                      */
/*                     AND Ccbtabla.tabla = "N/C"                                            */
/*                     AND Ccbtabla.codigo = Ccbddocu.codmat                                 */
/*                     NO-LOCK NO-ERROR.                                                     */
/*                 IF AVAILABLE Ccbtabla AND CcbTabla.Libre_L01 = NO THEN NEXT ESTADISTICAS. */
/*             END.                                                                          */
/*         END.                                                                              */
    END.
    /* SACAMOS LOS DATOS DEL DOCUMENTO BASE */
    ASSIGN
        pCodDiv     = IF Ccbcdocu.DivOri <> '' THEN Ccbcdocu.DivOri ELSE Ccbcdocu.CodDiv
        pDivDes     = Ccbcdocu.CodDiv
        x-PorIgv    = Ccbcdocu.porigv
        x-CodVen    = Ccbcdocu.codven
        x-FmaPgo    = Ccbcdocu.fmapgo
        x-NroCard   = Ccbcdocu.nrocard
        x-Sede      = Ccbcdocu.sede
        x-Tipo      = IF Ccbcdocu.Tipo = 'MOSTRADOR' THEN 'MOSTRADOR' ELSE 'CREDITO'
        x-Delivery  = "NO".
    /* ******************************************************* */
    IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0 AND Ccbcdocu.codref = "G/R" THEN x-Delivery = "SI".
    IF Ccbcdocu.TpoFac = "CR" THEN x-Tipo = "CREDITO".
    IF Ccbcdocu.TpoFac = "CO" THEN x-Tipo = "MOSTRADOR".
    IF LOOKUP(Ccbcdocu.CodDoc, 'N/C,A/C') > 0 THEN DO:
        /* SACAMOS LOS DATOS DEL DOCUMENTO DE REFERENCIA */
        FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
            AND B-CDOCU.CodDoc = CcbCdocu.Codref 
            AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-CDOCU THEN DO:
            ASSIGN
                pCodDiv     = IF B-CDOCU.DivOri <> '' THEN B-CDOCU.DivOri ELSE B-CDOCU.CodDiv
                /*pDivDes     = B-CDOCU.CodDiv*/
                x-PorIgv    = B-CDOCU.porigv
                x-CodVen    = B-CDOCU.codven
                x-FmaPgo    = B-CDOCU.fmapgo
                x-NroCard   = B-CDOCU.nrocard
                x-Sede      = B-CDOCU.sede
                x-Tipo      = IF B-CDOCU.Tipo = 'MOSTRADOR' THEN 'MOSTRADOR' ELSE 'CREDITO'.
            IF B-CDOCU.TpoFac = "CR" THEN x-Tipo = "CREDITO".
            IF B-CDOCU.TpoFac = "CO" THEN x-Tipo = "MOSTRADOR".
        END.
    END.
    /* Ajuste de la division en los valores historicos */
    IF Ccbcdocu.FchDoc <= 12/31/2012 THEN DO:
        IF pCodDiv <> '00017' AND x-codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
        IF pCodDiv <> '00018' AND LOOKUP(x-codven, '015,173,900,901,902,017') > 0 THEN pCodDiv = '00018'.    /* Provincias */
        IF pCodDiv <> '00019' AND x-codven = '081' THEN pCodDiv = '00019'.   /* Mesa redonda */
    END.
    IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
    IF pCodDiv <> '00099' AND x-codven = '998' THEN pCodDiv = '00099'.   /* Exportaciones */
    IF pCodDiv <> '00098' AND x-codven = '157' THEN pCodDiv = '00098'.   /* Refiles */
    /* FACTURAS ANTICIPOS Y/O SERVICIOS SE VAN A OTRA DIVISION */
    IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL") > 0 AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN pCodDiv = "99999".
    /* ADELANTOS DE CAMPAÑA VAN A OTRA DIVISION */
    IF CcbCDocu.CodDoc = "A/C" THEN pCodDiv = "99999".
    /* NOTAS DE CREDITO APLICADA A OTRAS VENTAS VAN A OTRA DIVISION */
    IF CcbCDocu.CodDoc = "N/C" AND LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN pCodDiv = "99999".
    /* NOTAS DE CREDITO: DE ACUERDO AL CONCEPTO */
/*     IF CcbCDocu.CodDoc = "N/C" AND LOOKUP(B-CDOCU.TpoFac, 'A,S') = 0 THEN DO:         */
/*         FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:                                        */
/*             FIND FIRST Ccbtabla WHERE Ccbtabla.codcia = s-codcia                      */
/*                 AND Ccbtabla.tabla = Ccbcdocu.coddoc                                  */
/*                 AND Ccbtabla.codigo = Ccbddocu.codmat                                 */
/*                 NO-LOCK NO-ERROR.                                                     */
/*             IF AVAILABLE Ccbtabla AND CcbTabla.Libre_L01 = NO THEN pCodDiv = "99999". */
/*         END.                                                                          */
/*     END.                                                                              */

    /* *********************************************************** */
    FIND B-DIVI WHERE B-DIVI.codcia = s-codcia AND B-DIVI.coddiv = pCodDiv NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-DIVI THEN NEXT.
    pCanalVenta = B-DIVI.CanalVenta.
    /* *********************************************************** */
    ASSIGN
        x-signo1 = ( IF LOOKUP(CcbCdocu.Coddoc, "N/C,A/C") > 0 THEN -1 ELSE 1 )
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
        x-Canal = ''
        x-CodCli = Ccbcdocu.CodCli
        x-CodUnico = Ccbcdocu.codcli
        x-Zona = ''.
    /* CANAL */
    FIND DimCliente WHERE DimCliente.codcli = Ccbcdocu.codcli NO-LOCK NO-ERROR.
    /* RHC 21.06.2012 En caso de no estar registrado en el maestro de clientes => Registrarlo */
    IF NOT AVAILABLE DimCliente THEN DO:
        CREATE DimCliente.
        ASSIGN
            DimCliente.ClfCli = "C"
            DimCliente.CodCli = Ccbcdocu.codcli
            DimCliente.CodDept = CcbCDocu.CodDpto
            DimCliente.CodDist = Ccbcdocu.coddist
            DimCliente.CodProv = Ccbcdocu.codprov
            DimCliente.NroCard = Ccbcdocu.nrocard
            DimCliente.FlgSit = "A"
            DimCliente.NomCli = Ccbcdocu.nomcli.
        DimCliente.NomCli = REPLACE(DimCliente.NomCli, '|', ' ').
        FIND CURRENT DimCliente NO-LOCK.
    END.
    IF AVAILABLE DimCliente AND DimCliente.codunico <> '' THEN x-codunico = DimCliente.codunico.
    IF AVAILABLE DimCliente THEN x-canal = DimCliente.Canal.
    /* ZONA */
    IF AVAILABLE DimCliente THEN DO:
        FIND TabDepto WHERE TabDepto.CodDepto = DimCliente.CodDept NO-LOCK NO-ERROR.
        IF AVAILABLE TabDepto THEN x-Zona = TabDepto.Zona.
        /* 20.07.10 */
        IF DimCliente.CodDept = '15' AND DimCliente.CodProv = '01' THEN x-Zona = 'LMC'.
    END.

    /* EN CASO DE SER UN CLIENTE VARIOS */
    IF Ccbcdocu.codcli = s-CliVar THEN DO:
        ASSIGN
            x-CodCli = s-CliVar
            x-CodUnico = s-CliVar.
        IF x-NroCard <> '' THEN DO:
            FIND FIRST DimCliente WHERE DimCliente.nrocard = x-NroCard
                AND DimCliente.flgsit = 'A'        /* ACTIVOS */
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE DimCliente 
            THEN FIND FIRST DimCliente WHERE DimCliente.nrocard = x-NroCard
                    NO-LOCK NO-ERROR.
            IF AVAILABLE DimCliente THEN x-CodUnico = DimCliente.CodUnico.
        END.
    END.
    /* ******************************** */
    RUN Carga-Ventas-Cabecera NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    /* ******************************** */

    ASSIGN
       x-Coe = 1
       x-Can = 1.
    /* FACTURAS POR ANTICIPOS Y/O SERVICIOS */
    IF LOOKUP(CcbCDocu.CodDoc,"FAC,BOL") > 0 AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN DO:
        RUN PROCESA-OTRAS-VENTAS.   /* CODMAT = '999999' */
        NEXT.
    END.
    /* ADELANTOS DE CAMPAÑA */
    IF CcbCDocu.CodDoc = "A/C" THEN DO:
        RUN PROCESA-OTRAS-VENTAS.   /* CODMAT = '999999' */
        NEXT.
    END.
    /* NOTAS DE CREDITO APLICADAS A FACTURAS DE SERVICIO Y/O ANTICIPOS DE CAMPAÑA */
    IF CcbCDocu.CodDoc = "N/C" AND LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN DO:
        RUN PROCESA-OTRAS-VENTAS.   /* CODMAT = '999999' */
        NEXT.
    END.
    /* NOTAS DE CREDITO POR OTROS CONCEPTOS */
    IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac <> "E" THEN DO:
        RUN PROCESA-NOTA.
        NEXT.
    END.
    IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac = "E" THEN DO:
        RUN PROCESA-NOTA-REBADE.
        NEXT.
    END.

    /* CONTINUAMOS CON FACTURAS Y NOTAS DE CREDITO POR DEVOLUCION */
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
       x-AlmDes = CcbDdocu.AlmDes.      /* Almacén de Despacho */
       IF CcbCDocu.CodDoc = "N/C" THEN DO:
           FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
               AND B-CDOCU.CodDoc = CcbCdocu.Codref 
               AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
               NO-LOCK.
           x-AlmDes = ENTRY(1, B-CDOCU.CodAlm).
           FIND FIRST B-DDOCU WHERE B-DDOCU.codcia = B-CDOCU.codcia
               AND B-DDOCU.coddoc = B-CDOCU.coddoc
               AND B-DDOCU.nrodoc = B-CDOCU.nrodoc
               AND B-DDOCU.codmat = CcbDdocu.codmat 
               NO-LOCK NO-ERROR.
           IF AVAILABLE B-DDOCU AND B-DDOCU.AlmDes <> '' THEN x-AlmDes = B-DDOCU.AlmDes.
       END.
       /* si aún no tiene código de almacén despacho */
       IF x-AlmDes = '' THEN x-AlmDes = ENTRY (1, CcbCDocu.CodAlm).
       RUN Carga-Ventas-Detalle.
    END.
    /* FACTURAS Y BOLETAS QUE TENGAN UNA APLICACION DE ANTICIPOS */
    IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0
        AND Ccbcdocu.ImpTot2 > 0 THEN DO:
        RUN PROCESA-ANTCIPOS-APLICADOS.
    END.
END.
IF AVAILABLE Ccbcdocu THEN RELEASE Ccbcdocu.
IF AVAILABLE Ccbddocu THEN RELEASE Ccbddocu.
IF AVAILABLE DimCliente THEN RELEASE DimCliente.
IF AVAILABLE Ventas_Detalle THEN RELEASE Ventas_Detalle.
IF AVAILABLE Ventas_Cabecera THEN RELEASE Ventas_Cabecera.

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

DO TRANSACTION ON ERROR UNDO, RETURN ERROR:
    CREATE Ventas_Cabecera.
    ASSIGN
        Ventas_Cabecera.DateKey = ccbcdocu.fchdoc
        Ventas_Cabecera.CodDiv = pcoddiv
        Ventas_Cabecera.DivDes = pdivdes
        Ventas_Cabecera.CodCli = x-codcli        /* x-codunico */
        Ventas_Cabecera.CodDoc = ccbcdocu.coddoc
        Ventas_Cabecera.NroDoc = ccbcdocu.nrodoc
        Ventas_Cabecera.CodVen = x-codven
        Ventas_Cabecera.FmaPgo = x-fmapgo
        Ventas_Cabecera.TpoCmb = ccbcdocu.tpocmb
        Ventas_Cabecera.TpoCmbVta = x-tpocmbvta
        Ventas_Cabecera.TpoCmbCmp = x-tpocmbcmp
        Ventas_Cabecera.Tipo    = x-Tipo
        Ventas_Cabecera.NroCard = x-nrocard
        Ventas_Cabecera.Delivery = x-Delivery
        Ventas_Cabecera.ListaBase = ""      /*pCodDiv.    /* <<< OJO <<< */*/
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.
        /*Ventas_Cabecera.ListaBase = pCodDiv.*/
    /* Buscamos Lista Base SOLO PARA EXPOLIBRERIA Y PROVINCIAS */
    IF Ccbcdocu.coddoc = "N/C" THEN FIND Faccpedi WHERE Faccpedi.codcia = B-CDOCU.codcia
        AND Faccpedi.coddoc = B-CDOCU.codped
        AND Faccpedi.nroped = B-CDOCU.nroped
        NO-LOCK NO-ERROR.
    ELSE FIND Faccpedi WHERE Faccpedi.codcia = Ccbcdocu.codcia
        AND Faccpedi.coddoc = Ccbcdocu.codped
        AND Faccpedi.nroped = Ccbcdocu.nroped
        NO-LOCK NO-ERROR.
    IF AVAILABLE Faccpedi THEN DO:
        FIND B-CPEDI WHERE B-CPEDI.codcia = Faccpedi.codcia
            AND B-CPEDI.coddoc = Faccpedi.codref
            AND B-CPEDI.nroped = Faccpedi.nroref
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-CPEDI AND B-CPEDI.Libre_C01 <> "" AND LOOKUP(B-CPEDI.TpoPed, "P,E") > 0
            THEN Ventas_Cabecera.ListaBase = B-CPEDI.Libre_C01.
    END.
END.

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
  Notes:       SOLO PARA FACTURAS Y N/C POR DEVOLUCION
------------------------------------------------------------------------------*/

    DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
    ASSIGN
        x-ImpLin = Ccbddocu.ImpLin - Ccbddocu.ImpDto2
        x-ImpCto = Ccbddocu.ImpCto.
    /* ************************************************************************ */
    /* RHC 30/04/2015 COSTO DE REPOSICION ACTUAL */
    FIND DimProducto WHERE DimProducto.CodMat = TRIM(ccbddocu.codmat) NO-LOCK NO-ERROR.
    IF AVAILABLE DimProducto THEN x-ImpCto = DimProducto.CtoTot.

    IF x-PorIgv <= 0 THEN x-PorIgv = Ccbddocu.ImpIgv / ( Ccbddocu.ImpLin - Ccbddocu.ImpIgv) * 100.
    IF x-ImpCto = ? THEN x-ImpCto = 0.

    CREATE Ventas_Detalle.
    ASSIGN
        Ventas_Detalle.DateKey = Ventas_Cabecera.DateKey
        Ventas_Detalle.CodDiv = Ventas_Cabecera.coddiv
        Ventas_Detalle.CodDoc = Ventas_Cabecera.coddoc
        Ventas_Detalle.NroDoc = Ventas_Cabecera.nrodoc
        Ventas_Detalle.CodMat = TRIM(ccbddocu.codmat)
        Ventas_Detalle.Cantidad = ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        Ventas_Detalle.AlmDes = x-AlmDes.
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            Ventas_Detalle.CostoExtCIGV = x-signo1 * x-ImpCto * x-coe / x-TpoCmbCmp
            Ventas_Detalle.CostoExtSIGV = Ventas_Detalle.CostoExtCIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.CostoNacCIGV = x-signo1 * x-ImpCto * x-coe
            Ventas_Detalle.CostoNacSIGV = Ventas_Detalle.CostoNacSIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            Ventas_Detalle.ImpExtSIGV   = Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
            Ventas_Detalle.ImpNacSIGV   = Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            Ventas_Detalle.CostoExtCIGV = x-signo1 * x-ImpCto * x-coe 
            Ventas_Detalle.CostoExtSIGV = Ventas_Detalle.CostoExtCIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.CostoNacCIGV = x-signo1 * x-ImpCto * x-coe * x-TpoCmbVta
            Ventas_Detalle.CostoNacSIGV = Ventas_Detalle.CostoNacSIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            Ventas_Detalle.ImpExtSIGV   = Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
            Ventas_Detalle.ImpNacSIGV   = Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    /* 11.09.10 INCORPORAMOS EL COSTO PROMEDIO */
    FIND LAST AlmStkGe WHERE Almstkge.codcia = Ccbcdocu.codcia
        AND Almstkge.codmat = Ccbddocu.codmat
        AND Almstkge.fecha <= Ccbcdocu.fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almstkge AND Almstkge.CtoUni <> ? THEN DO:
        ASSIGN
            Ventas_Detalle.PromExtSIGV = x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe / x-TpoCmbCmp
            Ventas_Detalle.PromExtCIGV = Ventas_Detalle.PromExtSIGV * ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.PromNacSIGV = x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe
            Ventas_Detalle.PromNacCIGV = Ventas_Detalle.PromNacSIGV * ( 1 + ( x-PorIgv / 100) ).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Limpiar-Texto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Limpiar-Texto Procedure 
PROCEDURE Limpiar-Texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER cadenaTexto AS CHAR.
DEF INPUT PARAMETER sustituirPor AS CHAR.
DEF OUTPUT PARAMETER cadenaResultado AS CHAR.

DEF VAR tamanoCadena AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR caracteresValidos AS CHAR NO-UNDO.
DEF VAR caracterActual AS CHAR NO-UNDO.
  
tamanoCadena = LENGTH(cadenaTexto).
If tamanoCadena > 0 THEN DO:
    caracteresValidos = ' 0123456789abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ-_.,"'.
    caracteresValidos = caracteresValidos + "/()#*+-$&%'áéíóúÁÉÍÓÚüÜº°:´Øª¿?!¡=@".
    DO i = 1 TO tamanoCadena:
        caracterActual = SUBSTRING(cadenaTexto, i, 1).
        IF INDEX(caracteresValidos, caracterActual) > 0 THEN
            cadenaResultado = cadenaResultado + caracterActual.
        ELSE 
            cadenaResultado = cadenaResultado + sustituirPor.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PROCESA-ANTCIPOS-APLICADOS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PROCESA-ANTCIPOS-APLICADOS Procedure 
PROCEDURE PROCESA-ANTCIPOS-APLICADOS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* CREAMOS LA CABECERA */                       
CREATE Ventas_Cabecera.
ASSIGN
    Ventas_Cabecera.DateKey = ccbcdocu.fchdoc
    Ventas_Cabecera.CodDiv = "99999"
    Ventas_Cabecera.DivDes = pdivdes
    Ventas_Cabecera.CodCli = x-codcli        /* x-codunico */
    Ventas_Cabecera.CodDoc = "N" + ccbcdocu.coddoc
    Ventas_Cabecera.NroDoc = ccbcdocu.nrodoc
    Ventas_Cabecera.CodVen = x-codven
    Ventas_Cabecera.FmaPgo = x-fmapgo
    Ventas_Cabecera.TpoCmb = ccbcdocu.tpocmb
    Ventas_Cabecera.TpoCmbVta = x-tpocmbvta
    Ventas_Cabecera.TpoCmbCmp = x-tpocmbcmp
    Ventas_Cabecera.Tipo    = x-Tipo
    Ventas_Cabecera.NroCard = x-nrocard.

DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

/* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
ASSIGN
    x-ImpLin = Ccbcdocu.ImpTot2
    x-ImpCto = 0
    x-signo1 = -1.
IF x-ImpCto = ? THEN x-ImpCto = 0.
/* ************************************************************************ */
CREATE Ventas_Detalle.
ASSIGN
    Ventas_Detalle.CodDiv = Ventas_Cabecera.coddiv
    Ventas_Detalle.CodDoc = Ventas_Cabecera.coddoc
    Ventas_Detalle.NroDoc = Ventas_Cabecera.nrodoc
    Ventas_Detalle.CodMat = "999999"
    Ventas_Detalle.Cantidad = 0
    Ventas_Detalle.AlmDes = "".
IF Ccbcdocu.CodMon = 1 THEN 
    ASSIGN
        Ventas_Detalle.CostoExtCIGV = 0
        Ventas_Detalle.CostoExtSIGV = 0
        Ventas_Detalle.CostoNacCIGV = 0
        Ventas_Detalle.CostoNacSIGV = 0
        Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
        Ventas_Detalle.ImpExtSIGV   = Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
        Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
        Ventas_Detalle.ImpNacSIGV   = Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
IF Ccbcdocu.CodMon = 2 THEN 
    ASSIGN
        Ventas_Detalle.CostoExtCIGV = 0
        Ventas_Detalle.CostoExtSIGV = 0
        Ventas_Detalle.CostoNacCIGV = 0
        Ventas_Detalle.CostoNacSIGV = 0
        Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
        Ventas_Detalle.ImpExtSIGV   = Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
        Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
        Ventas_Detalle.ImpNacSIGV   = Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).


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
/* RHC 25/04/2014 cambio en la lógica */
x-ImpTot = 0.
FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:
    x-ImpTot = x-ImpTot + Ccbddocu.ImpLin.
END.

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

    x-AlmDes = CcbDdocu.AlmDes.
    IF x-AlmDes = '' THEN x-AlmDes = ENTRY(1, B-CDOCU.CodAlm).
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

        x-AlmDes = CcbDdocu.AlmDes.
        IF x-AlmDes = '' THEN x-AlmDes = ENTRY(1, B-CDOCU.CodAlm).
        RUN Carga-Ventas-Detalle.

    END.  



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Procesa-Otras-Ventas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Otras-Ventas Procedure 
PROCEDURE Procesa-Otras-Ventas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
    ASSIGN
        x-ImpLin = Ccbcdocu.ImpTot
        x-ImpCto = 0.
    IF x-PorIgv <= 0 THEN x-PorIgv = Ccbcdocu.ImpIgv / ( Ccbcdocu.ImpTot - Ccbcdocu.ImpIgv) * 100.
    IF x-ImpCto = ? THEN x-ImpCto = 0.
    /* ************************************************************************ */
    CREATE Ventas_Detalle.
    ASSIGN
        Ventas_Detalle.CodDiv = Ventas_Cabecera.coddiv
        Ventas_Detalle.CodDoc = Ventas_Cabecera.coddoc
        Ventas_Detalle.NroDoc = Ventas_Cabecera.nrodoc
        Ventas_Detalle.CodMat = "999999"
        Ventas_Detalle.Cantidad = 0
        Ventas_Detalle.AlmDes = "".
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            Ventas_Detalle.CostoExtCIGV = 0
            Ventas_Detalle.CostoExtSIGV = 0
            Ventas_Detalle.CostoNacCIGV = 0
            Ventas_Detalle.CostoNacSIGV = 0
            Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            Ventas_Detalle.ImpExtSIGV   = Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
            Ventas_Detalle.ImpNacSIGV   = Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            Ventas_Detalle.CostoExtCIGV = 0
            Ventas_Detalle.CostoExtSIGV = 0
            Ventas_Detalle.CostoNacCIGV = 0
            Ventas_Detalle.CostoNacSIGV = 0
            Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            Ventas_Detalle.ImpExtSIGV   = Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
            Ventas_Detalle.ImpNacSIGV   = Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SemanaIso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SemanaIso Procedure 
PROCEDURE SemanaIso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pFecha AS DATE.
DEF OUTPUT PARAMETER pSemana AS INT.
DEF OUTPUT PARAMETER pAno AS INT.

DEF VAR xDia AS INT.
DEF VAR xMes AS INT.
DEF VAR xAno AS INT.
DEF VAR xA AS INT.
DEF VAR xB AS INT.
DEF VAR xC AS INT.
DEF VAR xD AS INT.
DEF VAR xS AS INT.
DEF VAR xE AS INT.
DEF VAR xF AS INT.
DEF VAR xG AS INT.
DEF VAR xN AS INT.
DEF VAR xSemana AS INT.

ASSIGN
    xDia = DAY(pFecha)
    xMes = MONTH(pFecha)
    xAno = YEAR(pFecha).

IF xMes = 01 OR xMes = 02 THEN DO:  /* Enero o Febrero */
    ASSIGN
        xA = xAno - 1
        xB = TRUNCATE(xA / 4, 0) - TRUNCATE(xA / 100, 0) + TRUNCATE(xA / 400, 0)
        xC = TRUNCATE( (xA - 1) / 4, 0) - TRUNCATE( (xA - 1) / 100, 0) + TRUNCATE( (xA - 1) / 400, 0)
        xS = xB - xC
        xE = 0
        xF = xDia - 1 + (31 * (xMes - 1) ).
END.
ELSE DO:    /* de Marzo a Diciembre */
    ASSIGN
        xA = xAno
        xB = TRUNCATE(xA / 4, 0 ) - TRUNCATE(xA / 100, 0) + TRUNCATE(xA / 400, 0)
        xC = TRUNCATE( (xA - 1) / 4, 0 ) - TRUNCATE( (xA - 1) / 100, 0) + TRUNCATE( (xA - 1) / 400, 0)
        xS = xB - xC
        xE = xS + 1
        xF = xDia + TRUNCATE( ( ( 153 * (xMes - 3) ) + 2 ) / 5, 0) + 58 + xS.
END.
/* Adicionalmente sumándole 1 a la variable xF 
    se obtiene numero ordinal del dia de la fecha ingresada con referencia al año actual
    */
/* Estos cálculos se aplican a cualquier mes */
ASSIGN
    xG = (xA + xB) MODULO 7
    xD = (xF + xG - xE) MODULO 7 /* Adicionalmente esta variable nos indica el dia de la semana 0=Lunes, ... , 6=Domingo */
    xN = xF + 3 - xD.
IF xN < 0 THEN DO:
    /* Si la variable n es menor a 0 se trata de una semana perteneciente al año anterior */
    ASSIGN
        xSemana = 53 - TRUNCATE( (xG - xS) / 5, 0)
        xAno = xAno - 1.
END.
ELSE IF xN > (364 + xS) THEN DO:
    /* Si n es mayor a 364 + $s entonces la fecha corresponde a la primera semana del año siguiente.*/
    ASSIGN
        xSemana = 1
        xAno = xAno + 1.
END.
ELSE DO:
    /* En cualquier otro caso es una semana del año actual */
    xSemana = TRUNCATE(xN / 7, 0) + 1.
END.

ASSIGN
    pAno = xAno
    pSemana = xSemana.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

