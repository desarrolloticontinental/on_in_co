&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER COTIZACION FOR FacCPedi.
DEFINE BUFFER PEDIDO FOR FacCPedi.



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
DEF BUFFER B-FAC   FOR CcbCdocu.

ASSIGN
    x-CodFchF = TODAY - 1
    x-CodFchI = DATE(01, 01, YEAR(TODAY)).      /* Por defecto */

/* Fecha de Cierre */
/* un meses atrás */
x-CodFchI = ADD-INTERVAL(TODAY, -1, "months").

FIND FIRST integral.Empresas WHERE integral.Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE integral.Empresas THEN DO:
    IF NOT integral.Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
    IF NOT integral.Empresas.Campo-CodPro THEN pv-codcia = s-codcia.
END.
FIND FIRST FacCfgGn WHERE FacCFgGn.codcia = s-codcia NO-LOCK NO-ERROR.
s-CliVar = FacCfgGn.CliVar.

x-CodFchI = DATE(01,01,2019).
x-CodFchF = DATE(12,31,2020).

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
      TABLE: COTIZACION B "?" ? INTEGRAL FacCPedi
      TABLE: PEDIDO B "?" ? INTEGRAL FacCPedi
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 19.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF TEMP-TABLE t-ventas_cabecera NO-UNDO LIKE ventas_cabecera.
/* 04/09/2024: Ya existen esos campos */
/* DEF TEMP-TABLE t-ventas_cabecera LIKE ventas_cabecera */
/*     FIELD CodRef AS CHAR FORMAT 'x(8)'                */
/*     FIELD NroRef AS CHAR FORMAT 'x(15)'.              */

DEF TEMP-TABLE t-ventas_detalle NO-UNDO LIKE ventas_detalle
    FIELD CodRef AS CHAR FORMAT 'x(8)'
    FIELD NroRef AS CHAR FORMAT 'x(15)'.

/* INFORMACION DETALLADA Y DEPURADA */
PUT 'carga ventas: ' NOW SKIP. 
PAUSE 0.
RUN Carga-Ventas-Basicas.

PUT 'pasa estadisticas' NOW SKIP.
PAUSE 0.
DEF STREAM Reporte.
RUN pasa-estadisticas.

PUT '** fin del proceso ** ' NOW SKIP. 
PAUSE 0.

QUIT.

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

FOR EACH Ventas EXCLUSIVE-LOCK WHERE Ventas.DateKey >= x-CodFchI AND Ventas.DateKey <= x-CodFchF:
    DELETE Ventas.
END.
FOR EACH VentasxCliente EXCLUSIVE-LOCK WHERE VentasxCliente.DateKey >= x-CodFchI AND VentasxCliente.DateKey <= x-CodFchF:
    DELETE VentasxCliente.
END.
FOR EACH VentasxProducto EXCLUSIVE-LOCK  WHERE VentasxProducto.DateKey >= x-CodFchI AND VentasxProducto.DateKey <= x-CodFchF:
    DELETE VentasxProducto.
END.
FOR EACH VentasxVendedor EXCLUSIVE-LOCK WHERE VentasxVendedor.DateKey >= x-CodFchI AND VentasxVendedor.DateKey <= x-CodFchF:
    DELETE VentasxVendedor.
END.
FOR EACH VentasxClienteLinea EXCLUSIVE-LOCK WHERE VentasxClienteLinea.DateKey >= x-CodFchI AND VentasxClienteLinea.DateKey <= x-CodFchF:
    DELETE VentasxClienteLinea.
END.
FOR EACH VentasxVendCliente EXCLUSIVE-LOCK WHERE VentasxVendCliente.DateKey >= x-CodFchI AND VentasxVendCliente.DateKey <= x-CodFchF:
    DELETE VentasxVendCliente.
END.
FOR EACH VentasxLinea EXCLUSIVE-LOCK WHERE VentasxLinea.DateKey >= x-CodFchI AND VentasxLinea.DateKey <= x-CodFchF:
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

FOR EACH t-ventas_cabecera EXCLUSIVE-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI AND t-ventas_cabecera.DateKey <= x-CodFchF:
    FOR EACH t-ventas_detalle OF t-ventas_cabecera EXCLUSIVE-LOCK:
        DELETE t-ventas_detalle.
    END.
    DELETE t-ventas_cabecera.
END.
IF AVAILABLE t-ventas_cabecera THEN RELEASE t-ventas_cabecera.
IF AVAILABLE t-ventas_detalle THEN RELEASE t-ventas_detalle.

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

    FIND FIRST VentasxCliente WHERE VentasxCliente.DateKey = t-ventas_cabecera.DateKey
        AND VentasxCliente.coddiv = t-ventas_cabecera.coddiv
        AND VentasxCliente.divdes = t-ventas_cabecera.divdes
        AND VentasxCliente.codcli = t-ventas_cabecera.codcli
        AND VentasxCliente.tipo   = t-ventas_cabecera.tipo
        AND VentasxCliente.delivery = t-ventas_cabecera.delivery
        AND VentasxCliente.ListaBase = t-ventas_cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxCliente THEN CREATE VentasxCliente.
    ASSIGN
        VentasxCliente.DateKey      = t-ventas_cabecera.DateKey
        VentasxCliente.CodDiv       = t-ventas_cabecera.coddiv
        VentasxCliente.DivDes       = t-ventas_cabecera.divdes
        VentasxCliente.CodCli       = t-ventas_cabecera.codcli
        VentasxCliente.Tipo         = t-ventas_cabecera.tipo
        VentasxCliente.delivery     = t-ventas_cabecera.delivery
        VentasxCliente.ListaBase     = t-ventas_cabecera.ListaBase
        VentasxCliente.ImpExtCIGV   = VentasxCliente.impextcigv + t-ventas_detalle.impextcigv
        VentasxCliente.ImpExtSIGV   = VentasxCliente.impextsigv + t-ventas_detalle.impextsigv
        VentasxCliente.ImpNacCIGV   = VentasxCliente.impnaccigv + t-ventas_detalle.impnaccigv
        VentasxCliente.ImpNacSIGV   = VentasxCliente.impnacsigv + t-ventas_detalle.impnacsigv
        VentasxCliente.CostoExtCIGV = VentasxCliente.costoextcigv + t-ventas_detalle.costoextcigv
        VentasxCliente.CostoExtSIGV = VentasxCliente.costoextsigv + t-ventas_detalle.costoextsigv
        VentasxCliente.CostoNacCIGV = VentasxCliente.costonaccigv + t-ventas_detalle.costonaccigv
        VentasxCliente.CostoNacSIGV = VentasxCliente.costonacsigv + t-ventas_detalle.costonacsigv
        VentasxCliente.PromExtCIGV  = VentasxCliente.promextcigv + t-ventas_detalle.promextcigv
        VentasxCliente.PromExtSIGV  = VentasxCliente.promextsigv + t-ventas_detalle.promextsigv
        VentasxCliente.PromNacCIGV  = VentasxCliente.promnaccigv + t-ventas_detalle.promnaccigv
        VentasxCliente.PromNacSIGV  = VentasxCliente.promnacsigv + t-ventas_detalle.promnacsigv.

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

    FIND FIRST VentasxClienteLinea WHERE VentasxClienteLinea.DateKey = t-ventas_cabecera.DateKey
        AND VentasxClienteLinea.coddiv = t-ventas_cabecera.coddiv
        AND VentasxClienteLinea.divdes = t-ventas_cabecera.divdes
        AND VentasxClienteLinea.codcli = t-ventas_cabecera.codcli
        AND VentasxClienteLinea.CodFam = DimProducto.CodFam
        AND VentasxClienteLinea.SubFam = DimProducto.SubFam
        AND VentasxClienteLinea.DesMar = DimProducto.DesMar
        AND VentasxClienteLinea.Licencia = DimProducto.Licencia
        AND VentasxClienteLinea.Tipo = t-ventas_cabecera.Tipo
        AND VentasxClienteLinea.Delivery = t-ventas_cabecera.Delivery
        AND VentasxClienteLinea.ListaBase = t-ventas_cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxClienteLinea THEN CREATE VentasxClienteLinea.
    ASSIGN
        VentasxClienteLinea.DateKey = t-ventas_cabecera.DateKey
        VentasxClienteLinea.CodDiv = t-ventas_cabecera.coddiv
        VentasxClienteLinea.DivDes = t-ventas_cabecera.divdes
        VentasxClienteLinea.CodCli = t-ventas_cabecera.codcli
        VentasxClienteLinea.CodFam = DimProducto.CodFam
        VentasxClienteLinea.SubFam = DimProducto.SubFam
        VentasxClienteLinea.DesMar = DimProducto.DesMar
        VentasxClienteLinea.Licencia = DimProducto.Licencia
        VentasxClienteLinea.Tipo     = t-ventas_cabecera.tipo
        VentasxClienteLinea.Delivery = t-ventas_cabecera.Delivery
        VentasxClienteLinea.ListaBase = t-ventas_cabecera.ListaBase
        VentasxClienteLinea.ImpExtCIGV = VentasxClienteLinea.impextcigv + t-ventas_detalle.impextcigv
        VentasxClienteLinea.ImpExtSIGV = VentasxClienteLinea.impextsigv + t-ventas_detalle.impextsigv
        VentasxClienteLinea.ImpNacCIGV = VentasxClienteLinea.impnaccigv + t-ventas_detalle.impnaccigv
        VentasxClienteLinea.ImpNacSIGV = VentasxClienteLinea.impnacsigv + t-ventas_detalle.impnacsigv
        VentasxClienteLinea.CostoExtCIGV = VentasxClienteLinea.costoextcigv + t-ventas_detalle.costoextcigv
        VentasxClienteLinea.CostoExtSIGV = VentasxClienteLinea.costoextsigv + t-ventas_detalle.costoextsigv
        VentasxClienteLinea.CostoNacCIGV = VentasxClienteLinea.costonaccigv + t-ventas_detalle.costonaccigv
        VentasxClienteLinea.CostoNacSIGV = VentasxClienteLinea.costonacsigv + t-ventas_detalle.costonacsigv
        VentasxClienteLinea.PromExtCIGV = VentasxClienteLinea.promextcigv + t-ventas_detalle.promextcigv
        VentasxClienteLinea.PromExtSIGV = VentasxClienteLinea.promextsigv + t-ventas_detalle.promextsigv
        VentasxClienteLinea.PromNacCIGV = VentasxClienteLinea.promnaccigv + t-ventas_detalle.promnaccigv
        VentasxClienteLinea.PromNacSIGV = VentasxClienteLinea.promnacsigv + t-ventas_detalle.promnacsigv.

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

/*     FIND FIRST VentasxAlmacen WHERE VentasxAlmacen.DateKey = t-ventas_cabecera.DateKey            */
/*         AND VentasxAlmacen.codmat = t-ventas_detalle.codmat                                       */
/*         AND VentasxAlmacen.almdes = t-ventas_detalle.almdes                                       */
/*         AND VentasxAlmacen.coddiv = t-ventas_cabecera.divdes                                      */
/*         NO-ERROR.                                                                               */
/*     IF NOT AVAILABLE VentasxAlmacen THEN CREATE VentasxAlmacen.                                 */
/*     ASSIGN                                                                                      */
/*         VentasxAlmacen.DateKey = t-ventas_cabecera.DateKey                                        */
/*         VentasxAlmacen.CodMat = t-ventas_detalle.codmat                                           */
/*         VentasxAlmacen.AlmDes = t-ventas_detalle.almdes                                           */
/*         VentasxAlmacen.CodDiv = t-ventas_cabecera.divdes                                          */
/*         VentasxAlmacen.Cantidad = VentasxAlmacen.Cantidad + t-ventas_detalle.cantidad             */
/*         VentasxAlmacen.ImpExtCIGV = VentasxAlmacen.impextcigv + t-ventas_detalle.impextcigv       */
/*         VentasxAlmacen.ImpExtSIGV = VentasxAlmacen.impextsigv + t-ventas_detalle.impextsigv       */
/*         VentasxAlmacen.ImpNacCIGV = VentasxAlmacen.impnaccigv + t-ventas_detalle.impnaccigv       */
/*         VentasxAlmacen.ImpNacSIGV = VentasxAlmacen.impnacsigv + t-ventas_detalle.impnacsigv       */
/*         VentasxAlmacen.CostoExtCIGV = VentasxAlmacen.costoextcigv + t-ventas_detalle.costoextcigv */
/*         VentasxAlmacen.CostoExtSIGV = VentasxAlmacen.costoextsigv + t-ventas_detalle.costoextsigv */
/*         VentasxAlmacen.CostoNacCIGV = VentasxAlmacen.costonaccigv + t-ventas_detalle.costonaccigv */
/*         VentasxAlmacen.CostoNacSIGV = VentasxAlmacen.costonacsigv + t-ventas_detalle.costonacsigv */
/*         VentasxAlmacen.PromExtCIGV = VentasxAlmacen.promextcigv + t-ventas_detalle.promextcigv    */
/*         VentasxAlmacen.PromExtSIGV = VentasxAlmacen.promextsigv + t-ventas_detalle.promextsigv    */
/*         VentasxAlmacen.PromNacCIGV = VentasxAlmacen.promnaccigv + t-ventas_detalle.promnaccigv    */
/*         VentasxAlmacen.PromNacSIGV = VentasxAlmacen.promnacsigv + t-ventas_detalle.promnacsigv.   */

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

FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
    /* VENTAS PRINCIPAL */
    RUN Carga-ventas.
END.
FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
    /* VENTAS POR CLIENTE */
    RUN Carga-cli.
END.
FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
    /* VENTAS POR CLIENTE */
    RUN Carga-mat.
END.
FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
    /* VENTAS POR VENDEDOR */
    RUN Carga-vend.
END.
FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
    /* VENTAS POR CLIENTE Y PRODUCTO */
    RUN Carga-climat.
END.
FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
    /* VENTAS POR VENDEDOR Y CLIENTE */
    RUN Carga-vendcli.
END.
FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
    /* VENTAS POR PRODUCTO RESUMIDO */
    RUN Carga-resmat.

END.
FOR EACH t-ventas_cabecera NO-LOCK WHERE t-ventas_cabecera.DateKey >= x-CodFchI
    AND t-ventas_cabecera.DateKey <= x-CodFchF,
    EACH t-ventas_detalle OF t-ventas_cabecera NO-LOCK,
    FIRST DimProducto OF t-ventas_detalle NO-LOCK:
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

    FIND FIRST VentasxProducto WHERE VentasxProducto.DateKey = t-ventas_cabecera.DateKey
        AND VentasxProducto.coddiv = t-ventas_cabecera.coddiv
        AND VentasxProducto.divdes = t-ventas_cabecera.divdes
        AND VentasxProducto.CodMat = t-ventas_detalle.codmat
        AND VentasxProducto.Tipo = t-ventas_cabecera.tipo
        AND VentasxProducto.Delivery = t-ventas_cabecera.Delivery
        AND VentasxProducto.ListaBase = t-ventas_cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxProducto THEN CREATE VentasxProducto.
    ASSIGN
        VentasxProducto.DateKey = t-ventas_cabecera.DateKey
        VentasxProducto.CodDiv = t-ventas_cabecera.coddiv
        VentasxProducto.DivDes = t-ventas_cabecera.divdes
        VentasxProducto.CodMat = TRIM(t-ventas_detalle.codmat)
        VentasxProducto.Tipo = t-ventas_cabecera.tipo
        VentasxProducto.Delivery = t-ventas_cabecera.Delivery
        VentasxProducto.ListaBase = t-ventas_cabecera.ListaBase
        VentasxProducto.Cantidad = VentasxProducto.cantidad + t-ventas_detalle.cantidad
        VentasxProducto.ImpExtCIGV = VentasxProducto.impextcigv + t-ventas_detalle.impextcigv
        VentasxProducto.ImpExtSIGV = VentasxProducto.impextsigv + t-ventas_detalle.impextsigv
        VentasxProducto.ImpNacCIGV = VentasxProducto.impnaccigv + t-ventas_detalle.impnaccigv
        VentasxProducto.ImpNacSIGV = VentasxProducto.impnacsigv + t-ventas_detalle.impnacsigv
        VentasxProducto.CostoExtCIGV = VentasxProducto.costoextcigv + t-ventas_detalle.costoextcigv
        VentasxProducto.CostoExtSIGV = VentasxProducto.costoextsigv + t-ventas_detalle.costoextsigv
        VentasxProducto.CostoNacCIGV = VentasxProducto.costonaccigv + t-ventas_detalle.costonaccigv
        VentasxProducto.CostoNacSIGV = VentasxProducto.costonacsigv + t-ventas_detalle.costonacsigv
        VentasxProducto.PromExtCIGV = VentasxProducto.promextcigv + t-ventas_detalle.promextcigv
        VentasxProducto.PromExtSIGV = VentasxProducto.promextsigv + t-ventas_detalle.promextsigv
        VentasxProducto.PromNacCIGV = VentasxProducto.promnaccigv + t-ventas_detalle.promnaccigv
        VentasxProducto.PromNacSIGV = VentasxProducto.promnacsigv + t-ventas_detalle.promnacsigv.

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

    FIND FIRST VentasxLinea WHERE VentasxLinea.DateKey = t-ventas_cabecera.DateKey
        AND VentasxLinea.coddiv = t-ventas_cabecera.coddiv
        AND VentasxLinea.divdes = t-ventas_cabecera.divdes
        AND VentasxLinea.codfam = DimProducto.codfam
        AND VentasxLinea.subfam = DimProducto.subfam
        AND VentasxLinea.desmar = DimProducto.desmar
        AND VentasxLinea.licencia = DimProducto.licencia
        AND VentasxLinea.codpro = DimProducto.codpro[1]
        AND VentasxLinea.codven = t-ventas_cabecera.codven
        AND VentasxLinea.tipo = t-ventas_cabecera.tipo
        AND VentasxLinea.delivery = t-ventas_cabecera.delivery
        AND VentasxLinea.ListaBase = t-ventas_cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxLinea THEN CREATE VentasxLinea.
    ASSIGN
        VentasxLinea.DateKey = t-ventas_cabecera.DateKey
        VentasxLinea.CodDiv = t-ventas_cabecera.coddiv
        VentasxLinea.DivDes = t-ventas_cabecera.divdes
        VentasxLinea.codfam = DimProducto.codfam
        VentasxLinea.subfam = DimProducto.subfam
        VentasxLinea.desmar = DimProducto.desmar
        VentasxLinea.licencia = DimProducto.licencia
        VentasxLinea.codpro = DimProducto.codpro[1]
        VentasxLinea.codven = t-ventas_cabecera.codven
        VentasxLinea.tipo = t-ventas_cabecera.tipo
        VentasxLinea.delivery = t-ventas_cabecera.delivery
        VentasxLinea.ListaBase = t-ventas_cabecera.ListaBase
        VentasxLinea.ImpExtCIGV = VentasxLinea.impextcigv + t-ventas_detalle.impextcigv
        VentasxLinea.ImpExtSIGV = VentasxLinea.impextsigv + t-ventas_detalle.impextsigv
        VentasxLinea.ImpNacCIGV = VentasxLinea.impnaccigv + t-ventas_detalle.impnaccigv
        VentasxLinea.ImpNacSIGV = VentasxLinea.impnacsigv + t-ventas_detalle.impnacsigv
        VentasxLinea.CostoExtCIGV = VentasxLinea.costoextcigv + t-ventas_detalle.costoextcigv
        VentasxLinea.CostoExtSIGV = VentasxLinea.costoextsigv + t-ventas_detalle.costoextsigv
        VentasxLinea.CostoNacCIGV = VentasxLinea.costonaccigv + t-ventas_detalle.costonaccigv
        VentasxLinea.CostoNacSIGV = VentasxLinea.costonacsigv + t-ventas_detalle.costonacsigv
        VentasxLinea.PromExtCIGV = VentasxLinea.promextcigv + t-ventas_detalle.promextcigv
        VentasxLinea.PromExtSIGV = VentasxLinea.promextsigv + t-ventas_detalle.promextsigv
        VentasxLinea.PromNacCIGV = VentasxLinea.promnaccigv + t-ventas_detalle.promnaccigv
        VentasxLinea.PromNacSIGV = VentasxLinea.promnacsigv + t-ventas_detalle.promnacsigv.

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

    FIND FIRST VentasxVendedor WHERE VentasxVendedor.DateKey = t-ventas_cabecera.DateKey
        AND VentasxVendedor.coddiv = t-ventas_cabecera.coddiv
        AND VentasxVendedor.divdes = t-ventas_cabecera.divdes
        AND VentasxVendedor.codven = t-ventas_cabecera.codven
        AND VentasxVendedor.tipo = t-ventas_cabecera.tipo
        AND VentasxVendedor.delivery = t-ventas_cabecera.delivery
        AND VentasxVendedor.ListaBase = t-ventas_cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxVendedor THEN CREATE VentasxVendedor.
    ASSIGN
        VentasxVendedor.DateKey = t-ventas_cabecera.DateKey
        VentasxVendedor.CodDiv = t-ventas_cabecera.coddiv
        VentasxVendedor.DivDes = t-ventas_cabecera.divdes
        VentasxVendedor.CodVen = t-ventas_cabecera.codven
        VentasxVendedor.Tipo = t-ventas_cabecera.tipo
        VentasxVendedor.delivery = t-ventas_cabecera.delivery
        VentasxVendedor.ListaBase = t-ventas_cabecera.ListaBase
        VentasxVendedor.ImpExtCIGV = VentasxVendedor.impextcigv + t-ventas_detalle.impextcigv
        VentasxVendedor.ImpExtSIGV = VentasxVendedor.impextsigv + t-ventas_detalle.impextsigv
        VentasxVendedor.ImpNacCIGV = VentasxVendedor.impnaccigv + t-ventas_detalle.impnaccigv
        VentasxVendedor.ImpNacSIGV = VentasxVendedor.impnacsigv + t-ventas_detalle.impnacsigv
        VentasxVendedor.CostoExtCIGV = VentasxVendedor.costoextcigv + t-ventas_detalle.costoextcigv
        VentasxVendedor.CostoExtSIGV = VentasxVendedor.costoextsigv + t-ventas_detalle.costoextsigv
        VentasxVendedor.CostoNacCIGV = VentasxVendedor.costonaccigv + t-ventas_detalle.costonaccigv
        VentasxVendedor.CostoNacSIGV = VentasxVendedor.costonacsigv + t-ventas_detalle.costonacsigv
        VentasxVendedor.PromExtCIGV = VentasxVendedor.promextcigv + t-ventas_detalle.promextcigv
        VentasxVendedor.PromExtSIGV = VentasxVendedor.promextsigv + t-ventas_detalle.promextsigv
        VentasxVendedor.PromNacCIGV = VentasxVendedor.promnaccigv + t-ventas_detalle.promnaccigv
        VentasxVendedor.PromNacSIGV = VentasxVendedor.promnacsigv + t-ventas_detalle.promnacsigv.

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

    FIND FIRST VentasxVendCliente WHERE VentasxVendCliente.DateKey = t-ventas_cabecera.DateKey
        AND VentasxVendCliente.coddiv = t-ventas_cabecera.coddiv
        AND VentasxVendCliente.divdes = t-ventas_cabecera.divdes
        AND VentasxVendCliente.codcli = t-ventas_cabecera.codcli
        AND VentasxVendCliente.codven = t-ventas_cabecera.codven
        AND VentasxVendCliente.tipo   = t-ventas_cabecera.tipo
        AND VentasxVendCliente.delivery = t-ventas_cabecera.delivery
        AND VentasxVendCliente.ListaBase = t-ventas_cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxVendCliente THEN CREATE VentasxVendCliente.
    ASSIGN
        VentasxVendCliente.DateKey = t-ventas_cabecera.DateKey
        VentasxVendCliente.CodDiv = t-ventas_cabecera.coddiv
        VentasxVendCliente.DivDes = t-ventas_cabecera.divdes
        VentasxVendCliente.CodCli = t-ventas_cabecera.codcli
        VentasxVendCliente.CodVen = t-ventas_cabecera.codven
        VentasxVendCliente.Tipo   = t-ventas_cabecera.tipo
        VentasxVendCliente.delivery = t-ventas_cabecera.delivery
        VentasxVendCliente.ListaBase = t-ventas_cabecera.ListaBase
        VentasxVendCliente.ImpExtCIGV = VentasxVendCliente.impextcigv + t-ventas_detalle.impextcigv
        VentasxVendCliente.ImpExtSIGV = VentasxVendCliente.impextsigv + t-ventas_detalle.impextsigv
        VentasxVendCliente.ImpNacCIGV = VentasxVendCliente.impnaccigv + t-ventas_detalle.impnaccigv
        VentasxVendCliente.ImpNacSIGV = VentasxVendCliente.impnacsigv + t-ventas_detalle.impnacsigv
        VentasxVendCliente.CostoExtCIGV = VentasxVendCliente.costoextcigv + t-ventas_detalle.costoextcigv
        VentasxVendCliente.CostoExtSIGV = VentasxVendCliente.costoextsigv + t-ventas_detalle.costoextsigv
        VentasxVendCliente.CostoNacCIGV = VentasxVendCliente.costonaccigv + t-ventas_detalle.costonaccigv
        VentasxVendCliente.CostoNacSIGV = VentasxVendCliente.costonacsigv + t-ventas_detalle.costonacsigv
        VentasxVendCliente.PromExtCIGV = VentasxVendCliente.promextcigv + t-ventas_detalle.promextcigv
        VentasxVendCliente.PromExtSIGV = VentasxVendCliente.promextsigv + t-ventas_detalle.promextsigv
        VentasxVendCliente.PromNacCIGV = VentasxVendCliente.promnaccigv + t-ventas_detalle.promnaccigv
        VentasxVendCliente.PromNacSIGV = VentasxVendCliente.promnacsigv + t-ventas_detalle.promnacsigv.

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
    FIND FIRST Ventas WHERE Ventas.DateKey = t-ventas_cabecera.DateKey
        AND Ventas.coddiv = t-ventas_cabecera.coddiv
        AND Ventas.divdes = t-ventas_cabecera.divdes
        AND Ventas.codcli = t-ventas_cabecera.codcli
        AND Ventas.codven = t-ventas_cabecera.codven
        AND Ventas.CodMat = t-ventas_detalle.codmat
        AND Ventas.Tipo   = t-ventas_cabecera.tipo
        AND Ventas.Delivery = t-ventas_cabecera.Delivery
        AND Ventas.ListaBase = t-ventas_cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE Ventas THEN CREATE Ventas.
    ASSIGN
        Ventas.DateKey = t-ventas_cabecera.DateKey
        Ventas.CodDiv = t-ventas_cabecera.coddiv
        Ventas.DivDes = t-ventas_cabecera.divdes
        Ventas.CodCli = t-ventas_cabecera.codcli
        Ventas.CodVen = t-ventas_cabecera.codven
        Ventas.CodMat = t-ventas_detalle.codmat
        Ventas.Tipo   = t-ventas_cabecera.tipo
        Ventas.Delivery = t-ventas_cabecera.Delivery
        Ventas.ListaBase = t-ventas_cabecera.ListaBase
        Ventas.Cantidad = Ventas.cantidad + t-ventas_detalle.cantidad
        Ventas.ImpExtCIGV = Ventas.impextcigv + t-ventas_detalle.impextcigv
        Ventas.ImpExtSIGV = Ventas.impextsigv + t-ventas_detalle.impextsigv
        Ventas.ImpNacCIGV = Ventas.impnaccigv + t-ventas_detalle.impnaccigv
        Ventas.ImpNacSIGV = Ventas.impnacsigv + t-ventas_detalle.impnacsigv
        Ventas.CostoExtCIGV = Ventas.costoextcigv + t-ventas_detalle.costoextcigv
        Ventas.CostoExtSIGV = Ventas.costoextsigv + t-ventas_detalle.costoextsigv
        Ventas.CostoNacCIGV = Ventas.costonaccigv + t-ventas_detalle.costonaccigv
        Ventas.CostoNacSIGV = Ventas.costonacsigv + t-ventas_detalle.costonacsigv
        Ventas.PromExtCIGV = Ventas.promextcigv + t-ventas_detalle.promextcigv
        Ventas.PromExtSIGV = Ventas.promextsigv + t-ventas_detalle.promextsigv
        Ventas.PromNacCIGV = Ventas.promnaccigv + t-ventas_detalle.promnaccigv
        Ventas.PromNacSIGV = Ventas.promnacsigv + t-ventas_detalle.promnacsigv
        Ventas.FleteExtCIGV = Ventas.Fleteextcigv + t-ventas_detalle.Fleteextcigv
        Ventas.FleteExtSIGV = Ventas.Fleteextsigv + t-ventas_detalle.Fleteextsigv
        Ventas.FleteNacCIGV = Ventas.Fletenaccigv + t-ventas_detalle.Fletenaccigv
        Ventas.FleteNacSIGV = Ventas.Fletenacsigv + t-ventas_detalle.Fletenacsigv.

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
FOR EACH DimDivision NO-LOCK:
    ESTADISTICAS:
    FOR EACH CcbCdocu USE-INDEX Llave10 NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
            AND CcbCdocu.CodDiv = DimDivision.CodDiv
            AND CcbCdocu.FchDoc >= x-CodFchI
            AND CcbCdocu.FchDoc <= x-CodFchF:
        /* ***************** FILTROS ********************************** */
        IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C") = 0 THEN NEXT.
        IF LOOKUP(CcbCDocu.FlgEst, "A,X") > 0 THEN NEXT.    /* ANULADO y CERRADO */
        IF LOOKUP(CcbCDocu.TpoFac, "B") > 0   THEN NEXT.    /* VIENE DE UNA BAJA DE SUNAT */
        IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
        /* NO facturas adelantadas NI servicios */
        IF LOOKUP(Ccbcdocu.CodDoc, 'N/C,A/C') > 0 THEN DO:
            FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia
                AND B-CDOCU.CodDoc = CcbCdocu.Codref
                AND B-CDOCU.NroDoc = CcbCdocu.Nroref
                NO-LOCK NO-ERROR.
             IF NOT AVAILABLE B-CDOCU THEN NEXT.
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
        /* *********************************************************************** */
        /* RHC 08/07/2015 EN CASO DE TENER UN VENDEDOR NO REGISTRADO EN EL SISTEMA */
        /* *********************************************************************** */
        FIND DimVendedor WHERE DimVendedor.CodVen = Ccbcdocu.codven NO-LOCK NO-ERROR.
        IF NOT AVAILABLE DimVendedor THEN DO:
            CREATE DimVendedor.
            ASSIGN
                DimVendedor.CodVen = Ccbcdocu.codven
                DimVendedor.NomVen = DimDivision.DesDiv.
            RELEASE DimVendedor.
        END.
        /* *********************************************************************** */
        /* ******************************** */
        DEF VAR pRowid AS ROWID NO-UNDO.
        RUN Carga-Ventas-Cabecera (OUTPUT pRowid).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        FIND t-ventas_cabecera WHERE ROWID(t-ventas_cabecera) = pRowid NO-LOCK.
        /* ******************************** */
        ASSIGN
           x-Coe = 1
           x-Can = 1.
        /* OTRAS NOTAS DE CREDITO */
        OTROS:
        DO:
            /* FACTURAS POR ANTICIPOS Y/O SERVICIOS */
            IF LOOKUP(CcbCDocu.CodDoc,"FAC,BOL") > 0 AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN DO:
                RUN PROCESA-OTRAS-VENTAS.   /* CODMAT = '999999' */
                NEXT ESTADISTICAS.
            END.
            /* ADELANTOS DE CAMPAÑA */
            IF CcbCDocu.CodDoc = "A/C" THEN DO:
                RUN PROCESA-OTRAS-VENTAS.   /* CODMAT = '999999' */
                NEXT ESTADISTICAS.
            END.
            /* *********************** */
            /* NOTAS DE CREDITO NUEVAS */
            /* *********************** */
            IF CcbCDocu.CodDoc = "N/C" AND CcbCdocu.CndCre = "N" AND CcbCdocu.TpoFac = "OTROS" THEN DO:
                RUN PROCESA-NC-DIFPRECIO.       /* RHC 04/02/2020 */
                NEXT ESTADISTICAS.
            END.
            /* *********************** */
            /* NOTAS DE CREDITO APLICADAS A FACTURAS DE SERVICIO Y/O ANTICIPOS DE CAMPAÑA */
            IF CcbCDocu.CodDoc = "N/C" AND LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN DO:
                RUN PROCESA-OTRAS-VENTAS.   /* CODMAT = '999999' */
                NEXT ESTADISTICAS.
            END.
            /* NOTAS DE CREDITO POR OTROS CONCEPTOS */
            IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac <> "E" THEN DO:
                RUN PROCESA-NOTA.
                NEXT ESTADISTICAS.
            END.
            IF CcbCdocu.Coddoc = "N/C" AND CcbCdocu.CndCre = "N" AND Ccbcdocu.TpoFac = "E" THEN DO:
                RUN PROCESA-NOTA-REBADE.
                NEXT ESTADISTICAS.
            END.
        END.
        /* CONTINUAMOS CON FACTURAS Y NOTAS DE CREDITO POR DEVOLUCION */
        FOR EACH CcbDdocu OF CcbCdocu NO-LOCK:
           /* ****************** Filtros ************************* */
           FIND FIRST Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
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
END.

IF AVAILABLE Ccbcdocu THEN RELEASE Ccbcdocu.
IF AVAILABLE Ccbddocu THEN RELEASE Ccbddocu.
IF AVAILABLE DimCliente THEN RELEASE DimCliente.
IF AVAILABLE t-ventas_detalle THEN RELEASE t-ventas_detalle.
IF AVAILABLE t-ventas_cabecera THEN RELEASE t-ventas_cabecera.

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
DEF OUTPUT PARAMETER pRowid AS ROWID NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR':
    CREATE t-ventas_cabecera.
    ASSIGN
        t-ventas_cabecera.DateKey = ccbcdocu.fchdoc
        t-ventas_cabecera.CodDiv = pcoddiv
        t-ventas_cabecera.DivDes = pdivdes
        t-ventas_cabecera.CodCli = x-codcli        /* x-codunico */
        t-ventas_cabecera.CodDoc = ccbcdocu.coddoc
        t-ventas_cabecera.NroDoc = ccbcdocu.nrodoc
        t-ventas_cabecera.CodVen = x-codven
        t-ventas_cabecera.FmaPgo = x-fmapgo
        t-ventas_cabecera.TpoCmb = ccbcdocu.tpocmb
        t-ventas_cabecera.TpoCmbVta = x-tpocmbvta
        t-ventas_cabecera.TpoCmbCmp = x-tpocmbcmp
        t-ventas_cabecera.Tipo    = x-Tipo
        t-ventas_cabecera.NroCard = x-nrocard
        t-ventas_cabecera.Delivery = x-Delivery
        t-ventas_cabecera.ListaBase = ""      /*pCodDiv.    /* <<< OJO <<< */*/
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
        /*t-ventas_cabecera.ListaBase = pCodDiv.*/
    /* Buscamos Lista Base SOLO PARA EXPOLIBRERIA Y PROVINCIAS */
    IF Ccbcdocu.coddoc = "N/C" THEN DO:
        IF NOT AVAILABLE B-CDOCU THEN NEXT.
        FIND Faccpedi WHERE Faccpedi.codcia = B-CDOCU.codcia
            AND Faccpedi.coddoc = B-CDOCU.codped    /* PED */
            AND Faccpedi.nroped = B-CDOCU.nroped
            NO-LOCK NO-ERROR.
    END.
    ELSE FIND Faccpedi WHERE Faccpedi.codcia = Ccbcdocu.codcia
        AND Faccpedi.coddoc = Ccbcdocu.codped   /* PED */
        AND Faccpedi.nroped = Ccbcdocu.nroped
        NO-LOCK NO-ERROR.
    IF AVAILABLE Faccpedi THEN DO:
        ASSIGN
            t-ventas_cabecera.Cotizacion = Faccpedi.nroref.   /* COT */
        /* Cotización */
        FIND FIRST B-CPEDI WHERE B-CPEDI.codcia = Faccpedi.codcia
            AND B-CPEDI.coddoc = Faccpedi.codref
            AND B-CPEDI.nroped = Faccpedi.nroref
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-CPEDI 
            THEN ASSIGN
                    t-ventas_cabecera.FechaPedComercial = B-CPEDI.FchPed      /* Fecha del Pedido Comercial */
                    t-ventas_cabecera.ListaBase = B-CPEDI.Libre_C01.
/*         IF AVAILABLE B-CPEDI AND B-CPEDI.Libre_C01 > "" AND LOOKUP(B-CPEDI.TpoPed, "P,E") > 0                */
/*             THEN ASSIGN                                                                                      */
/*                     t-ventas_cabecera.FechaPedComercial = B-CPEDI.FchPed      /* Fecha del Pedido Comercial */ */
/*                     t-ventas_cabecera.ListaBase = B-CPEDI.Libre_C01.                                           */
    END.
    ASSIGN
        pRowid = ROWID(t-ventas_cabecera).
END.
RETURN 'OK'.

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

    DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto INIT 0.
    DEF VAR x-ImpLis LIKE Almmmatg.CtoLis INIT 0.
    DEF VAR x-CanDes AS DECI NO-UNDO.

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
    ASSIGN
    x-ImpLin = Ccbddocu.ImpLin - Ccbddocu.ImpDto2.
    IF Ccbddocu.ImpCto > 0 THEN
        ASSIGN
        x-ImpCto = Ccbddocu.ImpCto
        x-ImpLis = Ccbddocu.ImpCto
        .
    IF CcbDDocu.AftIgv = YES AND CcbCDocu.PorIgv > 0 THEN
        x-ImpLis = x-ImpCto / (1 + CcbCDocu.PorIgv / 100).
    /* ************************************************************************ */
    /* RHC 30/04/2015 COSTO DE REPOSICION ACTUAL */
    /* ************************************************************************ */
/*     FIND DimProducto WHERE DimProducto.CodMat = TRIM(ccbddocu.codmat) NO-LOCK NO-ERROR. */
/*     ASSIGN                                                                              */
/*         x-ImpCto = Almmmatg.CtoTot                                                      */
/*         x-ImpLis = Almmmatg.CtoLis.                                                     */
    /* ************************************************************************ */
    /* RHC 06/04/2021 En caso de ser CONTRATO MARCO */
    /* ************************************************************************ */
/*     FIND PEDIDO WHERE PEDIDO.codcia = Ccbcdocu.codcia                */
/*         AND PEDIDO.coddoc = Ccbcdocu.codped                          */
/*         AND PEDIDO.nroped = Ccbcdocu.nroped                          */
/*         NO-LOCK NO-ERROR.                                            */
/*     IF AVAILABLE PEDIDO THEN DO:                                     */
/*         FIND COTIZACION WHERE COTIZACION.codcia = PEDIDO.codcia      */
/*             AND COTIZACION.coddoc = PEDIDO.codref                    */
/*             AND COTIZACION.nroped = PEDIDO.nroref                    */
/*             NO-LOCK NO-ERROR.                                        */
/*         /* CONTRATO MARCO */                                         */
/*         IF AVAILABLE COTIZACION AND COTIZACION.TpoPed = "M" THEN DO: */
/*             x-ImpCto = Almmmatg.CtoTotMarco.    /* OJO */            */
/*             x-ImpLis = Almmmatg.CtoLisMarco.                         */
/*         END.                                                         */
/*     END.                                                             */
    /* ************************************************************************ */
    /* RHC 06/03/2019 Caso de Productos SIN IGV */
    /* RHC 19/07/2021 veamos si el producto tiene IGV */
    /* ************************************************************************ */
    /*IF Almmmatg.AftIgv = NO THEN x-PorIgv = 0.00.*/
    IF Ccbddocu.AftIgv = NO THEN x-PorIgv = 0.
    ELSE DO:
        IF x-PorIgv <= 0 OR x-PorIgv = ? THEN 
            IF (Ccbddocu.ImpLin - Ccbddocu.ImpIgv) > 0 THEN x-PorIgv = Ccbddocu.ImpIgv / ( Ccbddocu.ImpLin - Ccbddocu.ImpIgv) * 100.
            ELSE x-PorIgv = 0.
    END.
    IF x-ImpCto = ? THEN x-ImpCto = 0.
    IF x-ImpLis = ? THEN x-ImpLis = 0.

    /* OJO */
    x-CanDes = CcbDdocu.CanDes * F-FACTOR * x-can.
    /* *** */

    CREATE t-ventas_detalle.
    ASSIGN
        t-ventas_detalle.DateKey = t-ventas_cabecera.DateKey
        t-ventas_detalle.CodDiv = t-ventas_cabecera.coddiv
        t-ventas_detalle.CodDoc = t-ventas_cabecera.coddoc
        t-ventas_detalle.NroDoc = t-ventas_cabecera.nrodoc
        t-ventas_detalle.CodMat = TRIM(ccbddocu.codmat)
        t-ventas_detalle.Cantidad = ( x-signo1 * x-CanDes )
        t-ventas_detalle.AlmDes = x-AlmDes
        t-ventas_detalle.CodFam = Almmmatg.CodFam
        t-ventas_detalle.SubFam = ALmmmatg.SubFam.
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            t-ventas_detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            t-ventas_detalle.ImpExtSIGV   = t-ventas_detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-ventas_detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
            t-ventas_detalle.ImpNacSIGV   = t-ventas_detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            t-ventas_detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            t-ventas_detalle.ImpExtSIGV   = t-ventas_detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-ventas_detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
            t-ventas_detalle.ImpNacSIGV   = t-ventas_detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    /* 11.09.10 INCORPORAMOS EL COSTO PROMEDIO */
    /* 06/05/2019 A todos excepto a las N/C por Otros conceptos cuyo concepto NO afecte al costo de ventas */
    DEF VAR x-CtoUni AS DEC NO-UNDO.
    x-CtoUni = 0.
    FIND LAST AlmStkGe WHERE Almstkge.codcia = Ccbcdocu.codcia
        AND Almstkge.codmat = Ccbddocu.codmat
        AND Almstkge.fecha <= Ccbcdocu.fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almstkge AND Almstkge.CtoUni <> ? THEN x-CtoUni = AlmStkge.CtoUni.
    IF Ccbcdocu.codcia = 1 and Ccbcdocu.coddoc = 'N/C' and Ccbcdocu.cndcre = "N" THEN DO:
        FIND CcbTabla WHERE CcbTabla.CodCia = s-codcia AND
            CcbTabla.Tabla = "N/C" AND
            CcbTabla.Codigo = Ccbcdocu.codcta
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbTabla AND CcbTabla.Libre_c02 = "NO" THEN x-CtoUni = 0.
    END.
    ASSIGN
        t-ventas_detalle.PromExtSIGV = x-signo1 * x-CanDes * x-CtoUni * x-coe / x-TpoCmbCmp
        t-ventas_detalle.PromExtCIGV = t-ventas_detalle.PromExtSIGV * ( 1 + ( x-PorIgv / 100) )
        t-ventas_detalle.PromNacSIGV = x-signo1 * x-CanDes * x-CtoUni * x-coe
        t-ventas_detalle.PromNacCIGV = t-ventas_detalle.PromNacSIGV * ( 1 + ( x-PorIgv / 100) ).
    /* RHC 21/08/2017 GRABAMOS EL COSTO UNITARIO */
    IF Almmmatg.TpoCmb > 0  THEN DO:
        IF Ccbcdocu.CodMon = 1 THEN DO:
            t-ventas_detalle.CostoNacCIGV = x-signo1 * x-CanDes * x-ImpCto * x-coe.
            t-ventas_detalle.CostoExtCIGV = x-signo1 * x-CanDes * x-ImpCto * x-coe / Almmmatg.TpoCmb.
            t-ventas_detalle.CostoNacSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * x-coe.
            t-ventas_detalle.CostoExtSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * x-coe / Almmmatg.TpoCmb.
        END.
        ELSE DO:
            t-ventas_detalle.CostoExtCIGV = x-signo1 * x-CanDes * x-ImpCto * x-coe.
            t-ventas_detalle.CostoNacCIGV = x-signo1 * x-CanDes * x-ImpCto * Almmmatg.TpoCmb * x-coe.
            t-ventas_detalle.CostoExtSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * x-coe.
            t-ventas_detalle.CostoNacSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * Almmmatg.TpoCmb * x-coe.
        END.
    END.
    IF Almmmatg.AftIgv = NO THEN DO:
        t-ventas_detalle.CostoExtCIGV = t-ventas_detalle.CostoExtSIGV.
        t-ventas_detalle.CostoNacCIGV = t-ventas_detalle.CostoNacSIGV.
    END.
    /* RHC 22/08/2019 Se guarda el Flete */
    DEF VAR x-ImpFlete AS DEC NO-UNDO.
    IF LOOKUP(DimDivision.CanalVenta, 'MIN') = 0 AND LOOKUP(Ccbcdocu.CodDoc, 'FAC,BOL') > 0 THEN DO:
        x-ImpFlete = x-CanDes * Ccbddocu.ImpDcto_Adelanto[4].
        IF Ccbcdocu.CodMon = 1 THEN 
            ASSIGN
                t-ventas_detalle.FleteExtCIGV   = x-signo1 * x-ImpFlete * x-coe / x-TpoCmbCmp
                t-ventas_detalle.FleteExtSIGV   = t-ventas_detalle.FleteExtCIGV / ( 1 + ( x-PorIgv / 100) )
                t-ventas_detalle.FleteNacCIGV   = x-signo1 * x-ImpFlete * x-coe
                t-ventas_detalle.FleteNacSIGV   = t-ventas_detalle.FleteNacCIGV / ( 1 + ( x-PorIgv / 100) ).
        IF Ccbcdocu.CodMon = 2 THEN 
            ASSIGN
                t-ventas_detalle.FleteExtCIGV   = x-signo1 * x-ImpFlete * x-coe 
                t-ventas_detalle.FleteExtSIGV   = t-ventas_detalle.FleteExtCIGV / ( 1 + ( x-PorIgv / 100) )
                t-ventas_detalle.FleteNacCIGV   = x-signo1 * x-ImpFlete * x-coe * x-TpoCmbVta
                t-ventas_detalle.FleteNacSIGV   = t-ventas_detalle.FleteNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    END.
    RELEASE t-ventas_detalle.

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

&IF DEFINED(EXCLUDE-pasa-estadisticas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pasa-estadisticas Procedure 
PROCEDURE pasa-estadisticas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Archivo AS CHAR NO-UNDO.

/* */
DEF BUFFER NCREDITO FOR Ccbcdocu.

FOR EACH t-Ventas_Cabecera:
    IF t-Ventas_Cabecera.CodDoc = "N/C"  THEN DO:
        FIND NCREDITO WHERE NCREDITO.codcia = s-codcia AND
            NCREDITO.coddoc = t-Ventas_Cabecera.CodDoc AND
            NCREDITO.nrodoc = t-Ventas_Cabecera.NroDoc NO-LOCK NO-ERROR.
        IF AVAILABLE NCREDITO THEN DO:
            t-Ventas_Cabecera.CodRef = NCREDITO.CodRef.
            t-Ventas_Cabecera.NroRef = NCREDITO.NroRef.
        END.
    END.
END.
x-Archivo = "/home/u/IN/log/" + "ventas_cabecera20192020" + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Cabecera NO-LOCK:
    EXPORT DELIMITER "~029" t-ventas_cabecera 
        EXCEPT t-Ventas_Cabecera.ImpNacSIGV 
        t-Ventas_Cabecera.ImpNacCIGV 
        t-Ventas_Cabecera.ImpExtSIGV 
        t-Ventas_Cabecera.ImpExtCIGV.
END.
OUTPUT CLOSE.
/* ********************************************************************************* */
/* VENTAS DETALLE */
/* ********************************************************************************* */
FOR EACH t-Ventas_Cabecera NO-LOCK,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK:
    ASSIGN
        t-Ventas_Detalle.CodRef = t-Ventas_Cabecera.CodRef
        t-Ventas_Detalle.NroRef = t-Ventas_Cabecera.NroRef.
END.
x-Archivo = "/home/u/IN/log/" + "ventas_detalle20192020" + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Detalle NO-LOCK:
    EXPORT delimiter "~029" t-Ventas_Detalle.
END.
OUTPUT CLOSE.
/* ******************************************************************************* */

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
CREATE t-ventas_cabecera.
ASSIGN
    t-ventas_cabecera.DateKey = ccbcdocu.fchdoc
    t-ventas_cabecera.CodDiv = "99999"
    t-ventas_cabecera.DivDes = pdivdes
    t-ventas_cabecera.CodCli = x-codcli        /* x-codunico */
    t-ventas_cabecera.CodDoc = "N" + ccbcdocu.coddoc
    t-ventas_cabecera.NroDoc = ccbcdocu.nrodoc
    t-ventas_cabecera.CodVen = x-codven
    t-ventas_cabecera.FmaPgo = x-fmapgo
    t-ventas_cabecera.TpoCmb = ccbcdocu.tpocmb
    t-ventas_cabecera.TpoCmbVta = x-tpocmbvta
    t-ventas_cabecera.TpoCmbCmp = x-tpocmbcmp
    t-ventas_cabecera.Tipo    = x-Tipo
    t-ventas_cabecera.NroCard = x-nrocard.

DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

/* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
ASSIGN
    x-ImpLin = Ccbcdocu.ImpTot2
    x-ImpCto = 0
    x-signo1 = -1.
IF x-ImpCto = ? THEN x-ImpCto = 0.
/* ************************************************************************ */
CREATE t-ventas_detalle.
ASSIGN
    t-ventas_detalle.CodDiv = t-ventas_cabecera.coddiv
    t-ventas_detalle.CodDoc = t-ventas_cabecera.coddoc
    t-ventas_detalle.NroDoc = t-ventas_cabecera.nrodoc
    t-ventas_detalle.CodMat = "999999"
    t-ventas_detalle.Cantidad = 0
    t-ventas_detalle.AlmDes = "".
IF Ccbcdocu.CodMon = 1 THEN 
    ASSIGN
        t-ventas_detalle.CostoExtCIGV = 0
        t-ventas_detalle.CostoExtSIGV = 0
        t-ventas_detalle.CostoNacCIGV = 0
        t-ventas_detalle.CostoNacSIGV = 0
        t-ventas_detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
        t-ventas_detalle.ImpExtSIGV   = t-ventas_detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
        t-ventas_detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
        t-ventas_detalle.ImpNacSIGV   = t-ventas_detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
IF Ccbcdocu.CodMon = 2 THEN 
    ASSIGN
        t-ventas_detalle.CostoExtCIGV = 0
        t-ventas_detalle.CostoExtSIGV = 0
        t-ventas_detalle.CostoNacCIGV = 0
        t-ventas_detalle.CostoNacSIGV = 0
        t-ventas_detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
        t-ventas_detalle.ImpExtSIGV   = t-ventas_detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
        t-ventas_detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
        t-ventas_detalle.ImpNacSIGV   = t-ventas_detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PROCESA-NC-DIFPRECIO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PROCESA-NC-DIFPRECIO Procedure 
PROCEDURE PROCESA-NC-DIFPRECIO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       La aplicación es directa al IMPORTE NO A LA CANTIDAD
------------------------------------------------------------------------------*/

FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
    AND B-CDOCU.CodDoc = CcbCdocu.Codref 
    AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
    NO-LOCK.
FOR EACH CcbDdocu OF CcbCdocu NO-LOCK, FIRST Almmmatg WHERE Almmmatg.Codcia = CcbDdocu.Codcia 
    AND Almmmatg.CodMat = CcbDdocu.CodMat NO-LOCK:
    x-AlmDes = CcbDdocu.AlmDes.
    x-AlmDes = ENTRY(1, B-CDOCU.CodAlm).
    FIND FIRST B-DDOCU WHERE B-DDOCU.codcia = B-CDOCU.codcia
        AND B-DDOCU.coddoc = B-CDOCU.coddoc
        AND B-DDOCU.nrodoc = B-CDOCU.nrodoc
        AND B-DDOCU.codmat = CcbDdocu.codmat 
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-DDOCU AND B-DDOCU.AlmDes <> '' THEN x-AlmDes = B-DDOCU.AlmDes.
    IF x-AlmDes = '' THEN x-AlmDes = ENTRY(1, B-CDOCU.CodAlm).
    F-FACTOR = 1.   /* NO afecta la cantidad */
    x-coe = 1.
    x-can = 0.
    RUN Carga-Ventas-Detalle.    
END.

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
    IF TRUE <> (x-AlmDes > '') THEN x-AlmDes = ENTRY(1, B-CDOCU.CodAlm).
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
    CREATE t-ventas_detalle.
    ASSIGN
        t-ventas_detalle.CodDiv = t-ventas_cabecera.coddiv
        t-ventas_detalle.CodDoc = t-ventas_cabecera.coddoc
        t-ventas_detalle.NroDoc = t-ventas_cabecera.nrodoc
        t-ventas_detalle.DateKey = t-ventas_cabecera.DateKey
        t-ventas_detalle.CodMat = "999999"
        t-ventas_detalle.Cantidad = 0
        t-ventas_detalle.AlmDes = "".
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            t-ventas_detalle.CostoExtCIGV = 0
            t-ventas_detalle.CostoExtSIGV = 0
            t-ventas_detalle.CostoNacCIGV = 0
            t-ventas_detalle.CostoNacSIGV = 0
            t-ventas_detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            t-ventas_detalle.ImpExtSIGV   = t-ventas_detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-ventas_detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
            t-ventas_detalle.ImpNacSIGV   = t-ventas_detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            t-ventas_detalle.CostoExtCIGV = 0
            t-ventas_detalle.CostoExtSIGV = 0
            t-ventas_detalle.CostoNacCIGV = 0
            t-ventas_detalle.CostoNacSIGV = 0
            t-ventas_detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            t-ventas_detalle.ImpExtSIGV   = t-ventas_detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            t-ventas_detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
            t-ventas_detalle.ImpNacSIGV   = t-ventas_detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).

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

