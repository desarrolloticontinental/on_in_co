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
x-CodFchI = DATE(07,01,2022).
x-CodFchF = DATE(12,31,2022).

FIND FIRST Empresas WHERE Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
    IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
    IF NOT Empresas.Campo-CodPro THEN pv-codcia = s-codcia.
END.
FIND FIRST FacCfgGn WHERE FacCFgGn.codcia = s-codcia NO-LOCK NO-ERROR.
s-CliVar = FacCfgGn.CliVar.

DEF TEMP-TABLE t-Ventas_Cabecera NO-UNDO LIKE Ventas_Cabecera.
DEF TEMP-TABLE t-Ventas_Detalle NO-UNDO LIKE Ventas_Detalle.

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
         HEIGHT             = 21.81
         WIDTH              = 67.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


RUN Carga-Ventas-Basicas.

DEF STREAM Reporte.
RUN pasa-estadisticas.

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

&IF DEFINED(EXCLUDE-Carga-cli) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-cli Procedure 
PROCEDURE Carga-cli :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST VentasxCliente WHERE VentasxCliente.DateKey = t-Ventas_Cabecera.DateKey
        AND VentasxCliente.coddiv = t-Ventas_Cabecera.coddiv
        AND VentasxCliente.divdes = t-Ventas_Cabecera.divdes
        AND VentasxCliente.codcli = t-Ventas_Cabecera.codcli
        AND VentasxCliente.tipo   = t-Ventas_Cabecera.tipo
        AND VentasxCliente.delivery = t-Ventas_Cabecera.delivery
        AND VentasxCliente.ListaBase = t-Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxCliente THEN CREATE VentasxCliente.
    ASSIGN
        VentasxCliente.DateKey      = t-Ventas_Cabecera.DateKey
        VentasxCliente.CodDiv       = t-Ventas_Cabecera.coddiv
        VentasxCliente.DivDes       = t-Ventas_Cabecera.divdes
        VentasxCliente.CodCli       = t-Ventas_Cabecera.codcli
        VentasxCliente.Tipo         = t-Ventas_Cabecera.tipo
        VentasxCliente.delivery     = t-Ventas_Cabecera.delivery
        VentasxCliente.ListaBase     = t-Ventas_Cabecera.ListaBase
        VentasxCliente.ImpExtCIGV   = VentasxCliente.impextcigv + t-Ventas_Detalle.impextcigv
        VentasxCliente.ImpExtSIGV   = VentasxCliente.impextsigv + t-Ventas_Detalle.impextsigv
        VentasxCliente.ImpNacCIGV   = VentasxCliente.impnaccigv + t-Ventas_Detalle.impnaccigv
        VentasxCliente.ImpNacSIGV   = VentasxCliente.impnacsigv + t-Ventas_Detalle.impnacsigv
        VentasxCliente.CostoExtCIGV = VentasxCliente.costoextcigv + t-Ventas_Detalle.costoextcigv
        VentasxCliente.CostoExtSIGV = VentasxCliente.costoextsigv + t-Ventas_Detalle.costoextsigv
        VentasxCliente.CostoNacCIGV = VentasxCliente.costonaccigv + t-Ventas_Detalle.costonaccigv
        VentasxCliente.CostoNacSIGV = VentasxCliente.costonacsigv + t-Ventas_Detalle.costonacsigv
        VentasxCliente.PromExtCIGV  = VentasxCliente.promextcigv + t-Ventas_Detalle.promextcigv
        VentasxCliente.PromExtSIGV  = VentasxCliente.promextsigv + t-Ventas_Detalle.promextsigv
        VentasxCliente.PromNacCIGV  = VentasxCliente.promnaccigv + t-Ventas_Detalle.promnaccigv
        VentasxCliente.PromNacSIGV  = VentasxCliente.promnacsigv + t-Ventas_Detalle.promnacsigv.

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

    FIND FIRST VentasxClienteLinea WHERE VentasxClienteLinea.DateKey = t-Ventas_Cabecera.DateKey
        AND VentasxClienteLinea.coddiv = t-Ventas_Cabecera.coddiv
        AND VentasxClienteLinea.divdes = t-Ventas_Cabecera.divdes
        AND VentasxClienteLinea.codcli = t-Ventas_Cabecera.codcli
        AND VentasxClienteLinea.CodFam = DimProducto.CodFam
        AND VentasxClienteLinea.SubFam = DimProducto.SubFam
        AND VentasxClienteLinea.DesMar = DimProducto.DesMar
        AND VentasxClienteLinea.Licencia = DimProducto.Licencia
        AND VentasxClienteLinea.Tipo = t-Ventas_Cabecera.Tipo
        AND VentasxClienteLinea.Delivery = t-Ventas_Cabecera.Delivery
        AND VentasxClienteLinea.ListaBase = t-Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxClienteLinea THEN CREATE VentasxClienteLinea.
    ASSIGN
        VentasxClienteLinea.DateKey = t-Ventas_Cabecera.DateKey
        VentasxClienteLinea.CodDiv = t-Ventas_Cabecera.coddiv
        VentasxClienteLinea.DivDes = t-Ventas_Cabecera.divdes
        VentasxClienteLinea.CodCli = t-Ventas_Cabecera.codcli
        VentasxClienteLinea.CodFam = DimProducto.CodFam
        VentasxClienteLinea.SubFam = DimProducto.SubFam
        VentasxClienteLinea.DesMar = DimProducto.DesMar
        VentasxClienteLinea.Licencia = DimProducto.Licencia
        VentasxClienteLinea.Tipo     = t-Ventas_Cabecera.tipo
        VentasxClienteLinea.Delivery = t-Ventas_Cabecera.Delivery
        VentasxClienteLinea.ListaBase = t-Ventas_Cabecera.ListaBase
        VentasxClienteLinea.ImpExtCIGV = VentasxClienteLinea.impextcigv + t-Ventas_Detalle.impextcigv
        VentasxClienteLinea.ImpExtSIGV = VentasxClienteLinea.impextsigv + t-Ventas_Detalle.impextsigv
        VentasxClienteLinea.ImpNacCIGV = VentasxClienteLinea.impnaccigv + t-Ventas_Detalle.impnaccigv
        VentasxClienteLinea.ImpNacSIGV = VentasxClienteLinea.impnacsigv + t-Ventas_Detalle.impnacsigv
        VentasxClienteLinea.CostoExtCIGV = VentasxClienteLinea.costoextcigv + t-Ventas_Detalle.costoextcigv
        VentasxClienteLinea.CostoExtSIGV = VentasxClienteLinea.costoextsigv + t-Ventas_Detalle.costoextsigv
        VentasxClienteLinea.CostoNacCIGV = VentasxClienteLinea.costonaccigv + t-Ventas_Detalle.costonaccigv
        VentasxClienteLinea.CostoNacSIGV = VentasxClienteLinea.costonacsigv + t-Ventas_Detalle.costonacsigv
        VentasxClienteLinea.PromExtCIGV = VentasxClienteLinea.promextcigv + t-Ventas_Detalle.promextcigv
        VentasxClienteLinea.PromExtSIGV = VentasxClienteLinea.promextsigv + t-Ventas_Detalle.promextsigv
        VentasxClienteLinea.PromNacCIGV = VentasxClienteLinea.promnaccigv + t-Ventas_Detalle.promnaccigv
        VentasxClienteLinea.PromNacSIGV = VentasxClienteLinea.promnacsigv + t-Ventas_Detalle.promnacsigv.

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

/*     FIND FIRST VentasxAlmacen WHERE VentasxAlmacen.DateKey = t-Ventas_Cabecera.DateKey            */
/*         AND VentasxAlmacen.codmat = t-Ventas_Detalle.codmat                                       */
/*         AND VentasxAlmacen.almdes = t-Ventas_Detalle.almdes                                       */
/*         AND VentasxAlmacen.coddiv = t-Ventas_Cabecera.divdes                                      */
/*         NO-ERROR.                                                                               */
/*     IF NOT AVAILABLE VentasxAlmacen THEN CREATE VentasxAlmacen.                                 */
/*     ASSIGN                                                                                      */
/*         VentasxAlmacen.DateKey = t-Ventas_Cabecera.DateKey                                        */
/*         VentasxAlmacen.CodMat = t-Ventas_Detalle.codmat                                           */
/*         VentasxAlmacen.AlmDes = t-Ventas_Detalle.almdes                                           */
/*         VentasxAlmacen.CodDiv = t-Ventas_Cabecera.divdes                                          */
/*         VentasxAlmacen.Cantidad = VentasxAlmacen.Cantidad + t-Ventas_Detalle.cantidad             */
/*         VentasxAlmacen.ImpExtCIGV = VentasxAlmacen.impextcigv + t-Ventas_Detalle.impextcigv       */
/*         VentasxAlmacen.ImpExtSIGV = VentasxAlmacen.impextsigv + t-Ventas_Detalle.impextsigv       */
/*         VentasxAlmacen.ImpNacCIGV = VentasxAlmacen.impnaccigv + t-Ventas_Detalle.impnaccigv       */
/*         VentasxAlmacen.ImpNacSIGV = VentasxAlmacen.impnacsigv + t-Ventas_Detalle.impnacsigv       */
/*         VentasxAlmacen.CostoExtCIGV = VentasxAlmacen.costoextcigv + t-Ventas_Detalle.costoextcigv */
/*         VentasxAlmacen.CostoExtSIGV = VentasxAlmacen.costoextsigv + t-Ventas_Detalle.costoextsigv */
/*         VentasxAlmacen.CostoNacCIGV = VentasxAlmacen.costonaccigv + t-Ventas_Detalle.costonaccigv */
/*         VentasxAlmacen.CostoNacSIGV = VentasxAlmacen.costonacsigv + t-Ventas_Detalle.costonacsigv */
/*         VentasxAlmacen.PromExtCIGV = VentasxAlmacen.promextcigv + t-Ventas_Detalle.promextcigv    */
/*         VentasxAlmacen.PromExtSIGV = VentasxAlmacen.promextsigv + t-Ventas_Detalle.promextsigv    */
/*         VentasxAlmacen.PromNacCIGV = VentasxAlmacen.promnaccigv + t-Ventas_Detalle.promnaccigv    */
/*         VentasxAlmacen.PromNacSIGV = VentasxAlmacen.promnacsigv + t-Ventas_Detalle.promnacsigv.   */

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

FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
    /* VENTAS PRINCIPAL */
    RUN Carga-ventas.
END.
FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
    /* VENTAS POR CLIENTE */
    RUN Carga-cli.
END.
FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
    /* VENTAS POR CLIENTE */
    RUN Carga-mat.
END.
FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
    /* VENTAS POR VENDEDOR */
    RUN Carga-vend.
END.
FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
    /* VENTAS POR CLIENTE Y PRODUCTO */
    RUN Carga-climat.
END.
FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
    /* VENTAS POR VENDEDOR Y CLIENTE */
    RUN Carga-vendcli.
END.
FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
    /* VENTAS POR PRODUCTO RESUMIDO */
    RUN Carga-resmat.

END.
FOR EACH t-Ventas_Cabecera NO-LOCK WHERE t-Ventas_Cabecera.DateKey >= x-CodFchI
    AND t-Ventas_Cabecera.DateKey <= x-CodFchF,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK,
    FIRST DimProducto OF t-Ventas_Detalle NO-LOCK:
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

    FIND FIRST VentasxProducto WHERE VentasxProducto.DateKey = t-Ventas_Cabecera.DateKey
        AND VentasxProducto.coddiv = t-Ventas_Cabecera.coddiv
        AND VentasxProducto.divdes = t-Ventas_Cabecera.divdes
        AND VentasxProducto.CodMat = t-Ventas_Detalle.codmat
        AND VentasxProducto.Tipo = t-Ventas_Cabecera.tipo
        AND VentasxProducto.Delivery = t-Ventas_Cabecera.Delivery
        AND VentasxProducto.ListaBase = t-Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxProducto THEN CREATE VentasxProducto.
    ASSIGN
        VentasxProducto.DateKey = t-Ventas_Cabecera.DateKey
        VentasxProducto.CodDiv = t-Ventas_Cabecera.coddiv
        VentasxProducto.DivDes = t-Ventas_Cabecera.divdes
        VentasxProducto.CodMat = TRIM(t-Ventas_Detalle.codmat)
        VentasxProducto.Tipo = t-Ventas_Cabecera.tipo
        VentasxProducto.Delivery = t-Ventas_Cabecera.Delivery
        VentasxProducto.ListaBase = t-Ventas_Cabecera.ListaBase
        VentasxProducto.Cantidad = VentasxProducto.cantidad + t-Ventas_Detalle.cantidad
        VentasxProducto.ImpExtCIGV = VentasxProducto.impextcigv + t-Ventas_Detalle.impextcigv
        VentasxProducto.ImpExtSIGV = VentasxProducto.impextsigv + t-Ventas_Detalle.impextsigv
        VentasxProducto.ImpNacCIGV = VentasxProducto.impnaccigv + t-Ventas_Detalle.impnaccigv
        VentasxProducto.ImpNacSIGV = VentasxProducto.impnacsigv + t-Ventas_Detalle.impnacsigv
        VentasxProducto.CostoExtCIGV = VentasxProducto.costoextcigv + t-Ventas_Detalle.costoextcigv
        VentasxProducto.CostoExtSIGV = VentasxProducto.costoextsigv + t-Ventas_Detalle.costoextsigv
        VentasxProducto.CostoNacCIGV = VentasxProducto.costonaccigv + t-Ventas_Detalle.costonaccigv
        VentasxProducto.CostoNacSIGV = VentasxProducto.costonacsigv + t-Ventas_Detalle.costonacsigv
        VentasxProducto.PromExtCIGV = VentasxProducto.promextcigv + t-Ventas_Detalle.promextcigv
        VentasxProducto.PromExtSIGV = VentasxProducto.promextsigv + t-Ventas_Detalle.promextsigv
        VentasxProducto.PromNacCIGV = VentasxProducto.promnaccigv + t-Ventas_Detalle.promnaccigv
        VentasxProducto.PromNacSIGV = VentasxProducto.promnacsigv + t-Ventas_Detalle.promnacsigv.

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

    FIND FIRST VentasxLinea WHERE VentasxLinea.DateKey = t-Ventas_Cabecera.DateKey
        AND VentasxLinea.coddiv = t-Ventas_Cabecera.coddiv
        AND VentasxLinea.divdes = t-Ventas_Cabecera.divdes
        AND VentasxLinea.codfam = DimProducto.codfam
        AND VentasxLinea.subfam = DimProducto.subfam
        AND VentasxLinea.desmar = DimProducto.desmar
        AND VentasxLinea.licencia = DimProducto.licencia
        AND VentasxLinea.codpro = DimProducto.codpro[1]
        AND VentasxLinea.codven = t-Ventas_Cabecera.codven
        AND VentasxLinea.tipo = t-Ventas_Cabecera.tipo
        AND VentasxLinea.delivery = t-Ventas_Cabecera.delivery
        AND VentasxLinea.ListaBase = t-Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxLinea THEN CREATE VentasxLinea.
    ASSIGN
        VentasxLinea.DateKey = t-Ventas_Cabecera.DateKey
        VentasxLinea.CodDiv = t-Ventas_Cabecera.coddiv
        VentasxLinea.DivDes = t-Ventas_Cabecera.divdes
        VentasxLinea.codfam = DimProducto.codfam
        VentasxLinea.subfam = DimProducto.subfam
        VentasxLinea.desmar = DimProducto.desmar
        VentasxLinea.licencia = DimProducto.licencia
        VentasxLinea.codpro = DimProducto.codpro[1]
        VentasxLinea.codven = t-Ventas_Cabecera.codven
        VentasxLinea.tipo = t-Ventas_Cabecera.tipo
        VentasxLinea.delivery = t-Ventas_Cabecera.delivery
        VentasxLinea.ListaBase = t-Ventas_Cabecera.ListaBase
        VentasxLinea.ImpExtCIGV = VentasxLinea.impextcigv + t-Ventas_Detalle.impextcigv
        VentasxLinea.ImpExtSIGV = VentasxLinea.impextsigv + t-Ventas_Detalle.impextsigv
        VentasxLinea.ImpNacCIGV = VentasxLinea.impnaccigv + t-Ventas_Detalle.impnaccigv
        VentasxLinea.ImpNacSIGV = VentasxLinea.impnacsigv + t-Ventas_Detalle.impnacsigv
        VentasxLinea.CostoExtCIGV = VentasxLinea.costoextcigv + t-Ventas_Detalle.costoextcigv
        VentasxLinea.CostoExtSIGV = VentasxLinea.costoextsigv + t-Ventas_Detalle.costoextsigv
        VentasxLinea.CostoNacCIGV = VentasxLinea.costonaccigv + t-Ventas_Detalle.costonaccigv
        VentasxLinea.CostoNacSIGV = VentasxLinea.costonacsigv + t-Ventas_Detalle.costonacsigv
        VentasxLinea.PromExtCIGV = VentasxLinea.promextcigv + t-Ventas_Detalle.promextcigv
        VentasxLinea.PromExtSIGV = VentasxLinea.promextsigv + t-Ventas_Detalle.promextsigv
        VentasxLinea.PromNacCIGV = VentasxLinea.promnaccigv + t-Ventas_Detalle.promnaccigv
        VentasxLinea.PromNacSIGV = VentasxLinea.promnacsigv + t-Ventas_Detalle.promnacsigv.

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

    FIND FIRST VentasxVendedor WHERE VentasxVendedor.DateKey = t-Ventas_Cabecera.DateKey
        AND VentasxVendedor.coddiv = t-Ventas_Cabecera.coddiv
        AND VentasxVendedor.divdes = t-Ventas_Cabecera.divdes
        AND VentasxVendedor.codven = t-Ventas_Cabecera.codven
        AND VentasxVendedor.tipo = t-Ventas_Cabecera.tipo
        AND VentasxVendedor.delivery = t-Ventas_Cabecera.delivery
        AND VentasxVendedor.ListaBase = t-Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxVendedor THEN CREATE VentasxVendedor.
    ASSIGN
        VentasxVendedor.DateKey = t-Ventas_Cabecera.DateKey
        VentasxVendedor.CodDiv = t-Ventas_Cabecera.coddiv
        VentasxVendedor.DivDes = t-Ventas_Cabecera.divdes
        VentasxVendedor.CodVen = t-Ventas_Cabecera.codven
        VentasxVendedor.Tipo = t-Ventas_Cabecera.tipo
        VentasxVendedor.delivery = t-Ventas_Cabecera.delivery
        VentasxVendedor.ListaBase = t-Ventas_Cabecera.ListaBase
        VentasxVendedor.ImpExtCIGV = VentasxVendedor.impextcigv + t-Ventas_Detalle.impextcigv
        VentasxVendedor.ImpExtSIGV = VentasxVendedor.impextsigv + t-Ventas_Detalle.impextsigv
        VentasxVendedor.ImpNacCIGV = VentasxVendedor.impnaccigv + t-Ventas_Detalle.impnaccigv
        VentasxVendedor.ImpNacSIGV = VentasxVendedor.impnacsigv + t-Ventas_Detalle.impnacsigv
        VentasxVendedor.CostoExtCIGV = VentasxVendedor.costoextcigv + t-Ventas_Detalle.costoextcigv
        VentasxVendedor.CostoExtSIGV = VentasxVendedor.costoextsigv + t-Ventas_Detalle.costoextsigv
        VentasxVendedor.CostoNacCIGV = VentasxVendedor.costonaccigv + t-Ventas_Detalle.costonaccigv
        VentasxVendedor.CostoNacSIGV = VentasxVendedor.costonacsigv + t-Ventas_Detalle.costonacsigv
        VentasxVendedor.PromExtCIGV = VentasxVendedor.promextcigv + t-Ventas_Detalle.promextcigv
        VentasxVendedor.PromExtSIGV = VentasxVendedor.promextsigv + t-Ventas_Detalle.promextsigv
        VentasxVendedor.PromNacCIGV = VentasxVendedor.promnaccigv + t-Ventas_Detalle.promnaccigv
        VentasxVendedor.PromNacSIGV = VentasxVendedor.promnacsigv + t-Ventas_Detalle.promnacsigv.

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

    FIND FIRST VentasxVendCliente WHERE VentasxVendCliente.DateKey = t-Ventas_Cabecera.DateKey
        AND VentasxVendCliente.coddiv = t-Ventas_Cabecera.coddiv
        AND VentasxVendCliente.divdes = t-Ventas_Cabecera.divdes
        AND VentasxVendCliente.codcli = t-Ventas_Cabecera.codcli
        AND VentasxVendCliente.codven = t-Ventas_Cabecera.codven
        AND VentasxVendCliente.tipo   = t-Ventas_Cabecera.tipo
        AND VentasxVendCliente.delivery = t-Ventas_Cabecera.delivery
        AND VentasxVendCliente.ListaBase = t-Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE VentasxVendCliente THEN CREATE VentasxVendCliente.
    ASSIGN
        VentasxVendCliente.DateKey = t-Ventas_Cabecera.DateKey
        VentasxVendCliente.CodDiv = t-Ventas_Cabecera.coddiv
        VentasxVendCliente.DivDes = t-Ventas_Cabecera.divdes
        VentasxVendCliente.CodCli = t-Ventas_Cabecera.codcli
        VentasxVendCliente.CodVen = t-Ventas_Cabecera.codven
        VentasxVendCliente.Tipo   = t-Ventas_Cabecera.tipo
        VentasxVendCliente.delivery = t-Ventas_Cabecera.delivery
        VentasxVendCliente.ListaBase = t-Ventas_Cabecera.ListaBase
        VentasxVendCliente.ImpExtCIGV = VentasxVendCliente.impextcigv + t-Ventas_Detalle.impextcigv
        VentasxVendCliente.ImpExtSIGV = VentasxVendCliente.impextsigv + t-Ventas_Detalle.impextsigv
        VentasxVendCliente.ImpNacCIGV = VentasxVendCliente.impnaccigv + t-Ventas_Detalle.impnaccigv
        VentasxVendCliente.ImpNacSIGV = VentasxVendCliente.impnacsigv + t-Ventas_Detalle.impnacsigv
        VentasxVendCliente.CostoExtCIGV = VentasxVendCliente.costoextcigv + t-Ventas_Detalle.costoextcigv
        VentasxVendCliente.CostoExtSIGV = VentasxVendCliente.costoextsigv + t-Ventas_Detalle.costoextsigv
        VentasxVendCliente.CostoNacCIGV = VentasxVendCliente.costonaccigv + t-Ventas_Detalle.costonaccigv
        VentasxVendCliente.CostoNacSIGV = VentasxVendCliente.costonacsigv + t-Ventas_Detalle.costonacsigv
        VentasxVendCliente.PromExtCIGV = VentasxVendCliente.promextcigv + t-Ventas_Detalle.promextcigv
        VentasxVendCliente.PromExtSIGV = VentasxVendCliente.promextsigv + t-Ventas_Detalle.promextsigv
        VentasxVendCliente.PromNacCIGV = VentasxVendCliente.promnaccigv + t-Ventas_Detalle.promnaccigv
        VentasxVendCliente.PromNacSIGV = VentasxVendCliente.promnacsigv + t-Ventas_Detalle.promnacsigv.

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
    FIND FIRST Ventas WHERE Ventas.DateKey = t-Ventas_Cabecera.DateKey
        AND Ventas.coddiv = t-Ventas_Cabecera.coddiv
        AND Ventas.divdes = t-Ventas_Cabecera.divdes
        AND Ventas.codcli = t-Ventas_Cabecera.codcli
        AND Ventas.codven = t-Ventas_Cabecera.codven
        AND Ventas.CodMat = t-Ventas_Detalle.codmat
        AND Ventas.Tipo   = t-Ventas_Cabecera.tipo
        AND Ventas.Delivery = t-Ventas_Cabecera.Delivery
        AND Ventas.ListaBase = t-Ventas_Cabecera.ListaBase
        NO-ERROR.
    IF NOT AVAILABLE Ventas THEN CREATE Ventas.
    ASSIGN
        Ventas.DateKey = t-Ventas_Cabecera.DateKey
        Ventas.CodDiv = t-Ventas_Cabecera.coddiv
        Ventas.DivDes = t-Ventas_Cabecera.divdes
        Ventas.CodCli = t-Ventas_Cabecera.codcli
        Ventas.CodVen = t-Ventas_Cabecera.codven
        Ventas.CodMat = t-Ventas_Detalle.codmat
        Ventas.Tipo   = t-Ventas_Cabecera.tipo
        Ventas.Delivery = t-Ventas_Cabecera.Delivery
        Ventas.ListaBase = t-Ventas_Cabecera.ListaBase
        Ventas.Cantidad = Ventas.cantidad + t-Ventas_Detalle.cantidad
        Ventas.ImpExtCIGV = Ventas.impextcigv + t-Ventas_Detalle.impextcigv
        Ventas.ImpExtSIGV = Ventas.impextsigv + t-Ventas_Detalle.impextsigv
        Ventas.ImpNacCIGV = Ventas.impnaccigv + t-Ventas_Detalle.impnaccigv
        Ventas.ImpNacSIGV = Ventas.impnacsigv + t-Ventas_Detalle.impnacsigv
        Ventas.CostoExtCIGV = Ventas.costoextcigv + t-Ventas_Detalle.costoextcigv
        Ventas.CostoExtSIGV = Ventas.costoextsigv + t-Ventas_Detalle.costoextsigv
        Ventas.CostoNacCIGV = Ventas.costonaccigv + t-Ventas_Detalle.costonaccigv
        Ventas.CostoNacSIGV = Ventas.costonacsigv + t-Ventas_Detalle.costonacsigv
        Ventas.PromExtCIGV = Ventas.promextcigv + t-Ventas_Detalle.promextcigv
        Ventas.PromExtSIGV = Ventas.promextsigv + t-Ventas_Detalle.promextsigv
        Ventas.PromNacCIGV = Ventas.promnaccigv + t-Ventas_Detalle.promnaccigv
        Ventas.PromNacSIGV = Ventas.promnacsigv + t-Ventas_Detalle.promnacsigv
        Ventas.FleteExtCIGV = Ventas.Fleteextcigv + t-Ventas_Detalle.Fleteextcigv
        Ventas.FleteExtSIGV = Ventas.Fleteextsigv + t-Ventas_Detalle.Fleteextsigv
        Ventas.FleteNacCIGV = Ventas.Fletenaccigv + t-Ventas_Detalle.Fletenaccigv
        Ventas.FleteNacSIGV = Ventas.Fletenacsigv + t-Ventas_Detalle.Fletenacsigv.

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
FOR EACH DimDivision NO-LOCK,
    EACH CcbCdocu USE-INDEX Llave10 NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
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
            x-ImpTot = Ccbcdocu.TotalVenta.     /* <<< OJO <<< */
            /*x-ImpTot = Ccbcdocu.ImpTot.     /* <<< OJO <<< */*/
    
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
        FIND t-Ventas_Cabecera WHERE ROWID(t-Ventas_Cabecera) = pRowid NO-LOCK.
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
        IF NOT CAN-FIND(FIRST Ccbddocu WHERE Ccbddocu.codcia = Ccbcdocu.codcia
                        AND Ccbddocu.coddiv = Ccbcdocu.coddiv
                        AND Ccbddocu.coddoc = Ccbcdocu.coddoc
                        AND Ccbddocu.nrodoc = Ccbcdocu.nrodoc NO-LOCK)
            THEN PUT UNFORMATTED 'NO TIENE DETALLE: ' Ccbcdocu.coddiv Ccbcdocu.coddoc Ccbcdocu.nrodoc SKIP.
        /* FACTURAS Y BOLETAS QUE TENGAN UNA APLICACION DE ANTICIPOS */
        IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0
            AND Ccbcdocu.ImpTot2 > 0 THEN DO:
            RUN PROCESA-ANTCIPOS-APLICADOS.
        END.
END.

IF AVAILABLE Ccbcdocu THEN RELEASE Ccbcdocu.
IF AVAILABLE Ccbddocu THEN RELEASE Ccbddocu.
IF AVAILABLE DimCliente THEN RELEASE DimCliente.
IF AVAILABLE t-Ventas_Detalle THEN RELEASE t-Ventas_Detalle.
IF AVAILABLE t-Ventas_Cabecera THEN RELEASE t-Ventas_Cabecera.

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
    CREATE t-Ventas_Cabecera.
    ASSIGN
        t-Ventas_Cabecera.DateKey = ccbcdocu.fchdoc
        t-Ventas_Cabecera.CodDiv = pcoddiv
        t-Ventas_Cabecera.DivDes = pdivdes
        t-Ventas_Cabecera.CodCli = x-codcli        /* x-codunico */
        t-Ventas_Cabecera.CodDoc = ccbcdocu.coddoc
        t-Ventas_Cabecera.NroDoc = ccbcdocu.nrodoc
        t-Ventas_Cabecera.CodVen = x-codven
        t-Ventas_Cabecera.FmaPgo = x-fmapgo
        t-Ventas_Cabecera.TpoCmb = ccbcdocu.tpocmb
        t-Ventas_Cabecera.TpoCmbVta = x-tpocmbvta
        t-Ventas_Cabecera.TpoCmbCmp = x-tpocmbcmp
        t-Ventas_Cabecera.Tipo    = x-Tipo
        t-Ventas_Cabecera.NroCard = x-nrocard
        t-Ventas_Cabecera.Delivery = x-Delivery
        t-Ventas_Cabecera.ListaBase = ""      /*pCodDiv.    /* <<< OJO <<< */*/
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
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
            t-Ventas_Cabecera.Cotizacion = Faccpedi.nroref.   /* COT */
        CASE Faccpedi.CodDoc:
            WHEN "PED" THEN DO:
                /* Cotización */
                FIND FIRST B-CPEDI WHERE B-CPEDI.codcia = Faccpedi.codcia
                    AND B-CPEDI.coddoc = Faccpedi.codref
                    AND B-CPEDI.nroped = Faccpedi.nroref
                    NO-LOCK NO-ERROR.
                IF AVAILABLE B-CPEDI 
                    THEN ASSIGN
                            t-Ventas_Cabecera.FechaPedComercial = B-CPEDI.FchPed      /* Fecha del Pedido Comercial */
                            t-Ventas_Cabecera.ListaBase = B-CPEDI.Libre_C01.
            END.
            OTHERWISE DO:
                ASSIGN t-Ventas_Cabecera.ListaBase = pCodDiv.
            END.
        END CASE.
    END.
/*     IF AVAILABLE Faccpedi THEN DO:                                                                           */
/*         ASSIGN                                                                                               */
/*             t-Ventas_Cabecera.Cotizacion = Faccpedi.nroref.   /* COT */                                        */
/*         /* Cotización */                                                                                     */
/*         FIND FIRST B-CPEDI WHERE B-CPEDI.codcia = Faccpedi.codcia                                            */
/*             AND B-CPEDI.coddoc = Faccpedi.codref                                                             */
/*             AND B-CPEDI.nroped = Faccpedi.nroref                                                             */
/*             NO-LOCK NO-ERROR.                                                                                */
/*         IF AVAILABLE B-CPEDI                                                                                 */
/*             THEN ASSIGN                                                                                      */
/*                     t-Ventas_Cabecera.FechaPedComercial = B-CPEDI.FchPed      /* Fecha del Pedido Comercial */ */
/*                     t-Ventas_Cabecera.ListaBase = B-CPEDI.Libre_C01.                                           */
/*     END.                                                                                                     */
    ASSIGN
        pRowid = ROWID(t-Ventas_Cabecera).
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
    DEF VAR x-ImpLinSIGV AS DECI NO-UNDO.

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
    ASSIGN
        /*x-ImpLin = Ccbddocu.ImpLin - Ccbddocu.ImpDto2.*/
        x-ImpLin = CcbDDocu.cImporteTotalConImpuesto
        x-ImpLinSIGV = CcbDDocu.ImporteTotalSinImpuesto
        .

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
    FIND DimProducto WHERE DimProducto.CodMat = TRIM(ccbddocu.codmat) NO-LOCK NO-ERROR.
    ASSIGN 
        x-ImpCto = Almmmatg.CtoTot
        x-ImpLis = Almmmatg.CtoLis.
    /* ************************************************************************ */
    /* RHC 06/04/2021 En caso de ser CONTRATO MARCO */
    /* ************************************************************************ */
    FIND PEDIDO WHERE PEDIDO.codcia = Ccbcdocu.codcia
        AND PEDIDO.coddoc = Ccbcdocu.codped
        AND PEDIDO.nroped = Ccbcdocu.nroped
        NO-LOCK NO-ERROR.
    IF AVAILABLE PEDIDO THEN DO:
        FIND COTIZACION WHERE COTIZACION.codcia = PEDIDO.codcia
            AND COTIZACION.coddoc = PEDIDO.codref
            AND COTIZACION.nroped = PEDIDO.nroref
            NO-LOCK NO-ERROR.
        /* CONTRATO MARCO */
        IF AVAILABLE COTIZACION AND COTIZACION.TpoPed = "M" THEN DO:
            x-ImpCto = Almmmatg.CtoTotMarco.    /* OJO */
            x-ImpLis = Almmmatg.CtoLisMarco.
        END.
    END.
    /* ************************************************************************ */
    /* RHC 06/03/2019 Caso de Productos SIN IGV */
    /* RHC 19/07/2021 veamos si el producto tiene IGV */
    /* ************************************************************************ */
/*     IF Ccbddocu.AftIgv = NO THEN x-PorIgv = 0.                                                                                       */
/*     ELSE DO:                                                                                                                         */
/*         IF x-PorIgv <= 0 OR x-PorIgv = ? THEN                                                                                        */
/*             IF (Ccbddocu.ImpLin - Ccbddocu.ImpIgv) > 0 THEN x-PorIgv = Ccbddocu.ImpIgv / ( Ccbddocu.ImpLin - Ccbddocu.ImpIgv) * 100. */
/*             ELSE x-PorIgv = 0.                                                                                                       */
/*     END.                                                                                                                             */
    x-PorIgv = Ccbddocu.TasaIgv * 100.
    IF x-ImpCto = ? THEN x-ImpCto = 0.
    IF x-ImpLis = ? THEN x-ImpLis = 0.

    /* OJO */
    x-CanDes = CcbDdocu.CanDes * F-FACTOR * x-can.
    /* *** */

    CREATE t-Ventas_Detalle.
    ASSIGN
        t-Ventas_Detalle.DateKey = t-Ventas_Cabecera.DateKey
        t-Ventas_Detalle.CodDiv = t-Ventas_Cabecera.coddiv
        t-Ventas_Detalle.CodDoc = t-Ventas_Cabecera.coddoc
        t-Ventas_Detalle.NroDoc = t-Ventas_Cabecera.nrodoc
        t-Ventas_Detalle.CodMat = TRIM(ccbddocu.codmat)
        t-Ventas_Detalle.Cantidad = ( x-signo1 * x-CanDes )
        t-Ventas_Detalle.AlmDes = x-AlmDes
        t-Ventas_Detalle.CodFam = Almmmatg.CodFam
        t-Ventas_Detalle.SubFam = ALmmmatg.SubFam.
    IF Ccbcdocu.CodMon = 1 THEN DO:
        ASSIGN
            t-Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            t-Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe.
        ASSIGN
            t-Ventas_Detalle.ImpExtSIGV   = x-signo1 * x-ImpLinSIGV * x-coe / x-TpoCmbCmp
            t-Ventas_Detalle.ImpNacSIGV   = x-signo1 * x-ImpLinSIGV * x-coe.
/*             t-Ventas_Detalle.ImpExtSIGV   = t-Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )  */
/*             t-Ventas_Detalle.ImpNacSIGV   = t-Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ). */
    END.
    IF Ccbcdocu.CodMon = 2 THEN DO:
        ASSIGN
            t-Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            t-Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta.
        ASSIGN
            t-Ventas_Detalle.ImpExtSIGV   = x-signo1 * x-ImpLinSIGV * x-coe
            t-Ventas_Detalle.ImpNacSIGV   = x-signo1 * x-ImpLinSIGV * x-coe * x-TpoCmbVta.
/*             t-Ventas_Detalle.ImpExtSIGV   = t-Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )  */
/*             t-Ventas_Detalle.ImpNacSIGV   = t-Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ). */
    END.
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
        t-Ventas_Detalle.PromExtSIGV = x-signo1 * x-CanDes * x-CtoUni * x-coe / x-TpoCmbCmp
        t-Ventas_Detalle.PromExtCIGV = t-Ventas_Detalle.PromExtSIGV * ( 1 + ( x-PorIgv / 100) )
        t-Ventas_Detalle.PromNacSIGV = x-signo1 * x-CanDes * x-CtoUni * x-coe
        t-Ventas_Detalle.PromNacCIGV = t-Ventas_Detalle.PromNacSIGV * ( 1 + ( x-PorIgv / 100) ).
    /* RHC 21/08/2017 GRABAMOS EL COSTO UNITARIO */
    IF Almmmatg.TpoCmb > 0  THEN DO:
        IF Almmmatg.MonVta = 1 THEN DO:
            t-Ventas_Detalle.CostoNacCIGV = x-signo1 * x-CanDes * x-ImpCto * x-coe.
            t-Ventas_Detalle.CostoExtCIGV = x-signo1 * x-CanDes * x-ImpCto * x-coe / Almmmatg.TpoCmb.
            t-Ventas_Detalle.CostoNacSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * x-coe.
            t-Ventas_Detalle.CostoExtSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * x-coe / Almmmatg.TpoCmb.
        END.
        ELSE DO:
            t-Ventas_Detalle.CostoExtCIGV = x-signo1 * x-CanDes * x-ImpCto * x-coe.
            t-Ventas_Detalle.CostoNacCIGV = x-signo1 * x-CanDes * x-ImpCto * Almmmatg.TpoCmb * x-coe.
            t-Ventas_Detalle.CostoExtSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * x-coe.
            t-Ventas_Detalle.CostoNacSIGV = x-signo1 * x-CanDes * x-ImpCto / ( 1 + ( x-PorIgv / 100) ) * Almmmatg.TpoCmb * x-coe.
        END.
    END.
    IF Almmmatg.AftIgv = NO THEN DO:
        t-Ventas_Detalle.CostoExtCIGV = t-Ventas_Detalle.CostoExtSIGV.
        t-Ventas_Detalle.CostoNacCIGV = t-Ventas_Detalle.CostoNacSIGV.
    END.
    /* RHC 22/08/2019 Se guarda el Flete */
    DEF VAR x-ImpFlete AS DEC NO-UNDO.
    IF LOOKUP(DimDivision.CanalVenta, 'MIN') = 0 AND LOOKUP(Ccbcdocu.CodDoc, 'FAC,BOL') > 0 THEN DO:
        x-ImpFlete = x-CanDes * Ccbddocu.ImpDcto_Adelanto[4].
        IF Ccbcdocu.CodMon = 1 THEN 
            ASSIGN
                t-Ventas_Detalle.FleteExtCIGV   = x-signo1 * x-ImpFlete * x-coe / x-TpoCmbCmp
                t-Ventas_Detalle.FleteExtSIGV   = t-Ventas_Detalle.FleteExtCIGV / ( 1 + ( x-PorIgv / 100) )
                t-Ventas_Detalle.FleteNacCIGV   = x-signo1 * x-ImpFlete * x-coe
                t-Ventas_Detalle.FleteNacSIGV   = t-Ventas_Detalle.FleteNacCIGV / ( 1 + ( x-PorIgv / 100) ).
        IF Ccbcdocu.CodMon = 2 THEN 
            ASSIGN
                t-Ventas_Detalle.FleteExtCIGV   = x-signo1 * x-ImpFlete * x-coe 
                t-Ventas_Detalle.FleteExtSIGV   = t-Ventas_Detalle.FleteExtCIGV / ( 1 + ( x-PorIgv / 100) )
                t-Ventas_Detalle.FleteNacCIGV   = x-signo1 * x-ImpFlete * x-coe * x-TpoCmbVta
                t-Ventas_Detalle.FleteNacSIGV   = t-Ventas_Detalle.FleteNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    END.
    RELEASE t-Ventas_Detalle.

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

x-Archivo = "d:\" + "Ventas_Cabecera" + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Cabecera NO-LOCK:
    EXPORT DELIMITER ";" t-Ventas_Cabecera.
END.
OUTPUT CLOSE.
/* ********************************************************************************* */
/* VENTAS DETALLE */
/* ********************************************************************************* */
x-Archivo = "d:\" + "Ventas_Detalle" + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Detalle NO-LOCK:
    EXPORT delimiter ";" t-Ventas_Detalle.
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
CREATE t-Ventas_Cabecera.
ASSIGN
    t-Ventas_Cabecera.DateKey = ccbcdocu.fchdoc
    t-Ventas_Cabecera.CodDiv = "99999"
    t-Ventas_Cabecera.DivDes = pdivdes
    t-Ventas_Cabecera.CodCli = x-codcli        /* x-codunico */
    t-Ventas_Cabecera.CodDoc = "N" + ccbcdocu.coddoc
    t-Ventas_Cabecera.NroDoc = ccbcdocu.nrodoc
    t-Ventas_Cabecera.CodVen = x-codven
    t-Ventas_Cabecera.FmaPgo = x-fmapgo
    t-Ventas_Cabecera.TpoCmb = ccbcdocu.tpocmb
    t-Ventas_Cabecera.TpoCmbVta = x-tpocmbvta
    t-Ventas_Cabecera.TpoCmbCmp = x-tpocmbcmp
    t-Ventas_Cabecera.Tipo    = x-Tipo
    t-Ventas_Cabecera.NroCard = x-nrocard.

DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

/* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
ASSIGN
    x-ImpLin = Ccbcdocu.ImpTot2
    x-ImpCto = 0
    x-signo1 = -1.
IF x-ImpCto = ? THEN x-ImpCto = 0.
/* ************************************************************************ */
CREATE t-Ventas_Detalle.
ASSIGN
    t-Ventas_Detalle.CodDiv = t-Ventas_Cabecera.coddiv
    t-Ventas_Detalle.CodDoc = t-Ventas_Cabecera.coddoc
    t-Ventas_Detalle.NroDoc = t-Ventas_Cabecera.nrodoc
    t-Ventas_Detalle.CodMat = "999999"
    t-Ventas_Detalle.Cantidad = 0
    t-Ventas_Detalle.AlmDes = "".
IF Ccbcdocu.CodMon = 1 THEN 
    ASSIGN
        t-Ventas_Detalle.CostoExtCIGV = 0
        t-Ventas_Detalle.CostoExtSIGV = 0
        t-Ventas_Detalle.CostoNacCIGV = 0
        t-Ventas_Detalle.CostoNacSIGV = 0
        t-Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
        t-Ventas_Detalle.ImpExtSIGV   = t-Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
        t-Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
        t-Ventas_Detalle.ImpNacSIGV   = t-Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
IF Ccbcdocu.CodMon = 2 THEN 
    ASSIGN
        t-Ventas_Detalle.CostoExtCIGV = 0
        t-Ventas_Detalle.CostoExtSIGV = 0
        t-Ventas_Detalle.CostoNacCIGV = 0
        t-Ventas_Detalle.CostoNacSIGV = 0
        t-Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
        t-Ventas_Detalle.ImpExtSIGV   = t-Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
        t-Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
        t-Ventas_Detalle.ImpNacSIGV   = t-Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).


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
    x-ImpTot = B-CDOCU.TotalVenta.      /* <<< OJO <<< */
    /*x-ImpTot = B-CDOCU.ImpTot.      /* <<< OJO <<< */*/
/* RHC 25/04/2014 cambio en la lógica */
x-ImpTot = 0.
FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:
    x-ImpTot = x-ImpTot + CcbDDocu.cImporteTotalConImpuesto.
    /*x-ImpTot = x-ImpTot + Ccbddocu.ImpLin.*/
END.

/* buscamos si hay una aplicación de fact adelantada */
FIND FIRST Ccbddocu OF B-CDOCU WHERE Ccbddocu.implin < 0 NO-LOCK NO-ERROR.
IF AVAILABLE Ccbddocu THEN x-ImpTot = x-ImpTot + ABSOLUTE(Ccbddocu.ImpLin).
/* ************************************************* */
x-Coe = Ccbcdocu.TotalVenta / x-ImpTot.     /* OJO */
/*x-Coe = Ccbcdocu.ImpTot / x-ImpTot.     /* OJO */*/
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
        x-ImpTot = x-ImpTot + CcbDDocu.cImporteTotalConImpuesto.
        /*x-ImpTot = x-ImpTot + Ccbddocu.ImpLin.*/
    END.  
    IF x-ImpTot <= 0 THEN RETURN.
    /* ************************************************* */
    x-Coe = Ccbcdocu.TotalVenta / x-ImpTot.     /* OJO */
    /*x-Coe = Ccbcdocu.ImpTot / x-ImpTot.     /* OJO */*/
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
    DEF VAR x-ImpLinSIGV AS DECI NO-UNDO.

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */
    ASSIGN
        x-ImpLin = Ccbcdocu.TotalVenta
        x-ImpLinSIGV = Ccbcdocu.TotalValorVenta
        /*x-ImpLin = Ccbcdocu.ImpTot*/
        x-ImpCto = 0.
    IF x-PorIgv <= 0 THEN x-PorIgv = ROUND(Ccbcdocu.TotalIGV / ( Ccbcdocu.TotalVenta - Ccbcdocu.TotalIGV) * 100, 2).
    /*IF x-PorIgv <= 0 THEN x-PorIgv = Ccbcdocu.ImpIgv / ( Ccbcdocu.ImpTot - Ccbcdocu.ImpIgv) * 100.*/
    IF x-ImpCto = ? THEN x-ImpCto = 0.
    /* ************************************************************************ */
    CREATE t-Ventas_Detalle.
    ASSIGN
        t-Ventas_Detalle.CodDiv = t-Ventas_Cabecera.coddiv
        t-Ventas_Detalle.CodDoc = t-Ventas_Cabecera.coddoc
        t-Ventas_Detalle.NroDoc = t-Ventas_Cabecera.nrodoc
        t-Ventas_Detalle.DateKey = t-Ventas_Cabecera.DateKey
        t-Ventas_Detalle.CodMat = "999999"
        t-Ventas_Detalle.Cantidad = 0
        t-Ventas_Detalle.AlmDes = "".
    IF Ccbcdocu.CodMon = 1 THEN DO:
        ASSIGN
            t-Ventas_Detalle.CostoExtCIGV = 0
            t-Ventas_Detalle.CostoExtSIGV = 0
            t-Ventas_Detalle.CostoNacCIGV = 0
            t-Ventas_Detalle.CostoNacSIGV = 0.
        ASSIGN
            t-Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            t-Ventas_Detalle.ImpExtSIGV   = x-signo1 * x-ImpLinSIGV * x-coe / x-TpoCmbCmp
            /*t-Ventas_Detalle.ImpExtSIGV   = t-Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )*/
            t-Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
            t-Ventas_Detalle.ImpNacSIGV   = x-signo1 * x-ImpLinSIGV * x-coe.
            /*t-Ventas_Detalle.ImpNacSIGV   = t-Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).*/
    END.
    IF Ccbcdocu.CodMon = 2 THEN DO:
        ASSIGN
            t-Ventas_Detalle.CostoExtCIGV = 0
            t-Ventas_Detalle.CostoExtSIGV = 0
            t-Ventas_Detalle.CostoNacCIGV = 0
            t-Ventas_Detalle.CostoNacSIGV = 0.
        ASSIGN
            t-Ventas_Detalle.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            t-Ventas_Detalle.ImpExtSIGV   = x-signo1 * x-ImpLinSIGV * x-coe 
            /*t-Ventas_Detalle.ImpExtSIGV   = t-Ventas_Detalle.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )*/
            t-Ventas_Detalle.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
            t-Ventas_Detalle.ImpNacSIGV   = x-signo1 * x-ImpLinSIGV * x-coe * x-TpoCmbVta.
            /*t-Ventas_Detalle.ImpNacSIGV   = t-Ventas_Detalle.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).*/
    END.

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

