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
DEF VAR x-AlmDes    AS CHAR.

DEF VAR s-clivar    AS CHAR FORMAT 'x(11)'.
DEF VAR s-CliUni    AS CHAR FORMAT 'x(11)' INIT '99999999999' NO-UNDO.

DEFINE VAR pCodDiv AS CHAR NO-UNDO.
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.

DEF BUFFER B-CDOCU FOR CcbCdocu.
DEF BUFFER B-DDOCU FOR CcbDdocu.
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
    
/* bloquear cuando no lo uses */
/*x-CodFchI = 01/01/2011. */
/* ************************** */

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
         HEIGHT             = 13.27
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

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

FOR EACH dwh_ventas WHERE dwh_ventas.CodCia = s-codcia
    AND dwh_ventas.Fecha >= FechaD
    AND dwh_ventas.Fecha <= FechaH:
    DELETE dwh_ventas.
END.
FOR EACH dwh_ventas_cli WHERE dwh_ventas_cli.CodCia = s-codcia
    AND dwh_ventas_cli.Fecha >= FechaD
    AND dwh_ventas_cli.Fecha <= FechaH:
    DELETE dwh_ventas_cli.
END.
FOR EACH dwh_ventas_mat WHERE dwh_ventas_mat.CodCia = s-codcia
    AND dwh_ventas_mat.Fecha >= FechaD
    AND dwh_ventas_mat.Fecha <= FechaH:
    DELETE dwh_ventas_mat.
END.
FOR EACH dwh_ventas_vend WHERE dwh_ventas_vend.CodCia = s-codcia
    AND dwh_ventas_vend.Fecha >= FechaD
    AND dwh_ventas_vend.Fecha <= FechaH:
    DELETE dwh_ventas_vend.
END.
FOR EACH dwh_ventas_climat WHERE dwh_ventas_climat.CodCia = s-codcia
    AND dwh_ventas_climat.Fecha >= FechaD
    AND dwh_ventas_climat.Fecha <= FechaH:
    DELETE dwh_ventas_climat.
END.
FOR EACH dwh_ventas_vendcli WHERE dwh_ventas_vendcli.CodCia = s-codcia
    AND dwh_ventas_vendcli.Fecha >= FechaD
    AND dwh_ventas_vendcli.Fecha <= FechaH:
    DELETE dwh_ventas_vendcli.
END.
FOR EACH dwh_ventas_resmat WHERE dwh_ventas_resmat.CodCia = s-codcia
    AND dwh_ventas_resmat.Fecha >= FechaD
    AND dwh_ventas_resmat.Fecha <= FechaH:
    DELETE dwh_ventas_resmat.
END.
FOR EACH dwh_ventas_despacho WHERE dwh_ventas_despacho.CodCia = s-codcia
    AND dwh_ventas_despacho.Fecha >= FechaD
    AND dwh_ventas_despacho.Fecha <= FechaH:
    DELETE dwh_ventas_despacho.
END.
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

FOR EACH dwh_ventas_cab WHERE dwh_ventas_cab.CodCia = s-codcia
    AND dwh_ventas_cab.Fecha >= FechaD
    AND dwh_ventas_cab.Fecha <= FechaH:
    FOR EACH dwh_ventas_det OF dwh_ventas_cab:
        DELETE dwh_ventas_det.
    END.
    DELETE dwh_ventas_cab.
END.

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

    FIND FIRST dwh_ventas_cli WHERE dwh_ventas_cli.codcia = s-codcia
        AND dwh_ventas_cli.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_cli.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_cli.codcli = dwh_ventas_cab.codcli
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_cli THEN CREATE dwh_ventas_cli.
    ASSIGN
        dwh_Ventas_cli.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_cli.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_cli.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_cli.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas_cli.ImpExtCIGV = dwh_ventas_cli.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_cli.ImpExtSIGV = dwh_ventas_cli.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_cli.ImpNacCIGV = dwh_ventas_cli.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_cli.ImpNacSIGV = dwh_ventas_cli.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_cli.CostoExtCIGV = dwh_ventas_cli.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_cli.CostoExtSIGV = dwh_ventas_cli.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_cli.CostoNacCIGV = dwh_ventas_cli.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_cli.CostoNacSIGV = dwh_ventas_cli.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_cli.PromExtCIGV = dwh_ventas_cli.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_cli.PromExtSIGV = dwh_ventas_cli.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_cli.PromNacCIGV = dwh_ventas_cli.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_cli.PromNacSIGV = dwh_ventas_cli.promnacsigv + dwh_ventas_det.promnacsigv.


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

    FIND FIRST dwh_ventas_climat WHERE dwh_ventas_climat.codcia = s-codcia
        AND dwh_ventas_climat.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_climat.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_climat.codcli = dwh_ventas_cab.codcli
        AND dwh_Ventas_climat.CodFam = Almmmatg.CodFam
        AND dwh_Ventas_climat.SubFam = Almmmatg.SubFam
        AND dwh_Ventas_climat.DesMar = Almmmatg.DesMar
        AND dwh_Ventas_climat.Licencia = Almmmatg.Licencia[1]
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_climat THEN CREATE dwh_ventas_climat.
    ASSIGN
        dwh_Ventas_climat.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_climat.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_climat.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_climat.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas_climat.CodFam = Almmmatg.CodFam
        dwh_Ventas_climat.SubFam = Almmmatg.SubFam
        dwh_Ventas_climat.DesMar = Almmmatg.DesMar
        dwh_Ventas_climat.Licencia = Almmmatg.Licencia[1]
        dwh_Ventas_climat.ImpExtCIGV = dwh_ventas_climat.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_climat.ImpExtSIGV = dwh_ventas_climat.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_climat.ImpNacCIGV = dwh_ventas_climat.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_climat.ImpNacSIGV = dwh_ventas_climat.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_climat.CostoExtCIGV = dwh_ventas_climat.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_climat.CostoExtSIGV = dwh_ventas_climat.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_climat.CostoNacCIGV = dwh_ventas_climat.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_climat.CostoNacSIGV = dwh_ventas_climat.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_climat.PromExtCIGV = dwh_ventas_climat.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_climat.PromExtSIGV = dwh_ventas_climat.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_climat.PromNacCIGV = dwh_ventas_climat.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_climat.PromNacSIGV = dwh_ventas_climat.promnacsigv + dwh_ventas_det.promnacsigv.


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

    FIND FIRST dwh_ventas_despacho WHERE dwh_ventas_despacho.codcia = s-codcia
        AND dwh_ventas_despacho.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_despacho.codmat = dwh_ventas_det.codmat
        AND dwh_ventas_despacho.almdes = dwh_ventas_det.almdes
        AND dwh_ventas_despacho.divori = dwh_ventas_cab.coddiv
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_despacho THEN CREATE dwh_ventas_despacho.
    ASSIGN
        dwh_ventas_despacho.Fecha = dwh_ventas_cab.fecha
        dwh_ventas_despacho.CodCia = dwh_ventas_cab.codcia
        dwh_ventas_despacho.CodMat = dwh_ventas_det.codmat
        dwh_ventas_despacho.AlmDes = dwh_ventas_det.almdes
        dwh_ventas_despacho.DivOri = dwh_ventas_cab.coddiv
        dwh_ventas_despacho.Cantidad = dwh_ventas_despacho.Cantidad + dwh_ventas_det.cantidad
        dwh_ventas_despacho.ImpExtCIGV = dwh_ventas_despacho.impextcigv + dwh_ventas_det.impextcigv
        dwh_ventas_despacho.ImpExtSIGV = dwh_ventas_despacho.impextsigv + dwh_ventas_det.impextsigv
        dwh_ventas_despacho.ImpNacCIGV = dwh_ventas_despacho.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_ventas_despacho.ImpNacSIGV = dwh_ventas_despacho.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_ventas_despacho.CostoExtCIGV = dwh_ventas_despacho.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_ventas_despacho.CostoExtSIGV = dwh_ventas_despacho.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_ventas_despacho.CostoNacCIGV = dwh_ventas_despacho.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_ventas_despacho.CostoNacSIGV = dwh_ventas_despacho.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_ventas_despacho.PromExtCIGV = dwh_ventas_despacho.promextcigv + dwh_ventas_det.promextcigv
        dwh_ventas_despacho.PromExtSIGV = dwh_ventas_despacho.promextsigv + dwh_ventas_det.promextsigv
        dwh_ventas_despacho.PromNacCIGV = dwh_ventas_despacho.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_ventas_despacho.PromNacSIGV = dwh_ventas_despacho.promnacsigv + dwh_ventas_det.promnacsigv.

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

FOR EACH dwh_ventas_cab NO-LOCK WHERE dwh_ventas_cab.CodCia = s-codcia
    AND dwh_ventas_cab.Fecha >= FechaD
    AND dwh_ventas_cab.Fecha <= FechaH,
    EACH dwh_ventas_det OF dwh_ventas_cab NO-LOCK,
    FIRST Almmmatg OF dwh_ventas_det NO-LOCK:
    /* VENTAS PRINCIPAL */
    RUN Carga-ventas.
    /* VENTAS POR CLIENTE */
    RUN Carga-cli.
    /* VENTAS POR PRODUCTO */
    RUN Carga-mat.
    /* VENTAS POR VENDEDOR */
    RUN Carga-vend.
    /* VENTAS POR CLIENTE Y PRODUCTO */
    RUN Carga-climat.
    /* VENTAS POR VENDEDOR Y CLIENTE */
    RUN Carga-vendcli.
    /* VENTAS POR PRODUCTO RESUMIDO */
    RUN Carga-resmat.
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

    FIND FIRST dwh_ventas_mat WHERE dwh_ventas_mat.codcia = s-codcia
        AND dwh_ventas_mat.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_mat.coddiv = dwh_ventas_cab.coddiv
        AND dwh_Ventas_mat.CodMat = dwh_ventas_det.codmat
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_mat THEN CREATE dwh_ventas_mat.
    ASSIGN
        dwh_Ventas_mat.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_mat.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_mat.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_mat.CodMat = dwh_ventas_det.codmat
        dwh_Ventas_mat.Cantidad = dwh_ventas_mat.cantidad + dwh_ventas_det.cantidad
        dwh_Ventas_mat.ImpExtCIGV = dwh_ventas_mat.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_mat.ImpExtSIGV = dwh_ventas_mat.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_mat.ImpNacCIGV = dwh_ventas_mat.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_mat.ImpNacSIGV = dwh_ventas_mat.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_mat.CostoExtCIGV = dwh_ventas_mat.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_mat.CostoExtSIGV = dwh_ventas_mat.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_mat.CostoNacCIGV = dwh_ventas_mat.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_mat.CostoNacSIGV = dwh_ventas_mat.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_mat.PromExtCIGV = dwh_ventas_mat.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_mat.PromExtSIGV = dwh_ventas_mat.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_mat.PromNacCIGV = dwh_ventas_mat.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_mat.PromNacSIGV = dwh_ventas_mat.promnacsigv + dwh_ventas_det.promnacsigv.

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

    FIND FIRST dwh_ventas_resmat WHERE dwh_ventas_resmat.codcia = s-codcia
        AND dwh_ventas_resmat.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_resmat.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_resmat.codfam = almmmatg.codfam
        AND dwh_ventas_resmat.subfam = almmmatg.subfam
        AND dwh_ventas_resmat.desmar = almmmatg.desmar
        AND dwh_ventas_resmat.licencia = almmmatg.licencia[1]
        AND dwh_ventas_resmat.codpro = almmmatg.codpr1
        AND dwh_ventas_resmat.codven = dwh_ventas_cab.codven
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_resmat THEN CREATE dwh_ventas_resmat.
    ASSIGN
        dwh_ventas_resmat.Fecha = dwh_ventas_cab.fecha
        dwh_ventas_resmat.CodCia = dwh_ventas_cab.codcia
        dwh_ventas_resmat.CodDiv = dwh_ventas_cab.coddiv
        dwh_ventas_resmat.codfam = almmmatg.codfam
        dwh_ventas_resmat.subfam = almmmatg.subfam
        dwh_ventas_resmat.desmar = almmmatg.desmar
        dwh_ventas_resmat.licencia = almmmatg.licencia[1]
        dwh_ventas_resmat.codpro = almmmatg.codpr1
        dwh_ventas_resmat.codven = dwh_ventas_cab.codven
        dwh_ventas_resmat.ImpExtCIGV = dwh_ventas_resmat.impextcigv + dwh_ventas_det.impextcigv
        dwh_ventas_resmat.ImpExtSIGV = dwh_ventas_resmat.impextsigv + dwh_ventas_det.impextsigv
        dwh_ventas_resmat.ImpNacCIGV = dwh_ventas_resmat.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_ventas_resmat.ImpNacSIGV = dwh_ventas_resmat.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_ventas_resmat.CostoExtCIGV = dwh_ventas_resmat.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_ventas_resmat.CostoExtSIGV = dwh_ventas_resmat.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_ventas_resmat.CostoNacCIGV = dwh_ventas_resmat.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_ventas_resmat.CostoNacSIGV = dwh_ventas_resmat.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_resmat.PromExtCIGV = dwh_ventas_resmat.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_resmat.PromExtSIGV = dwh_ventas_resmat.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_resmat.PromNacCIGV = dwh_ventas_resmat.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_resmat.PromNacSIGV = dwh_ventas_resmat.promnacsigv + dwh_ventas_det.promnacsigv.


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

    FIND FIRST dwh_ventas_vend WHERE dwh_ventas_vend.codcia = s-codcia
        AND dwh_ventas_vend.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_vend.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_vend.codven = dwh_ventas_cab.codven
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_vend THEN CREATE dwh_ventas_vend.
    ASSIGN
        dwh_Ventas_vend.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_vend.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_vend.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_vend.CodVen = dwh_ventas_cab.codven
        dwh_Ventas_vend.ImpExtCIGV = dwh_ventas_vend.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_vend.ImpExtSIGV = dwh_ventas_vend.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_vend.ImpNacCIGV = dwh_ventas_vend.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_vend.ImpNacSIGV = dwh_ventas_vend.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_vend.CostoExtCIGV = dwh_ventas_vend.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_vend.CostoExtSIGV = dwh_ventas_vend.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_vend.CostoNacCIGV = dwh_ventas_vend.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_vend.CostoNacSIGV = dwh_ventas_vend.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_vend.PromExtCIGV = dwh_ventas_vend.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_vend.PromExtSIGV = dwh_ventas_vend.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_vend.PromNacCIGV = dwh_ventas_vend.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_vend.PromNacSIGV = dwh_ventas_vend.promnacsigv + dwh_ventas_det.promnacsigv.

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

    FIND FIRST dwh_ventas_vendcli WHERE dwh_ventas_vendcli.codcia = s-codcia
        AND dwh_ventas_vendcli.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_vendcli.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_vendcli.codcli = dwh_ventas_cab.codcli
        AND dwh_ventas_vendcli.codven = dwh_ventas_cab.codven
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_vendcli THEN CREATE dwh_ventas_vendcli.
    ASSIGN
        dwh_Ventas_vendcli.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_vendcli.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_vendcli.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_vendcli.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas_vendcli.CodVen = dwh_ventas_cab.codven
        dwh_Ventas_vendcli.ImpExtCIGV = dwh_ventas_vendcli.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_vendcli.ImpExtSIGV = dwh_ventas_vendcli.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_vendcli.ImpNacCIGV = dwh_ventas_vendcli.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_vendcli.ImpNacSIGV = dwh_ventas_vendcli.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_vendcli.CostoExtCIGV = dwh_ventas_vendcli.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_vendcli.CostoExtSIGV = dwh_ventas_vendcli.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_vendcli.CostoNacCIGV = dwh_ventas_vendcli.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_vendcli.CostoNacSIGV = dwh_ventas_vendcli.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_vendcli.PromExtCIGV = dwh_ventas_vendcli.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_vendcli.PromExtSIGV = dwh_ventas_vendcli.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_vendcli.PromNacCIGV = dwh_ventas_vendcli.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_vendcli.PromNacSIGV = dwh_ventas_vendcli.promnacsigv + dwh_ventas_det.promnacsigv.


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
    FIND FIRST dwh_ventas WHERE dwh_ventas.codcia = s-codcia
        AND dwh_ventas.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas.codcli = dwh_ventas_cab.codcli
        AND dwh_ventas.codven = dwh_ventas_cab.codven
        AND dwh_Ventas.CodMat = dwh_ventas_det.codmat
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas THEN CREATE dwh_ventas.
    ASSIGN
        dwh_Ventas.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas.CodVen = dwh_ventas_cab.codven
        dwh_Ventas.CodMat = dwh_ventas_det.codmat
        dwh_Ventas.Cantidad = dwh_ventas.cantidad + dwh_ventas_det.cantidad
        dwh_Ventas.ImpExtCIGV = dwh_ventas.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas.ImpExtSIGV = dwh_ventas.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas.ImpNacCIGV = dwh_ventas.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas.ImpNacSIGV = dwh_ventas.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas.CostoExtCIGV = dwh_ventas.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas.CostoExtSIGV = dwh_ventas.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas.CostoNacCIGV = dwh_ventas.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas.CostoNacSIGV = dwh_ventas.costonacsigv + dwh_ventas_det.costonacsigv.

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
FOR EACH Gn-Divi WHERE Gn-Divi.Codcia = S-CodCia NO-LOCK,
    EACH CcbCdocu USE-INDEX Llave10 NO-LOCK WHERE CcbCdocu.CodCia = S-CODCIA 
        AND CcbCdocu.CodDiv = Gn-Divi.CodDiv
        AND CcbCdocu.FchDoc >= x-CodFchI
        AND CcbCdocu.FchDoc <= x-CodFchF:
    /* ***************** FILTROS ********************************** */
    IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL,N/C,N/D") = 0 THEN NEXT.
    IF CcbCDocu.FlgEst = "A"  THEN NEXT.
    IF DAY(CcbCDocu.FchDoc) = 0 OR DAY(CcbCDocu.FchDoc) = ? THEN NEXT.
    IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL") > 0 
        AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0      /* NO facturas adelantadas NI servicios */
        THEN NEXT.
    IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
        FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
            AND B-CDOCU.CodDoc = CcbCdocu.Codref 
            AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CDOCU THEN NEXT.
        IF LOOKUP(B-CDOCU.TpoFac, 'A,S') > 0 THEN NEXT.     /* NO SERVICIOS NI ADELANTADAS */
        IF Ccbcdocu.CndCre = "N" THEN DO:       /* OTRAS */
            /* NO por APLICACION DE ANTICIPO */
            FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
                FIND FIRST Ccbtabla WHERE Ccbtabla.codcia = s-codcia
                    AND Ccbtabla.tabla = "N/C"
                    AND Ccbtabla.codigo = Ccbddocu.codmat
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Ccbtabla AND CcbTabla.Libre_L01 = NO THEN NEXT ESTADISTICAS.
            END.
        END.
    END.

    ASSIGN
        pCodDiv = IF Ccbcdocu.DivOri <> '' THEN Ccbcdocu.DivOri ELSE Ccbcdocu.CodDiv
        x-CodVen = Ccbcdocu.codven
        x-FmaPgo = Ccbcdocu.fmapgo
        x-NroCard = Ccbcdocu.nrocard
        x-Sede = Ccbcdocu.sede.
    IF Ccbcdocu.CodDoc = 'N/C' THEN DO:
        FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia 
            AND B-CDOCU.CodDoc = CcbCdocu.Codref 
            AND B-CDOCU.NroDoc = CcbCdocu.Nroref 
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-CDOCU THEN DO:
            ASSIGN
                pCodDiv = IF B-CDOCU.DivOri <> '' THEN B-CDOCU.DivOri ELSE B-CDOCU.CodDiv
                x-CodVen = B-CDOCU.codven
                x-FmaPgo = B-CDOCU.fmapgo
                x-NroCard = B-CDOCU.nrocard
                x-Sede = B-CDOCU.sede.
        END.
    END.
    /* Ajuste de la division en los valores historicos */
    IF pCodDiv <> '00017' AND x-codven = '151' THEN pCodDiv = '00017'.   /* Supermercados */
    IF pCodDiv <> '00018' AND LOOKUP(x-codven, '015,173,900,901,902,017') > 0 THEN pCodDiv = '00018'.    /* Provincias */
    IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
    IF pCodDiv <> '00019' AND x-codven = '081' THEN pCodDiv = '00019'.   /* Mesa redonda */
    IF pCodDiv <> '00099' AND x-codven = '998' THEN pCodDiv = '00099'.   /* Exportaciones */
    IF pCodDiv <> '00098' AND x-codven = '157' THEN pCodDiv = '00098'.   /* Refiles */
    /* *********************************************************** */
    FIND B-DIVI WHERE B-DIVI.codcia = s-codcia AND B-DIVI.coddiv = pCodDiv NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-DIVI THEN NEXT.
    pCanalVenta = B-DIVI.CanalVenta.
    /* *********************************************************** */
    ASSIGN
        x-signo1 = ( IF CcbCdocu.Coddoc = "N/C" THEN -1 ELSE 1 )
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
        /*x-codven = Ccbcdocu.CodVen*/
        /*X-FMAPGO = CcbCdocu.FmaPgo*/
        /*x-NroCard = Ccbcdocu.nrocard*/
        /*x-Sede = Ccbcdocu.Sede */
        x-Canal = ''
        x-CodCli = Ccbcdocu.CodCli
        x-CodUnico = Ccbcdocu.codcli
        x-Zona = ''.

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

CREATE dwh_ventas_cab.
ASSIGN
    dwh_ventas_cab.CodCia = s-codcia
    dwh_ventas_cab.CodCli = x-codcli        /* x-codunico */
    dwh_ventas_cab.CodDiv = pcoddiv
    dwh_ventas_cab.CodDoc = ccbcdocu.coddoc
    dwh_ventas_cab.CodVen = x-codven
    dwh_ventas_cab.Fecha  = YEAR(ccbcdocu.fchdoc) * 10000 + MONTH(ccbcdocu.fchdoc) * 100 + DAY(ccbcdocu.fchdoc)
    dwh_ventas_cab.FmaPgo = x-fmapgo
    dwh_ventas_cab.NroDoc = ccbcdocu.nrodoc
    dwh_ventas_cab.TpoCmb = ccbcdocu.tpocmb
    dwh_ventas_cab.TpoCmbVta = x-tpocmbvta
    dwh_ventas_cab.TpoCmbCmp = x-tpocmbcmp.



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

    DEF VAR x-ImpCto LIKE CcbDdocu.ImpCto.

    /* AJUSTE DEL IMPORTE DE VENTA DEBIDO A LAS VENTAS CON DESCUENTOS EN UTILEX */

    ASSIGN
        x-ImpLin = Ccbddocu.ImpLin - Ccbddocu.ImpDto2
        x-ImpCto = Ccbddocu.ImpCto.
    IF x-PorIgv <= 0 THEN x-PorIgv = Ccbddocu.ImpIgv / ( Ccbddocu.ImpLin - Ccbddocu.ImpIgv) * 100.
    IF x-ImpCto = ? THEN x-ImpCto = 0.
    /* ************************************************************************ */
    CREATE dwh_ventas_det.
    ASSIGN
        dwh_ventas_det.CodCia = dwh_ventas_cab.codcia
        dwh_ventas_det.CodDiv = dwh_ventas_cab.coddiv
        dwh_ventas_det.CodDoc = dwh_ventas_cab.coddoc
        dwh_ventas_det.NroDoc = dwh_ventas_cab.nrodoc
        dwh_ventas_det.CodMat = ccbddocu.codmat
        dwh_ventas_det.Cantidad = ( x-signo1 * CcbDdocu.CanDes * F-FACTOR * x-can )
        dwh_ventas_det.AlmDes = x-AlmDes.
    IF Ccbcdocu.CodMon = 1 THEN 
        ASSIGN
            dwh_ventas_det.CostoExtCIGV = x-signo1 * x-ImpCto * x-coe / x-TpoCmbCmp
            dwh_ventas_det.CostoExtSIGV = dwh_ventas_det.CostoExtCIGV / ( 1 + ( x-PorIgv / 100) )
            dwh_ventas_det.CostoNacCIGV = x-signo1 * x-ImpCto * x-coe
            dwh_ventas_det.CostoNacSIGV = dwh_ventas_det.CostoNacSIGV / ( 1 + ( x-PorIgv / 100) )
            dwh_ventas_det.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe / x-TpoCmbCmp
            dwh_ventas_det.ImpExtSIGV   = dwh_ventas_det.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            dwh_ventas_det.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe
            dwh_ventas_det.ImpNacSIGV   = dwh_ventas_det.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    IF Ccbcdocu.CodMon = 2 THEN 
        ASSIGN
            dwh_ventas_det.CostoExtCIGV = x-signo1 * x-ImpCto * x-coe 
            dwh_ventas_det.CostoExtSIGV = dwh_ventas_det.CostoExtCIGV / ( 1 + ( x-PorIgv / 100) )
            dwh_ventas_det.CostoNacCIGV = x-signo1 * x-ImpCto * x-coe * x-TpoCmbVta
            dwh_ventas_det.CostoNacSIGV = dwh_ventas_det.CostoNacSIGV / ( 1 + ( x-PorIgv / 100) )
            dwh_ventas_det.ImpExtCIGV   = x-signo1 * x-ImpLin * x-coe 
            dwh_ventas_det.ImpExtSIGV   = dwh_ventas_det.ImpExtCIGV / ( 1 + ( x-PorIgv / 100) )
            dwh_ventas_det.ImpNacCIGV   = x-signo1 * x-ImpLin * x-coe * x-TpoCmbVta
            dwh_ventas_det.ImpNacSIGV   = dwh_ventas_det.ImpNacCIGV / ( 1 + ( x-PorIgv / 100) ).
    /* 11.09.10 INCORPORAMOS EL COSTO PROMEDIO */
    FIND LAST AlmStkGe WHERE Almstkge.codcia = Ccbcdocu.codcia
        AND Almstkge.codmat = Ccbddocu.codmat
        AND Almstkge.fecha <= Ccbcdocu.fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almstkge AND Almstkge.CtoUni <> ? THEN DO:
        ASSIGN
            dwh_ventas_det.PromExtSIGV = x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe / x-TpoCmbCmp
            dwh_ventas_det.PromExtCIGV = dwh_ventas_det.PromExtSIGV * ( 1 + ( x-PorIgv / 100) )
            dwh_ventas_det.PromNacSIGV = x-signo1 * CcbDdocu.CanDes * F-FACTOR * AlmStkge.CtoUni * x-coe
            dwh_ventas_det.PromNacCIGV = dwh_ventas_det.PromNacSIGV * ( 1 + ( x-PorIgv / 100) ).
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

