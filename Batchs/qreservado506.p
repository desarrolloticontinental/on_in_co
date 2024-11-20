&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Cargar tabla de Stocks vs Transf y Compras en transito

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR pv-codcia AS INT INIT 000.

DEF VAR s-User-Id AS CHAR INIT "SYSTEM" NO-UNDO.

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
         HEIGHT             = 9.54
         WIDTH              = 60.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR k AS INT NO-UNDO.
DEF VAR x-StockComprometido AS DEC NO-UNDO.

/* Borramos detalle */
PUT UNFORMATTED 'INICIO ' NOW SKIP.

RUN Borra-Detalle.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    PUT UNFORMATTED 'NO SE PUDO ELIMINAR EL ARCHIVO ANTERIOR' SKIP.
    QUIT.
END.

/* Cargamos Stocks Comprometidos */
PUT UNFORMATTED "Carga comprometidos " DATETIME(TODAY, MTIME) SKIP.
RUN Carga-Comprometidos.

PUT UNFORMATTED "FIN " NOW SKIP.

IF CONNECTED("integral") THEN DISCONNECT "integral" NO-ERROR.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Detalle Procedure 
PROCEDURE Borra-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Reservado = 0
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.CodAlm = '506'
    AND estavtas.Almacen_Stocks.Reservado > 0
    EXCLUSIVE-LOCK ON ERROR UNDO, RETURN 'ADM-ERROR':
    estavtas.Almacen_Stocks.Reservado = 0.
END.
IF AVAILABLE(estavtas.Almacen_Stocks) THEN RELEASE estavtas.Almacen_Stocks.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Comprometidos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Comprometidos Procedure 
PROCEDURE Carga-Comprometidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.CodAlm = '506'
    AND estavtas.Almacen_Stocks.StkAct > 0
    EXCLUSIVE-LOCK :
    PUT UNFORMATTED "COMPROMETIDOS: " estavtas.Almacen_Stocks.codmat " " estavtas.Almacen_Stocks.codalm " " NOW SKIP.
    RUN ./gn/stock-comprometido-v2.p (estavtas.Almacen_Stocks.codmat, 
                                      estavtas.Almacen_Stocks.codalm, 
                                      NO,
                                      OUTPUT x-StockComprometido).
    ASSIGN
        estavtas.Almacen_Stocks.Reservado = x-StockComprometido.
END.
IF AVAILABLE estavtas.Almacen_Stocks THEN RELEASE estavtas.Almacen_Stocks.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

