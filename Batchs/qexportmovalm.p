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

/* Fecha de Cierre */
DEF VAR dFchCie AS DATE NO-UNDO.
DEF VAR i-FchDoc AS DATE NO-UNDO.

dFchCie = TODAY.
dFchCie = dFchCie - DAY(dFchCie) + 1. 
i-FchDoc = dFchCie.        /* OJO */
i-FchDoc = ADD-INTERVAL(dFchCie, -3, "months").

/* Exportamos almcmov */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

DEF STREAM cabecera.
DEF STREAM detalle.
DEF STREAM historico.
DEF STREAM valorizado.

PUT UNFORMATTED 'INICIO: ' NOW SKIP.

OUTPUT STREAM cabecera TO /v/IN/ON_IN_CO/txt/almcmov.txt.
OUTPUT STREAM detalle TO /v/IN/ON_IN_CO/txt/almdmov.txt.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia AND
    almcmov.codalm = almacen.codalm AND
    almcmov.fchdoc >= i-FchDoc AND 
    almcmov.fchdoc < TODAY:
    EXPORT STREAM cabecera  DELIMITER "~011" almcmov.
    FOR EACH almdmov OF almcmov NO-LOCK:
        EXPORT STREAM detalle  DELIMITER "~011" almdmov.
    END.
END.
OUTPUT STREAM cabecera CLOSE.
OUTPUT STREAM detalle CLOSE.

OUTPUT STREAM historico TO /v/IN/ON_IN_CO/txt/almstkal.txt.
FOR EACH almstkal NO-LOCK WHERE almstkal.codcia = s-codcia AND
    almstkal.fecha >= i-FchDoc AND
    almstkal.fecha < TODAY:
    EXPORT STREAM historico  DELIMITER "~011" almstkal.
END.
OUTPUT STREAM historico CLOSE.

OUTPUT STREAM valorizado TO /v/IN/ON_IN_CO/txt/almstkge.txt.
FOR EACH almstkge NO-LOCK WHERE almstkge.codcia = s-codcia AND
    almstkge.fecha >= i-FchDoc AND
    almstkge.fecha < TODAY:
    EXPORT STREAM valorizado  DELIMITER "~011" almstkge.
END.
OUTPUT STREAM valorizado CLOSE.

PUT UNFORMATTED 'FIN: ' NOW SKIP.

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
         HEIGHT             = 4.31
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


