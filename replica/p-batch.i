&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 4.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE L-FLG  AS LOGICAL NO-UNDO.
DEFINE VARIABLE X-PROG AS CHAR FORMAT "x(20)" NO-UNDO.
DEFINE BUFFER b-replog FOR integral.replog.
DEFINE VARIABLE pMensaje AS CHAR NO-UNDO.

/* Va a grabar en orden, si hay un problema se corta el proceso */
FOR EACH integral.replog NO-LOCK WHERE integral.replog.{&Campo} = NO USE-INDEX {&Llave}:
    {lib\lock-genericov3.i &Tabla="b-replog" ~
        &Alcance="FIRST" ~
        &Condicion="ROWID(b-replog) = ROWID(integral.replog)" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="pMensaje" ~
        &TipoError="UNDO, LEAVE" ~
        &Intentos="10"}
    DISPLAY "PROCESANDO:" integral.replog.LogDate integral.replog.TableName integral.replog.Event 
        TODAY STRING(TIME,'HH:MM') 
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
    PAUSE 0.
    X-PROG = TRIM(LC(integral.replog.RunProgram)).
    RUN VALUE(X-PROG) (INPUT integral.replog.KeyValue,
                       INPUT integral.replog.Event,
                       INPUT integral.replog.DataRecord,
                       OUTPUT L-FLG).
    IF L-FLG = NO THEN DO:
        DISPLAY "**ERROR Proceso Abortado en en la transaccion" INTEGRAL.replog.TransactionID
            WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
        PAUSE 0.
        LEAVE.   /* Detenemos el proceso */
    END.
    ASSIGN b-replog.{&Campo} = L-FLG.
END. 
IF AVAILABLE(b-replog) THEN RELEASE b-replog.
IF pMensaje > '' THEN PUT UNFORMATTED pMensaje SKIP.
PUT 'FIN: ' NOW SKIP.
QUIT.   /* crontab en linux lo ejecuta cada 5 minutos */

/*
  Tabla: "Almacen"
  Alcance: [{"FIRST" | "LAST" | "CURRENT" }]   Valor opcional
  Condicion: "Almacen.codcia = s-codcia AND Almacen.codalm = s-codalm"
  Bloqueo: { "NO-LOCK" | "SHARED-LOCK" | "EXCLUSIVE-LOCK" }
  Accion: { "RETRY" | "LEAVE" }
  Mensaje: { "YES" | "NO" }
  TipoError: { "[UNDO,] RETURN {'ADM-ERROR' | ERROR | }" | "NEXT" | "LEAVE" }
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


