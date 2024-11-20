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

DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
DEF VAR s-User-Id AS CHAR INIT "SYSTEM" NO-UNDO.

ASSIGN
    x-CodFchF = TODAY - 1
    x-CodFchI = DATE(01, 01, YEAR(TODAY)).      /* Por defecto */

/* Un mes atrás */
x-CodFchI = ADD-INTERVAL(TODAY, -1, "month").


DISABLE TRIGGERS FOR LOAD OF Almdrepo.
DISABLE TRIGGERS FOR LOAD OF Almcrepo.

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
         HEIGHT             = 5.5
         WIDTH              = 49.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF STREAM Reporte.
DEF VAR x-Archivo AS CHAR NO-UNDO.

PUT UNFORMATTED 'INICIO: ' NOW SKIP.

/* 14/02/2022: POr ahora NO pasar las estadísticas */
/* **************
DEF TEMP-TABLE t-Ventas_Cabecera LIKE Ventas_Cabecera
    FIELD CodRef AS CHAR FORMAT 'x(8)'
    FIELD NroRef AS CHAR FORMAT 'x(15)'.
DEF TEMP-TABLE t-Ventas_Detalle LIKE Ventas_Detalle
    FIELD CodRef AS CHAR FORMAT 'x(8)'
    FIELD NroRef AS CHAR FORMAT 'x(15)'.
DEF BUFFER NCREDITO FOR Ccbcdocu.
EMPTY TEMP-TABLE t-Ventas_Cabecera.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI AND
    Ventas_Cabecera.DateKey <= x-CodFchF:
    CREATE t-Ventas_Cabecera.
    BUFFER-COPY Ventas_Cabecera TO t-Ventas_Cabecera.
    IF Ventas_Cabecera.CodDoc = "N/C"  THEN DO:
        FIND NCREDITO WHERE NCREDITO.codcia = s-codcia AND
            NCREDITO.coddoc = Ventas_Cabecera.CodDoc AND
            NCREDITO.nrodoc = Ventas_Cabecera.NroDoc NO-LOCK NO-ERROR.
        IF AVAILABLE NCREDITO THEN DO:
            t-Ventas_Cabecera.CodRef = NCREDITO.CodRef.
            t-Ventas_Cabecera.NroRef = NCREDITO.NroRef.
        END.
    END.
END.
x-Archivo = "/home/v/IN/dbs/" + "ventas_cabecera" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
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
EMPTY TEMP-TABLE t-Ventas_Detalle.
FOR EACH t-Ventas_Cabecera NO-LOCK,
    EACH Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK:
    CREATE t-Ventas_Detalle.
    BUFFER-COPY Ventas_Detalle TO t-Ventas_Detalle
        ASSIGN
        t-Ventas_Detalle.CodRef = t-Ventas_Cabecera.CodRef
        t-Ventas_Detalle.NroRef = t-Ventas_Cabecera.NroRef.
END.
x-Archivo = "/home/v/IN/dbs/" + "ventas_detalle" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Detalle NO-LOCK:
    EXPORT delimiter "~029" t-Ventas_Detalle.
END.
OUTPUT CLOSE.

********* */

/* ********************************************************************************* */
/* ALMACEN_STOCKS */
/* ********************************************************************************* */
x-Archivo = "/home/v/IN/dbs/" + "almacen_stocks" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
x-Archivo = "/home/v/IN/dbs/" + "almacen_stocks.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH Almacen_Stocks NO-LOCK:
    EXPORT delimiter "~029" Almacen_Stocks.
END.
OUTPUT CLOSE.
/* ******************************************************************************* */

DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        
comm-line = "/usr/bin/qonvtaexport3".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.
PUT UNFORMATTED 'PROCESO TERMINADO ' NOW SKIP.

/* ******************************************************************************* */

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


