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
         HEIGHT             = 5.54
         WIDTH              = 68.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF NEW SHARED VAR s-codcia AS INTE INIT 001.

DEF TEMP-TABLE Resumen
    FIELD codalm AS CHAR FORMAT 'x(8)' LABEL 'Almacen'
    FIELD codmat AS CHAR FORMAT 'x(10)' LABEL 'Articulo'
    FIELD stkact AS DECI FORMAT '->>>,>>>,>>9.9999' LABEL 'Stock'
    FIELD stockmax AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD vctmn1 AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD vctmn2 AS DECI FORMAT '->>>,>>>,>>9.9999' 
    INDEX Idx00 AS PRIMARY codalm codmat
    .

/* 17/10/23: Todo */
PUT 'INICIO: ' NOW SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almmmate OF almacen NO-LOCK:
    CREATE Resumen.
    ASSIGN
        Resumen.codalm = Almmmate.codalm
        Resumen.codmat = Almmmate.codmat
        Resumen.stkact = Almmmate.stkact
        Resumen.stockmax = Almmmate.StockMax
        Resumen.vctmn1 = Almmmate.vctmn1
        Resumen.vctmn2 = Almmmate.vctmn2
        .
END.

/* ********************************************************************************* */
/* Pasamos el archivo texto */
/* ********************************************************************************* */
PUT 'TEXTO: ' NOW SKIP.
DEF VAR x-Archivo AS CHAR NO-UNDO.

x-Archivo = "/home/v/IN/dbs/" + "almmmate.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH Resumen NO-LOCK:
    EXPORT DELIMITER ";" Resumen.
END.
OUTPUT CLOSE.
PUT 'FIN PROCESO: ' NOW SKIP.

/* ******************************************************************************* */

/* ******************************************************************************* */
/* EXPORTA A OLAP */
/* ******************************************************************************* */
DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        

PUT 'EXPORT TXT: ' NOW SKIP.

comm-line = "/usr/bin/qonvtaexport8".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.

/*
curl -X 'POST' \
  'http://192.168.100.223:5000/datawarehouse/stockdepo/freezer/stock/artcode' \
  -H 'accept: application/json' \
  -H 'Content-Type: multipart/form-data' \
  -F 'file=@almmmate.txt;type=text/plain'
*/
PUT 'FIN TODO: ' NOW SKIP.


QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


