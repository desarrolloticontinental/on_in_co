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


/* SINTAXIS:
C:\Progress\OpenEdge\bin\prowin32.exe -ininame on_in_co_213qa.ini -p src/bin/_connect_database.p 
-param "-db integral -H 192.168.100.209 -S 65010 -N tcp"
*/
/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cDbName       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDbParameters AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCsh          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewCsh       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i             AS INTEGER     NO-UNDO.
DEFINE VARIABLE lCacheProblem AS LOGICAL     NO-UNDO.

cDbParameters = SESSION:PARAMETER.
i = INDEX(cDbParameters, '-db ').
i = i + 4.
REPEAT:
    IF SUBSTRING(cDbParameters, i, 1) = ' ' THEN LEAVE.
    cDbName = cDbName + SUBSTRING(cDbParameters, i, 1).
    i = i + 1.
END.
cCsh = TRIM(cDbName) + ".csh" .

IF INDEX(PROPATH, SESSION:TEMP-DIRECTORY) = 0 THEN
    PROPATH = PROPATH + SESSION:TEMP-DIRECTORY.
        
cNewCsh = SESSION:TEMP-DIRECTORY + cDbName + ".csh".

cCsh = SEARCH(cCsh) .
IF cCsh = ? THEN cCsh = SEARCH(cNewCsh).

IF cCsh = ? THEN ASSIGN cCsh = "".
  ELSE ASSIGN cCsh = "-cache " + cCsh.

IF cCsh = "" THEN lCacheProblem = TRUE.

/*CONNECT VALUE(cDbName) -H hostname -S servicename -N tcp VALUE(cCsh) NO-ERROR.*/
CONNECT VALUE(cDbParameters) VALUE(cCsh) NO-ERROR.
IF ERROR-STATUS:NUM-MESSAGES > 1 THEN
DO i = 1 TO ERROR-STATUS:NUM-MESSAGES :
/* 
Trap errors that have to do with the cache file being used for this specific connection attempt...   
     The errors trapped in this example are:
     The file <filename> is not a valid local cache file. (823)
     CRC error in -cache <filename>.  The file has been corrupted. (825) 
     The time stamp in the database does not match the time stamp in the -cache file: <filename>. (840) 
     Error while reading -cache file. ret=<return-code> errno=<errno>. (844)
     WARNING: The -cache <filename> parameter was used. An error occurred while attempting to read the schema cache from the named file.
     The schema cache will be read from the database instead. (6126)         
*/
     IF ERROR-STATUS:GET-NUMBER(i) = 823 OR
        ERROR-STATUS:GET-NUMBER(i) = 825 OR
        ERROR-STATUS:GET-NUMBER(i) = 840 OR
        ERROR-STATUS:GET-NUMBER(i) = 844 OR
        ERROR-STATUS:GET-NUMBER(i) = 6126 THEN
           lCacheProblem = TRUE.

    /* 
     If none of the cache related errors where reported, 
     then it could be a genuine problem with connecting the database so we should log these. 
    */

     MESSAGE "Error " ERROR-STATUS:GET-NUMBER(i) ": " ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX.
END.
IF CONNECTED(cDbName) THEN
DO:

    /* 
      If we are connected and there was a problem with the cache file,   
       then we have to re-generate it ... 
    */

    IF lCacheProblem THEN DO:
        SAVE CACHE COMPLETE VALUE(cDbName) TO VALUE(cNewCsh) .
    END.

    /* 
       Otherwise, we are connected and there were no errors with the schema file so we don't need to generate it, 
         or we were already connected and received message 1012 so we have already used the -cache file that was correct, 
         or we are going to re-generate it on the next connection.
    */
    RUN src/bin/_inicio.p.
END.
ELSE MESSAGE "Problem connecting to the database" VIEW-AS ALERT-BOX.

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
         HEIGHT             = 5.65
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


