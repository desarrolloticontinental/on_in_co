&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Conecta la base de datos de estadisticas y luego la cierra

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT PARAMETER pParametro AS CHAR.

DEF VAR pProfile AS CHAR INIT 'estadisticas.pf' NO-UNDO.

IF NOT connected('estavtas')                                                                
        THEN CONNECT -db estavtas -N TCP -S 65000 -H 192.168.100.201 NO-ERROR.
                                                                                          
 IF ERROR-STATUS:ERROR THEN DO:                                                            
     MESSAGE 'NO se ha podido conectar la base de datos de ESTAVTAS' SKIP                 
         'NO podemos capturar el stock'                                                    
         VIEW-AS ALERT-BOX WARNING.     
END.    

IF NOT connected('cissac') THEN CONNECT -db integral -ld cissac -N TCP -S 65030 -H 192.168.100.202 NO-ERROR.  
                                                                                          
 IF ERROR-STATUS:ERROR THEN DO:                                                            
     MESSAGE 'NO se ha podido conectar la base de datos de CISSAC' SKIP                 
         'NO podemos capturar el stock'                                                    
         VIEW-AS ALERT-BOX WARNING.     
END.    


/*
IF NOT CONNECTED("estavtas") THEN DO:
    pProfile = SEARCH(pProfile).
    IF pProfile = ? THEN DO:
        MESSAGE 'NO se ha encontrado el archivo estadisticas.pf'
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    CONNECT -pf estadisticas.pf NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'NO se pudo conectar la base de estad�sticas' SKIP
            'Comunicar al administrador de base de datos'
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
END.
*/

pProfile = 'cissac.pf'.
IF NOT CONNECTED("cissac") THEN DO:
    pProfile = SEARCH(pProfile).
    IF pProfile = ? THEN DO:
        MESSAGE 'NO se ha encontrado el archivo cissac.pf'
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    CONNECT -pf cissac.pf NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'NO se pudo conectar la base de CISSAC' SKIP
            'Comunicar al administrador de base de datos'
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
END.

DEF VAR pPrograma AS CHAR INIT 'est/' NO-UNDO.

pPrograma = pPrograma + pParametro.

RUN VALUE(pPrograma).

IF CONNECTED("estavtas") THEN DISCONNECT estavtas NO-ERROR.

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
         HEIGHT             = 4.27
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


