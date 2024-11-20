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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/*
tabla : connection_log

Connect_estado          : Estado de la connexion    (ABIERTA, CERRADA)
Connect_Id              : Numero de conexion  
Connect_Usr             : Numero de conexion de usuario    PK1a
Connect_Type            : Tipo de conexion                
Connect_Name            : Usuario                         PK1b
Connect_Device          : Dispositivo                   PK1c
Connect_Time            : Fehc/Hora inicio de conexion    PK1d
Connect_Pid             : PID de conexion
Connect_ClientType      : Tipo de Cliente de conexion
Connect_thora_procesada : Proceso
*/
    
    
DEFINE BUFFER b-connection_log FOR connection_log.

DEFINE TEMP-TABLE x-connect LIKE _connect
    FIELD thora_procesada AS CHAR FORMAT 'x(25)'.

PUT UNFORMATTED 'Inicio: ' DATETIME(TODAY, MTIME) SKIP.

/* Leer todas las conexiones actuales */
FOR EACH _connect WHERE _connect-type = "REMC" NO-LOCK:
    CREATE x-connect.
    BUFFER-COPY _connect TO x-connect.
        ASSIGN x-connect.thora_procesada = STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS").
END.

/* Comparar conexiones actuales vs log */
FOR EACH x-connect NO-LOCK:
    FIND FIRST b-connection_log WHERE b-connection_log.Connect_Usr = x-connect._Connect-Usr AND
                                        b-connection_log.Connect_Name =   x-connect._Connect-Name AND 
                                        b-connection_log.Connect_Device =  x-connect._Connect-Device AND 
                                        b-connection_log.Connect_Time  =  x-connect._Connect-Time EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE b-connection_log THEN DO:
        CREATE b-connection_log.
        ASSIGN 
            b-connection_log.Connect_estado = "NUEVAX"
            b-connection_log.Connect_Id     = x-connect._Connect-Id
            b-connection_log.Connect_Usr    = x-connect._Connect-Usr
            b-connection_log.Connect_Type   = x-connect._Connect-Type
            b-connection_log.Connect_Name   = x-connect._Connect-Name
            b-connection_log.Connect_Device = x-connect._Connect-Device
            b-connection_log.Connect_Time   = x-connect._Connect-Time
            b-connection_log.Connect_Pid    = x-connect._Connect-Pid
            b-connection_log.Connect_ClientType = x-connect._Connect-ClientType
            b-connection_log.Connect_thora_procesada = x-connect.thora_procesada
        .
    END.
    ELSE DO:
        ASSIGN 
            b-connection_log.Connect_estado = "SIGUEN"
            b-connection_log.Connect_thora_procesada = x-connect.thora_procesada.
    END.
    RELEASE b-connection_log NO-ERROR.
END.


/* Cerrar las conexiones que ya no estan abiertas */
FOR EACH connection_log WHERE connection_log.Connect_estado = "ABIERTA" NO-LOCK:    
    FIND FIRST b-connection_log WHERE ROWID(b-connection_log) = ROWID(connection_log) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE b-connection_log THEN DO:
        ASSIGN 
            b-connection_log.Connect_estado = "CERRADA"
            b-connection_log.Connect_thora_procesada = STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS").
    END.
    RELEASE b-connection_log NO-ERROR.
END.

/* Reafirmo las que siguen abiertas */
FOR EACH connection_log WHERE connection_log.Connect_estado = "SIGUEN" OR 
                             connection_log.Connect_estado = "NUEVAX" NO-LOCK:
    FIND FIRST b-connection_log WHERE ROWID(b-connection_log) = ROWID(connection_log) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE b-connection_log THEN DO:
        ASSIGN 
            b-connection_log.Connect_estado = "ABIERTA".
    END.
    RELEASE b-connection_log NO-ERROR.
END.

PUT UNFORMATTED 'Fin: ' DATETIME(TODAY, MTIME) SKIP.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


