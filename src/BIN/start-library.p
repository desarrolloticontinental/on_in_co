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
    Notes       : Start Libraries
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*
DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN <libreria> PERSISTENT SET hProc.

RUN <libreria>.rutina_internaIN hProc (input  buffer tt-excel:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tt-excel:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.

*/
DEFINE TEMP-TABLE ttEstacionesPC 
    FIELD   tNamePC     AS  CHAR
    FIELD   tConnectPID AS  INT
    FIELD   tUsrConexion AS  INT.

DEFINE VAR x-fechahora-inicio-session AS DATETIME.
DEFINE VAR x-vtatabla AS CHAR INIT "CONFIG-SESSION".
DEFINE VAR x-llave_c1 AS CHAR INIT "DESBLOQUEO".
DEFINE VAR x-llave_c2 AS CHAR INIT "AUTOMATICO".
DEFINE VAR lPid                 AS INT.
DEFINE VAR lXRowId              AS CHAR.
DEFINE VAR lRowId               AS INT.

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
         HEIGHT             = 7.5
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-check-desbloqueo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-desbloqueo Procedure 
PROCEDURE check-desbloqueo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER pRetVal AS CHAR.

pRetVal = "NO".

FIND FIRST vtatabla WHERE vtatabla.codcia = 1 AND 
                            vtatabla.tabla = x-vtatabla AND
                            vtatabla.llave_c1 = x-llave_c1 AND 
                            vtatabla.llave_c2 = x-llave_c2 NO-LOCK NO-ERROR.
IF AVAILABLE vtatabla THEN DO:
    IF vtatabla.libre_c01 = 'SI' THEN DO:
        pRetVal = "SI".
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-grabar-desbloqueo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grabar-desbloqueo Procedure 
PROCEDURE grabar-desbloqueo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pIdConexion AS INT.
DEFINE INPUT PARAMETER pPcUser AS CHAR.
DEFINE INPUT PARAMETER pUserDB AS CHAR.

DEFINE VAR x-intervalo AS INT.
DEFINE VAR x-minutos-trascurridos AS INT.
DEFINE VAR x-retval AS CHAR.

/* */
RUN check-desbloqueo(OUTPUT x-retval).

IF x-retval = "SI" THEN DO:

    IF x-fechahora-inicio-session = ? THEN RETURN.

    /* Cada cuantos minutos */
    RUN intervalo-desbloqueo(OUTPUT x-intervalo).

    /**/
    x-minutos-trascurridos = INTERVAL(NOW,x-fechahora-inicio-session,"minutes").

    IF x-intervalo > 0 AND x-minutos-trascurridos >= x-intervalo THEN DO:
        CREATE shut_user.
          ASSIGN shut_user.shut_status = YES
                  shut_user.shut_id = pIdConexion
                  shut_user.pc_user = pPcUser
                  shut_user.USER_db = pUserDB
                  shut_user.fcreacion = NOW
                  shut_user.codcia = 1.

        RELEASE shut_user NO-ERROR.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-intervalo-desbloqueo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE intervalo-desbloqueo Procedure 
PROCEDURE intervalo-desbloqueo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER pRetVal AS INT.

pRetVal = -1.

FIND FIRST vtatabla WHERE vtatabla.codcia = 1 AND 
                            vtatabla.tabla = x-vtatabla AND
                            vtatabla.llave_c1 = x-llave_c1 AND 
                            vtatabla.llave_c2 = x-llave_c2 NO-LOCK NO-ERROR.
IF AVAILABLE vtatabla THEN DO:
    IF vtatabla.libre_c01 = 'SI' THEN DO:
        pRetVal = vtatabla.valor[1].
    END.    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-um-control-usuario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE um-control-usuario Procedure 
PROCEDURE um-control-usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pExcConn AS CHAR.
DEFINE INPUT PARAMETER lClientIPAddress AS CHAR.
DEFINE INPUT PARAMETER lClientComputerName AS CHAR.
DEFINE INPUT PARAMETER lClientName AS CHAR.
DEFINE INPUT PARAMETER lComputerName AS CHAR.
DEFINE INPUT PARAMETER lUserName AS CHAR.
DEFINE INPUT PARAMETER lRemote-User AS CHAR.
DEFINE INPUT PARAMETER lxClientName AS CHAR.
DEFINE OUTPUT PARAMETER pRCID AS INTE.

/* Grabo los datos */
SESSION:TIME-SOURCE = "integral". 
/*
DEFINE VAR xxxx AS CHAR.
xxxx =  CLIENT-CONNECTION-ID /* SERVER-CONNECTION-ID*/  /*string(_Connect._Connect-Pid,"999999999999")*/.
MESSAGE '(' + xxxx + ")" VIEW-AS ALERT-BOX ERROR.
*/

lPid = 0.

FIND FIRST _Myconnection.
IF AVAILABLE _Myconnection THEN DO:    
    lPid    = _MyConnection._MyConn-pid.
                /*_MyConnection._MyConn-UserId.*/

    FIND FIRST _Connect WHERE _Connect._Connect-Usr =  _MyConnection._MyConn-UserId AND 
                            _Connect._Connect-Pid = _MyConnection._MyConn-pid NO-LOCK NO-ERROR.
END.
ELSE DO:
    /* Forzar a que no exista */
    FIND FIRST _Connect WHERE _Connect._Connect-Usr =  -99 AND 
                            _Connect._Connect-Pid = -999 NO-LOCK NO-ERROR.

END.

CREATE AdmCtrlUser.

    lxRowId = STRING(RECID(AdmCtrlUser),"999999999999").
    lRowId  = INTEGER(lxRowId).
    /* Variable de CONTROL GLOBAL */
    pRCID   = lRowId.

    ASSIGN  AdmCtrlUser.Numid       = lRowid
            AdmCtrlUser.IPRemoto    = lClientIPAddress
            AdmCtrlUser.PCRemoto    = IF (lClientComputerName = ? OR lClientComputerName = "") THEN lClientName ELSE lClientComputerName
            AdmCtrlUser.PCCliente   = lComputerName
            AdmCtrlUser.UserCliente = lUserName
            AdmCtrlUser.UserRemoto  = lRemote-User
            AdmCtrlUser.UserDB      = USERID("integral") /*s-user-id*/
            AdmCtrlUser.FechInicio  = TODAY
            AdmCtrlUser.HoraInicio  = STRING(TIME,"HH:MM:SS")
            AdmCtrlUser.PCUsuario   = lxClientName
            AdmCtrlUser.IDUsuario   = IF (lRemote-user = ? OR lRemote-user = "") THEN lUserName ELSE lRemote-user
            AdmCtrlUser.num-pid     = lPid      
            AdmCtrlUsers.campo-c[2] = IF (AVAILABLE _MyConnection) THEN STRING(_MyConnection._MyConn-UserId) ELSE "0". /* nro de la conexion ( _connect._connect-usr )*/

    IF AVAILABLE _Connect THEN DO:
        ASSIGN AdmCtrlUsers.campo-c[3] = STRING(_Connect._Connect-Usr)
                AdmCtrlUsers.campo-c[4] = STRING(_Connect._Connect-Name)
                AdmCtrlUsers.campo-c[5] = STRING(_Connect._Connect-Device)
                AdmCtrlUsers.campo-c[6] = STRING(_Connect._Connect-Pid)
                .

    END.
/*
        FechTermino
        HoraTermino
*/        

    RELEASE AdmCtrlUser NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-um-verifica-conexiones) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE um-verifica-conexiones Procedure 
PROCEDURE um-verifica-conexiones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER lComputerName AS CHAR.
DEFINE INPUT PARAMETER lxClientName AS CHAR.
DEFINE INPUT PARAMETER lRemote-user AS CHAR.
DEFINE INPUT PARAMETER lUserName AS CHAR.
DEFINE OUTPUT PARAMETER pConnOk AS LOGICAL.
DEFINE OUTPUT PARAMETER pExcConn AS CHAR.

DEFINE VAR lPid-PC                 AS INT.


DEFINE VAR lUserDB              AS CHAR.
DEFINE VAR lConnPermitidas      AS INT.
DEFINE VAR x-pc-conectadas      AS INT.
DEFINE VAR lNConexiones         AS INT.
DEFINE VAR lRowId               AS ROWID.

DEFINE VAR x-tabla      AS CHAR INIT "LOGIN-BLOQUEADOS".

DEFINE VAR x-tabla1      AS CHAR INIT "LOGIN".
DEFINE VAR x-llave_c1      AS CHAR INIT "PCS-X-USUARIO".

DEFINE VAR x-msg-lck    AS CHAR.

DEFINE VAR x-maximo-pcs AS INT.

DEFINE VAR x-connect-usr AS INT INIT 0.

DEFINE VAR x-retval AS CHAR.
DEFINE VAR x-datos-de-la-pc AS CHAR.

pConnOk = NO.
pExcConn = "". 

EMPTY TEMP-TABLE ttEstacionesPC.
x-datos-de-la-pc = lComputerName.                       /* Terminal Server */
x-datos-de-la-pc = x-datos-de-la-pc + "|" + lxClientName.     /* Pc del usuario */
x-datos-de-la-pc = x-datos-de-la-pc + "|" + IF (lRemote-user = ? OR lRemote-user = "") THEN lUserName ELSE lRemote-user. /* Usuario de Windows */
x-datos-de-la-pc = x-datos-de-la-pc + "|" + STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS").

lUserDB = USERID("integral").
IF CAPS(lUserDB) <> 'ADMIN' THEN DO:

    pConnOk = YES.

    FIND FIRST _user WHERE _user._userid = lUserDB NO-LOCK NO-ERROR.
    IF AVAILABLE _user THEN DO:        

        x-fechahora-inicio-session = ?.

        lNConexiones = 0.
        x-connect-usr = 0.
        lPid-pc = 0.
        FOR EACH _connect WHERE _connect._connect-name = lUserDB NO-LOCK by _connect-time DESC:
            lNConexiones = lNConexiones + 1.
            x-connect-usr = _connect._connect-usr.  /* Numero de la conexion */
            lPid-PC = _connect._connect-pid.

            /* Almaceno de que PCs son las conexionaes activas */
            /*
            FIND FIRST AdmCtrlUsers WHERE AdmCtrlUsers.num-PID = _connect._connect-pid AND                                            
                                            AdmCtrlUsers.fechinicio >= TODAY - 2 NO-LOCK NO-ERROR.
            */
            FIND LAST AdmCtrlUsers WHERE AdmCtrlUsers.num-PID = _connect._connect-pid AND                                            
                                            AdmCtrlUsers.fechinicio <= TODAY AND 
                                            AdmCtrlUsers.UserDB = lUserDB  NO-LOCK NO-ERROR.

            IF AVAILABLE AdmCtrlUsers THEN DO:

                x-fechahora-inicio-session = DATETIME(string(fechinicio,"99/99/9999") + " " + horainicio).

                CREATE ttEstacionesPC.
                    ASSIGN ttEstacionesPC.tNamePC = AdmCtrlUsers.pcusuario
                            ttEstacionesPC.tConnectPID = _connect._connect-pid
                            ttEstacionesPC.tUsrConexion = _connect._connect-usr.
                    x-connect-usr = _connect._connect-usr.
            END.

        END.
        /* Desde cualquier PC puede ingresar un maximo de  */
        x-maximo-pcs = 5.
        FIND FIRST vtatabla WHERE vtatabla.codcia = 1 AND
                                    vtatabla.tabla = x-tabla1 AND
                                    vtatabla.llave_c1 = x-llave_c1
                                    NO-LOCK NO-ERROR.

        IF AVAILABLE vtatabla THEN x-maximo-pcs = vtatabla.rango_valor[1].

        IF _user._user-misc <> "GENERICO" THEN DO:
            /* El usuario NO es de uso GENERICO */            
            IF lNConexiones > 1 THEN DO:

                /* Ic - 06Ago2021, almaceno en el log */
                x-msg-lck = "".
                FIND FIRST logvtatabla WHERE logvtatabla.codcia = 1 AND
                                            logvtatabla.tabla = x-tabla AND
                                            logvtatabla.logdate = TODAY AND
                                            logvtatabla.numid = x-connect-usr AND
                                            logvtatabla.llave_c1 = lUserDB EXCLUSIVE-LOCK NO-ERROR.
                IF LOCKED logvtatabla THEN DO:
                    /*x-msg-lck = "LOGVTATABLA bloqueado no se pudo registrar la incidencia".*/
                END.
                ELSE DO:
                    IF NOT AVAILABLE logvtatabla THEN DO:
                        
                        CREATE logvtatabla.
                            ASSIGN logvtatabla.codcia = 1
                                    logvtatabla.tabla = x-tabla
                                    logvtatabla.logdate = TODAY
                                    logvtatabla.numid = x-connect-usr
                                    logvtatabla.llave_c1 = lUserDB
                                    logvtatabla.valor[1] = 0
                                    logvtatabla.valor[2] = lPid-pc          /* Id de la PC que tiene la conexion ocupada */
                                    /*logvtatabla.valor[3] = lPid             /* Id de la PC que quizo entrar y no pudo ingresar */*/
                                    logvtatabla.logTIME = STRING(TIME,"HH:MM:SS")
                                    logvtatabla.libre_c01 = "Usted (" + lUserDB + " - PERSONAL) ya tiene una sesion abierta en Progress en esta PC"
                                    logvtatabla.libre_c02 = x-datos-de-la-pc NO-ERROR.
                    END.
                    ASSIGN logvtatabla.valor[1] = logvtatabla.valor[1] + 1 NO-ERROR.
                END.
                RELEASE logvtatabla NO-ERROR.

                x-retval = "NO".
                RUN check-desbloqueo(OUTPUT x-retval).

                IF x-retval = "SI" THEN x-msg-lck = "Espere un minuto y vuelva intentar".

                MESSAGE "Usted (" + lUserDB + ") ya tiene una sesion abierta en Progress en esta PC" SKIP
                        "NO puede abrir una segunda sesion Progress con este mismo usuario " SKIP 
                        "por POLITICA DE LA EMPRESA" SKIP
                        " " SKIP
                        "Excepto a los usuarios GENERICOS que si estan autorizados a aperturar sesiones" SKIP
                        "con un maximo de " + STRING(x-maximo-pcs) + " pero en diferentes PCs " SKIP
                        " " SKIP
                        "Su ID de conexion actual es (" + STRING(x-connect-usr) + ")" SKIP
                        x-msg-lck SKIP
                        NOW
                        VIEW-AS ALERT-BOX TITLE "CONTINENTAL SAC".

                RUN grabar-desbloqueo(INPUT x-connect-usr, INPUT lxClientName, INPUT lUserDB).
                        
                /*
                MESSAGE 'El usuario ' + lUserDB + ' cuyo tipo es ' + TRIM(_user._user-misc) + ' no puede ingresar al sistema por que :' SKIP
                    '    - Otro usuario esta usando su codigo ' SKIP
                    '    - Salio del sistema de forma incorrecta y su conexion aun esta activa' SKIP
                    'Por favor contactar con el area de soporte y proporcionarle el siguiente numero de sesion (' + STRING(x-connect-usr) + ")" SKIP 
                    "                                                                               Muchas gracias!!!"
                    VIEW-AS ALERT-BOX TITLE "Politica de la empresa".
                */
                /*
                MESSAGE 'NO puede ingresar al sistema porque el usuario esta siendo usuado (' + STRING(x-connect-usr) + ")" SKIP 
                    "Se esta monitoreando el nro de accesos al sistema..."                    
                    VIEW-AS ALERT-BOX INFORMATION.
                */
                pConnOk = NO.
            END.
        END.
        ELSE DO:
            /* El usuario es GENERICO */
            x-pc-conectadas = 0.
            x-connect-usr = 0.
            /* Verificamos cuantas conexiones son desde la misma PC */
            FOR EACH ttEstacionesPC WHERE ttEstacionesPC.tNamePc = lxClientName NO-LOCK:
                x-pc-conectadas = x-pc-conectadas + 1.
                IF x-connect-usr = 0 THEN x-connect-usr = ttEstacionesPC.tUsrConexion.
            END.            
            IF x-pc-conectadas > 0 THEN DO:
                /* Tiene mas de una conexion en la misma PC */

                x-msg-lck = "".
                FIND FIRST logvtatabla WHERE logvtatabla.codcia = 1 AND
                                            logvtatabla.tabla = x-tabla AND
                                            logvtatabla.logdate = TODAY AND
                                            logvtatabla.numid = x-connect-usr AND
                                            logvtatabla.llave_c1 = lUserDB EXCLUSIVE-LOCK NO-ERROR.
                IF LOCKED logvtatabla THEN DO:
                    /*x-msg-lck = "LOGVTATABLA bloqueado no se pudo registrar la incidencia".*/
                END.
                ELSE DO:
                    IF NOT AVAILABLE logvtatabla THEN DO:
                        CREATE logvtatabla.
                            ASSIGN logvtatabla.codcia = 1
                                    logvtatabla.tabla = x-tabla
                                    logvtatabla.logdate = TODAY
                                    logvtatabla.numid = x-connect-usr
                                    logvtatabla.llave_c1 = lUserDB
                                    logvtatabla.valor[1] = 0
                                    logvtatabla.valor[2] = lPid-pc          /* Id de la PC que tiene la conexion ocupada */
                                    /*logvtatabla.valor[3] = lPid             /* Id de la PC que quizo entrar y no pudo ingresar */*/
                                    logvtatabla.logTIME = STRING(TIME,"HH:MM:SS")
                                    logvtatabla.libre_c01 = "Usted (" + lUserDB + " - GENERICO) ya tiene una sesion abierta en Progress en esta PC"
                                    logvtatabla.libre_c02 = x-datos-de-la-pc NO-ERROR.
                    END.
                    ASSIGN logvtatabla.valor[1] = logvtatabla.valor[1] + 1 NO-ERROR.
                END.
                RELEASE logvtatabla NO-ERROR.

                x-retval = "NO".
                RUN check-desbloqueo(OUTPUT x-retval).

                IF x-retval = "SI" THEN x-msg-lck = "Espere un minuto y vuelva intentar".

                MESSAGE "Usted ( ** " + lUserDB + " ** ) ya tiene una sesion abierta en Progress en esta PC" SKIP
                        "NO puede abrir una segunda sesion Progress con este mismo usuario " SKIP 
                        "por POLITICA DE LA EMPRESA" SKIP
                        " " SKIP
                        "Excepto a los usuarios GENERICOS que si estan autorizados a aperturar sesiones" SKIP
                        "con un maximo de " + STRING(x-maximo-pcs) + " pero en diferentes PCs " SKIP
                        " " SKIP
                        "Su ID de conexion actual es (" + STRING(x-connect-usr) + ")" SKIP
                        x-msg-lck SKIP
                        NOW
                        VIEW-AS ALERT-BOX TITLE "CONTINENTAL SAC".

                RUN grabar-desbloqueo(INPUT x-connect-usr, INPUT lxClientName, INPUT lUserDB).

                /*
                MESSAGE 'El usuario ' + lUserDB + ' cuyo tipo es ' + TRIM(_user._user-misc) + ' no puede ingresar al sistema por que :' SKIP
                    '    - Esta intentando conectarse en esta PC : ' + lxClientName + ' en mas de una sesion' SKIP
                    '    - Salio del sistema de forma incorrecta y su conexion aun esta activa en esta PC' SKIP
                    'Por favor contactar con el area de soporte y proporcionarle el siguiente numero de sesion (' + STRING(x-connect-usr) + ")" SKIP 
                    "                                                                               Muchas gracias!!!"
                    VIEW-AS ALERT-BOX TITLE "Politica de la empresa".
                */
                /*
                MESSAGE "No puede ingresar al sistema porque el usuario ya tienen una sesión abierta (" + STRING(x-connect-usr) + ") o el sistema se cerro de manera" SKIP 
                    "inesperada en este equipo"                    
                    VIEW-AS ALERT-BOX INFORMATION.
                */
                pConnOk = NO.
            END.
            ELSE DO:
                IF lNConexiones > x-maximo-pcs THEN DO:

                    x-msg-lck = "".
                    FIND FIRST logvtatabla WHERE logvtatabla.codcia = 1 AND
                                                logvtatabla.tabla = x-tabla AND
                                                logvtatabla.logdate = TODAY AND
                                                logvtatabla.numid = x-connect-usr AND
                                                logvtatabla.llave_c1 = lUserDB EXCLUSIVE-LOCK NO-ERROR.
                    IF LOCKED logvtatabla THEN DO:
                        /*x-msg-lck = "LOGVTATABLA bloqueado no se pudo registrar la incidencia".*/
                    END.
                    ELSE DO:
                        IF NOT AVAILABLE logvtatabla THEN DO:
                            CREATE logvtatabla.
                                ASSIGN logvtatabla.codcia = 1
                                        logvtatabla.tabla = x-tabla
                                        logvtatabla.logdate = TODAY
                                        logvtatabla.numid = x-connect-usr
                                        logvtatabla.llave_c1 = lUserDB
                                        logvtatabla.valor[1] = 0
                                        logvtatabla.valor[2] = lPid-pc          /* Id de la PC que tiene la conexion ocupada */
                                        /*logvtatabla.valor[3] = lPid             /* Id de la PC que quizo entrar y no pudo ingresar */*/
                                        logvtatabla.logTIME = STRING(TIME,"HH:MM:SS")
                                        logvtatabla.libre_c01 = "El usuario (" + lUserDB + ") tiene sesion abierta en " + STRING(x-maximo-pcs) + " PCs"
                                        logvtatabla.libre_c02 = x-datos-de-la-pc  NO-ERROR.
                        END.
                        ASSIGN logvtatabla.valor[1] = logvtatabla.valor[1] + 1  NO-ERROR.
                    END.
                    RELEASE logvtatabla NO-ERROR.

                    x-retval = "NO".
                    RUN check-desbloqueo(OUTPUT x-retval).

                    IF x-retval = "SI" THEN x-msg-lck = "Espere un minuto y vuelva intentar".

                    MESSAGE "El usuario (" + lUserDB + ") tiene sesion abierta en " + STRING(x-maximo-pcs) + " PCs" SKIP
                            "consulte y coordine con su administrador(a)"
                            "  " SKIP
                            "Es POLITICA DE LA EMPRESA el maximo de sesiones" SKIP
                            x-msg-lck SKIP
                            NOW
                            VIEW-AS ALERT-BOX TITLE "CONTINENTAL SAC".

                    RUN grabar-desbloqueo(INPUT x-connect-usr, INPUT lxClientName, INPUT lUserDB).

                    /*
                    MESSAGE 'El usuario ' + lUserDB + ' cuyo tipo es ' + TRIM(_user._user-misc) + ' no puede ingresar al sistema por que :' SKIP
                        '    - Esta intentando conectarse y ya supero el maximo de sesiones abiertas' SKIP
                        '    - Salio del sistema de forma incorrecta y su conexion aun esta activa' SKIP
                        'Por favor contactar con el area de soporte y proporcionarle el siguiente numero de sesion (' + STRING(x-connect-usr) + ")" SKIP 
                        "                                                                               Muchas gracias!!!"
                        VIEW-AS ALERT-BOX TITLE "Politica de la empresa".
                    */
                    /*
                    MESSAGE "Su usuario se esta excediendo de la cantidad de PCs" 
                            "a la cual debe ingresar " SKIP
                            "La cantidad maximo de PCs es " + STRING(x-maximo-pcs)
                    VIEW-AS ALERT-BOX INFORMATION.
                    */
                    pConnOk = NO.
                END.
            END.
        END.
    END.    
END.
ELSE pConnOk = YES.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

