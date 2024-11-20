&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Actualizar el archivo de transf SYPSA WMIGRV

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
         HEIGHT             = 4.54
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* ESTE PROCESO DEBE EJECUTARSE CON UN CRONTAB EN EL SERVIDOR */
DISABLE TRIGGERS FOR LOAD OF WMIGRV.
DISABLE TRIGGERS FOR LOAD OF WMIGCORR.
DISABLE TRIGGERS FOR LOAD OF RPL-WMIGRV.

FOR EACH rpl-wmigrv WHERE rpl-wmigrv.FlgEst = "N":
    PUT UNFORMATTED
        rpl-wmigrv.coddiv
        rpl-wmigrv.twcorre
        rpl-wmigrv.wvtdoc
        rpl-wmigrv.wvndoc
        SKIP.

    /* BLOQUEAMOS Sí o Sí el correlativo */
    REPEAT:
        FIND wmigcorr WHERE wmigcorr.Proceso = "RV"
            AND wmigcorr.Periodo = rpl-wmigrv.wvejer
            AND wmigcorr.Mes = rpl-wmigrv.wvperi
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE wmigcorr THEN DO:
            IF NOT LOCKED wmigcorr THEN DO:
                /* CREAMOS EL CONTROL */
                CREATE wmigcorr.
                ASSIGN
                    wmigcorr.Correlativo = 1
                    wmigcorr.Periodo = rpl-wmigrv.wvejer
                    wmigcorr.Mes = rpl-wmigrv.wvperi
                    wmigcorr.Proceso = "RV".
            END.
            ELSE UNDO, RETRY.
        END.
        LEAVE.
    END.
    CREATE WMIGRV.
    BUFFER-COPY RPL-WMIGRV
        TO WMIGRV
        ASSIGN
        wmigrv.FlagFecha = DATETIME(TODAY, MTIME)
        wmigrv.FlagTipo = "I"
        wmigrv.FlagUsuario = "AUTOMATICO".
    ASSIGN
        wmigrv.wcorre = STRING(wmigcorr.Periodo, '9999') + STRING(wmigcorr.Mes, '99') + 
                        STRING(wmigcorr.Correlativo, '9999').
    ASSIGN
        wmigcorr.Correlativo = wmigcorr.Correlativo +  1.
    ASSIGN
        rpl-wmigrv.flgest = "S".
END.
IF AVAILABLE(rpl-wmigrv) THEN RELEASE rpl-wmigrv.
IF AVAILABLE(wmigcorr) THEN RELEASE wmigcorr.
IF AVAILABLE(wmigrv)   THEN RELEASE wmigrv.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


