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

/* Programa a ejecutar
    Formato pPrograma: aplic/gn/p-ecommerce-precios 
    Formato pParametros: "Dato1,Dato2,...".
    */
                                            
DEF INPUT PARAMETER pPrograma AS CHAR.     
DEF INPUT PARAMETER pParametros AS CHAR.
DEF OUTPUT PARAMETER pEstado AS CHAR NO-UNDO.

/* ******************************************** */
/* RHC 27/11/2020 Bloqueo de la tabla ecommerce */
/* ******************************************** */
pEstado = ''.
RETURN.
/* ******************************************** */
/* ******************************************** */


DEF SHARED VAR s-nomcia AS CHAR.
IF CONNECTED("ecommerce") THEN DISCONNECT ecommerce NO-ERROR.
IF INDEX(s-nomcia, 'PRUEBA') > 0 THEN DO:
    IF NOT CONNECTED('ecommerce') THEN 
        CONNECT -db ecommerce -N TCP -S 65020 -H 192.168.100.216 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:                                                            
         MESSAGE 'NO se ha podido conectar la base de datos de ECOMMERCE' VIEW-AS ALERT-BOX WARNING.     
    END.    
END.
ELSE DO:
    IF NOT CONNECTED('ecommerce') 
    THEN CONNECT -db ecommerce -N TCP -S 65020 -H 192.168.100.242 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:                                                            
         MESSAGE 'NO se ha podido conectar la base de datos de ECOMMERCE' VIEW-AS ALERT-BOX WARNING.     
    END.    
END.

CASE TRUE:
    WHEN INDEX(pPrograma, "p-ecommerce-precios") > 0 THEN DO:
        RUN VALUE(pPrograma) (INTEGER(ENTRY(1,pParametros)),
                              ENTRY(2,pParametros),
                              DECIMAL(ENTRY(3,pParametros)),
                              DECIMAL(ENTRY(4,pParametros)),
                              ENTRY(5,pParametros),
                              DECIMAL(ENTRY(6,pParametros)),
                              INTEGER(ENTRY(7,pParametros)),
                              ENTRY(8,pParametros)
                              ).
        IF CONNECTED("ecommerce") THEN DISCONNECT ecommerce NO-ERROR.
    END.
    WHEN INDEX(pPrograma, "p-ecommerce-import-ped") > 0 THEN DO:
        RUN VALUE(pPrograma) (OUTPUT pEstado).
        IF CONNECTED("ecommerce") THEN DISCONNECT ecommerce NO-ERROR.
    END.
    WHEN INDEX(pPrograma, "p-ecommerce-import-le") > 0 THEN DO:
        RUN VALUE(pPrograma) (OUTPUT pEstado).
        IF CONNECTED("ecommerce") THEN DISCONNECT ecommerce NO-ERROR.
    END.
    WHEN INDEX(pPrograma, "p-ecommerce-stocks") > 0 THEN DO:
        RUN VALUE(pPrograma) (INTEGER(ENTRY(1,pParametros)),
                              ENTRY(2,pParametros),
                              ENTRY(3,pParametros)
                              ).
        IF CONNECTED("ecommerce") THEN DISCONNECT ecommerce NO-ERROR.
    END.
END CASE.
IF CONNECTED("ecommerce") THEN DISCONNECT ecommerce NO-ERROR.

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


