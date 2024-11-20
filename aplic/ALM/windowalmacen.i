&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : Definir la Divisi�n de Trabajo 

    Author(s)   :
    Created     :
    Notes       : Incluirlo en la secci�n "Definitions" de las ventanas (w*.w)
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* RHC 20/07/2015 TENER UN MEJOR CONTROL DEL S-CODDIV */
DEFINE SHARED VAR s-CodAlm AS CHAR.
DEFINE SHARED VAR s-CodCia AS INT.
DEFINE SHARED VAR S-DESALM AS CHAR.

FIND Almacen WHERE Almacen.codcia = s-codcia
    AND Almacen.codalm = s-codalm
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen OR NOT CAN-FIND(gn-divi WHERE gn-divi.codcia = s-codcia
                                         AND gn-divi.coddiv = Almacen.coddiv
                                         NO-LOCK)
    THEN DO:
    MESSAGE 'C�digo del almac�n:' s-codalm 'mal configurado'
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
ASSIGN s-DesAlm = Almacen.Descripcion.

/* Definimos la divisi�n de trabajo en base al almac�n seleccionado */
DEF NEW SHARED VAR s-coddiv AS CHAR.    
s-coddiv = Almacen.coddiv.
FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = s-coddiv
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi THEN DO:
    MESSAGE 'C�digo de divisi�n:' s-coddiv 'del almac�n:' s-codalm
        'MAL configurado'
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

/* ************************************************** */

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
         HEIGHT             = 3.65
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


