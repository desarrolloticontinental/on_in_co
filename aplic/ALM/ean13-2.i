&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
    PUT STREAM REPORTE '^FO170,00'                     SKIP.   /* Coordenadas de origen campo1 */
    PUT STREAM REPORTE '^A0R,25,15'                    SKIP.
    PUT STREAM REPORTE '^FD'.
    PUT STREAM REPORTE x-DesMat                        SKIP.
    PUT STREAM REPORTE '^FS'                           SKIP.   /* Fin de Campo1 */
    PUT STREAM REPORTE '^FO80,30'                      SKIP.   /* Coordenadas de origen barras */

    PUT STREAM REPORTE '^BY2^BCR,80,Y,N,N'         SKIP.   /* Codigo 128 */
    IF x-Tipo = 1 THEN DO:
        /* BCN */
        PUT STREAM REPORTE '^FD'.
        PUT STREAM REPORTE Almmmatg.CodMat FORMAT 'x(6)' SKIP.
    END.
    ELSE DO:
        /* ECN */
        PUT STREAM REPORTE '^FD>;'.
        PUT STREAM REPORTE Barras[x-Barras - 1] FORMAT 'x(14)' SKIP.
    END.
    PUT STREAM REPORTE '^FS'                       SKIP.   /* Fin de Campo2 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

