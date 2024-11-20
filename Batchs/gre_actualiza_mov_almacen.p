&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER COTIZACION FOR FacCPedi.
DEFINE BUFFER PEDIDO FOR FacCPedi.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
DEFINE NEW SHARED VAR s-codcia AS INT.
DEFINE NEW SHARED VAR s-coddiv AS CHAR.

s-codcia = 1.

DEFINE BUFFER b-gre_header FOR gre_header.
DEFINE BUFFER b-gre_detail FOR gre_detail.

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
   Temp-Tables and Buffers:
      TABLE: COTIZACION B "?" ? INTEGRAL FacCPedi
      TABLE: PEDIDO B "?" ? INTEGRAL FacCPedi
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 19.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* INFORMACION DETALLADA Y DEPURADA */
PUT 'inicio del proceso: ' NOW SKIP.


PUT 'Actualizar movimientos de almacen ' NOW SKIP. 
PAUSE 0.
RUN gre_aceptadas_x_sunat.

/*RETURN. /* x ahora */*/

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-gre_aceptadas_x_sunat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gre_aceptadas_x_sunat Procedure 
PROCEDURE gre_aceptadas_x_sunat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR cMensajeRet AS CHAR.
DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN gre/library-generate-docs.r PERSISTENT SET hProc.

PROCESAR:
FOR EACH b-gre_header WHERE b-gre_header.m_estado_mov_almacen = "POR PROCESAR" NO-LOCK:
    IF b-gre_header.m_rspta_sunat = "ACEPTADO POR SUNAT" THEN DO:
        
        RUN GRE_Generacion_documentos IN hProc (INPUT b-gre_header.nCorrelatio, OUTPUT cMensajeRet).
        
        IF RETURN-VALUE = "ADM-ERROR" THEN DO:            
            PUT UNFORMATTED "ERROR --> No. de PGRE " + STRING(b-gre_header.ncorrelatio) + cMensajeRet SKIP.
        END.
        /*
            MESSAGE "1111Leave".
            /* Para propositos de pruebas...solo que procese un registro */
            LEAVE PROCESAR.
       */ 
    END.
END.

DELETE PROCEDURE hProc.                 /* Release Libreria */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

