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

DEF VAR x-FchCorte AS DATE NO-UNDO.

x-FchCorte = DATE(12,31,2018).

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
         HEIGHT             = 10.54
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR x-inicio AS DATETIME NO-UNDO.
DEF VAR x-fin AS DATETIME NO-UNDO.

x-inicio = NOW.

RUN Paso1.
DISPLAY 'paso1 ok'. PAUSE 0.

RUN Paso2.
DISPLAY 'paso2 ok'. PAUSE 0.

RUN Paso3.
DISPLAY 'paso3 ok'. PAUSE 0.

RUN Paso4.
DISPLAY 'paso4 ok'. PAUSE 0.

RUN Paso5.
DISPLAY 'paso5 ok'. PAUSE 0.

RUN Paso6.
DISPLAY 'paso6 ok'. PAUSE 0.

RUN Paso7.
DISPLAY 'paso7 ok'. PAUSE 0.

RUN Paso8.
DISPLAY 'paso8 ok'. PAUSE 0.

RUN Paso9.
DISPLAY 'paso9 ok'. PAUSE 0.

RUN Paso10.
DISPLAY 'paso10 ok'. PAUSE 0.

x-fin = NOW.

DISPLAY x-inicio x-fin. PAUSE 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Paso1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso1 Procedure 
PROCEDURE Paso1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF VentasxCliente.

FOR EACH VentasxCliente EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE VentasxCliente.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso10) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso10 Procedure 
PROCEDURE Paso10 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF dwh_Ventas.

FOR EACH dwh_Ventas EXCLUSIVE-LOCK:
    DELETE dwh_Ventas.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso2 Procedure 
PROCEDURE Paso2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF VentasxClienteLinea.

FOR EACH VentasxClienteLinea EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE VentasxClienteLinea.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso3 Procedure 
PROCEDURE Paso3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF VentasxLinea.

FOR EACH VentasxLinea EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE VentasxLinea.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso4) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso4 Procedure 
PROCEDURE Paso4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF VentasxProducto.

FOR EACH VentasxProducto EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE VentasxProducto.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso5) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso5 Procedure 
PROCEDURE Paso5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF VentasxVendCliente.

FOR EACH VentasxVendCliente EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE VentasxVendCliente.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso6) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso6 Procedure 
PROCEDURE Paso6 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF VentasxVendedor.

FOR EACH VentasxVendedor EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE VentasxVendedor.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso7) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso7 Procedure 
PROCEDURE Paso7 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF Ventas_Cabecera.

FOR EACH Ventas_Cabecera EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE Ventas_Cabecera.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso8) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso8 Procedure 
PROCEDURE Paso8 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF Ventas_Detalle.

FOR EACH Ventas_Detalle EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE Ventas_Detalle.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso9) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso9 Procedure 
PROCEDURE Paso9 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF Ventas.

FOR EACH Ventas EXCLUSIVE-LOCK WHERE DateKey <= x-FchCorte:
    DELETE Ventas.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

