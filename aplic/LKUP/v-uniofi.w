&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE SHARED VARIABLE output-var-4 LIKE FacDPedi.PreUni.
DEFINE SHARED VARIABLE output-var-5 LIKE FacDPedi.PorDto.

DEFINE SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE S-CODALM   AS CHAR.
DEFINE SHARED VARIABLE S-CODMAT   AS CHAR.
DEFINE SHARED VARIABLE S-CODMON   AS INTEGER.

DEFINE VAR X-CanPed AS DEC NO-UNDO.
DEFINE VAR X-PedCon AS DEC NO-UNDO.

DEFINE BUFFER B-Almmmatg FOR Almmmatg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-29 RECT-30 
&Scoped-Define DISPLAYED-OBJECTS F-descuento F-CODIGO F-descrip F-marca ~
F-totstk F-comprometido F-dispo F-ubase 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F-CODIGO AS CHARACTER FORMAT "X(256)":U 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE F-comprometido AS DECIMAL FORMAT "-zzz,zz9.99":U INITIAL 0 
     LABEL "Stock Comprometido" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .81
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE F-descrip AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripcion" 
     VIEW-AS FILL-IN 
     SIZE 41.43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-descuento AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.86 BY .81
     BGCOLOR 15 FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE F-dispo AS DECIMAL FORMAT "-zzz,zz9.99":U INITIAL 0 
     LABEL "Stock Disponible" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .81
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE F-marca AS CHARACTER FORMAT "X(256)":U 
     LABEL "Marca" 
     VIEW-AS FILL-IN 
     SIZE 41.43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-totstk AS DECIMAL FORMAT "-zzz,zz9.99":U INITIAL 0 
     LABEL "Stock Actual" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .81
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE F-ubase AS CHARACTER FORMAT "X(256)":U 
     LABEL "Unidad Base" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52.29 BY 3.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52.29 BY 3.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     F-descuento AT ROW 1 COL 2 NO-LABEL
     F-CODIGO AT ROW 1.92 COL 9.43 COLON-ALIGNED
     F-descrip AT ROW 2.85 COL 9.43 COLON-ALIGNED
     F-marca AT ROW 3.81 COL 9.43 COLON-ALIGNED
     F-totstk AT ROW 5.08 COL 17.72 COLON-ALIGNED
     F-comprometido AT ROW 6.04 COL 17.72 COLON-ALIGNED
     F-dispo AT ROW 6.96 COL 17.72 COLON-ALIGNED
     F-ubase AT ROW 1.92 COL 33.29 COLON-ALIGNED
     RECT-29 AT ROW 1.81 COL 1.72
     RECT-30 AT ROW 4.85 COL 1.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 6.96
         WIDTH              = 54.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F-CODIGO IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-comprometido IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-descuento IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F-dispo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-marca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-totstk IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ubase IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
/*
ON "MOUSE-SELECT-DBLCLICK", F8 OF FRAME {&FRAME-NAME} ANYWHERE DO:
*/
ON F8 OF FRAME {&FRAME-NAME} /* br_table */
DO:
  run vta/d-stkalm.
END.

ON F7 OF FRAME {&FRAME-NAME} /* br_table */
DO:
/*  run Lkup/c-conped.*/
    run Lkup/c-conode.
END.

ON F9 OF FRAME {&FRAME-NAME} /* br_table */
DO:
  RUN Vta/D-Dtovol2.r(trim(s-codmat) ).
END.

FIND FIRST B-Almmmatg WHERE B-Almmmatg.CodCia = S-CODCIA
                       AND  LOOKUP(trim(S-CODALM),B-Almmmatg.almacenes) <> 0
                       AND  B-Almmmatg.FchCes = ?
                       AND  (B-Almmmatg.codmat = trim(S-CODMAT))
                      NO-LOCK NO-ERROR.
IF AVAIL B-Almmmatg THEN DO:

    /************Descuento Promocional ************/
    DEFINE VAR J AS INTEGER .
    DEFINE VAR X-PROMO AS CHAR INIT "".
    DO J = 1 TO 10 :
       IF TODAY >= B-Almmmatg.PromFchD[J] AND 
          TODAY <= B-Almmmatg.PromFchH[J] THEN X-PROMO = "Promocional " .
          
    END.
    /************************************************/
 
    /***************Descuento Volumen****************/                    
     DEFINE VAR X-VOLU AS CHAR INIT "".
     DO J = 1 TO 10 :
        IF  B-Almmmatg.DtoVolD[J] > 0  THEN X-VOLU = "Volumen " .                 
     END.        
    /************************************************/
    DO WITH FRAME {&FRAME-NAME}:
       F-DESCUENTO = "". 
       IF X-PROMO <> "" THEN F-DESCUENTO = "Producto con Descuento " + X-VOLU.
       IF X-VOLU  <> "" THEN F-DESCUENTO = "Producto con Descuento " + X-VOLU.
       IF X-PROMO <> "" AND X-VOLU <> "" THEN F-DESCUENTO = "Producto con Descuento " + X-PROMO + " y " + X-VOLU.
       F-DESCUENTO:BGCOLOR = 8 .
       F-DESCUENTO:FGCOLOR = 8 .
       
       IF F-DESCUENTO <> "" THEN DO WITH FRAME {&FRAME-NAME}:
          F-DESCUENTO:BGCOLOR = 12 .
          F-DESCUENTO:FGCOLOR = 15 .
          DISPLAY F-DESCUENTO @ F-DESCUENTO.
       END. 
    END.

END.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
        /****/
/*
        FOR EACH FacDPedi WHERE FacDPedi.CodCia = s-codcia 
                           AND  FacDPedi.almdes = s-codalm
                           AND  FacDPedi.codmat = s-codmat 
                           AND  LOOKUP(FacDPedi.CodDoc,'PED') > 0 
                           AND  LOOKUP(FacDPedi.FlgEst, 'P,X,') > 0:
            FIND FIRST FacCPedi OF FacDPedi WHERE FacCPedi.codcia = FacDPedi.CodCia
                                             AND  FacCPedi.CodAlm = FacDPedi.almdes
                                             AND  Faccpedi.FlgEst = "P"
                                             AND  Faccpedi.TpoPed = "1"
                                            NO-LOCK NO-ERROR.
            IF NOT AVAIL Faccpedi THEN NEXT.
        
            X-CanPed = X-CanPed + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate ).
        END.
*/    

   
        /*********   Barremos las O/D que son parciales y totales    ****************/
        FOR EACH FacDPedi WHERE FacDPedi.CodCia = s-codcia
                           /*AND  FacDPedi.almdes = s-codalm*/
                           AND  FacDPedi.codmat = s-codmat 
                           AND  LOOKUP(FacDPedi.CodDoc,'O/D') > 0 
                           AND  LOOKUP(FacDPedi.FlgEst, 'P,X,') > 0:
            FIND FIRST FacCPedi OF FacDPedi WHERE FacCPedi.codcia = FacDPedi.CodCia
                                             AND  FacCPedi.CodAlm = s-codalm 
                                             AND  Faccpedi.FlgEst = "P"
                                            NO-LOCK NO-ERROR.
            IF NOT AVAIL FacCPedi THEN NEXT.
            X-CanPed = X-CanPed + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate ).
        END.
        
        /*******************************************************/
/* RHC 29.04.04 Aparentemente no hay pedidos mostrador an ATE        
        /* Segundo barremos los pedidos de mostrador de acuerdo a la vigencia */
        DEF VAR TimeOut AS INTEGER NO-UNDO.
        DEF VAR TimeNow AS INTEGER NO-UNDO.
        FIND FacCfgGn WHERE faccfggn.codcia = s-codcia NO-LOCK.
        TimeOut = (FacCfgGn.Dias-Res * 24 * 3600) +
                  (FacCfgGn.Hora-Res * 3600) + 
                  (FacCfgGn.Minu-Res * 60).
        FOR EACH Facdpedm WHERE Facdpedm.CodCia = s-codcia 
                           AND  Facdpedm.AlmDes = S-CODALM
                           AND  Facdpedm.codmat = s-codmat 
                           AND  Facdpedm.FlgEst = "P" :
            FIND FIRST Faccpedm OF Facdpedm WHERE Faccpedm.CodCia = Facdpedm.CodCia 
                                             AND  Faccpedm.CodAlm = s-codalm 
                                             AND  Faccpedm.FlgEst = "P"  
                                            NO-LOCK NO-ERROR. 
            IF NOT AVAIL Faccpedm THEN NEXT.
            
            TimeNow = (TODAY - FacCPedm.FchPed) * 24 * 3600.
            TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(FacCPedm.Hora, 1, 2)) * 3600) +
                      (INTEGER(SUBSTRING(FacCPedm.Hora, 4, 2)) * 60) ).
            IF TimeOut > 0 THEN DO:
                IF TimeNow <= TimeOut   /* Dentro de la valides */
                THEN DO:
                    /* cantidad en reservacion */
                    X-CanPed = X-CanPed + FacDPedm.Factor * FacDPedm.CanPed.
                END.
            END.
        END.
        X-PedCon = X-CanPed.  
******************************************************* */        
        
        /*****/
        FIND FIRST B-Almmmatg WHERE B-Almmmatg.CodCia = S-CODCIA
                               AND  LOOKUP(TRIM(S-CODALM),B-Almmmatg.almacenes) <> 0
                               AND  B-Almmmatg.FchCes = ?
                               AND  (B-Almmmatg.codmat = S-CODMAT)
                              NO-LOCK NO-ERROR.
        IF AVAIL B-Almmmatg THEN DO:
            FIND almmmate WHERE Almmmate.CodCia = B-Almmmatg.CodCia
                           AND  Almmmate.CodAlm = s-codalm
                           AND  Almmmate.codmat = B-Almmmatg.codmat
                          NO-LOCK.
            IF AVAILABLE almmmate THEN DO:
                ASSIGN
                    F-totstk    = Almmmate.StkAct
                    F-CODIGO    = B-Almmmatg.codmat  
                    F-descrip   = B-Almmmatg.DesMat
                    F-marca     = B-Almmmatg.DesMar
                    F-ubase     = B-Almmmatg.UndBas
                    F-comprometido = X-CanPed
                    F-dispo     = F-totstk - X-CanPed
                    .
                DISPLAY 
                    F-totstk 
                    F-CODIGO 
                    F-descrip
                    F-marca  
                    F-ubase  
                    F-comprometido
                    F-dispo .
            END.
        END.
  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros V-table-Win 
PROCEDURE procesa-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    output-var-1 como ROWID
    output-var-2 como CHARACTER
    output-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
    END CASE.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros V-table-Win 
PROCEDURE recoge-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    input-var-1 como CHARACTER
    input-var-2 como CHARACTER
    input-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
        /*
            ASSIGN
                input-para-1 = ""
                input-para-2 = ""
                input-para-3 = "".
         */      
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  IF p-state = 'update-begin':U THEN DO:
     RUN valida-update.
     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.
  END.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida V-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     Validacion de datos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME} :
   /* IF CAMPO:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Campo no debe ser blanco"
         VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO CAMPO.
         RETURN "ADM-ERROR".   
   
      END.
   */

END.

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update V-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Rutina de validacion en caso de modificacion
  Parameters:  Regresar "ADM-ERROR" si no se quiere modificar
  Notes:       
------------------------------------------------------------------------------*/

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

