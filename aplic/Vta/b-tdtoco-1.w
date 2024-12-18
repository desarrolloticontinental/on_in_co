&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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

/*
    Modific�    : Miguel Landeo /*ML01*/
    Fecha       : 03/Nov/2009
    Objetivo    : Captura Porcenta de Descuento M�ximo de tabla gn-clids
*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Shared Variable Definitions ---                                       */
DEFINE SHARED TEMP-TABLE PEDI LIKE FacDPedi.
DEFINE SHARED VARIABLE S-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE S-USER-ID AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC  AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV  AS CHAR.
DEFINE SHARED VARIABLE S-CODVEN  AS CHAR.
DEFINE SHARED VARIABLE S-CODCLI  AS CHAR.
DEFINE SHARED VARIABLE S-CODMON  AS INTEGER.
DEFINE SHARED VARIABLE S-CODALM  AS CHAR.
DEFINE SHARED VARIABLE s-nivel   AS CHAR.
DEFINE SHARED VARIABLE S-TPOCMB  AS DECIMAL.  

DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.

DEFINE SHARED VARIABLE s-tipo     AS CHAR.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE F-DTOMAX AS DECIMAL   NO-UNDO.
DEFINE VARIABLE S-OK     AS Logical NO-UNDO.

DEFINE VARIABLE F-PREVTA LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE F-PorImp LIKE Almmmatg.PreBas NO-UNDO.
DEFINE BUFFER B-PEDI FOR PEDI.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE VARIABLE x-password AS LOGICAL NO-UNDO INITIAL FALSE.

DEFINE VARIABLE X-VALVTA AS DECIMAL.
DEFINE VARIABLE X-NEWPRE AS DECIMAL.
DEFINE VARIABLE X-MARGEN AS DECIMAL.

/*ML01* ***/
DEFINE VARIABLE cl-codcia AS INTEGER INITIAL 0 NO-UNDO.
FOR Empresas FIELDS
    (Empresas.CodCia Empresas.Campo-CodCli) WHERE
    Empresas.CodCia = S-CODCIA NO-LOCK:
END.
IF NOT Empresas.Campo-CodCli THEN cl-codcia = S-CODCIA.
/*ML01* ***/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PEDI Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table PEDI.codmat Almmmatg.DesMat Almmmatg.DesMar PEDI.UndVta PEDI.CanPed PEDI.PreUni PEDI.PorDto PEDI.Por_DSCTOS[1] Pedi.Por_DSCTOS[3] PEDI.ImpLin   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table PEDI.Por_DSCTOS[1]   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table PEDI
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table PEDI
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH PEDI  NO-LOCK, ~
             FIRST Almmmatg OF PEDI NO-LOCK       BY PEDI.NroItm
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH PEDI  NO-LOCK, ~
             FIRST Almmmatg OF PEDI NO-LOCK       BY PEDI.NroItm.
&Scoped-define TABLES-IN-QUERY-br_table PEDI Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table PEDI
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table RECT-11 
&Scoped-Define DISPLAYED-OBJECTS F-TotBrt F-ImpExo F-ImpDes F-ValVta ~
F-ImpIsc F-ImpIgv F-ImpTot 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F-ImpDes AS DECIMAL FORMAT "-Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpExo AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpIgv AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpIsc AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpTot AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-TotBrt AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ValVta AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 1.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      PEDI, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      PEDI.codmat COLUMN-LABEL "Articulo"
      Almmmatg.DesMat FORMAT "X(33)"
      Almmmatg.DesMar FORMAT "X(10)"
      PEDI.UndVta COLUMN-LABEL "Unidad" FORMAT "X(10)"
      PEDI.CanPed COLUMN-LABEL "Cantidad" FORMAT ">>>,>>>.99"
      PEDI.PreUni COLUMN-LABEL "Precio!Unitario" FORMAT ">>,>>9.999999"
      PEDI.PorDto COLUMN-LABEL "% Dscto.!1" FORMAT "->>9.99" 
      PEDI.Por_DSCTOS[1] COLUMN-LABEL "% Dscto.!2" FORMAT "->>9.9999" 
      Pedi.Por_DSCTOS[3] COLUMN-LABEL  "%Margen" FORMAT "->>9.99"
      PEDI.ImpLin COLUMN-LABEL "Importe" FORMAT ">,>>>,>>9.99"
  ENABLE
        PEDI.Por_DSCTOS[1]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 91 BY 7.19
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     F-TotBrt AT ROW 8.85 COL 12.57 NO-LABEL
     F-ImpExo AT ROW 8.85 COL 23.29 NO-LABEL
     F-ImpDes AT ROW 8.85 COL 32.14 COLON-ALIGNED NO-LABEL
     F-ValVta AT ROW 8.85 COL 43.29 COLON-ALIGNED NO-LABEL
     F-ImpIsc AT ROW 8.85 COL 54.29 COLON-ALIGNED NO-LABEL
     F-ImpIgv AT ROW 8.85 COL 65.43 COLON-ALIGNED NO-LABEL
     F-ImpTot AT ROW 8.85 COL 76.72 COLON-ALIGNED NO-LABEL
     "T.Exonerado" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 8.35 COL 23.43
     "I.G.V." VIEW-AS TEXT
          SIZE 5.14 BY .5 AT ROW 8.31 COL 70.57
     "Valor Venta" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 8.35 COL 46.86
     "I.S.C." VIEW-AS TEXT
          SIZE 4.57 BY .5 AT ROW 8.35 COL 59.43
     "Total Bruto" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 8.35 COL 14.29
     "Total Importe" VIEW-AS TEXT
          SIZE 9.29 BY .5 AT ROW 8.35 COL 79
     "T.Descuento" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 8.35 COL 34.57
     RECT-11 AT ROW 8.23 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 9.35
         WIDTH              = 97.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F-ImpDes IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ImpExo IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F-ImpIgv IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ImpIsc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ImpTot IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-TotBrt IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F-ValVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM

OPEN QUERY {&SELF-NAME} FOR EACH PEDI  NO-LOCK,
      FIRST Almmmatg OF PEDI NO-LOCK
      BY PEDI.NroItm
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/*  Modifico: Rosa D�az Poma 
    Fecha   : 16/10/2009
    Motivo  : Permite porcentaje de descuento mayor al permitido.
*/

DEFINE VARIABLE x-cto1    AS DECIMAL.
DEFINE VARIABLE x-cto2    AS DECIMAL.
DEFINE VARIABLE x-uni     AS DECIMAL.
DEFINE VARIABLE x-cto     AS DECIMAL.

ON "RETURN":U OF PEDI.Por_DSCTOS[1]
DO:
   APPLY "TAB":U.
   RETURN NO-APPLY.
END.

    
ON "LEAVE":U OF PEDI.Por_DSCTOS[1]
DO:
  FIND Almmmatg WHERE 
       Almmmatg.CodCia = S-CODCIA AND  
       Almmmatg.codmat = PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
       NO-LOCK NO-ERROR.
       
  DEFINE VARIABLE pre1 AS DECIMAL.
  DEFINE VARIABLE pre2 AS DECIMAL.
  DEFINE VARIABLE dsct_disp AS DECIMAL.
  DEFINE VARIABLE X-NEWDSC AS DECIMAL.
  DEFINE VARIABLE F-DSCTO AS DECIMAL.
  DEF VAR f-DtoMax AS DEC NO-UNDO.
       
  pre1 = 0.
  pre2 = 0.
  dsct_disp = 0.
       
  X-NEWDSC = DEC(PEDI.PorDto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
  X-NEWPRE = DEC(PEDI.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

/*ML01* Inicio de bloque ***/
    FIND gn-clieds WHERE
        gn-clieds.CodCia = cl-CodCia AND
        gn-clieds.CodCli = s-CodCli AND
        ((gn-clieds.fecini = ? AND gn-clieds.fecfin = ?) OR
        (gn-clieds.fecini = ? AND gn-clieds.fecfin >= TODAY) OR
        (gn-clieds.fecini <= TODAY AND gn-clieds.fecfin = ?) OR
        (gn-clieds.fecini <= TODAY AND gn-clieds.fecfin >= TODAY))
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clieds THEN DO:
        f-DtoMax = gn-clieds.dscto.
        /*RD01*****
        IF DECIMAL(SELF:SCREEN-VALUE) > f-DtoMax THEN DO:
        
            MESSAGE
                "M�ximo Descuento Permitido es : " f-DtoMax " %"
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.            
        END.
        ***********/
    END.
    ELSE DO:
/*ML01* Fin de bloque ***/
  FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.
  FIND Almtabla WHERE 
       almtabla.Tabla =  "NA" AND  
       almtabla.Codigo = s-nivel
       NO-LOCK NO-ERROR.
  CASE almtabla.Codigo:
      WHEN "D1" THEN f-DtoMax = FacCfgGn.DtoMax.
      WHEN "D2" THEN f-DtoMax = FacCfgGn.DtoDis.
      WHEN "D3" THEN f-DtoMax = FacCfgGn.DtoMay.
      WHEN "D4" THEN f-DtoMax = FacCfgGn.DtoPro.
      OTHERWISE DO:
          MESSAGE "Nivel de Usuario no existe : " s-nivel
              VIEW-AS ALERT-BOX WARNING.
          APPLY "ENTRY" TO PEDI.Por_DSCTOS[1].
          RETURN NO-APPLY.
      END.
  END CASE.


  IF DEC(PEDI.Por_DSCTOS[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > f-DtoMax THEN DO:
      /*ASSIGN s-tipo = 'Si'.*/
/****
      MESSAGE "M�ximo Descuento permitido" SKIP
          "para el nivel de Usuario" SKIP
          TRIM(s-nivel) " : " almtabla.Nombre " es : " f-DtoMax " %" SKIP
          VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO PEDI.Por_DSCTOS[1].
      RETURN NO-APPLY.
******/      
  END.


/*ML01*/ END.

  dsct_disp = ROUND((1 - (1 - Almmmatg.PorMax / 100) / (1 - PEDI.PorDto / 100)), 4) * 100.

  /**** Habilitar en caso de que el precio se quiera guardar ya aplicado el dscto. adicional    ***/
  X-NEWDSC = X-NEWDSC /*+ DEC(PEDI.Por_DSCTOS[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})*/.

  /* O.G.L. DESCUENTOS ADICIONALES */
  F-DSCTO = DEC(PEDI.Por_DSCTOS[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
  X-NEWDSC = ROUND(((1 - (1 - PEDI.PorDto / 100) * (1 - F-DSCTO / 100))) * 100 , 4).

  X-NEWPRE = ((PEDI.Preuni / ( 1 - PEDI.Por_DSCTOS[1] / 100 ) - (PEDI.Preuni  / ( 1 - PEDI.Por_DSCTOS[1] / 100 ) * (F-DSCTO / 100) ))).
  PEDI.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(X-NEWPRE).

  /* CALCULO DEL MARGEN */
  /* RHC 13.12.2010 Margen de Utilidad */
  DEF VAR pError AS CHAR NO-UNDO.
  DEF VAR X-MARGEN AS DEC NO-UNDO.
  DEF VAR X-LIMITE AS DE NO-UNDO.
  RUN vtagn/p-margen-utilidad (
      PEDI.CodMat,      /* Producto */
      x-NewPre,         /* Precio de venta unitario */
      PEDI.UndVta,
      s-CodMon,         /* Moneda de venta */
      s-TpoCmb,         /* Tipo de cambio */
      NO,               /* Muestra el error */
      OUTPUT x-Margen,        /* Margen de utilidad */
      OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
      OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
      ).

/*   ASSIGN                                                      */
/*       x-cto1 = 0                                              */
/*       x-cto2 = 0                                              */
/*       x-cto  = 0                                              */
/*       x-uni  = 0                                              */
/*       x-uni     = X-NEWPRE                                    */
/*       x-margen  = Almmmatg.CtoTot.                            */
/*   IF Almmmatg.MonVta = 1 THEN                                 */
/*       ASSIGN                                                  */
/*         x-cto1 = x-margen                                     */
/*         x-cto2 = ROUND(x-margen / S-TPOCMB,4).                */
/*   ELSE                                                        */
/*       ASSIGN                                                  */
/*           x-cto2 = x-margen                                   */
/*           x-cto1 = ROUND(x-margen * S-TPOCMB,4).              */
/*   ASSIGN                                                      */
/*       X-cto    = IF S-CODMON = 1 THEN x-cto1 ELSE x-cto2      */
/*       x-margen = ROUND((( x-uni - x-cto ) / x-cto) * 100 ,2). */

  PEDI.Por_DSCTOS[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(x-margen).

END.


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp-Total B-table-Win 
PROCEDURE Imp-Total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE F-IGV AS DECIMAL INIT 0 NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL INIT 0 NO-UNDO.
ASSIGN F-ImpDes = 0
       F-ImpExo = 0
       F-ImpIgv = 0
       F-ImpIsc = 0
       F-ImpTot = 0
       F-TotBrt = 0
       F-ValVta = 0.
FOR EACH B-PEDI:
    F-ImpTot = F-ImpTot + B-PEDI.ImpLin.
    F-Igv = F-Igv + B-PEDI.ImpIgv.
    F-Isc = F-Isc + B-PEDI.ImpIsc.
    F-ImpDes = F-ImpDes + B-PEDI.ImpDto.
    IF NOT B-PEDI.AftIgv THEN F-ImpExo = F-ImpExo + B-PEDI.ImpLin.
END.
F-ImpIgv = ROUND(F-Igv,2).
F-ImpIsc = ROUND(F-Isc,2).
F-TotBrt = F-ImpTot - F-ImpIgv - F-ImpIsc + F-ImpDes - F-ImpExo.
F-ValVta = F-TotBrt -  F-ImpDes.

/**********************
   /*** DESCUENTO GLOBAL ****/
   IF B-cpedm.PorDto > 0 THEN DO:
      /**** Add by C.Q. 29/03/2000  ****/
      /*********************************/
      B-CPEDM.ImpDto = B-CPEDM.ImpDto + ROUND(B-CPEDM.ImpTot * B-cpedm.PorDto / 100,2).
      B-CPEDM.ImpTot = ROUND(B-CPEDM.ImpTot * (1 - B-cpedm.PorDto / 100),2).
      B-CPEDM.ImpVta = ROUND(B-CPEDM.ImpTot / (1 + B-CPEDM.PorIgv / 100),2).
      B-CPEDM.ImpIgv = B-CPEDM.ImpTot - B-CPEDM.ImpVta.
      B-CPEDM.ImpBrt = B-CPEDM.ImpTot - B-CPEDM.ImpIgv - B-CPEDM.ImpIsc + 
                       B-CPEDM.ImpDto - B-CPEDM.ImpExo.
   END.
*********************/

DISPLAY F-ImpDes
        F-ImpExo
        F-ImpIgv
        F-ImpIsc
        F-ImpTot
        F-TotBrt
        F-ValVta WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND Almmmatg WHERE 
       Almmmatg.CodCia = S-CODCIA  AND  
       Almmmatg.codmat = PEDI.codmat 
       NO-LOCK NO-ERROR.
  ASSIGN 
      PEDI.PreUni = DEC(PEDI.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      PEDI.Por_Dsctos[1] = DEC(PEDI.Por_Dsctos[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})    /* Add by C.Q. 23/03/2000 */
      PEDI.Por_Dsctos[3] = DEC(PEDI.Por_Dsctos[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})    /* Add by C.Q. 23/03/2000 */
      PEDI.ImpDto = PEDI.ImpDto + ROUND( PEDI.PreBas * PEDI.CanPed * (1 - PEDI.PorDto / 100) * (PEDI.Por_Dsctos[1] / 100),4 )
      PEDI.ImpLin = ROUND( PEDI.PreUni * PEDI.CanPed , 2 ).
  IF PEDI.AftIsc THEN PEDI.ImpIsc = ROUND(PEDI.PreBas * PEDI.CanPed * (Almmmatg.PorIsc / 100),4).
  IF PEDI.AftIgv THEN PEDI.ImpIgv = PEDI.ImpLin - ROUND(PEDI.ImpLin  / (1 + (FacCfgGn.PorIgv / 100)),4).
  
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-busca B-table-Win 
PROCEDURE local-busca :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  ASSIGN  input-var-1 = ""
          input-var-2 = ""
          input-var-3 = ""
          output-var-1 = ?
          OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").

  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    /*RUN PL/C-XXX.W("").*/
    IF OUTPUT-VAR-1 <> ? THEN DO:
         FIND {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
              ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = OUTPUT-VAR-1
              NO-LOCK NO-ERROR.
         IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            REPOSITION {&BROWSE-NAME}  TO ROWID OUTPUT-VAR-1.
         END.
    END.
  END.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Imp-Total.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
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
  
  RUN Imp-Total .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros B-table-Win 
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
        WHEN "" THEN.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros B-table-Win 
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
        WHEN "" THEN ASSIGN input-var-1 = "".
        WHEN "" THEN ASSIGN input-var-1 = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "PEDI"}
  {src/adm/template/snd-list.i "Almmmatg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida B-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  EN CASO DE ERROR RETORNAR : RETURN "ADM-ERROR"
------------------------------------------------------------------------------*/

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Consistenciar la modificacion de la fila
  Parameters:  Retornar "ADM-ERROR" en caso de bloquear la modificacion
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE F-DES1 AS DECIMAL.
IF NOT AVAILABLE PEDI THEN RETURN "ADM-ERROR".
F-DES1 = PEDI.PorDto.
F-DTOMAX = ROUND(1 - (Almmmatg.PorMax / 100 - 1) / (1 - F-DES1 / 100),2).
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

