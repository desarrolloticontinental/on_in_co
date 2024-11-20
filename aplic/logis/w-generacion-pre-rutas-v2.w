&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  
  Output Parameters:
      <none>

  History: 
          
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

DEFINE SHARED VAR s-codcia AS INT.
DEFINE SHARED VAR cl-codcia AS INT.
DEFINE SHARED VAR s-coddiv AS CHAR.
DEFINE SHARED VAR s-user-id AS CHAR.

FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = s-coddiv NO-LOCK.
IF gn-divi.Campo-Char[6] <> "PR" THEN DO:
    MESSAGE 'Su divisi�n NO est� configurada para PICKING POR RUTAS' VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.

DEFINE VAR x-emision-desde AS DATE.
DEFINE VAR x-emision-hasta AS DATE.
DEFINE VAR x-tot-ordenes AS INT INIT 0.
DEFINE VAR x-tot-ordenes-generadas AS INT INIT 0.

/* Si adiciona un campo a esta tabla tambien hacerlo en logis/d-resumen-pre-rutas.w y d-pre-rutas-x-distrito.w*/
DEFINE TEMP-TABLE ttResumen
    FIELD   tcuadrante     AS  CHAR    FORMAT 'x(5)'
    FIELD   ttotclie    AS  INT     INIT 0
    FIELD   ttotpeso    AS  DEC     INIT 0
    FIELD   ttotvol     AS  DEC     INIT 0
    FIELD   ttotimp     AS  DEC     INIT 0
    FIELD   ttotord     AS  INT     INIT 0
    FIELD   tnro-phruta AS CHAR     FORMAT 'x(15)'
    FIELD   tobserva    AS CHAR     FORMAT 'x(15)'
    FIELD   tswok       AS CHAR     FORMAT 'x(1)' INIT ""
.
/* Si adiciona un campo a esta tabla tambien hacerlo en d-pre-rutas-x-distrito*/
DEFINE TEMP-TABLE   ttDetalle
    FIELD   tcuadrante     AS  CHAR    FORMAT 'x(5)'
    FIELD   tcoddoc     AS  CHAR    FORMAT '(5)'
    FIELD   tnroped     AS  CHAR    FORMAT 'x(15)'
    FIELD   tcodcli     AS  CHAR    FORMAT 'x(12)'
    FIELD   ttotpeso    AS  DEC     INIT 0
    FIELD   ttotvol     AS  DEC     INIT 0
    FIELD   ttotimp     AS  DEC     INIT 0
    FIELD   tubigeo     AS  CHAR    FORMAT 'x(8)'
    FIELD   titems      AS  INT     INIT 0
    FIELD   tfchped     AS  DATE
    FIELD   tfchent     AS  DATE
    FIELD   tnomcli     AS  CHAR
.
/*
O/D , Cliente , Fecha Entrega,Items,Peso,S/,m3
*/
DEFINE TEMP-TABLE ttCliente
    FIELD   tcuadrante     AS  CHAR    FORMAT 'X(5)'
    FIELD   tcodcli     AS  CHAR    FORMAT 'x(11)'.
    .

DEF VAR s-coddoc AS CHAR INIT 'PHR' NO-UNDO.    /* Pre Hoja de Ruta */


DEFINE TEMP-TABLE x-di-rutaC LIKE di-rutaC.
DEFINE TEMP-TABLE x-di-rutaD LIKE di-rutaD.

DEFINE VAR x-numero-orden AS INT .
DEFINE VAR x-serie-orden AS INT .


IF NOT CAN-FIND(FIRST Faccorre WHERE FacCorre.CodCia = s-codcia 
                AND FacCorre.CodDiv = s-coddiv 
                AND FacCorre.CodDoc = s-coddoc 
                AND FacCorre.FlgEst = YES
                NO-LOCK) THEN DO:
    MESSAGE "NO definido el correlativo para el documento:" +  s-coddoc SKIP
        'Proceso Abortado' VIEW-AS ALERT-BOX INFORMATION.
    RETURN ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD evalua-condicion W-Win 
FUNCTION evalua-condicion RETURNS LOGICAL
  ( INPUT pValor1 AS DEC, INPUT pValor2 AS DEC, INPUT pDato AS DEC, INPUT pCondLogica AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-3 
     LABEL "Calcular Pre-Rutas" 
     SIZE 52 BY 2.15
     FONT 8.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-3 AT ROW 2.08 COL 9 WIDGET-ID 12 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.72 BY 23.23 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Generacion de Pre-Rutas"
         HEIGHT             = 5.38
         WIDTH              = 81
         MAX-HEIGHT         = 27.12
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 27.12
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Generacion de Pre-Rutas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Generacion de Pre-Rutas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Calcular Pre-Rutas */
DO:
  SESSION:SET-WAIT-STATE("GENERAL").
  RUN procesar.
  SESSION:SET-WAIT-STATE("").
  RUN generar-pre-rutas.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE BUTTON-3 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generar-pre-rutas W-Win 
PROCEDURE generar-pre-rutas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR x-peso-desde AS DEC INIT 0.
DEFINE VAR x-peso-hasta AS DEC INIT 0.
DEFINE VAR x-vol-desde AS DEC INIT 0.
DEFINE VAR x-vol-hasta AS DEC INIT 0.
DEFINE VAR x-imp-desde AS DEC INIT 0.
DEFINE VAR x-imp-hasta AS DEC INIT 0.

DEFINE VAR x-condicion AS LOG.
DEFINE VAR x-procesado AS CHAR.

x-tot-ordenes = 0.
x-tot-ordenes-generadas = 0.

EMPTY TEMP-TABLE x-di-rutaC.
EMPTY TEMP-TABLE x-di-rutaD.

SESSION:SET-WAIT-STATE("GENERAL").

x-numero-orden = 0.
x-serie-orden = 0.

FOR EACH ttResumen :
    FIND FIRST rut-cuadrante-cab WHERE rut-cuadrante-cab.codcia = s-codcia AND
        rut-cuadrante-cab.cuadrante = ttResumen.tcuadrante NO-LOCK NO-ERROR.
    /* Solo aquellos que tengan ubicado el CUADRANTE */
    IF NOT AVAILABLE rut-cuadrante-cab THEN NEXT.
    /**/
    CONFIGURACION:
    FOR EACH VtaCTabla WHERE VtaCTabla.codcia = s-codcia AND 
        VtaCTabla.tabla = 'COND-PRE-HRUTA' NO-LOCK BY VtaCTabla.libre_d03:
        x-condicion = NO.
        x-procesado = "".
        /* Camiones */
        CAMIONES:        
        FOR EACH rut-conf-phr WHERE rut-conf-phr.codcia = s-codcia AND 
            rut-conf-phr.tabla = VtaCTabla.tabla AND
            rut-conf-phr.llave = VtaCTabla.llave NO-LOCK BY rut-conf-phr.libre_d10 :
            x-condicion = NO.

            x-peso-hasta = rut-conf-phr.libre_d01.
            x-peso-desde = x-peso-hasta * ((100 - rut-conf-phr.libre_d04) / 100).

            x-condicion = evalua-condicion(x-peso-desde, x-peso-hasta, ttResumen.ttotpeso, TRIM(rut-conf-phr.libre_c01)).
            /* Si la condicion es FALSO siguiente CAMION */
            IF x-condicion = NO THEN NEXT.

            x-vol-hasta = rut-conf-phr.libre_d02.
            x-vol-desde = x-vol-hasta * ((100 - rut-conf-phr.libre_d05) / 100).

            x-condicion = evalua-condicion(x-vol-desde, x-vol-hasta, ttResumen.ttotvol, TRIM(rut-conf-phr.libre_c02)).
            /* Si la condicion es FALSO siguiente CAMION */
            IF x-condicion = NO THEN NEXT.

            x-imp-hasta = rut-conf-phr.libre_d03.
            x-imp-desde = x-imp-hasta * ((100 - rut-conf-phr.libre_d06) / 100).

            x-condicion = evalua-condicion(x-imp-desde, x-imp-hasta, ttResumen.ttotimp, TRIM(rut-conf-phr.libre_c03)).
            /* Si la condicion es FALSO siguiente CAMION */
            IF x-condicion = YES THEN DO:
                /*  */      
                x-procesado = "".
                RUN grabar-ordenes-en-pre-ruta(INPUT ttResumen.tcuadrante, OUTPUT x-procesado, INPUT STRING(rut-conf-phr.libre_d10)).
                LEAVE CONFIGURACION.
            END.
        END.
        IF x-condicion = NO AND x-procesado = "" THEN DO:
            /* Carros en TRANSITO */
            x-procesado = "".
            RUN grabar-ordenes-en-pre-ruta(INPUT ttResumen.tcuadrante, OUTPUT x-procesado, INPUT "CARRO DE TRANSITO").
            IF x-procesado = "OK" THEN DO:
                /**/
            END.
        END.
        ELSE DO:
            /* Hubo problemas al crear las PRE-RUTAS */
        END.
    END.
END.
/* Elimino los cuadrante que no generaron Pre-Ruta */
FOR EACH ttResumen WHERE ttResumen.tswok = '' :
    /*MESSAGE 'eliminado' ttdetalle.tcoddoc ttdetalle.tnroped.*/
    FOR EACH ttDetalle WHERE ttDetalle.tcuadrante = ttResumen.tcuadrante :
        DELETE ttDetalle.
    END.
    DELETE ttResumen.
END.
SESSION:SET-WAIT-STATE("").
/* FOR EACH ttdetalle:                              */
/*     MESSAGE ttdetalle.tcoddoc ttdetalle.tnroped. */
/* END.                                             */
x-procesado = "".
RUN logis/d-modificacion-pre-rutas.r(INPUT TABLE ttResumen, 
                                     INPUT TABLE ttDetalle, 
                                     INPUT TABLE x-di-rutaC,
                                     INPUT TABLE x-di-rutaD,
                                     OUTPUT x-procesado).
/* /* A mostrar la Pre-Rutas x distrito */                                                               */
/* x-procesado = "".                                                                                     */
/* RUN logis/d-pre-rutas-x-distrito.r(INPUT TABLE ttResumen, INPUT TABLE ttDetalle, OUTPUT x-procesado). */
/* IF x-procesado = 'SI' THEN DO:                                                                        */
/*     /* La siguiente pantalla UNIR pre-RUtas*/                                                         */
/*     x-procesado = "".                                                                                 */
/*     RUN logis/d-modificacion-pre-rutas.r(INPUT TABLE ttResumen,                                       */
/*                                          INPUT TABLE ttDetalle,                                       */
/*                                          INPUT TABLE x-di-rutaC,                                      */
/*                                          INPUT TABLE x-di-rutaD,                                      */
/*                                          OUTPUT x-procesado).                                         */
/* END.                                                                                                  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grabar-ordenes-en-pre-ruta W-Win 
PROCEDURE grabar-ordenes-en-pre-ruta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCuadrante AS CHAR.
DEFINE OUTPUT PARAMETER pProcesado AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pObserva AS CHAR.

pProcesado = "".

IF NOT CAN-FIND(FIRST Faccorre WHERE FacCorre.CodCia = s-codcia 
                AND FacCorre.CodDiv = s-coddiv 
                AND FacCorre.CodDoc = s-coddoc 
                AND FacCorre.FlgEst = YES
                NO-LOCK) THEN DO:
    pProcesado = "NO definido el correlativo para el documento:" +  s-coddoc.
    RETURN.
END.

DEFINE BUFFER b-ttResumen FOR ttResumen.

pProcesado = "".
GRABAR_DATOS:
DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    x-numero-orden = x-numero-orden + 1.
    DO:
        /* Header update block */
        CREATE x-DI-RutaC.
        ASSIGN
            x-DI-RutaC.CodCia = s-codcia
            x-DI-RutaC.CodDiv = s-coddiv
            x-DI-RutaC.CodDoc = s-coddoc
            x-DI-RutaC.FchDoc = TODAY
            x-DI-RutaC.NroDoc = STRING(x-serie-orden, '999') + 
                                STRING(x-numero-orden, '999999')
            x-DI-RutaC.codrut = "AUTOMATICO"    /*pGlosa*/
            x-DI-RutaC.observ = pObserva
            x-DI-RutaC.usuario = s-user-id
            x-DI-RutaC.flgest  = "PX".     /* Pendiente x Pickear*/
        /* Datos de Topes M�ximos */
        ASSIGN
            x-DI-RutaC.Libre_d01 = ttResumen.ttotclie
            x-DI-RutaC.Libre_d02 = ttResumen.ttotpeso
            x-DI-RutaC.Libre_d03 = ttResumen.ttotvol.
        ASSIGN
/*             x-DI-RutaC.Libre_f01 = FILL-IN-Desde                          */
/*             x-DI-RutaC.Libre_f02 = FILL-IN-Hasta                          */
/*             x-DI-RutaC.Libre_c01 = COMBO-BOX-corte    /* Hora de Corte */ */
            x-DI-RutaC.Libre_c02 = ""                 /* FILL-IN_NroPed */ 
            x-DI-RutaC.Libre_c03 = ""                 /* FILL-IN_Cliente. */
            NO-ERROR
        .
        IF ERROR-STATUS:ERROR = YES  THEN DO:
            UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
        END.
    END.
    /* Detalle de Ordenes */
    FOR EACH ttDetalle WHERE ttDetalle.tcuadrante = pCuadrante :
        CREATE x-DI-RutaD.
        ASSIGN
            x-DI-RutaD.CodCia = x-DI-RutaC.CodCia
            x-DI-RutaD.CodDiv = x-DI-RutaC.CodDiv
            x-DI-RutaD.CodDoc = x-DI-RutaC.CodDoc
            x-DI-RutaD.NroDoc = x-DI-RutaC.NroDoc
            x-DI-RutaD.CodRef = ttDetalle.tcoddoc
            x-DI-RutaD.NroRef = ttDetalle.tnroped.
        
        ASSIGN
            x-DI-RutaD.ImpCob    = ttDetalle.ttotvol
            x-DI-RutaD.Libre_d01 = ttDetalle.ttotpeso
            x-DI-RutaD.Libre_d02 = ttDetalle.ttotimp
            x-DI-RutaD.Libre_c01 = "0"                         /* STRING(T-RUTAD.Bultos). */
            NO-ERROR
        .
        IF ERROR-STATUS:ERROR = YES  THEN DO:
            UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
        END.

    END.
    /*  */
    FIND FIRST b-ttResumen WHERE b-ttResumen.tcuadrante = pCuadrante EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE b-ttResumen THEN DO:
        ASSIGN b-ttResumen.tnro-phruta = x-DI-RutaC.NroDoc
                b-ttResumen.tobserva = pObserva
                b-ttResumen.tswok = 'S'.
    END.
    ELSE DO:
        UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
    END.
    pProcesado = "OK".
END. /* TRANSACTION block */
RELEASE b-ttResumen.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar W-Win 
PROCEDURE procesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR x-sec AS INT.
DEFINE VAR x-coddiv AS CHAR.

EMPTY TEMP-TABLE ttResumen.
EMPTY TEMP-TABLE ttDetalle.
EMPTY TEMP-TABLE ttCliente.

DO WITH FRAME {&FRAME-NAME}:
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND
        gn-divi.campo-log[1] = NO:
        x-coddiv = gn-divi.coddiv.
        RUN procesar-ordenes (INPUT x-coddiv, INPUT "O/D").
        RUN procesar-ordenes (INPUT x-coddiv, INPUT "OTR").
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar-orden W-Win 
PROCEDURE procesar-orden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pCodDoc AS CHAR.
DEFINE INPUT PARAMETER pNroDoc AS CHAR.
DEFINE OUTPUT PARAMETER pMsg AS CHAR.

DEFINE VAR x-codped AS CHAR INIT "".
DEFINE VAR x-nroped AS CHAR INIT "".

DEFINE BUFFER x-faccpedi FOR faccpedi.
DEFINE BUFFER x-pedido FOR faccpedi.

DEFINE VAR x-hora-emision AS CHAR.

FIND FIRST x-faccpedi WHERE x-faccpedi.codcia = s-codcia AND
    x-faccpedi.coddoc = pCoddoc AND
    x-faccpedi.nroped = pNroDoc NO-LOCK NO-ERROR.
IF NOT AVAILABLE x-faccpedi THEN DO:
    pMsg = "Orden no existe " + pCoddoc + " " + pNroDoc.
    /*MESSAGE x-faccpedi.coddoc x-faccpedi.nroped SKIP pmsg.*/
    RETURN "ADM-ERROR".
END.

/* Validar la HORA de corte vs la emision */
x-hora-emision = x-faccpedi.hora.
IF TRUE <> (x-hora-emision > "") THEN DO:
    x-hora-emision = "00:00".
END.
IF LENGTH(x-hora-emision) > 5 THEN x-hora-emision = SUBSTRING(x-hora-emision,1,5).

/* IF x-faccpedi.fchped = fill-in-hasta AND x-hora-emision <= combo-box-corte  THEN DO: */
/*     pMsg = "La HORA esta fuera del CORTE " + pCoddoc + " " + pNroDoc.                */
/*     MESSAGE x-faccpedi.coddoc x-faccpedi.nroped SKIP pmsg.                           */
/*     RETURN "ADM-ERROR".                                                              */
/* END.                                                                                 */

/* Pedido */
/* Ubicacion de coordenadas */
DEFINE VAR x-ubigeo AS CHAR INIT "".
DEFINE VAR x-longuitud AS DEC INIT 0.
DEFINE VAR x-latitud AS DEC INIT 0.
DEFINE VAR x-filer AS DEC INIT 0.
DEFINE VAR x-cuadrante AS CHAR INIT "".
DEFINE VAR x-items AS INT.

x-codped = x-faccpedi.codref.
x-nroped = x-faccpedi.nroref.
CASE TRUE:
    WHEN pCodDoc = "O/D" OR pCodDoc = "O/M" THEN DO:
        FIND FIRST x-pedido WHERE x-pedido.codcia = s-codcia AND
                                    x-pedido.coddoc = x-codped AND 
                                    x-pedido.nroped = x-nroped NO-LOCK NO-ERROR.
        IF NOT AVAILABLE x-pedido THEN DO:
            pMsg = "Pedido no existe " + x-codped + " " + x-nroped.
            RETURN "ADM-ERROR".
        END.
        RUN logis/p-datos-sede-auxiliar(INPUT x-pedido.ubigeo[2], 
                                        INPUT x-pedido.ubigeo[3],
                                        INPUT x-pedido.ubigeo[1],
                                        OUTPUT x-ubigeo,
                                        OUTPUT x-longuitud,
                                        OUTPUT x-latitud).
    END.
    WHEN pCodDoc = "OTR" THEN DO:
        RUN logis/p-datos-sede-auxiliar(INPUT x-faccpedi.ubigeo[2], 
                                        INPUT x-faccpedi.ubigeo[3],
                                        INPUT x-faccpedi.ubigeo[1],
                                        OUTPUT x-ubigeo,
                                        OUTPUT x-longuitud,
                                        OUTPUT x-latitud).
    END.
END CASE.

IF TRUE <> (x-ubigeo > "") THEN DO:
    x-cuadrante = "ERR".
END.
ELSE DO:
    RUN logis/p-cuadrante(INPUT x-longuitud, INPUT x-latitud, OUTPUT x-cuadrante).
    IF x-cuadrante = "" THEN x-cuadrante = "P0".  /*XYZ*/
END.

pMsg = "".
/* Resumen */
FIND FIRST ttResumen WHERE ttResumen.tcuadrante = x-cuadrante NO-ERROR.
IF NOT AVAILABLE ttResumen THEN DO:
    CREATE ttResumen.
        ASSIGN ttResumen.tcuadrante = x-cuadrante.
END.
ASSIGN ttResumen.ttotord = ttResumen.ttotord + 1.

/* Ordenes x Cuadrante */
FIND FIRST ttDetalle WHERE ttDetalle.tcuadrante = x-cuadrante AND
    ttDetalle.tcoddoc = pCodDoc AND
    ttDetalle.tnroped = pNroDoc NO-ERROR.
IF NOT AVAILABLE ttDEtalle THEN DO:
    CREATE ttDetalle.
    ASSIGN 
        ttDetalle.tcuadrante = x-cuadrante
        ttDetalle.tcoddoc = pCodDoc
        ttDetalle.tnroped = pNroDoc
        ttDetalle.tubigeo = x-ubigeo
        ttDetalle.tcodcli = x-faccpedi.codcli
        ttDetalle.tfchped  = x-faccpedi.fchped
        ttDetalle.tfchent  = x-faccpedi.fchent
        ttDetalle.tnomcli = x-faccpedi.nomcli
        .
END.
/* Clientes x Cuadrante */
FIND FIRST ttCliente WHERE ttCliente.tcuadrante = x-cuadrante AND
    ttCliente.tcodcli = x-faccpedi.codcli NO-ERROR.
IF NOT AVAILABLE ttCliente THEN DO:
    ASSIGN 
        ttResumen.ttotcli = ttResumen.ttotcli + 1.
    /**/
    CREATE ttCliente.
    ASSIGN 
        ttCliente.tcuadrante = x-cuadrante
        ttCliente.tcodcli = x-faccpedi.codcli
    .
END.
/* RHC TOTALES */
/* Totales x Resumen */
ASSIGN 
    ttResumen.ttotpeso = ttResumen.ttotpeso + x-Faccpedi.Peso
    ttResumen.ttotvol = ttResumen.ttotvol + x-Faccpedi.Volumen
    ttResumen.ttotimp = ttResumen.ttotimp + x-Faccpedi.AcuBon[8].
    .
/* Totales de la Orden */
ASSIGN 
    ttDetalle.ttotpeso = ttDetalle.ttotpeso + x-Faccpedi.Peso
    ttDetalle.ttotvol = ttDetalle.ttotvol + x-Faccpedi.Volumen
    ttDetalle.ttotimp = ttDetalle.ttotimp + x-Faccpedi.AcuBon[8]
    ttDetalle.titems = ttDetalle.titems + x-Faccpedi.Items
    .
/* CASE pCodDoc:                                                                                                                                                         */
/*     WHEN "OTR" THEN DO:                                                                                                                                               */
/*         FOR EACH facdpedi WHERE facdpedi.codcia = s-codcia AND                                                                                                        */
/*             facdpedi.coddoc = pCodDoc AND                                                                                                                             */
/*             facdpedi.nroped = pNroDoc NO-LOCK:                                                                                                                        */
/*             FIND FIRST almmmatg OF facdpedi NO-LOCK NO-ERROR.                                                                                                         */
/*             /* Totales x Resumen */                                                                                                                                   */
/*             ASSIGN                                                                                                                                                    */
/*                 ttResumen.ttotpeso = ttResumen.ttotpeso + ((facdpedi.canped * facdpedi.factor) * almmmatg.pesmat)                                                     */
/*                 ttResumen.ttotvol = ttResumen.ttotvol + ((facdpedi.canped * facdpedi.factor) * (almmmatg.libre_d02 / 1000000))                                        */
/*                 ttResumen.ttotimp = ttResumen.ttotimp + (Almmmatg.CtoLis * facdpedi.canped * facdpedi.factor) * (IF Almmmatg.MonVta = 2 THEN Almmmatg.TpoCmb ELSE 1). */
/*                 .                                                                                                                                                     */
/*             /* Totales de la Orden */                                                                                                                                 */
/*             ASSIGN                                                                                                                                                    */
/*                 ttDetalle.ttotpeso = ttDetalle.ttotpeso + ((facdpedi.canped * facdpedi.factor) * almmmatg.pesmat)                                                     */
/*                 ttDetalle.ttotvol = ttDetalle.ttotvol + ((facdpedi.canped * facdpedi.factor) * (almmmatg.libre_d02 / 1000000))                                        */
/*                 ttDetalle.ttotimp = ttDetalle.ttotimp + (Almmmatg.CtoLis * facdpedi.canped * facdpedi.factor) * (IF Almmmatg.MonVta = 2 THEN Almmmatg.TpoCmb ELSE 1). */
/*                 ttDetalle.titems = ttDetalle.titems + 1                                                                                                               */
/*                 .                                                                                                                                                     */
/*         END.                                                                                                                                                          */
/*     END.                                                                                                                                                              */
/*     OTHERWISE DO:                                                                                                                                                     */
/*         FOR EACH facdpedi WHERE facdpedi.codcia = s-codcia AND                                                                                                        */
/*             facdpedi.coddoc = pCodDoc AND                                                                                                                             */
/*             facdpedi.nroped = pNroDoc NO-LOCK:                                                                                                                        */
/*             FIND FIRST almmmatg OF facdpedi NO-LOCK NO-ERROR.                                                                                                         */
/*             /* Totales x Resumen */                                                                                                                                   */
/*             ASSIGN                                                                                                                                                    */
/*                 ttResumen.ttotpeso = ttResumen.ttotpeso + ((facdpedi.canped * facdpedi.factor) * almmmatg.pesmat)                                                     */
/*                 ttResumen.ttotvol = ttResumen.ttotvol + ((facdpedi.canped * facdpedi.factor) * (almmmatg.libre_d02 / 1000000))                                        */
/*                 ttResumen.ttotimp = ttResumen.ttotimp + facdpedi.implin                                                                                               */
/*                 .                                                                                                                                                     */
/*             /* Totales de la Orden */                                                                                                                                 */
/*             ASSIGN                                                                                                                                                    */
/*                 ttDetalle.ttotpeso = ttDetalle.ttotpeso + ((facdpedi.canped * facdpedi.factor) * almmmatg.pesmat)                                                     */
/*                 ttDetalle.ttotvol = ttDetalle.ttotvol + ((facdpedi.canped * facdpedi.factor) * (almmmatg.libre_d02 / 1000000))                                        */
/*                 ttDetalle.ttotimp = ttDetalle.ttotimp + facdpedi.implin                                                                                               */
/*                 ttDetalle.titems = ttDetalle.titems + 1                                                                                                               */
/*                 .                                                                                                                                                     */
/*         END.                                                                                                                                                          */
/*     END.                                                                                                                                                              */
/* END CASE.                                                                                                                                                             */
/*  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar-ordenes W-Win 
PROCEDURE procesar-ordenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       OJO => NO O/M
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCoddiv AS CHAR.
DEFINE INPUT PARAMETER pCodDoc AS CHAR.

/* RHC 28/01/2019 Solo cuando Faccpedi.FlgEst = "P" y Faccpedi.FlgSit = "T" */
DEFINE VAR x-msg AS CHAR.
/* RHC 10/03/2019 Nueva Rutina */
RLOOP:
FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
    Faccpedi.coddiv = pCoddiv AND
    Faccpedi.divdes = s-coddiv AND
    Faccpedi.coddoc = pCodDoc AND
    Faccpedi.FlgEst = 'P' AND          /* PENDIENTES o CERRADOS */
    (FacCPedi.FlgSit = "T" OR FacCPedi.FlgSit = "TG" ):
    CASE TRUE:
        WHEN Faccpedi.CodDoc = "OTR" THEN DO:
            IF Faccpedi.TpoPed = "INC" THEN DO:
                IF NOT Faccpedi.Glosa BEGINS 'INCIDENCIA: MAL ESTADO' THEN NEXT RLOOP.
            END.
            IF Faccpedi.TpoPed = "XD" THEN NEXT RLOOP.
        END.
        WHEN Faccpedi.CodDoc = "O/D" THEN DO:
            IF Faccpedi.TipVta = "SI"  THEN NEXT RLOOP.       /* TRAMITE DOCUMENTARIO */
        END.
    END CASE.
    /* ******************************************************************** */
    /* SOLO SE VAN A ACEPTAR O/D OTR O/M QUE NO TENGAN HISTORIAL EN UNA PHR */
    /* ******************************************************************** */
    FOR EACH Di-RutaD NO-LOCK WHERE Di-RutaD.codcia = s-codcia AND
        Di-RutaD.coddiv = s-coddiv AND
        Di-RutaD.coddoc = "PHR" AND
        Di-RutaD.codref = Faccpedi.coddoc AND
        Di-RutaD.nroref = Faccpedi.nroped,
        FIRST Di-RutaC OF Di-RutaD NO-LOCK WHERE Di-RutaC.FlgEst <> "A":
        /* Cabecera */
        IF Di-RutaC.FlgEst BEGINS "P" THEN NEXT RLOOP.   /* NO debe estar en una PHR en PROCESO */
        /* Detalle */
        IF Di-RutaD.FlgEst = "A" THEN NEXT.         /* NO se considera el ANULADO */
        /* Si est� REPROGRAMADO se acepta, caso contrario no pasa */
        IF Di-RutaD.FlgEst <> "R" THEN NEXT RLOOP.  /* Otra Orden */
    END.
    /* **************************************************************** */
    /* RHC 12/10/2019 Ninguna que venga por reprogramaci�n */
    /* **************************************************************** */
    FIND FIRST Almcdocu WHERE Almcdocu.codcia = s-codcia AND
        Almcdocu.coddoc = Faccpedi.coddoc AND 
        Almcdocu.nrodoc = Faccpedi.nroped AND
        Almcdocu.codllave = s-coddiv AND
        Almcdocu.libre_c01 = 'H/R' NO-LOCK NO-ERROR.
    IF AVAILABLE Almcdocu THEN NEXT.
    /* **************************************************************** */
        /* Grabar */
        x-msg = "".
        RUN procesar-orden(INPUT Faccpedi.coddoc, INPUT Faccpedi.nroped, OUTPUT x-msg).
        IF x-msg = 'OK' THEN DO:
            x-tot-ordenes-generadas = x-tot-ordenes-generadas + 1.
        END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validar-orden W-Win 
PROCEDURE validar-orden PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCodDoc AS CHAR.
DEFINE INPUT PARAMETER pNroDoc AS CHAR.

DEF BUFFER B-RutaC FOR Di-RutaC.
DEF BUFFER B-RUTAD FOR Di-RutaD.
DEF BUFFER B-RUTAG FOR Di-RutaG.
DEFINE BUFFER x-faccpedi FOR faccpedi.

FIND FIRST x-faccpedi WHERE x-faccpedi.codcia = s-codcia AND
                               x-faccpedi.coddoc = pCoddoc AND 
                                x-faccpedi.nroped = pNroDoc NO-LOCK NO-ERROR.
IF NOT AVAILABLE x-faccpedi THEN RETURN "ADM-ERROR".

{dist/valida-od-para-phr.i}

/* ****************
/* Est� en una PHR en tr�mite => NO va */
FIND FIRST DI-RutaD WHERE DI-RutaD.CodCia = s-CodCia AND
    DI-RutaD.CodDiv = s-CodDiv AND
    DI-RutaD.CodDoc = "PHR" AND
    DI-RutaD.CodRef = pCodDoc AND
    DI-RutaD.NroRef = pNroDoc AND
    CAN-FIND(DI-RutaC OF DI-RutaD WHERE DI-RutaC.FlgEst BEGINS "P" NO-LOCK)
    NO-LOCK NO-ERROR.
IF AVAILABLE DI-RutaD THEN RETURN 'ADM-ERROR'.

/* Si tiene una reprogramaci�n por aprobar => No va  */
FIND FIRST Almcdocu WHERE AlmCDocu.CodCia = s-CodCia AND
  AlmCDocu.CodLlave = s-CodDiv AND
  AlmCDocu.CodDoc = pCodDoc AND 
  AlmCDocu.NroDoc = pNroDoc AND
  AlmCDocu.FlgEst = "P" NO-LOCK NO-ERROR.
IF AVAILABLE Almcdocu THEN RETURN 'ADM-ERROR'.

/* Si ya fue entregado o tiene una devoluci�n parcial NO va */
FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
  AND CcbCDocu.CodPed = x-Faccpedi.codref       /* PED */
  AND CcbCDocu.NroPed = x-Faccpedi.nroref
  AND Ccbcdocu.Libre_c01 = x-Faccpedi.coddoc    /* O/D */
  AND Ccbcdocu.Libre_c02 = x-Faccpedi.nroped
  AND Ccbcdocu.FlgEst <> "A":
  IF NOT (Ccbcdocu.coddiv = s-coddiv AND Ccbcdocu.coddoc = "G/R") THEN NEXT.
  FOR EACH B-RutaD NO-LOCK WHERE B-RutaD.CodCia = s-codcia
      AND B-RutaD.CodDiv = s-CodDiv
      AND B-RutaD.CodDoc = "H/R"
      AND B-RutaD.CodRef = Ccbcdocu.coddoc
      AND B-RutaD.NroRef = Ccbcdocu.nrodoc,
      FIRST B-RutaC OF B-RutaD NO-LOCK WHERE B-RutaC.FlgEst = 'C':
      IF B-RutaD.FlgEst = "C" THEN RETURN 'ADM-ERROR'.  /* Entregado */
      IF B-RutaD.FlgEst = "D" THEN RETURN 'ADM-ERROR'.  /* Devoluci�n Parcial */

      /* Verificamos que haya pasado por PHR */
      IF NOT CAN-FIND(FIRST DI-RutaD WHERE DI-RutaD.CodCia = s-CodCia AND
                      DI-RutaD.CodDiv = s-CodDiv AND
                      DI-RutaD.CodDoc = "PHR" AND
                      DI-RutaD.CodRef = x-Faccpedi.CodDoc AND
                      DI-RutaD.NroRef = x-Faccpedi.NroPed AND
                      CAN-FIND(DI-RutaC OF DI-RutaD WHERE DI-RutaC.FlgEst = "C" NO-LOCK)
                      NO-LOCK)
          THEN RETURN 'ADM-ERROR'.
  END.
END.

FOR EACH Almcmov NO-LOCK WHERE Almcmov.CodCia = s-CodCia AND 
  Almcmov.CodRef = x-Faccpedi.CodDoc AND 
  Almcmov.NroRef = x-Faccpedi.NroPed AND
  Almcmov.FlgEst <> 'A':
  FOR EACH B-RutaG NO-LOCK WHERE B-RutaG.CodCia = s-codcia
      AND B-RutaG.CodDiv = s-CodDiv
      AND B-RutaG.CodDoc = "H/R"
      AND B-RutaG.CodAlm = Almcmov.CodAlm
      AND B-RutaG.Tipmov = Almcmov.TipMov
      AND B-RutaG.Codmov = Almcmov.CodMov
      AND B-RutaG.SerRef = Almcmov.NroSer
      AND B-RutaG.NroRef = Almcmov.NroDoc,
      FIRST B-RutaC OF B-RutaG NO-LOCK WHERE B-RutaC.FlgEst = 'C':
      IF B-RutaG.FlgEst = "C" THEN RETURN 'ADM-ERROR'.  /* Entregado */
      IF B-RutaG.FlgEst = "D" THEN RETURN 'ADM-ERROR'.  /* Devoluci�n Parcial */
  END.
END.

/* Si est� en una H/R en tr�mite => No va */
FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
  AND CcbCDocu.CodPed = x-Faccpedi.codref       /* PED */
  AND CcbCDocu.NroPed = x-Faccpedi.nroref
  AND Ccbcdocu.Libre_c01 = x-Faccpedi.coddoc    /* O/D */
  AND Ccbcdocu.Libre_c02 = x-Faccpedi.nroped
  AND Ccbcdocu.FlgEst <> "A":
  IF NOT (Ccbcdocu.coddiv = s-coddiv AND Ccbcdocu.coddoc = "G/R") THEN NEXT.
  FOR EACH B-RutaD NO-LOCK WHERE B-RutaD.CodCia = s-codcia
      AND B-RutaD.CodDiv = s-CodDiv
      AND B-RutaD.CodDoc = "H/R"
      AND B-RutaD.CodRef = Ccbcdocu.coddoc
      AND B-RutaD.NroRef = Ccbcdocu.nrodoc,
      FIRST B-RutaC OF B-RutaD NO-LOCK WHERE B-RutaC.FlgEst = 'P':
      RETURN 'ADM-ERROR'.
  END.
END.
FOR EACH Almcmov NO-LOCK WHERE Almcmov.CodCia = s-CodCia AND 
  Almcmov.CodRef = Faccpedi.CodDoc AND 
  Almcmov.NroRef = Faccpedi.NroPed AND
  Almcmov.FlgEst <> 'A':
  FOR EACH B-RutaG NO-LOCK WHERE B-RutaG.CodCia = s-codcia
      AND B-RutaG.CodDiv = s-CodDiv
      AND B-RutaG.CodDoc = "H/R"
      AND B-RutaG.CodAlm = Almcmov.CodAlm
      AND B-RutaG.Tipmov = Almcmov.TipMov
      AND B-RutaG.Codmov = Almcmov.CodMov
      AND B-RutaG.SerRef = Almcmov.NroSer
      AND B-RutaG.NroRef = Almcmov.NroDoc,
      FIRST B-RutaC OF B-RutaG NO-LOCK WHERE B-RutaC.FlgEst BEGINS 'P':
      RETURN 'ADM-ERROR'.
  END.
END.
**************** */

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION evalua-condicion W-Win 
FUNCTION evalua-condicion RETURNS LOGICAL
  ( INPUT pValor1 AS DEC, INPUT pValor2 AS DEC, INPUT pDato AS DEC, INPUT pCondLogica AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR x-retval AS LOG INIT NO.

    IF (pDato >= pValor1 AND pDato <= pValor2) THEN x-retval = YES.

    /*
    IF pCondLogica = '>' THEN DO:
        x-retval = IF (pValor1 > pValor2) THEN YES ELSE NO.
    END.
    IF pCondLogica = '<' THEN DO:
        x-retval = IF (pValor1 < pValor2) THEN YES ELSE NO.
    END.
    IF pCondLogica = '=' THEN DO:
        x-retval = IF (pValor1 = pValor2) THEN YES ELSE NO.
    END.
    IF pCondLogica = '>=' THEN DO:
        x-retval = IF (pValor1 >= pValor2) THEN YES ELSE NO.
    END.
    IF pCondLogica = '<=' THEN DO:            
        x-retval = IF (pValor1 <= pValor2) THEN YES ELSE NO.
    END.
    */

    RETURN x-retval.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

