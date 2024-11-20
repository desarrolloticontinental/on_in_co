&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
CREATE WIDGET-POOL.
DEFINE VAR OK-W-S AS LOGICAL NO-UNDO.
OK-W-S = SESSION:SET-WAIT-STATE("").

DEF VAR E-CONTENIDO AS CHAR.

E-CONTENIDO = 
"   Este procedimiento tiene por finalidad trasladar los saldos 
del presente per¡odo o a¤o contable al mes de apertura del 
siguiente per¡odo contable; Si ya se realiz¢ se anular  la apertura 
anterior y se trasladaran nuevamente los saldos actualizados. 
Las cuentas de Balance que esten configuradas como : [X] Cierre Anual Resumido,
 trasladar n su saldo total y 
Las cuentas de Balance que esten configuradas como :
  [ ] Cierre Anual Resumido trasladar n saldos individulaes por 
cada documento.".

     
    
DEF VAR RELOJ AS LOGICAL.

DEF VAR X-MENSAJE AS CHAR FORMAT "X(50)".

DEFINE FRAME F-AUXILIAR 
X-MENSAJE
WITH TITLE "Espere un momento por favor"
     CENTERED VIEW-AS DIALOG-BOX NO-LABELS.
     
{BIN/S-GLOBAL.I}
{CBD/CBGLOBAL.I}     

DEF VAR x-sdonac   AS DECIMAL INITIAL 0.
DEF VAR x-sdousa   AS DECIMAL INITIAL 0.
DEF BUFFER detalle FOR cb-dmov.
DEF VAR x-ROWID    AS ROWID.

DEF VAR  x-d1      AS DECIMAL  INITIAL 0.
DEF VAR  x-h1      AS DECIMAL  INITIAL 0.
DEF VAR  x-d2      AS DECIMAL  INITIAL 0.
DEF VAR  x-h2      AS DECIMAL  INITIAL 0.

DEF VAR  T-d1      AS DECIMAL  INITIAL 0.
DEF VAR  T-h1      AS DECIMAL  INITIAL 0.
DEF VAR  T-d2      AS DECIMAL  INITIAL 0.
DEF VAR  T-h2      AS DECIMAL  INITIAL 0.



DEF VAR  x-nromes  AS INTEGER INITIAL 0.
DEF VAR  x-codope  AS CHAR INITIAL "000".
DEF VAR  x-tpomov  AS LOGICAL.
DEF VAR  x-nroast  AS CHAR INITIAL "000001".
DEF VAR  x-CodCta  AS CHAR.
DEF VAR  x-CodMon  AS INTEGER.
DEF VAR  x-nroitm  AS INTEGER INITIAL  0.
DEF VAR  y-glodoc   as char.
DEF VAR  x-tpocmb   as decimal.
DEF VAR  x-clfaux   as char.
DEF VAR  x-codaux   as char.
DEF VAR  x-coddoc   as char.
DEF VAR  x-nrodoc   as char.
DEF VAR  x-nroref   as char.
DEF VAR  x-fchdoc   as date.
DEF VAR  x-fchvto   as date.
DEF VAR  x-coddiv   as char.
def var  x-nroruc   as char.
def var  x-cfcAja    as char.
def var  x-cco      as char.
DEF VAR x-glodoc  AS CHAR INITIAL "ASIENTO AUTOMATICO DE APERTURA".


s-codcia  = 1.
s-periodo = 2007.

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK X-PER x-gan Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 X-PER x-gan 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel":U
     LABEL "Cancel" 
     SIZE 11 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "img/b-ok":U
     LABEL "OK" 
     SIZE 11 BY 1.5
     BGCOLOR 8 .

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 59.14 BY 6.04
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE x-gan AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cuenta de Ganancias Acumuladas" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE X-PER AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cuenta de Pérdidas Acumuladas" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     EDITOR-1 AT ROW 3.08 COL 1.14 NO-LABEL
     Btn_OK AT ROW 9.27 COL 9.57
     X-PER AT ROW 1.15 COL 32.43 COLON-ALIGNED
     x-gan AT ROW 2.08 COL 32.43 COLON-ALIGNED
     Btn_Cancel AT ROW 9.27 COL 40.72
     SPACE(8.56) SKIP(0.10)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Apertura del nuevo Per¡odo Contable".


/* *********************** Procedure Settings ************************ */

/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */


/* ***************  Runtime Attributes and UIB Settings  ************** */

/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-1:AUTO-INDENT IN FRAME D-Dialog      = TRUE
       EDITOR-1:AUTO-RESIZE IN FRAME D-Dialog      = TRUE
       EDITOR-1:RETURN-INSERTED IN FRAME D-Dialog  = TRUE
       EDITOR-1:READ-ONLY IN FRAME D-Dialog        = TRUE.



/* Setting information for Queries and Browse Widgets fields            */

/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */

 


/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Apertura del nuevo Per¡odo Contable */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.


&Scoped-define SELF-NAME Btn_OK
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
    ASSIGN X-PER X-GAN.
    
    RUN PROCESO.
    
END.


&Scoped-define SELF-NAME x-gan
ON F8 OF x-gan IN FRAME D-Dialog /* Cuenta de Ganancias Acumuladas */
OR "MOUSE-SELECT-DBLCLICK":U OF X-GAN DO:
    DEF VAR T-ROWID AS ROWID.
    RUN CBD/H-CTAS01.W(s-codcia, 0,OUTPUT T-ROWID).
    IF T-ROWID <> ?
    THEN DO:
        find cb-ctas WHERE ROWID(cb-ctas) = T-ROWID NO-LOCK NO-ERROR.
        IF avail cb-ctas
        THEN DO:
            X-GAN:SCREEN-VALUE = cb-ctas.CodCta.
        END.
        ELSE MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR
            BUTTONS OK.
    END.
    APPLY "CHOOSE":U TO X-GAN.
    RETURN NO-APPLY.
END.


&Scoped-define SELF-NAME X-PER
ON F8 OF X-PER IN FRAME D-Dialog /* Cuenta de Pérdidas Acumuladas */
OR "MOUSE-SELECT-DBLCLICK":U OF X-PER DO:
    DEF VAR T-ROWID AS ROWID.
    RUN CBD/H-CTAS01.W(s-codcia, 0,OUTPUT T-ROWID).
    IF T-ROWID <> ?
    THEN DO:
        find cb-ctas WHERE ROWID(cb-ctas) = T-ROWID NO-LOCK NO-ERROR.
        IF avail cb-ctas
        THEN DO:
            X-PER:SCREEN-VALUE = cb-ctas.CodCta.
        END.
        ELSE MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR
            BUTTONS OK.
    END.
    APPLY "CHOOSE":U TO X-PER.
    RETURN NO-APPLY.
END.


&UNDEFINE SELF-NAME



/* ***************************  Main Block  *************************** */
EDITOR-1 = E-CONTENIDO.
{src/adm/template/dialogmn.i}


/* **********************  Internal Procedures  *********************** */

PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.


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


PROCEDURE ANULA :
/*
FOR EACH cb-dmov WHERE      cb-dmov.codcia   = s-codcia          AND
                            cb-dmov.periodo  = s-periodo + 1     AND
                            cb-dmov.nromes   = x-nromes          AND   
                            cb-dmov.codope   = x-codope          AND
                            cb-dmov.nroast   = x-nroast :
    /*RUN cbd/cb-acmd.p(RECID(cb-dmov), NO, YES). */
    DELETE cb-dmov.
END.

FIND cb-cmov WHERE     cb-cmov.codcia   = s-codcia       AND
                       cb-cmov.periodo  = s-periodo + 1  AND
                       cb-cmov.nromes   = x-nromes       AND
                       cb-cmov.codope   = x-codope       AND
                       cb-cmov.nroast   = x-nroast  
                       NO-ERROR.
IF AVAIL cb-cmov THEN DELETE cb-cmov.                   
*/
/* RHC 12-01-2004 Se va a modificar la transaccion */
/*RUN GRABA-CAB.*/

END PROCEDURE.


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
  HIDE FRAME D-Dialog.
END PROCEDURE.


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
  DISPLAY EDITOR-1 X-PER x-gan 
      WITH FRAME D-Dialog.
  ENABLE Btn_OK X-PER x-gan Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.


PROCEDURE GENERA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR H-D1 AS DECIMAL.
DEF VAR H-D2 AS DECIMAL.
DEF VAR H-H1 AS DECIMAL.
DEF VAR H-H2 AS DECIMAL.

RUN GRABA-CAB.

message "Proceso" S-CODCIA s-periodo view-as alert-box.

FOR EACH GN-DIVI WHERE GN-DIVI.CODCIA = S-CODCIA 
        AND LENGTH(GN-DIVI.CODDIV) = 5
        NO-LOCK:
    ASSIGN
        x-d1 = 0 
        x-d2 = 0
        x-h1 = 0
        x-d1 = 0
        x-Coddiv = gn-divi.coddiv.    
    FOR EACH CB-CTAS WHERE CB-CTAS.CODCIA = CB-CODCIA 
            AND LENGTH(CB-CTAS.CODCTA) = CB-MAXNIVEL 
            AND    CB-CTAS.CODCTA  = "384101"
            NO-LOCK:
        ASSIGN 
            X-CODMON = CB-CTAS.CODMON
            X-CODCTA = CB-CTAS.CODCTA.
        IF CB-CTAS.CIERES 
        THEN RUN GRAB-SALDO.
        ELSE RUN GRAB-CTACTE.
        DISPLAY "Procesando: " + x-coddiv + "-" + x-codcta  @ x-mensaje
            WITH FRAME f-auxiliar.
        PAUSE 0.
   END.
   ASSIGN
        H-D1 = X-D1
        H-D2 = X-D2
        H-H1 = X-H1
        H-H2 = X-H2.
   RUN GRAB-TOTAL(X-GAN ,
                    H-D1,
                    H-D2,
                    YES).
   RUN GRAB-TOTAL(X-PER ,
                    H-H1,
                    H-H2,
                    FALSE).
END.
IF AVAIL CB-CMOV 
THEN ASSIGN 
        CB-CMOV.DBEMN1 = T-D1
        CB-CMOV.DBEMN2 = T-D2
        CB-CMOV.HBEMN1 = T-H1
        CB-CMOV.HBEMN2 = T-H2
        CB-CMOV.TOTITM = X-NROITM.

END PROCEDURE.


PROCEDURE GRAB-CTACTE :

DEF VAR IMP AS DEC.
FOR EACH cb-dmov  NO-LOCK
                  WHERE  cb-dmov.codcia  = s-codcia  AND
                         cb-dmov.periodo = s-periodo AND
                         cb-dmov.NroMes  < 13        AND
                         cb-dmov.coddiv  BEGINS x-coddiv  AND 
                         cb-dmov.codcta  = x-codcta                               
                         BREAK 
                         BY cb-dmov.codaux  
                         BY cb-dmov.coddoc 
                         BY cb-dmov.nrodoc 
                         BY cb-dmov.fchdoc with frame c down :

    IF FIRST-OF(cb-dmov.nrodoc) THEN DO:
         ASSIGN 
           y-glodoc = cb-dmov.glodoc
           x-tpocmb = cb-dmov.tpocmb
           x-clfaux = cb-dmov.clfaux
           x-codaux = cb-dmov.codaux
           x-coddoc = cb-dmov.coddoc
           x-nrodoc = cb-dmov.nrodoc
           x-nroref = cb-dmov.nroref
           x-fchdoc = cb-dmov.fchdoc
           x-fchvto = cb-dmov.fchvto
           x-cco    = cb-dmov.cco
           x-nroruc = cb-dmov.nroruc
           x-cfcaja = cb-dmov.c-fcaja.
           IF X-CODMON = 3 THEN X-CodMon = cb-dmov.codmon.
        x-sdonac = 0.
        x-sdousa = 0.
    END. 
    
    IF NOT cb-dmov.TpoMov 
      THEN ASSIGN x-Sdonac = x-SdoNac + cb-dmov.ImpMn1           
                  x-Sdousa = x-SdoUsa + cb-dmov.ImpMn2.
      ELSE ASSIGN x-Sdonac = x-SdoNac - cb-dmov.ImpMn1           
                  x-Sdousa = x-SdoUsa - cb-dmov.ImpMn2.           

        if cb-dmov.codaux begins "00000844" then do:

        IMP = IF cb-dmov.TPOMOV THEN cb-dmov.impmn1 ELSE cb-dmov.impmn1 * -1.
        message
            cb-dmov.coddiv
            cb-dmov.codope
            cb-dmov.nromes
            cb-dmov.codmon
            IMP
            cb-dmov.ImpMn2
            cb-dmov.coddoc
            cb-dmov.nrodoc
            view-as alert-box.
        DISPLAY
            cb-dmov.coddiv
            cb-dmov.codope
            cb-dmov.nromes
            cb-dmov.codmon
            IMP(TOTAL)
            cb-dmov.ImpMn2
            cb-dmov.coddoc
            cb-dmov.nrodoc.
            down.
end.
     IF LAST-OF(cb-dmov.nrodoc) THEN
     DO:            
        if x-codaux begins "00000844" then
            message
                x-codaux skip
                x-sdonac skip
                x-sdousa skip
                x-codmon
                view-as alert-box.         
        IF (x-sdonac <> 0) OR (x-sdousa <> 0) THEN RUN GRAB-DETALLE.
        ELSE
        IF (x-codmon = 1 AND x-sdonac <> 0) 
           THEN RUN GRAB-DETALLE.
           ELSE 
               IF (x-codmon = 2 AND x-sdousa <> 0) 
                    THEN RUN GRAB-DETALLE.
 
     END.
END. 

END PROCEDURE.


PROCEDURE GRAB-DETALLE :

if x-codaux begins "00000844" then
message x-codaux "00000844" view-as alert-box.

/*
CREATE detalle.
  x-nroitm = x-nroitm + 1.
  IF (x-sdonac <>0 )  THEN x-tpomov = (x-sdonac < 0).       
                      ELSE x-tpomov = (x-sdousa < 0).
  x-sdousa = ABS(x-sdousa).
  x-sdonac = ABS(x-sdonac).
  ASSIGN
        detalle.CodCia   = s-codcia
        detalle.Periodo  = s-periodo + 1
        detalle.coddiv   = x-coddiv
        detalle.Codcta   = x-codcta
        detalle.Codmon   = x-codmon
        detalle.Codope   = x-codope
        detalle.Glodoc   = y-glodoc
        detalle.ImpMn1   = x-sdonac
        detalle.ImpMn2   = x-sdousa
        detalle.Nroast   = x-nroast
        detalle.Nroitm   = x-nroitm 
        detalle.NroMes   = x-nromes
        detalle.Tpocmb   = x-tpocmb
        detalle.clfaux   = x-clfaux
        detalle.codaux   = x-codaux
        detalle.coddoc   = x-coddoc
        detalle.nrodoc   = x-nrodoc
        detalle.nroref   = x-nroref
        detalle.fchdoc   = x-fchdoc
        detalle.fchvto   = x-fchvto
        detalle.TpoMov   = x-tpomov
        detalle.cco      = x-cco  
        detalle.nroruc   = x-nroruc 
        detalle.c-fcaja  = x-cfcaja.
  RUN SUMA.
/*  RUN cbd/cb-acmd.p(RECID(detalle), YES, YES).*/
*/  
END PROCEDURE.


PROCEDURE GRAB-SALDO :
ASSIGN 
           y-glodoc = X-glodoc
           x-tpocmb = 0
           x-clfaux = ""
           x-codaux = ""
           x-coddoc = ""
           x-nrodoc = ""
           x-nroref = ""
           x-fchdoc = ?
           x-fchvto = ?
           x-cco    = ""
           x-nroruc = ""
           x-cfcaja = ""
           x-sdonac = 0
           x-sdousa = 0.


FOR EACH cb-dmov  NO-LOCK WHERE  
         cb-dmov.codcia  = s-codcia  AND
         cb-dmov.periodo = s-periodo AND
         cb-dmov.coddiv  BEGINS x-coddiv  AND 
         cb-dmov.codcta  = x-codcta  AND
         cb-dmov.NroMes  < 13 :
    
    IF NOT cb-dmov.TpoMov 
      THEN ASSIGN x-Sdonac = x-SdoNac + cb-dmov.ImpMn1           
                  x-Sdousa = x-SdoUsa + cb-dmov.ImpMn2.
      ELSE ASSIGN x-Sdonac = x-SdoNac - cb-dmov.ImpMn1           
                  x-Sdousa = x-SdoUsa - cb-dmov.ImpMn2.           
              
END. 
IF (x-sdonac <> 0) AND (x-sdousa <> 0) THEN RUN GRAB-DETALLE.
ELSE IF (x-codmon = 1 AND x-sdonac <> 0) THEN RUN GRAB-DETALLE.
     ELSE IF (x-codmon = 2 AND x-sdousa <> 0) THEN RUN GRAB-DETALLE.

END PROCEDURE.


PROCEDURE GRAB-TOTAL :
DEFINE INPUT PARAMETER X-CODCTA AS CHAR.
DEFINE INPUT PARAMETER X-SDONAC AS DECIMAL.
DEFINE INPUT PARAMETER X-SDOUSA AS DECIMAL.
DEFINE INPUT PARAMETER X-TPOMOV AS LOGICAL.

/*
CREATE detalle.
  x-nroitm = x-nroitm + 1.
  x-sdousa = ABS(x-sdousa).
  x-sdonac = ABS(x-sdonac).
  ASSIGN
        detalle.CodCia   = s-codcia
        detalle.Codcta   = x-codcta
        detalle.coddiv   = x-coddiv
        detalle.Codmon   = 1
        detalle.Codope   = x-codope
        detalle.Glodoc   = x-glodoc
        detalle.ImpMn1   = x-sdonac
        detalle.ImpMn2   = x-sdousa
        detalle.Nroast   = x-nroast
        detalle.Nroitm   = x-nroitm 
        detalle.NroMes   = x-nromes
        detalle.Periodo  = s-periodo + 1
        detalle.Tpocmb   = 1
        detalle.TpoMov   = x-tpomov.
   RUN SUMA. 
   /* RUN CBD/cb-acmd.p(RECID(detalle), YES, NO).  */
*/
END PROCEDURE.


PROCEDURE GRABA-CAB :
/*
CREATE integral.cb-cmov.
  ASSIGN 
    integral.cb-cmov.CodCia  = s-codcia
    integral.cb-cmov.Periodo = s-periodo + 1
    integral.cb-cmov.NroMes  = x-nromes
    integral.cb-cmov.CodOpe  = x-CodOpe
    integral.cb-cmov.NroAst  = STRING(1,"999999")
    integral.cb-cmov.FchAst  = DATE( 1, 1, s-periodo + 1) - 1
    integral.cb-cmov.TpoCmb  = 1
    integral.cb-cmov.Notast  = x-glodoc
    integral.cb-cmov.GloAst  = x-glodoc
    integral.cb-cmov.codmon  = 1.
        */
END PROCEDURE.


PROCEDURE PROCESO :
/* RHC 12-01-2004 */
DISPLAY  "Eliminando movimiento Anterior" @  x-mensaje WITH FRAME F-AUXILIAR.
PAUSE 0.
RUN ANULA.  /* En este proceso se anulo la generacion de la cabecera, ahora se hace en GENERA */

DISPLAY "Seleccionando información a procesar " @ x-mensaje
    WITH FRAME f-auxIliar.
PAUSE 0.
RUN GENERA.

HIDE FRAME F-AUXILIAR.

/* ************* RHC 12-01-2004 Se va a partir la transaccion en dos 
DEF VAR H-D1 AS DECIMAL.
DEF VAR H-D2 AS DECIMAL.
DEF VAR H-H1 AS DECIMAL.
DEF VAR H-H2 AS DECIMA.

DISPLAY  "Eliminando movimiento Anterior" @  x-mensaje WITH FRAME F-AUXILIAR.
PAUSE 0.
RUN ANULA.

DISPLAY "Seleccionando información a procesar " @ x-mensaje
with frame f-auxIliar.
pause 0.

FOR EACH GN-DIVI NO-LOCK WHERE GN-DIVI.CODCIA = S-CODCIA AND
                        LENGTH(GN-DIVI.CODDIV) = 5 :
    assign x-d1 = 0 
           x-d2 = 0
           x-h1 = 0
           x-d1 = 0
           x-Coddiv = gn-divi.coddiv.    
    FOR EACH CB-CTAS WHERE CB-CTAS.CODCIA  = CB-CODCIA AND
                    LENGTH(CB-CTAS.CODCTA) = CB-MAXNIVEL 
                    AND    CB-CTAS.CODCTA  = "384101" :
        ASSIGN X-CODMON = CB-CTAS.CODMON
               X-CODCTA = CB-CTAS.CODCTA.
        IF CB-CTAS.CIERES THEN RUN GRAB-SALDO.
                          ELSE RUN GRAB-CTACTE.
                           
        DISPLAY "Procesando: " + x-coddiv + "-" + x-codcta  @ x-mensaje
        with frame f-auxiliar.
        pause 0.
   END.
   ASSIGN
        H-D1 = X-D1
        H-D2 = X-D2
        H-H1 = X-H1
        H-H2 = X-H2.
   RUN GRAB-TOTAL(X-GAN ,
                    H-D1,
                    H-D2,
                    YES).
   RUN GRAB-TOTAL(X-PER ,
                    H-H1,
                    H-H2,
                    FALSE).
END.
IF AVAIL CB-CMOV THEN ASSIGN CB-CMOV.DBEMN1 = T-D1
                             CB-CMOV.DBEMN2 = T-D2
                             CB-CMOV.HBEMN1 = T-H1
                             CB-CMOV.HBEMN2 = T-H2
                             CB-CMOV.TOTITM = X-NROITM.
HIDE FRAME F-AUXILIAR.
*************************************************************** */

END PROCEDURE.


PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.


PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.


PROCEDURE SUMA :
IF detalle.tpomov  THEN
DO:
   x-h1 = x-h1 + detalle.impmn1.
   x-h2 = x-h2 + detalle.impmn2.
END.
ELSE
DO:
   x-d1 = x-d1 + detalle.impmn1.
   x-d2 = x-d2 + detalle.impmn2.
END.   

IF detalle.tpomov  THEN
DO:
   T-h1 = T-h1 + detalle.impmn1.
   T-h2 = T-h2 + detalle.impmn2.
END.
ELSE
DO:
   T-d1 = T-d1 + detalle.impmn1.
   T-d2 = T-d2 + detalle.impmn2.
END.   



END PROCEDURE.


