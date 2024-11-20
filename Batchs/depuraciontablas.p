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
         HEIGHT             = 15.73
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

RUN Paso11.
DISPLAY 'paso11 ok'. PAUSE 0.

RUN Paso12.
DISPLAY 'paso12 ok'. PAUSE 0.

RUN Paso13.
DISPLAY 'paso13 ok'. PAUSE 0.

RUN Paso14.
DISPLAY 'paso14 ok'. PAUSE 0.

RUN Paso15.
DISPLAY 'paso15 ok'. PAUSE 0.

RUN Paso16.
DISPLAY 'paso16 ok'. PAUSE 0.

RUN Paso17.
DISPLAY 'paso17 ok'. PAUSE 0.

RUN Paso18.
DISPLAY 'paso18 ok'. PAUSE 0.

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

DISABLE TRIGGERS FOR LOAD OF Faccpedi.
DISABLE TRIGGERS FOR LOAD OF Facdpedi.

x-FchCorte = DATE(12,31,2017).

FOR EACH Faccpedi EXCLUSIVE-LOCK WHERE codcia = 001 AND fchped <= x-FchCorte:
    FOR EACH Facdpedi EXCLUSIVE-LOCK WHERE FacDPedi.CodCia = Faccpedi.codcia AND
        FacDPedi.CodDoc = Faccpedi.coddoc AND 
        FacDPedi.NroPed = Faccpedi.nroped: 
        DELETE Facdpedi.
    END.
    DELETE Faccpedi.
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

DISABLE TRIGGERS FOR LOAD OF Di-rutac.
DISABLE TRIGGERS FOR LOAD OF Di-rutad.
DISABLE TRIGGERS FOR LOAD OF Di-rutadg.
DISABLE TRIGGERS FOR LOAD OF Di-rutadv.
DISABLE TRIGGERS FOR LOAD OF Di-rutag.
DISABLE TRIGGERS FOR LOAD OF Di-rutagri.

x-FchCorte = DATE(12,31,2017).

FOR EACH Di-rutac EXCLUSIVE-LOCK WHERE codcia = 001 AND fchdoc <= x-FchCorte:
    FOR EACH Di-rutad EXCLUSIVE-LOCK WHERE Di-rutad.CodCia = Di-rutac.codcia AND
        Di-rutad.CodDiv = Di-rutac.coddiv AND 
        Di-rutad.CodDoc = Di-rutac.coddoc AND 
        Di-rutad.NroDoc = Di-rutac.nrodoc: 
        DELETE Di-rutad.
    END.
    FOR EACH Di-rutadg EXCLUSIVE-LOCK WHERE Di-rutadg.CodCia = Di-rutac.codcia AND
        Di-rutadg.CodDiv = Di-rutac.coddiv AND 
        Di-rutadg.CodDoc = Di-rutac.coddoc AND 
        Di-rutadg.NroDoc = Di-rutac.nrodoc: 
        DELETE Di-rutadg.
    END.
    FOR EACH Di-rutadv EXCLUSIVE-LOCK WHERE Di-rutadv.CodCia = Di-rutac.codcia AND
        Di-rutadv.CodDiv = Di-rutac.coddiv AND 
        Di-rutadv.CodDoc = Di-rutac.coddoc AND 
        Di-rutadv.NroDoc = Di-rutac.nrodoc: 
        DELETE Di-rutadv.
    END.
    FOR EACH Di-rutag EXCLUSIVE-LOCK WHERE Di-rutag.CodCia = Di-rutac.codcia AND
        Di-rutag.CodDiv = Di-rutac.coddiv AND 
        Di-rutag.CodDoc = Di-rutac.coddoc AND 
        Di-rutag.NroDoc = Di-rutac.nrodoc: 
        DELETE Di-rutag.
    END.
    FOR EACH Di-rutagri EXCLUSIVE-LOCK WHERE Di-rutagri.CodCia = Di-rutac.codcia AND
        Di-rutagri.CodDiv = Di-rutac.coddiv AND 
        Di-rutagri.CodDoc = Di-rutac.coddoc AND 
        Di-rutagri.NroDoc = Di-rutac.nrodoc: 
        DELETE Di-rutagri.
    END.
    DELETE Di-rutac.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso11) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso11 Procedure 
PROCEDURE Paso11 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF Logisdchequeo.

x-FchCorte = DATE(12,31,2017).

FOR EACH Logisdchequeo EXCLUSIVE-LOCK WHERE codcia = 000:
    DELETE Logisdchequeo.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso12) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso12 Procedure 
PROCEDURE Paso12 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF PikSacadores.

x-FchCorte = DATE(12,31,2017).

FOR EACH PikSacadores EXCLUSIVE-LOCK WHERE codcia = 001 AND 
    fecha <= DATETIME(STRING(x-FchCorte) + " 23:59:00.000"):
    DELETE PikSacadores.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso13) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso13 Procedure 
PROCEDURE Paso13 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF TabTrkDocs.

x-FchCorte = DATE(12,31,2017).

FOR EACH TabTrkDocs EXCLUSIVE-LOCK WHERE codcia = 001 AND 
    fchcreacion <= DATETIME(STRING(x-FchCorte) + " 23:59:00.000"):
    DELETE TabTrkDocs.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso14) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso14 Procedure 
PROCEDURE Paso14 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF TraIngSal.

x-FchCorte = DATE(12,31,2017).

FOR EACH TraIngSal EXCLUSIVE-LOCK WHERE codcia = 001 AND 
    fechaingreso <= x-FchCorte:
    DELETE TraIngSal.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso15) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso15 Procedure 
PROCEDURE Paso15 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF Vtactrkped.
DISABLE TRIGGERS FOR LOAD OF Vtadtrkped.


x-FchCorte = DATE(12,31,2017).

FOR EACH Vtactrkped EXCLUSIVE-LOCK WHERE codcia = 001 
    AND fechai <= DATETIME(STRING(x-FchCorte) + " 23:59:00.000"):
    FOR EACH Vtadtrkped EXCLUSIVE-LOCK WHERE Vtadtrkped.CodCia = Vtactrkped.codcia AND
        Vtadtrkped.CodDoc = Vtactrkped.coddoc AND 
        Vtadtrkped.NroPed = Vtactrkped.nroped: 
        DELETE Vtadtrkped.
    END.
    DELETE Vtactrkped.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso16) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso16 Procedure 
PROCEDURE Paso16 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF Ccbccgoc.
DISABLE TRIGGERS FOR LOAD OF Ccbdcgoc.


x-FchCorte = DATE(12,31,2014).

FOR EACH Ccbccgoc EXCLUSIVE-LOCK WHERE codcia = 001 AND fecha <= x-FchCorte:
    FOR EACH Ccbdcgoc EXCLUSIVE-LOCK WHERE Ccbdcgoc.CodCia = Ccbccgoc.codcia AND
        Ccbdcgoc.CodDiv = Ccbccgoc.coddiv AND 
        Ccbdcgoc.CodDoc = Ccbccgoc.coddoc AND 
        Ccbdcgoc.NroDoc = Ccbccgoc.nrodoc: 
        DELETE Ccbdcgoc.
    END.
    DELETE Ccbccgoc.
END.
FOR EACH Ccbmovlet EXCLUSIVE-LOCK WHERE Ccbmovlet.codcia = 001 AND 
    Ccbmovlet.fchmov <= x-FchCorte:
    DELETE Ccbmovlet.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso17) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso17 Procedure 
PROCEDURE Paso17 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF Almcrossdocking.
DISABLE TRIGGERS FOR LOAD OF Almdcdoc.
DISABLE TRIGGERS FOR LOAD OF Almrcdoc.
DISABLE TRIGGERS FOR LOAD OF Almlserv.
DISABLE TRIGGERS FOR LOAD OF CcbAntRec.
DISABLE TRIGGERS FOR LOAD OF Ccbcierr.
DISABLE TRIGGERS FOR LOAD OF Ccbdecl.
DISABLE TRIGGERS FOR LOAD OF Oomovialmacen.

x-FchCorte = DATE(12,31,2016).

FOR EACH Almcrossdocking EXCLUSIVE-LOCK WHERE codcia = 001 AND fchdoc <= x-FchCorte:
    DELETE Almcrossdocking.
END.
FOR EACH Almdcdoc EXCLUSIVE-LOCK WHERE Almdcdoc.codcia = 001 AND Almdcdoc.fecha <= x-FchCorte:
    DELETE Almdcdoc.
END.
FOR EACH Almrcdoc EXCLUSIVE-LOCK WHERE Almrcdoc.codcia = 001 AND Almrcdoc.fecha <= x-FchCorte:
    DELETE Almrcdoc.
END.
FOR EACH Almlserv EXCLUSIVE-LOCK WHERE Almlserv.codcia = 001 AND Almlserv.fchact <= x-FchCorte:
    DELETE Almlserv.
END.
FOR EACH CcbAntRec EXCLUSIVE-LOCK WHERE CcbAntRec.codcia = 001 AND CcbAntRec.fchdoc <= x-FchCorte:
    DELETE CcbAntRec.
END.
FOR EACH Ccbcierr EXCLUSIVE-LOCK WHERE Ccbcierr.codcia = 001 AND Ccbcierr.fchcie <= x-FchCorte:
    DELETE Ccbcierr.
END.
FOR EACH Ccbdecl EXCLUSIVE-LOCK WHERE Ccbdecl.codcia = 001 AND Ccbdecl.fchcie <= x-FchCorte:
    DELETE Ccbdecl.
END.
FOR EACH Oomovialmacen EXCLUSIVE-LOCK WHERE Oomovialmacen.codcia = 001 AND Oomovialmacen.fchdoc <= x-FchCorte:
    DELETE Oomovialmacen.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Paso18) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paso18 Procedure 
PROCEDURE Paso18 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF OOCb-cmov.
DISABLE TRIGGERS FOR LOAD OF OOCb-dmov.

x-FchCorte = DATE(12,31,2017).

FOR EACH OOCb-cmov EXCLUSIVE-LOCK WHERE codcia = 001 AND fchast <= x-FchCorte:
    FOR EACH OOCb-dmov EXCLUSIVE-LOCK WHERE OOCb-dmov.CodCia = OOCb-cmov.codcia AND
        OOCb-dmov.Periodo = OOCb-cmov.periodo AND 
        OOCb-dmov.NroMes = OOCb-cmov.nromes AND 
        OOCb-dmov.CodOpe = OOCb-cmov.codope AND 
        OOCb-dmov.NroAst = OOCb-cmov.nroast:
        DELETE OOCb-dmov.
    END.
    DELETE OOCb-cmov.
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

DISABLE TRIGGERS FOR LOAD OF Almcotrb.
DISABLE TRIGGERS FOR LOAD OF Almdotrb.
DISABLE TRIGGERS FOR LOAD OF ExpTurno.
DISABLE TRIGGERS FOR LOAD OF Lg-cocmp.
DISABLE TRIGGERS FOR LOAD OF Lg-docmp.
DISABLE TRIGGERS FOR LOAD OF Lg-coser.
DISABLE TRIGGERS FOR LOAD OF Lg-doser.
DISABLE TRIGGERS FOR LOAD OF Lg-crequ.
DISABLE TRIGGERS FOR LOAD OF Lg-drequ.
DISABLE TRIGGERS FOR LOAD OF Loglistamingn.
DISABLE TRIGGERS FOR LOAD OF Logmmate.
DISABLE TRIGGERS FOR LOAD OF Logmmatgci.
DISABLE TRIGGERS FOR LOAD OF Ooalmcmov.
DISABLE TRIGGERS FOR LOAD OF Openprecios.

FOR EACH Almcotrb EXCLUSIVE-LOCK:
    DELETE Almcotrb.
END.
FOR EACH Almdotrb EXCLUSIVE-LOCK:
    DELETE Almdotrb.
END.
FOR EACH ExpTurno EXCLUSIVE-LOCK:
    DELETE ExpTurno.
END.
FOR EACH Lg-cocmp EXCLUSIVE-LOCK:
    DELETE Lg-cocmp.
END.
FOR EACH Lg-docmp EXCLUSIVE-LOCK:
    DELETE Lg-docmp.
END.
FOR EACH Lg-coser EXCLUSIVE-LOCK:
    DELETE Lg-coser.
END.
FOR EACH Lg-doser EXCLUSIVE-LOCK:
    DELETE Lg-doser.
END.
FOR EACH Lg-crequ EXCLUSIVE-LOCK:
    DELETE Lg-crequ.
END.
FOR EACH Lg-drequ EXCLUSIVE-LOCK:
    DELETE Lg-drequ.
END.
FOR EACH Loglistamingn EXCLUSIVE-LOCK:
    DELETE loglistamingn.
END.
FOR EACH Logmmate EXCLUSIVE-LOCK:
    DELETE logmmate.
END.
FOR EACH Logmmatgci EXCLUSIVE-LOCK:
    DELETE logmmatgci.
END.
FOR EACH ooalmcmov EXCLUSIVE-LOCK:
    DELETE ooalmcmov.
END.
FOR EACH openprecios EXCLUSIVE-LOCK:
    DELETE openprecios.
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

DISABLE TRIGGERS FOR LOAD OF SupControlOC.

x-FchCorte = DATE(12,31,2017).

FOR EACH SupControlOC EXCLUSIVE-LOCK WHERE SupControlOC.codcia = 001 AND
    SupControlOC.fchped <= x-FchCorte:
    DELETE SupControlOC.
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

DISABLE TRIGGERS FOR LOAD OF Vtacdocu.
DISABLE TRIGGERS FOR LOAD OF Vtaddocu.

x-FchCorte = DATE(12,31,2017).

FOR EACH Vtacdocu EXCLUSIVE-LOCK WHERE codcia = 001 AND fchped <= x-FchCorte:
    FOR EACH Vtaddocu EXCLUSIVE-LOCK WHERE Vtaddocu.CodCia = Vtacdocu.codcia AND
        Vtaddocu.CodPed = Vtacdocu.codped AND 
        Vtaddocu.NroPed = Vtacdocu.nroped: 
        DELETE Vtaddocu.
    END.
    DELETE Vtacdocu.
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

DISABLE TRIGGERS FOR LOAD OF Vtacrecl.
DISABLE TRIGGERS FOR LOAD OF Vtadrecl.
DISABLE TRIGGERS FOR LOAD OF Vtactrkped.
DISABLE TRIGGERS FOR LOAD OF Vtadtrkped.
DISABLE TRIGGERS FOR LOAD OF Logtrkdocs.

x-FchCorte = DATE(12,31,2017).

FOR EACH Vtacrecl EXCLUSIVE-LOCK WHERE codcia = 001 AND fchrcl <= x-FchCorte:
    FOR EACH Vtadrecl EXCLUSIVE-LOCK WHERE Vtadrecl.CodCia = Vtacrecl.codcia AND
        Vtadrecl.CodDoc = Vtacrecl.coddoc AND 
        Vtadrecl.NroDoc = Vtacrecl.nrodoc: 
        DELETE Vtadrecl.
    END.
    DELETE Vtacrecl.
END.

FOR EACH Vtactrkped EXCLUSIVE-LOCK WHERE Vtactrkped.codcia = 001 AND
    Vtactrkped.fechai <= DATETIME(STRING(x-FchCorte) + " 23:59:00.000"):
    FOR EACH Vtadtrkped EXCLUSIVE-LOCK WHERE vtadtrkped.CodCia = vtactrkped.codcia AND
        vtadtrkped.CodDiv = vtactrkped.coddiv AND
        vtadtrkped.CodDoc = vtactrkped.coddoc AND
        vtadtrkped.NroPed = vtactrkped.nroped:
        DELETE vtadtrkped.
    END.
    DELETE Vtactrkped.
END.

FOR EACH Logtrkdocs EXCLUSIVE-LOCK WHERE LogTrkDocs.CodCia = 001 AND 
    LogTrkDocs.Fecha <= DATETIME(STRING(x-FchCorte) + " 23:59:00.000"):
    DELETE logtrkdocs.
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

DISABLE TRIGGERS FOR LOAD OF Almcincidencia.
DISABLE TRIGGERS FOR LOAD OF Almdincidencia.

x-FchCorte = DATE(12,31,2017).

FOR EACH Almcincidencia EXCLUSIVE-LOCK WHERE codcia = 001 AND fecha <= x-FchCorte:
    FOR EACH Almdincidencia EXCLUSIVE-LOCK WHERE Almdincidencia.CodCia = Almcincidencia.codcia AND
        Almdincidencia.CodDiv = Almcincidencia.coddiv AND 
        Almdincidencia.NroControl = Almcincidencia.nrocontrol: 
        DELETE Almdincidencia.
    END.
    DELETE Almcincidencia.
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

DISABLE TRIGGERS FOR LOAD OF Almcincidencia.
DISABLE TRIGGERS FOR LOAD OF Almdincidencia.

x-FchCorte = DATE(12,31,2017).

FOR EACH Almcincidencia EXCLUSIVE-LOCK WHERE codcia = 001 AND fecha <= x-FchCorte:
    FOR EACH Almdincidencia EXCLUSIVE-LOCK WHERE Almdincidencia.CodCia = Almcincidencia.codcia AND
        Almdincidencia.CodDiv = Almcincidencia.coddiv AND 
        Almdincidencia.NroControl = Almcincidencia.nrocontrol: 
        DELETE Almdincidencia.
    END.
    DELETE Almcincidencia.
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

DISABLE TRIGGERS FOR LOAD OF Almcrequ.
DISABLE TRIGGERS FOR LOAD OF Almdrequ.

x-FchCorte = DATE(12,31,2017).

FOR EACH Almcrequ EXCLUSIVE-LOCK WHERE codcia = 001 AND fchdoc <= x-FchCorte:
    FOR EACH Almdrequ EXCLUSIVE-LOCK WHERE Almdrequ.CodCia = Almcrequ.codcia AND
        Almdrequ.CodAlm = Almcrequ.codalm AND 
        Almdrequ.NroSer = Almcrequ.nroser AND
        Almdrequ.NroDoc = Almcrequ.nrodoc:
        DELETE Almdrequ.
    END.
    DELETE Almcrequ.
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

DISABLE TRIGGERS FOR LOAD OF Ccbcbult.
DISABLE TRIGGERS FOR LOAD OF Ccbdbult.

x-FchCorte = DATE(12,31,2017).

FOR EACH Ccbcbult EXCLUSIVE-LOCK WHERE codcia = 001 AND fchdoc <= x-FchCorte:
    DELETE Ccbcbult.
END.
FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = 1 AND
    Ccbcdocu.coddoc = 'G/R' AND
    Ccbcdocu.fchdoc <= x-FchCorte:
    FOR EACH Ccbdbult EXCLUSIVE-LOCK WHERE Ccbdbult.CodCia = Ccbcdocu.codcia AND
        Ccbdbult.CodDoc = Ccbcdocu.coddoc AND 
        Ccbdbult.NroDoc = Ccbcdocu.nrodoc: 
        DELETE Ccbdbult.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

