&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : VOLVER A REENVIAR LA INFORMACION DE UTILEX A LIMA

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       : SE DEBE EJECUTAR DESDE EL SERVIDOR PRINCIPAL WINDOWS
                    PARA QUE SE EJECUTEN LAS REPLICAS
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.

DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
DEF VAR x-FchDoc-2 AS DATE NO-UNDO.

/* Debe ser el mes pasado */
/* ASSIGN                                              */
/*     x-FchDoc-1 = TODAY                              */
/*     x-FchDoc-2 = TODAY.                             */
/* x-FchDoc-1 = ADD-INTERVAL(x-FchDoc-1, -30, 'days'). */
/* x-FchDoc-1 = x-FchDoc-1 - DAY(x-FchDoc-1) + 1.      */
/* x-FchDoc-2 = TODAY - DAY(x-FchDoc-2).               */

/* Solo se volverá a replicar los datos de dos dias atrás */
/* OJO: correr el CRON en la madrugada 1:00am */
ASSIGN
    x-FchDoc-1 = ADD-INTERVAL(TODAY, -2, 'days')
    x-FchDoc-2 = ADD-INTERVAL(TODAY, -1, 'days').

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
         HEIGHT             = 5.38
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
    AND LOOKUP(ccbcdocu.coddoc, 'TCK,FAC,BOL,N/C') > 0
    AND ccbcdocu.fchdoc >= x-fchdoc-1
    AND ccbcdocu.fchdoc <= x-fchdoc-2:
    DISPLAY ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc.
    PAUSE 0.
    /* *************************** */
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
        RUN Replica-ccbddocu.
    END.
    RUN Replica-ccbcdocu.
END.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Replica-ccbcdocu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-ccbcdocu Procedure 
PROCEDURE Replica-ccbcdocu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = ccbcdocu
    &Key    = "string(ccbcdocu.codcia,'999') + string(ccbcdocu.coddiv, 'x(5)') ~
        + string(ccbcdocu.coddoc, 'x(3)') + string(ccbcdocu.nrodoc, 'x(15)') "
    &Prg    = r-ccbcdocu
    &Event  = WRITE
    &FlgDB0 = NO
    &FlgDB1 = TRUE
    &FlgDB2 = TRUE
    &FlgDB3 = TRUE
    &FlgDB4 = TRUE
    &FlgDB5 = TRUE
    &FlgDB6 = TRUE
    &FlgDB7 = TRUE
    &FlgDB8 = TRUE
    &FlgDB9 = TRUE
    &FlgDB10 = TRUE
    &FlgDB11 = TRUE
    &FlgDB12 = TRUE
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE
    &FlgDB19 = TRUE
    &FlgDB20 = TRUE
    &FlgDB30 = TRUE
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-ccbdcaja) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-ccbdcaja Procedure 
PROCEDURE Replica-ccbdcaja :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = ccbdcaja
    &Key    =  "string(ccbdcaja.codcia,'999') + string(ccbdcaja.coddoc,'x(3)') ~
        + string(ccbdcaja.nrodoc,'x(15)') + string(ccbdcaja.codref,'x(3)') ~
        + string(ccbdcaja.nroref,'x(15)')"
    &Prg    = r-ccbdcaja
    &Event  = WRITE
    &FlgDB0 = FALSE
    &FlgDB1 = TRUE      
    &FlgDB2 = TRUE
    &FlgDB3 = TRUE
    &FlgDB4 = TRUE
    &FlgDB5 = TRUE
    &FlgDB6 = TRUE
    &FlgDB7 = TRUE
    &FlgDB8 = TRUE
    &FlgDB9 = TRUE
    &FlgDB10 = TRUE
    &FlgDB11 = TRUE
    &FlgDB12 = TRUE
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE
    &FlgDB19 = TRUE
    &FlgDB20 = TRUE
    &FlgDB30 = TRUE
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-ccbddocu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-ccbddocu Procedure 
PROCEDURE Replica-ccbddocu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = ccbddocu
    &Key    =  "string(ccbddocu.codcia,'999') + string(ccbddocu.coddiv,'x(5)') ~
        + string(ccbddocu.coddoc,'x(3)') + string(ccbddocu.nrodoc,'x(15)') ~
        + string(ccbddocu.codmat,'x(6)')"
    &Prg    = r-ccbddocu
    &Event  = WRITE
    &FlgDB0 = NO
    &FlgDB1 = TRUE
    &FlgDB2 = TRUE
    &FlgDB3 = TRUE
    &FlgDB4 = TRUE
    &FlgDB5 = TRUE
    &FlgDB6 = TRUE
    &FlgDB7 = TRUE
    &FlgDB8 = TRUE
    &FlgDB9 = TRUE
    &FlgDB10 = TRUE
    &FlgDB11 = TRUE
    &FlgDB12 = TRUE
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE
    &FlgDB19 = TRUE
    &FlgDB20 = TRUE
    &FlgDB30 = TRUE
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

