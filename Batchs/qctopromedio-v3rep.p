&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : qctopromedio
    Purpose     : calculo del kardex por producto

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* NO Triggers */
disable triggers for load of almcmov.
disable triggers for load of almdmov.
disable triggers for load of almmmate.
disable triggers for load of almmmatg.
disable triggers for load of almstkge.
disable triggers for load of almstkal.
disable triggers for load of almcieal.

DEFINE NEW SHARED VAR s-codcia as inte init 001.
DEFINE NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.

DEFINE VAR pMensaje AS CHAR NO-UNDO.

def var f-candes as deci no-undo.
def var i-fchdoc as date format '99/99/9999' init ?.

def buffer B-STKAL FOR AlmStkAl.
def buffer B-STKGE FOR AlmStkGe.
def buffer b-mov   FOR Almtmov.
DEF BUFFER B-MATG  FOR Almmmatg.

i-FchDoc = ADD-INTERVAL(TODAY, -3, "months").
/* Siempre desde el primer día del mes pasado */
i-FchDoc = i-FchDoc - DAY(i-FchDoc) + 1.


DEF STREAM LogErrores.

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
      TABLE: T-MATG T "?" NO-UNDO INTEGRAL Almmmatg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.58
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

PUT UNFORMATTED "INICIO v3rep: Fecha corte " i-FchDoc " Fecha/Hora Inicio " now skip.

/* SE VA A DIVIDIR EN DOS PARTE
LA 1ra. REVISARÁ SI TODOS LOS MOVIMIENTOS DE VENTAS ESTAN EN EL KARDEX
LA 2da. REGENERAR EL KARDEX */

/* 1ro. */
PUT UNFORMATTED 'Carga Temporal ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Carga-Temporal.

/* 2do. */
PUT UNFORMATTED 'Regenera Kardex ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Regenera-Kardex.

/* 3ro */
PUT UNFORMATTED 'Margenes de Utilidad ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Margenes-de-Utilidad.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-Temporal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal Procedure 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE T-MATG.

DEF VAR x-CodMat AS CHAR NO-UNDO.

INPUT FROM /u/backup/IN/ON_IN_CO/log/logkardex01.txt.
REPEAT:
    IMPORT UNFORMATTED x-CodMat.
    IF TRUE <> (x-CodMat > '') THEN LEAVE.
    CREATE T-MATG.
    ASSIGN
        T-MATG.CodCia = s-CodCia
        T-MATG.Codmat = x-CodMat.
END.
INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Margenes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Margenes Procedure 
PROCEDURE Margenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE F-PreVta-A AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-PreVta-B AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-PreVta-C AS DECIMAL NO-UNDO.
  DEFINE VARIABLE X-CTOUND AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-MrgUti-A AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-MrgUti-B AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-MrgUti-C AS DECIMAL NO-UNDO.
  DEFINE VARIABLE f-MonVta LIKE Almmmatg.MonVta NO-UNDO.
  DEFINE VARIABLE f-MrgLis AS DECIMAL NO-UNDO.
  DEFINE VARIABLE f-Factor AS DECIMAL NO-UNDO.

  IF Almmmatg.CtoTot <= 0 THEN RETURN.

  /**** MARGEN PRECIO DE LISTA ****/
  ASSIGN f-MrgLis = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100.
  /****   MARGEN A   ****/
  ASSIGN
      F-PreVta-A = Almmmatg.Prevta[2]
      X-CTOUND = Almmmatg.CtoTot
      F-FACTOR = 1
      F-MrgUti-A = 0.    
  IF Almmmatg.UndA <> "" THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
          AND  Almtconv.Codalter = Almmmatg.UndA
          NO-LOCK NO-ERROR.
      IF AVAILABLE Almtconv THEN DO:
          F-FACTOR = Almtconv.Equival.
          F-MrgUti-A = ROUND(((((F-PreVta-A / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
      END.
  END.
  ASSIGN
      F-PreVta-B = Almmmatg.Prevta[3]
      X-CTOUND = Almmmatg.CtoTot
      F-FACTOR = 1
      F-MrgUti-B = 0.   
  /****   MARGEN B   ****/
  IF Almmmatg.UndB <> "" THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
          AND  Almtconv.Codalter = Almmmatg.UndB
          NO-LOCK NO-ERROR.
      IF AVAILABLE Almtconv THEN DO:
          F-FACTOR = Almtconv.Equival.
          F-MrgUti-B = ROUND(((((F-PreVta-B / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
      END.
  END.
  /****   MARGEN C   ****/
  ASSIGN
      F-PreVta-C = Almmmatg.Prevta[4]
      X-CTOUND = Almmmatg.CtoTot
      F-FACTOR = 1
      F-MrgUti-C = 0.
  IF Almmmatg.UndC <> "" THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
          AND  Almtconv.Codalter = Almmmatg.UndC
          NO-LOCK NO-ERROR.
      IF AVAILABLE Almtconv THEN DO:
          F-FACTOR = Almtconv.Equival.
          F-MrgUti-C = ROUND(((((F-PreVta-C / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
      END.
  END.

  /**** MARGEN PRECIO DE OFICINA ****/
  DEFINE VARIABLE fMot LIKE Almmmatg.PreOfi.
  DEFINE VARIABLE MrgOfi LIKE Almmmatg.MrgUti-A.

  ASSIGN
      fMot   = 0
      MrgOfi = 0
      F-FACTOR = 1
      X-CTOUND = Almmmatg.CtoTot.
  CASE Almmmatg.Chr__02 :
      WHEN "T" THEN DO:        
          IF Almmmatg.Chr__01 <> "" THEN DO:
              FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                  AND  Almtconv.Codalter = Almmmatg.Chr__01
                  NO-LOCK NO-ERROR.
              IF AVAILABLE Almtconv THEN DO:
                  F-FACTOR = Almtconv.Equival.
              END.
          END.
          fMot = Almmmatg.PreOfi / X-CTOUND / F-FACTOR.
          MrgOfi = ROUND((fMot - 1) * 100, 6).
      END.
      WHEN "P" THEN DO:
          MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
      END. 
  END.    

  /* Grabamos */
  {lib/lock-genericov3.i
      &Tabla="B-MATG"
      &Condicion="ROWID(B-MATG) = ROWID(Almmmatg)"
      &Bloqueo="EXCLUSIVE-LOCK NO-ERROR NO-WAIT"
      &Accion="RETRY"
      &Mensaje="NO"
      &TipoError="UNDO, RETURN 'ADM-ERROR'"
      &Intentos="10"
      }
  ASSIGN
      B-MATG.MrgUti   = f-MrgLis
      B-MATG.MrgUti-A = f-MrgUti-A
      B-MATG.MrgUti-B = f-MrgUti-B
      B-MATG.MrgUti-C = f-MrgUti-C
      B-MATG.DEC__01  = MrgOfi.
  IF AVAILABLE(B-MATG) THEN RELEASE B-MATG.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Margenes-de-Utilidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Margenes-de-Utilidad Procedure 
PROCEDURE Margenes-de-Utilidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CATALOGO:
FOR EACH T-MATG NO-LOCK WHERE T-MATG.CodCia = s-CodCia, FIRST Almmmatg OF T-MATG NO-LOCK:
    /* RHC 24/07/2015 Recalcula Margenes de Utilidad */
    RUN Margenes.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Regenera-Kardex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Regenera-Kardex Procedure 
PROCEDURE Regenera-Kardex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pMensaje AS CHAR NO-UNDO.

OUTPUT STREAM LogErrores TO /u/backup/IN/ON_IN_CO/log/logkardex02.txt.
PUT STREAM LogErrores UNFORMATTED "INICIO: " NOW SKIP.
CATALOGO:
FOR EACH T-MATG NO-LOCK WHERE T-MATG.CodCia = s-CodCia,
    FIRST Almmmatg OF T-MATG NO-LOCK:
    PUT UNFORMATTED almmmatg.codmat " " NOW SKIP.
    PAUSE 0.
    RUN /v/IN/ON_IN_CO/prg/alm/calc-costo-promedio (INPUT Almmmatg.codmat, INPUT i-FchDoc, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        PUT STREAM LogErrores UNFORMATTED Almmmatg.CodMat SKIP.
        NEXT.
    END.
END.
PUT STREAM LogErrores UNFORMATTED "FIN: " NOW SKIP.
OUTPUT STREAM LogErrores CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

