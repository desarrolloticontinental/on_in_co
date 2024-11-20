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

/* Solo del día de hoy */
i-FchDoc = TODAY.

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
         HEIGHT             = 6.96
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

PUT UNFORMATTED "INICIO v3: Fecha corte " i-FchDoc " Fecha/Hora Inicio " now skip.

DEF VAR pRowid AS ROWID NO-UNDO.

RUN Log-Inicio (OUTPUT pRowid).

/* SE VA A DIVIDIR EN DOS PARTE
LA 1ra. REVISARÁ SI TODOS LOS MOVIMIENTOS DE VENTAS ESTAN EN EL KARDEX
LA 2da. REGENERAR EL KARDEX */

/* 1ro. */
PUT UNFORMATTED 'INICIO ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Carga-Temporal.

/* 2do. */
PUT UNFORMATTED 'Regenera Kardex ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Regenera-Kardex.

PUT UNFORMATTED 'FIN    ' STRING(DATETIME(TODAY, MTIME)) SKIP.

RUN Log-Fin (INPUT pRowid).

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
  Notes:       Solo almacenes de tiendas UTILEX
  Agregamos las divisiones LPG 00524 y 00525
------------------------------------------------------------------------------*/

/* Divisiones válidas */
DEF VAR pDivisiones AS CHAR NO-UNDO.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND
    gn-divi.campo-log[1] = NO AND
    LOOKUP(gn-divi.canalventa, 'MIN,B2C') > 0:
    pDivisiones = pDivisiones + (IF TRUE <> (pDivisiones > '') THEN '' ELSE ',') +
        gn-divi.coddiv.
END.

PUT UNFORMATTED 'Divisiones: ' pdivisiones SKIP.

EMPTY TEMP-TABLE T-MATG.
FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = s-CodCia
        AND LOOKUP(Almacen.CodDiv, pDivisiones) > 0,
    EACH Almdmov NO-LOCK WHERE Almdmov.CodCia = s-CodCia AND
        Almdmov.CodAlm = Almacen.CodAlm AND
        Almdmov.FchDoc >= i-FchDoc:
    FIND FIRST T-MATG WHERE T-MATG.CodCia = s-CodCia AND
        T-MATG.CodMat = Almdmov.CodMat NO-ERROR.
    IF NOT AVAILABLE T-MATG THEN DO:
        PUT UNFORMATTED "Cargando " Almdmov.codmat SKIP.
        CREATE T-MATG.
    END.
    ASSIGN
        T-MATG.CodCia = s-CodCia
        T-MATG.CodMat = Almdmov.CodMat.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Log-Fin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Log-Fin Procedure 
PROCEDURE Log-Fin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.

FIND LogBacheros WHERE ROWID(LogBacheros) = pRowid.
ASSIGN
    LogBacheros.FechaFin = TODAY
    LogBacheros.HoraFin = STRING(TIME,'HH:MM:SS')
    LogBacheros.Estado = 'CONCLUIDO'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Log-Inicio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Log-Inicio Procedure 
PROCEDURE Log-Inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pRowid AS ROWID.

CREATE LogBacheros.
ASSIGN
    LogBacheros.Detalle = 'Actualiza el stock en la tabla Almmmate de los almacenes de UTILEX'
    LogBacheros.Grupo = 'Stocks'
    LogBacheros.FechaInicio = TODAY
    LogBacheros.HoraInicio = STRING(TIME,'HH:MM:SS')
    LogBacheros.Tabla = 'Almmmate'
    LogBacheros.Usuario = s-User-Id.
ASSIGN pRowid = ROWID(LogBacheros).

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

CATALOGO:
FOR EACH T-MATG NO-LOCK WHERE T-MATG.CodCia = s-CodCia,
    FIRST Almmmatg OF T-MATG NO-LOCK:
    PUT UNFORMATTED "Procesando " almmmatg.codmat " " NOW SKIP.
    RUN /v/IN/ON_IN_CO/prg/alm/calc-costo-promedio (INPUT Almmmatg.codmat, INPUT i-FchDoc, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        PUT UNFORMATTED "ERROR " pmensaje " " NOW SKIP.
        NEXT.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

