&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
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
DISABLE TRIGGERS FOR LOAD OF Almtfami.
DISABLE TRIGGERS FOR LOAD OF lg-cocmp.
DISABLE TRIGGERS FOR LOAD OF lg-docmp.

DEFINE NEW SHARED VAR s-codcia as inte init 001.
DEFINE NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.

DEFINE VAR pMensaje AS CHAR NO-UNDO.

def buffer almdmov2 for almdmov.
def buffer almacen2 for almacen.
def buffer almcmov2 for almcmov.
def buffer ccbcdocu2 for ccbcdocu.

DEF TEMP-TABLE t-cocmp LIKE lg-cocmp 
    FIELD pRowid AS ROWID
    INDEX Llave01 AS PRIMARY codcia tpodoc nrodoc.

DEF TEMP-TABLE t-docmp LIKE lg-docmp 
    FIELD pRowid AS ROWID
    INDEX llave01 AS PRIMARY codcia tpodoc nrodoc codmat.


def var f-candes as deci no-undo.

def var desdec as char init "" format "x(6)".
def var hastac as char init "" format "x(6)".
                        
def var i-fchdoc as date format '99/99/9999' init ?.

def buffer B-STKAL FOR AlmStkAl.
def buffer B-STKGE FOR AlmStkGe.
def buffer b-mov   FOR Almtmov.
DEF BUFFER B-MATG  FOR Almmmatg.

def var x-signo  as deci no-undo.  
def var x-newcto as deci no-undo.
def var x-ctomed as deci no-undo.
def var x-stkgen as deci no-undo.
def var x-cto    as deci no-undo.
def var x-movtrf as deci no-undo.
def var x-factor as inte no-undo.
DEF VAR f-Factor AS DEC NO-UNDO.        /* Factor de Equivalencia */
DEF VAR x-SaldoActual AS DEC NO-UNDO.

IF desdec = '' then desdec = '000001'.
IF hastac = '' then hastac = '999999'.

/* Fecha de Cierre */
DEF VAR dFchCie AS DATE NO-UNDO.

IF DAY(TODAY) < 15 
THEN dFchCie = TODAY - DAY(TODAY).
ELSE dFchCie = TODAY.
dFchCie = dFchCie - DAY(dFchCie) + 1. 
i-FchDoc = dFchCie.        /* OJO */
i-FchDoc = ADD-INTERVAL(dFchCie, -1, "months").

/* BUSCAMOS FECHA DE ULTIMA MODIFICACION */
/* FIND Almcfggn WHERE Almcfggn.codcia = s-codcia NO-LOCK NO-ERROR. */
/* IF AVAILABLE Almcfggn AND AlmCfgGn.FchModMin <> ?                */
/*     THEN i-FchDoc = MINIMUM(i-FchDoc, AlmCfgGn.FchModMin).       */

DEFINE NEW SHARED VARIABLE s-tabla      AS CHARACTER.
DEFINE NEW SHARED VARIABLE s-codigo     AS CHARACTER.

ASSIGN
    s-Tabla = "CIERRE"
    s-Codigo = "ALMACEN".
FIND TabGener WHERE TabGener.codcia = s-codcia 
    AND TabGener.Clave = s-Tabla
    AND TabGener.Codigo = s-Codigo
    NO-LOCK NO-ERROR.
IF AVAILABLE TabGener THEN i-FchDoc = MAXIMUM(i-FchDoc, (TabGener.Libre_f01 + 1)).

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
         HEIGHT             = 8.54
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

PUT UNFORMATTED "INICIO: Fecha corte " i-FchDoc " Fecha/Hora Inicio " now skip.

/* SE VA A DIVIDIR EN DOS PARTE
LA 1ra. REVISARÁ SI TODOS LOS MOVIMIENTOS DE VENTAS ESTAN EN EL KARDEX
LA 2da. REGENERAR EL KARDEX */

/* 1ro. */
PUT UNFORMATTED 'Verifica Ventas ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Verifica-Ventas-Kardex.

/* 2do. */
PUT UNFORMATTED 'Regenera Kardex ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Regenera-Kardex.

/* 3ro. */
PUT UNFORMATTED 'Cierre Diario ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Cierre-Diario.

/* 5to */
PUT UNFORMATTED 'Limpia Fecha ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Limpia-Fecha.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Act_Alm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Act_Alm Procedure 
PROCEDURE Act_Alm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN vta2/act_almv2.p (ROWID(Ccbcdocu), OUTPUT pMensaje).
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    PUT UNFORMATTED 
        pMensaje '|'
        ccbcdocu.coddiv '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc SKIP.
    RETURN 'ADM-ERROR'.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Ajusta-OC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ajusta-OC Procedure 
PROCEDURE Ajusta-OC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Fecha AS DATE NO-UNDO.

x-Fecha = TODAY - 180.  /* Retrocedemos 6 meses */

EMPTY TEMP-TABLE t-cocmp.
FOR EACH lg-cocmp NO-LOCK WHERE lg-cocmp.codcia = s-codcia
    AND lg-cocmp.tpodoc = "N"       /* COMPRAS NACIONALES */
    AND LOOKUP(lg-cocmp.flgsit, 'P,T') > 0      /* SOLO Aprobadas y Atendidas */
    AND lg-cocmp.fchdoc >= x-Fecha:
    CREATE t-cocmp.
    BUFFER-COPY lg-cocmp
        TO t-cocmp
        ASSIGN t-cocmp.prowid = ROWID(lg-cocmp).
END.
EMPTY TEMP-TABLE t-docmp.
FOR EACH t-cocmp:
    FOR EACH lg-docmp NO-LOCK WHERE lg-docmp.codcia = s-codcia
        AND lg-docmp.coddiv = t-cocmp.coddiv
        AND lg-docmp.tpodoc = t-cocmp.tpodoc
        AND lg-docmp.nrodoc = t-cocmp.nrodoc:
        CREATE t-docmp.
        BUFFER-COPY lg-docmp
            TO t-docmp
            ASSIGN t-docmp.prowid = ROWID(lg-docmp).
    END.
END.

FOR EACH t-docmp:
    t-docmp.canaten = 0.
END.

FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 'I'
    AND almcmov.codmov = 02     /* COMPRAS NACIONALES */
    AND almcmov.flgest <> 'A'
    AND almcmov.fchdoc >= x-Fecha,
    EACH almdmov OF almcmov NO-LOCK:
    FIND t-cocmp WHERE t-cocmp.codcia = s-codcia
        AND t-cocmp.tpodoc = 'N'
        AND t-cocmp.nrodoc = INTEGER(almcmov.nrorf1)
        AND t-cocmp.codalm = almcmov.codalm
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-cocmp THEN NEXT.
    FIND FIRST t-docmp WHERE t-docmp.codcia = s-codcia
        AND t-docmp.tpodoc = 'N'
        AND t-docmp.nrodoc = INTEGER(almcmov.nrorf1)
        AND t-docmp.codmat = almdmov.codmat
        NO-ERROR.
    IF NOT AVAILABLE t-docmp THEN NEXT.
    ASSIGN
        t-docmp.canaten = t-docmp.canaten + almdmov.candes.
END.
/* buscamos errores */
FOR EACH t-cocmp WHERE t-cocmp.codcia = s-codcia:
    IF CAN-FIND(FIRST t-docmp WHERE t-docmp.codcia = s-codcia
                AND t-docmp.tpodoc = t-cocmp.tpodoc
                AND t-docmp.coddiv = t-cocmp.coddiv
                AND t-docmp.nrodoc = t-cocmp.nrodoc
                AND t-docmp.canpedi > t-docmp.canaten
                NO-LOCK)
        THEN t-cocmp.flgsit = "P".
END.
FOR EACH t-cocmp:
    FIND lg-cocmp WHERE ROWID(lg-cocmp) = t-cocmp.prowid.
    IF lg-cocmp.flgsit <> t-cocmp.flgsit THEN lg-cocmp.flgsit = t-cocmp.flgsit.
    FOR EACH t-docmp WHERE t-docmp.codcia = s-codcia
        AND t-docmp.coddiv = t-cocmp.coddiv
        AND t-docmp.tpodoc = t-cocmp.tpodoc
        AND t-docmp.nrodoc = t-cocmp.nrodoc:
        FIND lg-docmp WHERE ROWID(lg-docmp) = t-docmp.prowid.
        IF t-docmp.canaten <> lg-docmp.canate THEN lg-docmp.canaten = t-docmp.canate.
    END.
END.
IF AVAILABLE(lg-cocmp) THEN RELEASE lg-cocmp.
IF AVAILABLE(lg-docmp) THEN RELEASE lg-docmp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Cierre-Diario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierre-Diario Procedure 
PROCEDURE Cierre-Diario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Contador AS INT NO-UNDO.

FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia:
    REPEAT:
        x-Contador = x-Contador + 1.
        IF x-Contador >= 10 THEN LEAVE.     /* Pasa al siguiente */
        FIND FIRST AlmCieAl WHERE Almcieal.codcia = s-codcia
            AND Almcieal.codalm = Almacen.codalm
            AND Almcieal.fchcie = (TODAY - 1)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE AlmCieAl THEN DO:
            IF LOCKED(AlmCieAl) THEN NEXT.  /* Nuevo intento */
            CREATE Almcieal.
            ASSIGN
                Almcieal.codcia = s-codcia
                Almcieal.codalm = Almacen.codalm
                Almcieal.fchcie = TODAY - 1
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO, LEAVE.     /* Pasa al siguiente */
        END.
        ASSIGN
            Almcieal.flgcie = Yes
            Almcieal.usucierr = 'AUTOMATICO'.
        RELEASE AlmCieAl.
        LEAVE.  /* Pasa al siguiente */
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Limpia-Fecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Limpia-Fecha Procedure 
PROCEDURE Limpia-Fecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND Almcfggn WHERE Almcfggn.codcia = s-codcia EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE Almcfggn THEN  AlmCfgGn.FchModMin = ?.
RELEASE Almcfggn.

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
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = S-CODCIA USE-INDEX MATG01:
    PUT UNFORMATTED almmmatg.codmat " " NOW SKIP.
    PAUSE 0.
    RUN alm/calc-costo-promedio (INPUT Almmmatg.codmat, INPUT i-FchDoc, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "ERROR: " + almmmatg.codmat + " bloqueado por otro usuario".
        ELSE pMensaje = "ERROR: " + almmmatg.codmat + " " + pMensaje.
        PUT UNFORMATTED pMensaje " " NOW SKIP.
        NEXT.
    END.
    /* RHC 24/07/2015 Recalcula Margenes de Utilidad */
    RUN Margenes.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Verifica-Ventas-Kardex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica-Ventas-Kardex Procedure 
PROCEDURE Verifica-Ventas-Kardex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* RHC 25/11/2019 */
RETURN.


DEF VAR x-Comprobantes AS CHAR INIT 'FAC,BOL' NO-UNDO.
DEF VAR x-Error AS CHAR FORMAT 'x(50)' NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR x-FchDoc AS DATE NO-UNDO.

x-FchDoc = i-FchDoc.
IF x-FchDoc < 11/01/2012 THEN x-FchDoc = 11/01/2012.

/* *********************************************************************************************** */
/* 1RO VERIFICAMOS LOS MOVIMIENTOS DE ALMACEN */
/* *********************************************************************************************** */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia AND almacen.coddiv = gn-divi.coddiv,
    EACH almcmov WHERE almcmov.codcia = s-codcia
        AND almcmov.codalm = almacen.codalm
        AND almcmov.tipmov = "S"    /* Salidas */
        AND almcmov.codmov = 02     /* Por Ventas */
        AND almcmov.fchdoc >= x-FchDoc
        AND almcmov.fchdoc < TODAY - 1 EXCLUSIVE-LOCK
    TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR':
    /* COnsistencia de Anulados */
    k = 0.
    FOR EACH almdmov OF almcmov NO-LOCK:
        k = k + 1.
    END.
    IF almcmov.flgest = "A" AND k > 0 THEN DO:
        FOR EACH almdmov OF almcmov EXCLUSIVE-LOCK:
            DELETE almdmov.
        END.
        /* buscamos si el comprobante está anulado */
        FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = s-codcia
            AND ccbcdocu.coddoc = almcmov.codref
            AND ccbcdocu.nrodoc = almcmov.nroref
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu OR ccbcdocu.flgest <> "A" THEN DELETE almcmov.
        NEXT.   /* Siguiente Registro */
    END.
    /* Consistencia del comprobante */
    FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddoc = almcmov.codref
        AND ccbcdocu.nrodoc = almcmov.nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN DO:
        FOR EACH almdmov OF almcmov EXCLUSIVE-LOCK:
            DELETE almdmov.
        END.
        DELETE almcmov.
        NEXT.   /* Siguiente Registro */
    END.
    /* Consistencia de Fechas */
    /* RHC 28/04/2015 NO para la 00500 WEB UTILEX */
    IF Ccbcdocu.CodDiv <> "00500" AND almcmov.fchdoc <> ccbcdocu.fchdoc THEN DO:
        almcmov.fchdoc = ccbcdocu.fchdoc.
        FOR EACH almdmov OF almcmov EXCLUSIVE-LOCK:
            almdmov.fchdoc = ccbcdocu.fchdoc.
        END.
    END.
    /* Consistencia de detalles */
    FOR EACH almdmov OF almcmov EXCLUSIVE-LOCK:
        FIND FIRST ccbddocu OF ccbcdocu WHERE ccbddocu.codmat = almdmov.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbddocu THEN DO:
            DELETE almdmov.
            NEXT.   /* Siguiente Registro */
        END.
        IF (almdmov.candes * almdmov.factor) <> (ccbddocu.candes * ccbddocu.factor) THEN DO:
            DELETE almdmov.
            NEXT.   /* Siguiente Registro */
        END.
    END.
END.
/* *********************************************************************************************** */
/*  2DO VERIFICAMOS LOS COMPROBANTES */
/* *********************************************************************************************** */

PORCOMPROBANTES:
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND LOOKUP(ccbcdocu.coddoc, x-comprobantes) > 0
        AND LOOKUP(CcbCdocu.TpoFac, 'A,S,B,F,M') = 0    /* NO Adelanto, Servicio, Canje Sunat, x FAI ni x Mercaderia*/
        AND ccbcdocu.fchdoc >= x-FchDoc
        AND ccbcdocu.fchdoc < TODAY - 1
    TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR':
    /* *********************************************************************************************** */
    /* POR FAI */
    /* *********************************************************************************************** */
    IF LOOKUP(Ccbcdocu.CodDoc, "FAC,BOL") > 0 AND Ccbcdocu.CodRef = "FAI" THEN NEXT.
    /* *********************************************************************************************** */
    /* ANULADOS */
    /* *********************************************************************************************** */
    IF ccbcdocu.flgest = "A" THEN DO:
        FOR EACH almcmov WHERE almcmov.codcia = s-codcia
            AND almcmov.tipmov = "S"
            AND almcmov.codmov = 02
            AND almcmov.codref = ccbcdocu.coddoc
            AND almcmov.nroref = ccbcdocu.nrodoc EXCLUSIVE-LOCK:
            FOR EACH almdmov OF almcmov EXCLUSIVE-LOCK:
                DELETE almdmov.
            END.
            almcmov.flgest = "A".
        END.
        NEXT PORCOMPROBANTES.   /* Siguiente Registro */
    END.
    /* *********************************************************************************************** */
    /* CABECERAS */
    /* *********************************************************************************************** */
    FIND almcmov WHERE almcmov.codcia = s-codcia
        AND almcmov.tipmov = "S"
        AND almcmov.codmov = 02
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc
        AND almcmov.flgest <> 'A'
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE almcmov THEN DO:
        IF AMBIGUOUS almcmov THEN DO:
            x-Error = "COMPROBANTE REPETIDO EN ALMACEN".
            PUT UNFORMATTED
                x-Error '|'
                ccbcdocu.coddiv '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.fchdoc SKIP.
            FOR EACH almcmov WHERE almcmov.codcia = s-codcia
                AND almcmov.tipmov = "S"
                AND almcmov.codmov = 02
                AND almcmov.codref = ccbcdocu.coddoc
                AND almcmov.nroref = ccbcdocu.nrodoc EXCLUSIVE-LOCK:
                FOR EACH almdmov OF almcmov EXCLUSIVE-LOCK:
                    DELETE almdmov.
                END.
                DELETE almcmov.
            END.
            /* CORREGIR ERROR */
            RUN Act_Alm.
            NEXT PORCOMPROBANTES.
        END.
        x-Error = "COMPROBANTE NO REGISTRADO EN ALMACEN".
        PUT UNFORMATTED
            x-Error '|'
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc SKIP.
        /* CORREGIR ERROR */
        RUN Act_Alm.
        NEXT PORCOMPROBANTES.
    END.
    /* DETALLLE */
    x-Error = "".
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
        FIND almdmov OF almcmov WHERE almdmov.codmat = ccbddocu.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE almdmov OR ccbddocu.candes <> almdmov.candes THEN DO:
            IF AMBIGUOUS almdmov THEN x-Error = "PRODUCTO: " + ccbddocu.codmat + " REPETIDO EN MOV ALMACEN".
            ELSE x-Error = "PRODUCTO: " + ccbddocu.codmat + " NO REGISTRADO EN MOV ALMACEN".
        END.
    END.
    IF x-Error <> "" THEN DO:
        PUT UNFORMATTED
            x-Error '|'
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc SKIP.
        FOR EACH almdmov OF almcmov:
            DELETE almdmov.
        END.
        DELETE almcmov.
        /* CORREGIR ERROR */
        RUN Act_Alm.
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

