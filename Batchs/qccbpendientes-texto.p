&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
          estavtas         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-CDOCU FOR CcbCDocu.
DEFINE TEMP-TABLE t-ccb_pendientes NO-UNDO LIKE ccb_pendientes.



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

DEFINE TEMP-TABLE ttCanjes
    FIELD   tcoddoc     AS  CHAR    FORMAT 'x(5)'
    FIELD   tnrodoc     AS  CHAR    FORMAT 'x(15)'
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-f-get-referencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-get-referencia Procedure 
FUNCTION f-get-referencia RETURNS CHARACTER
  ( INPUT pCodDiv AS CHAR, INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fBanco) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBanco Procedure 
FUNCTION fBanco RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fEstado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEstado Procedure 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fSituacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSituacion Procedure 
FUNCTION fSituacion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fUbicacion Procedure 
FUNCTION fUbicacion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importe-fotocopia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD importe-fotocopia Procedure 
FUNCTION importe-fotocopia RETURNS DECIMAL
  ( INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: B-CDOCU B "?" ? INTEGRAL CcbCDocu
      TABLE: t-ccb_pendientes T "?" NO-UNDO estavtas ccb_pendientes
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 9.31
         WIDTH              = 59.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

PUT 'INICIO: ' NOW SKIP.
RUN Borra-Tabla.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    PUT 'NO se pudo borrar la tabla ' NOW SKIP.
    QUIT.
END.


DEF VAR f-Estado AS CHAR INIT 'P,J' NO-UNDO.
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.

DEF VAR x-Comprobantes AS CHAR NO-UNDO.
FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-codcia AND FacDocum.TpoDoc <> ?:
    x-Comprobantes = x-Comprobantes + (IF x-Comprobantes = '' THEN '' ELSE ',') + FacDocum.CodDoc.
END.

DEF VAR k AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DO k = 1 TO NUM-ENTRIES(f-Estado):
    DO j =1 TO NUM-ENTRIES(x-Comprobantes):
        FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia AND
            Ccbcdocu.flgest = ENTRY(k, f-Estado) AND
            Ccbcdocu.coddoc = ENTRY(j, x-Comprobantes):
            RUN Carga-Tabla.
        END.
    END.
END.

PUT UNFORMATTED 'PASA TEXTO ' NOW SKIP.

RUN Pasa-Texto.

PUT 'FIN: ' NOW SKIP.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Tabla) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Tabla Procedure 
PROCEDURE Borra-Tabla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE t-ccb_pendientes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Tabla) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Tabla Procedure 
PROCEDURE Carga-Tabla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Factor AS INT NO-UNDO.
DEF VAR x-CodDoc AS CHAR NO-UNDO.
DEF VAR x-doc-ref AS CHAR.
DEF VAR x-NomCli LIKE ccbcdocu.nomcli NO-UNDO.
DEF VAR x-DirCli LIKE ccbcdocu.dircli NO-UNDO.
DEF VAR x-NomVen LIKE gn-ven.NomVen NO-UNDO.

CREATE t-ccb_pendientes.
ASSIGN
    t-ccb_pendientes.CodDiv = ccbcdocu.divori    /* OJO */
    t-ccb_pendientes.CodDoc = ccbcdocu.coddoc
    t-ccb_pendientes.NroDoc = ccbcdocu.nrodoc
    t-ccb_pendientes.CodRef = ccbcdocu.codref
    t-ccb_pendientes.NroRef = ccbcdocu.nroref
    t-ccb_pendientes.FchDoc = ccbcdocu.fchdoc
    t-ccb_pendientes.FchVto = ccbcdocu.fchvto
    t-ccb_pendientes.CodCli = ccbcdocu.codcli
    t-ccb_pendientes.NomCli = ccbcdocu.nomcli
    t-ccb_pendientes.DirCli = ccbcdocu.dircli
    t-ccb_pendientes.FmaPgo = ccbcdocu.fmapgo.

RUN lib/limpiar-texto (t-ccb_pendientes.NomCli, ' ', OUTPUT x-NomCli).
RUN lib/limpiar-texto (t-ccb_pendientes.DirCli, ' ', OUTPUT x-DirCli).
ASSIGN
    t-ccb_pendientes.NomCli = x-NomCli
    t-ccb_pendientes.DirCli = x-DirCli.

IF ccbcdocu.codmon = 1 THEN
    ASSIGN
        t-ccb_pendientes.Importe = ccbcdocu.imptot
        t-ccb_pendientes.Saldo   = ccbcdocu.sdoact.
ELSE
    ASSIGN
        t-ccb_pendientes.ImporteUS = ccbcdocu.imptot
        t-ccb_pendientes.SaldoUS   = ccbcdocu.sdoact.
/* Fin de filtro principal */ 
x-Factor = 1. 
FIND FacDocum OF CcbCDocu NO-LOCK NO-ERROR. 
IF AVAILABLE FacDocum THEN x-Factor = (IF FacDocum.TpoDoc = NO THEN -1 ELSE 1). 
IF Ccbcdocu.flgest = 'A' THEN x-Factor = 0. 
ASSIGN 
    t-ccb_pendientes.Importe = x-Factor * t-ccb_pendientes.Importe
    t-ccb_pendientes.Saldo = x-Factor * t-ccb_pendientes.Saldo
    t-ccb_pendientes.ImporteUS = x-Factor * t-ccb_pendientes.ImporteUS
    t-ccb_pendientes.SaldoUS = x-Factor * t-ccb_pendientes.SaldoUS
    .
x-doc-ref = f-get-referencia(t-ccb_pendientes.coddiv, t-ccb_pendientes.coddoc, t-ccb_pendientes.nrodoc). 
ASSIGN 
    t-ccb_pendientes.codref = ENTRY(1,x-doc-ref,"|") 
    t-ccb_pendientes.nroref = ENTRY(2,x-doc-ref,"|").
/* Vendedor */
ASSIGN
    t-ccb_pendientes.Estado = fEstado().
FIND gn-ven WHERE gn-ven.CodCia = s-codcia
    AND gn-ven.CodVen = ccbcdocu.codven NO-LOCK NO-ERROR.
IF AVAILABLE gn-ven THEN DO:
    t-ccb_pendientes.NomVen = gn-ven.NomVen.
    RUN lib/limpiar-texto (t-ccb_pendientes.NomVen, ' ', OUTPUT x-NomVen).
    t-ccb_pendientes.NomVen = x-NomVen.
END.

FIND Ccbaudit OF ccbcdocu NO-LOCK NO-ERROR.
IF AVAILABLE Ccbaudit THEN FIND ccbtabla WHERE ccbtabla.codcia = ccbcdocu.codcia
    AND ccbtabla.tabla = 'MA'
    AND ccbtabla.codigo = ccbaudit.codref NO-LOCK NO-ERROR.
t-ccb_pendientes.Fecha = TODAY.
t-ccb_pendientes.Ubicacion = fUbicacion().
t-ccb_pendientes.Banco = fBanco().
t-ccb_pendientes.Situacion = fSituacion().
/* RHC 03/04/17 Verifica si tiene canje pendiente */
DEF VAR x-Canje AS CHAR NO-UNDO.
x-Canje = "".
IF LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
    AND ccbcdocu.flgest = "P"
    AND ccbcdocu.FlgSit = 'X' THEN DO:
    /* Buscamos canje */
    CANJE:
    FOR EACH ccbcmvto NO-LOCK WHERE CcbCMvto.CodCia = ccbcdocu.codcia
        AND CcbCMvto.CodCli = ccbcdocu.codcli
        AND CcbCMvto.CodDoc = "CJE"
        AND CcbCMvto.FlgEst = "P"
        AND CcbCMvto.FchDoc >= ccbcdocu.fchdoc:
        FOR EACH Ccbdmvto NO-LOCK WHERE CcbDMvto.CodCia = Ccbcmvto.codcia
            AND CcbDMvto.CodDoc = Ccbcmvto.coddoc
            AND CcbDMvto.NroDoc = Ccbcmvto.nrodoc
            AND CcbDMvto.TpoRef = "O"
            AND CcbDMvto.CodRef = Ccbcdocu.coddoc
            AND CcbDMvto.NroRef = Ccbcdocu.nrodoc:
            x-Canje = "CANJE DE LETRA".
            LEAVE CANJE.
        END.
    END.
END.
IF x-Canje > '' THEN DO:
    t-ccb_pendientes.Canje = x-Canje.
END.
/* RHC 06/06/2017 Datos adicionales Julissa Calderon */
DEF VAR pCodDepto AS CHAR NO-UNDO.
DEF VAR pNomDepto AS CHAR NO-UNDO.
DEF VAR pCodProvi AS CHAR NO-UNDO.
DEF VAR pNomProvi AS CHAR NO-UNDO.
DEF VAR pCodDistr AS CHAR NO-UNDO.
DEF VAR pNomDistr AS CHAR NO-UNDO.
FIND gn-clie WHERE gn-clie.codcia = cl-codcia AND 
    gn-clie.codcli = ccbcdocu.codcli NO-LOCK NO-ERROR.
IF AVAILABLE gn-clie THEN DO:
    RUN gn/ubigeo-cliente (
        INPUT gn-clie.CodCia ,
        INPUT gn-clie.CodCli ,
        OUTPUT pCodDepto, 
        OUTPUT pNomDepto, 
        OUTPUT pCodProvi, 
        OUTPUT pNomProvi, 
        OUTPUT pCodDistr, 
        OUTPUT pNomDistr 
        ).
    t-ccb_pendientes.Departamento = pNomDepto.
    t-ccb_pendientes.Provincia = pNomProvi.
    t-ccb_pendientes.Distrito = pNomDistr.
END.
t-ccb_pendientes.NroUnico = (IF ccbcdocu.coddoc = 'LET' THEN ccbcdocu.nrosal ELSE '').

/* Ic - 30Oct2018, pedido de Julissa Calderon*/
DEF VAR lde_ImpFoto AS DEC NO-UNDO.

lde_ImpFoto = 0.
x-factor = 0.
IF ccbcdocu.sdoact > 0 THEN DO:
    /* % Saldo pendiente */
    x-factor = ccbcdocu.sdoact / ccbcdocu.imptot.
    IF ccbcdocu.coddoc = 'LET' AND ccbcdocu.codref = 'CJE' THEN DO:
        lde_impfoto = importe-fotocopia(INPUT ccbcdocu.codref, INPUT ccbcdocu.nroref).
    END.
    ELSE DO:
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK, FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = Ccbddocu.codcia
            AND Almmmatg.codmat = Ccbddocu.codmat
            AND Almmmatg.codfam = '011':
            lde_ImpFoto = lde_ImpFoto + Ccbddocu.ImpLin.
        END.
    END.
    lde_ImpFoto = lde_ImpFoto * x-factor.
END.
/* Es papel fotocopia */
IF lde_ImpFoto > 0 THEN DO:        
    IF ccbcdocu.codmon = 2 THEN DO:
        t-ccb_pendientes.FotocopiaUS = lde_ImpFoto.    /*lde_ImpFoto.*/
    END.
    ELSE DO:
        t-ccb_pendientes.FotocopiaNac = lde_ImpFoto.     /*lde_ImpFoto.*/
    END.
END.

/* COTIZACION */
IF Ccbcdocu.coddoc = "N/C" THEN DO:
    FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia
        AND B-CDOCU.CodDoc = CcbCdocu.Codref
        AND B-CDOCU.NroDoc = CcbCdocu.Nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDOCU THEN RETURN.
    FIND Faccpedi WHERE Faccpedi.codcia = B-CDOCU.codcia
        AND Faccpedi.coddoc = B-CDOCU.codped    /* PED */
        AND Faccpedi.nroped = B-CDOCU.nroped    
        NO-LOCK NO-ERROR.
END.
ELSE FIND Faccpedi WHERE Faccpedi.codcia = Ccbcdocu.codcia
    AND Faccpedi.coddoc = Ccbcdocu.codped   /* PED */
    AND Faccpedi.nroped = Ccbcdocu.nroped
    NO-LOCK NO-ERROR.
IF AVAILABLE Faccpedi THEN DO:
    ASSIGN
        t-ccb_pendientes.CodOrig  = Faccpedi.codref.
        t-ccb_pendientes.NroOrig  = Faccpedi.nroref.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pasa-Texto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pasa-Texto Procedure 
PROCEDURE Pasa-Texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Archivo AS CHAR NO-UNDO.

x-Archivo = "/home/v/IN/dbs/" + "ccb_pendientes.txt".

OUTPUT TO VALUE(x-Archivo) KEEP-MESSAGES.
FOR EACH t-ccb_pendientes NO-LOCK:
    EXPORT DELIMITER "~029" t-ccb_pendientes.
END.
OUTPUT CLOSE.

DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        
comm-line = "/usr/bin/qonvtaexport6".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-f-get-referencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-get-referencia Procedure 
FUNCTION f-get-referencia RETURNS CHARACTER
  ( INPUT pCodDiv AS CHAR, INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR ) :

  DEFINE BUFFER b-ccbcdocu FOR ccbcdocu.
  DEFINE BUFFER c-ccbcdocu FOR ccbcdocu.
  DEFINE VAR x-nro-gr AS CHAR.

  x-nro-gr = "|". 

  FIND FIRST b-ccbcdocu WHERE b-ccbcdocu.codcia = s-codcia AND       
      b-ccbcdocu.coddiv = pCodDiv AND 
      b-ccbcdocu.coddoc = pCodDoc AND
      b-ccbcdocu.nrodoc = pNroDoc NO-LOCK
      NO-ERROR.
  IF AVAILABLE b-ccbcdocu THEN DO:
      IF TRUE <> (b-ccbcdocu.nroref > "") THEN DO:
            xLoop:
            FOR EACH c-ccbcdocu WHERE c-ccbcdocu.codcia = s-codcia AND 
                c-ccbcdocu.coddoc = 'G/R' AND 
                c-ccbcdocu.codref = pCodDoc AND
                c-ccbcdocu.nroref = pNroDoc NO-LOCK:
                x-nro-gr = TRIM(c-ccbcdocu.coddoc) + "|" + TRIM(c-ccbcdocu.nrodoc).
                LEAVE xLoop.
            END.
      END.
      ELSE DO:
          x-nro-gr = TRIM(b-ccbcdocu.codref) + "|" + TRIM(b-ccbcdocu.nroref).
      END.
  END.  
  RETURN x-nro-gr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fBanco) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBanco Procedure 
FUNCTION fBanco RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pValor AS CHAR NO-UNDO.

  IF Ccbcdocu.coddoc <> "LET" THEN RETURN "".   /* Function return value. */

  RUN vta2/p-estado-letras ("Banco", Ccbcdocu.codcta, OUTPUT pValor).
  RETURN pValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fEstado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEstado Procedure 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR pEstado AS CHAR.

RUN gn/fFlgEstCCBv2 (ccbcdocu.coddoc,
                     ccbcdocu.flgest,
                     OUTPUT pEstado).
RETURN pEstado.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fSituacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSituacion Procedure 
FUNCTION fSituacion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pValor AS CHAR NO-UNDO.

  IF Ccbcdocu.coddoc <> "LET" THEN RETURN "".   /* Function return value. */

  RUN vta2/p-estado-letras ("Situacion", Ccbcdocu.flgsit, OUTPUT pValor).
  RETURN pValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fUbicacion Procedure 
FUNCTION fUbicacion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pValor AS CHAR NO-UNDO.

  IF Ccbcdocu.coddoc <> "LET" THEN RETURN "".   /* Function return value. */

  RUN vta2/p-estado-letras ("Ubicacion", Ccbcdocu.flgubi, OUTPUT pValor).
  RETURN pValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importe-fotocopia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION importe-fotocopia Procedure 
FUNCTION importe-fotocopia RETURNS DECIMAL
  ( INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR x-retval AS DEC INIT 0.

FIND FIRST ttCanjes WHERE ttCanjes.tcoddoc = pCodDoc AND 
                            ttCanjes.tnrodoc = pNroDOc NO-ERROR.
IF NOT AVAILABLE ttCanjes THEN DO:
    FOR EACH ccbdcaja WHERE ccbdcaja.codcia = s-codcia AND
                            ccbdcaja.coddoc = pCoddoc AND
                            ccbdcaja.nrodoc = pNrodoc NO-LOCK:
        FOR EACH ccbddocu WHERE ccbddocu.codcia = s-codcia AND 
                                ccbddocu.coddoc = ccbdcaja.codref AND
                                ccbddocu.nrodoc = ccbdcaja.nroref NO-LOCK, 
            FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = Ccbddocu.codcia
                                            AND Almmmatg.codmat = Ccbddocu.codmat
                                            AND Almmmatg.codfam = '011':
            x-retval = x-retval + Ccbddocu.ImpLin.
        END.

    END.
    IF x-retval > 0 THEN DO:
        CREATE ttCanjes.
            ASSIGN 
                ttCanjes.tcoddoc = ccbcdocu.codref
                ttCanjes.tnrodoc = ccbcdocu.nroref.
    END.
END.

RETURN x-retval.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

