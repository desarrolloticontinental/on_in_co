&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-FELogErrores NO-UNDO LIKE FELogErrores.



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

DEF INPUT PARAMETER pRowid AS ROWID.
DEF INPUT-OUTPUT PARAMETER TABLE FOR T-FELogErrores.
DEF OUTPUT PARAMETER pCodError AS CHAR NO-UNDO.

DEF SHARED VAR s-user-id AS CHAR.

/*IF s-user-id = "ADMIN" THEN RETURN "OK".*/

DEF BUFFER B-CDOCU FOR Ccbcdocu.
FIND B-CDOCU WHERE ROWID(B-CDOCU) = pRowid NO-LOCK NO-ERROR.
IF LOOKUP(B-CDOCU.CodDoc, 'FAC,BOL,N/C,N/D') = 0 THEN RETURN "OK".

/* ********************************************* */
/* Inicio de actividades facturaci�n electr�nica */
/* ********************************************* */
DEF VAR pStatus AS LOG.
RUN sunat\p-inicio-actividades (INPUT B-CDOCU.fchdoc, OUTPUT pStatus).
IF pStatus = NO THEN RETURN "OK".       /* Todav�a no han iniciado las actividades */
/* ********************************************* */
/*RETURN 'OK'.*/

DEF VAR pID_Pos AS CHAR NO-UNDO.
DEF VAR pTipo  AS CHAR.     /* MOSTRADOR CREDITO */
DEF VAR pCodTer AS CHAR.    /* SOLO PARA MOSTRADOR */

ASSIGN
    pTipo = B-CDOCU.Tipo
    pCodTer = B-CDOCU.CodCaja.

DEF VAR s-Sunat-Activo AS LOG INIT NO NO-UNDO.
FIND gn-divi WHERE GN-DIVI.CodCia = B-CDOCU.CodCia
    AND GN-DIVI.CodDiv = B-CDOCU.CodDiv NO-LOCK NO-ERROR.
IF AVAILABLE gn-divi THEN s-Sunat-Activo = gn-divi.campo-log[10].

/* CASE pTipo:                                                                                              */
/*     WHEN "MOSTRADOR" THEN DO:                                                                            */
/*         FIND CcbCTerm WHERE CcbCTerm.CodCia = B-CDOCU.codcia                                             */
/*             AND CcbCTerm.CodDiv = B-CDOCU.coddiv                                                         */
/*             AND CcbCTerm.CodTer = pCodTer                                                                */
/*             NO-LOCK NO-ERROR.                                                                            */
/*         IF NOT AVAILABLE CcbCTerm THEN DO:                                                               */
/*             pCodError = "ERROR e-Pos: Terminal " + pCodTer + " NO configurado".                          */
/*             RETURN 'ADM-ERROR'.                                                                          */
/*         END.                                                                                             */
/*         IF CcbCTerm.ID_Pos = '' THEN DO:                                                                 */
/*             pCodError = "ERROR e-Pos: Terminal " + pCodTer + " NO tiene configurado ID para el e-Pos".   */
/*             RETURN 'ADM-ERROR'.                                                                          */
/*         END.                                                                                             */
/*         pID_Pos = CcbCTerm.ID_Pos.                                                                       */
/*     END.                                                                                                 */
/*     WHEN "CREDITO" THEN DO:                                                                              */
/*         FIND FacCorre WHERE FacCorre.CodCia = B-CDOCU.codcia                                             */
/*             AND FacCorre.CodDoc = B-CDOCU.coddoc                                                         */
/*             AND FacCorre.NroSer = INTEGER(SUBSTRING(B-CDOCU.nrodoc,1,3))                                 */
/*             NO-LOCK NO-ERROR.                                                                            */
/*         IF NOT AVAILABLE FacCorre THEN DO:                                                               */
/*             pCodError = "ERROR e-Pos: Correlativo " + STRING(FacCorre.NroSer,'999') + " NO configurado". */
/*             RETURN 'ADM-ERROR'.                                                                          */
/*         END.                                                                                             */
/*         IF FacCorre.ID_Pos = '' THEN DO:                                                                 */
/*             pCodError = "ERROR e-Pos: El Comprobante NO tiene configurado ID para el e-Pos".             */
/*             RETURN 'ADM-ERROR'.                                                                          */
/*         END.                                                                                             */
/*         pID_Pos = FacCorre.ID_Pos.                                                                       */
/*     END.                                                                                                 */
/*     OTHERWISE DO:                                                                                        */
/*         RETURN 'OK'.                                                                                     */
/*     END.                                                                                                 */
/* END CASE.                                                                                                */

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
      TABLE: T-FELogErrores T "?" NO-UNDO INTEGRAL FELogErrores
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 5.5
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/** ***************************  Main Block  *************************** */
DEF VAR cFilled AS CHAR NO-UNDO.
DEF VAR ix AS INT NO-UNDO.
DEFINE VAR hProc AS HANDLE NO-UNDO.
DEFINE VAR cRetVal AS CHAR.
DEFINE VAR cCodHash AS CHAR NO-UNDO.
DEFINE VAR iEstadoPPLL AS INT NO-UNDO.
DEFINE VAR cIP_ePos AS CHAR NO-UNDO.
DEFINE VAR cID_Pos  AS CHAR NO-UNDO.
DEFINE VAR cNumDocumento AS CHAR NO-UNDO.
DEFINE VAR iFlagPPLL AS INT NO-UNDO.

DEFINE SHARED VAR hSocket AS HANDLE NO-UNDO.

/* Levantamos las rutinas a memoria */
RUN sunat\facturacion-electronica.p PERSISTENT SET hProc NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pCodError = "ERROR en las librerias de la Facturaci�n Electr�nica" + CHR(10) +
        "Salir del Sistema, volver a entrar y repetir el proceso".
    RETURN "ADM-ERROR".
END.

IF VALID-HANDLE(hSocket) THEN DELETE OBJECT hSocket.
CREATE SOCKET hSocket.

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* **************************************************************************** */
    /* 1ro. Nos conectamos al e-Pos */
    /* Si devuelve 000|xxxxxxxxxxx la coneci�n est� OK */
    /* **************************************************************************** */
    RUN pconecto-epos IN hProc (INPUT B-CDOCU.CodDiv, 
                                 INPUT B-CDOCU.CodCaja,
                                 OUTPUT cRetVal).
    IF USERID("integral") <> "MASTER" THEN DO:
        /*MESSAGE "pconecto-epos" cRetVal.*/
    END.
    IF SUBSTRING(cRetVal,1,3) <> "000" THEN DO:
        iFlagPPLL = 0.  
        /* NO se pudo conectar */
        /* Borramos de la memoria las rutinas antes cargadas */
        cFilled = DYNAMIC-FUNCTION('fdesconectar-epos':U IN  hProc).
        DELETE OBJECT hSocket.
        DELETE PROCEDURE hProc.
        pCodError = "ERROR conexi�n e-Pos: " + cRetVal.
        /* *************** LOG DE ERRORES **************** */
        RUN Log-Error.
        /* *********************************************** */
        RETURN "ADM-ERROR".
    END.
    /* *************** LOG DE CONTROL **************** */
    RUN Log-Error.
    /* *********************************************** */
    
    IF NUM-ENTRIES(cRetVal,'|') >= 4 THEN 
        ASSIGN
        cIP_ePos = ENTRY(3,cRetVal,'|')
        cID_Pos  = ENTRY(4,cRetVal,'|').
    
    /* **************************************************************************** */
    /* 2do. Generamos el XML en el e-Pos */
    /* Generacion de Documento, TipoDocumnto, NroDocumnto, Division y Valor de Retorno */
    /* **************************************************************************** */
    cRetVal = "".
    RUN penvio-documento IN hProc (
        INPUT cIP_ePos,
        INPUT cID_Pos,
        INPUT B-CDOCU.CodDoc, 
        INPUT B-CDOCU.NroDoc, 
        INPUT B-CDOCU.CodDiv,  
        OUTPUT cRetVal).

    IF USERID("integral") <> "MASTER" THEN DO:
        /*MESSAGE "penvio-documento" cRetVal.*/
    END.

    /* RetVal = NNN|xxxxxxxxxxxxxxxx|hhhhaaaasssssshhhh */
    /* Si los 3 primeros digitos del cRetVal = "000", generacion OK y trae el HASHHHHHH */
    IF SUBSTRING(cRetVal,1,3) <> "000" THEN DO:
        iFlagPPLL = 0.
        /* Error en el XML */
        cFilled = DYNAMIC-FUNCTION('fdesconectar-epos':U IN  hProc).
        DELETE OBJECT hSocket.
        DELETE PROCEDURE hProc.
        /* Guardamos el error pero grabamos el documento por ahora */
        pCodError = "ERROR env�o e-Pos: " + cRetVal.
        /* *************** LOG DE ERRORES **************** */
        RUN Log-Error.
        /* *********************************************** */
        RETURN "ADM-ERROR".
    END.
    ASSIGN 
        iEstadoPPLL = INTEGER(ENTRY(1,cRetVal,'|')) NO-ERROR.
    IF NUM-ENTRIES(cRetVal,'|') >= 3 THEN cCodHash = ENTRY(3,cRetVal,'|').
    IF NUM-ENTRIES(cRetVal,'|') >= 4 THEN cNumDocumento = ENTRY(4,cRetVal,'|').
    IF TRUE <> (cCodHash > '') THEN DO:
        cFilled = DYNAMIC-FUNCTION('fdesconectar-epos':U IN  hProc).
        DELETE OBJECT hSocket.
        DELETE PROCEDURE hProc.
        /* Guardamos el error pero grabamos el documento por ahora */
        pCodError = "ERROR env�o e-Pos: c�digo de Hash en blanco." + CHR(10) + cRetVal.
        /* *************** LOG DE ERRORES **************** */
        RUN Log-Error.
        /* *********************************************** */
        RETURN "ADM-ERROR".
    END.
    /* *************** LOG DE CONTROL **************** */
    RUN Log-Error.
    /* *********************************************** */

    /* **************************************************************************** */
    /* 3ro. Enviamos la Confirmacion */
    /* **************************************************************************** */
    cRetVal = "".
    RUN pconfirmo-documento IN hProc (
        INPUT cIP_ePos,
        INPUT cID_Pos,
        INPUT B-CDOCU.CodDoc,
        INPUT B-CDOCU.NroDoc,
        INPUT B-CDOCU.CodDiv,
        OUTPUT cRetVal).

    IF USERID("integral") <> "MASTER" THEN DO:
        /*MESSAGE "pconfirmo-documento" cRetVal.*/
    END.

    /*cRetVal = '888'.*/
    /* Si los 3 primeros digitos del cRetVal = "000" */
    IF SUBSTRING(cRetVal,1,3) <> "000" THEN DO:
        iFlagPPLL = 2.
        cFilled = DYNAMIC-FUNCTION('fdesconectar-epos':U IN  hProc).
        DELETE OBJECT hSocket.
        DELETE PROCEDURE hProc.
        pCodError = "ERROR confirmaci�n e-Pos: " + cRetVal.
        RUN New-Log.
        /* *************** LOG DE ERRORES **************** */
        RUN Log-Error.
        /* *********************************************** */
        RETURN "ERROR-EPOS".
        /* **************************************************** */
    END.
    /* *************** LOG DE CONTROL **************** */
    RUN Log-Error.
    /* *********************************************** */

    /* 4ro. Grabamos el LOG de control */
    RUN New-Log.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        cFilled = DYNAMIC-FUNCTION('fdesconectar-epos':U IN  hProc).
        DELETE OBJECT hSocket.
        DELETE PROCEDURE hProc.
        /* *************** LOG DE ERRORES **************** */
        RUN Log-Error.
        /* *********************************************** */
        RETURN "ADM-ERROR".
    END.

    CATCH eBlockError AS PROGRESS.Lang.Error:
        IF eBlockError:NumMessages > 0 THEN DO:
            pCodError = eBlockError:GetMessage(1).
            DO ix = 2 TO eBlockError:NumMessages:
                pCodError = pCodError + CHR(10) + eBlockError:GetMessage(ix).
            END.
        END. 
        /* *************** LOG DE ERRORES **************** */
        pCodError = "ERROR INESPERADO DEL SISTEMA".
        RUN Log-Error.
        /* *********************************************** */
        /* Borramos de la memoria las rutinas antes cargadas */
        cFilled = DYNAMIC-FUNCTION('fdesconectar-epos':U IN  hProc).
        DELETE OBJECT hSocket.
        DELETE PROCEDURE hProc.
        DELETE OBJECT eBlockError.
        UNDO, RETURN 'ADM-ERROR'.
    END CATCH.
END.

/* Borramos de la memoria las rutinas antes cargadas */
cFilled = DYNAMIC-FUNCTION('fdesconectar-epos':U IN  hProc).
DELETE OBJECT hSocket.
DELETE PROCEDURE hProc.

IF AVAILABLE(FELogComprobantes) THEN RELEASE FELogComprobantes.

RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Log-Error) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Log-Error Procedure 
PROCEDURE Log-Error :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE T-FELogErrores.
ASSIGN
    T-FELogErrores.CodCia = B-CDOCU.codcia
    T-FELogErrores.CodDiv = B-CDOCU.coddiv
    T-FELogErrores.CodDoc = B-CDOCU.coddoc
    T-FELogErrores.NroDoc = B-CDOCU.nrodoc
    T-FELogErrores.CodHash = cCodHash
    T-FELogErrores.ErrorDate = NOW
    T-FELogErrores.EstadoPPLL = iEstadoPPLL
    T-FELogErrores.FlagPPLL = (IF SUBSTRING(cRetVal,1,3) = "000" THEN 1 ELSE iFlagPPLL)
    T-FELogErrores.ID_Pos = cID_Pos
    T-FELogErrores.IP_ePos = cIP_ePos
    T-FELogErrores.LogDate = NOW
    T-FELogErrores.LogEstado = pCodError      /*cRetVal*/
    T-FELogErrores.LogUser = s-user-id
    T-FELogErrores.NumDocumento = cNumDocumento
    NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-New-Log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New-Log Procedure 
PROCEDURE New-Log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* NO duplicados */
    FIND FIRST FELogComprobantes WHERE FELogComprobantes.CodCia = B-CDOCU.codcia
        AND FELogComprobantes.CodDiv = B-CDOCU.coddiv
        AND FELogComprobantes.CodDoc = B-CDOCU.coddoc
        AND FELogComprobantes.NroDoc = B-CDOCU.nrodoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE FELogComprobantes THEN DO:
        /* ***************************** */
        pCodError = "Duplicado comprobante en FELogComprobantes: " + B-CDOCU.coddoc + ' ' + B-CDOCU.nrodoc.
        RETURN 'ADM-ERROR'.
    END.
    CREATE FELogComprobantes.
    ASSIGN
        FELogComprobantes.CodCia = B-CDOCU.codcia
        FELogComprobantes.CodDiv = B-CDOCU.coddiv
        FELogComprobantes.CodDoc = B-CDOCU.coddoc
        FELogComprobantes.NroDoc = B-CDOCU.nrodoc
        FELogComprobantes.LogDate = NOW
        FELogComprobantes.LogEstado = cRetVal
        FELogComprobantes.LogUser   = B-CDOCU.usuario
        FELogComprobantes.FlagPPLL   = (IF SUBSTRING(cRetVal,1,3) = "000" THEN 1 ELSE iFlagPPLL)
        FELogComprobantes.EstadoPPLL = iEstadoPPLL
        FELogComprobantes.CodHash      = cCodHash
        FELogComprobantes.NumDocumento = cNumDocumento
        FELogComprobantes.IP_ePos = cIP_ePos
        FELogComprobantes.ID_Pos  = cID_Pos
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pCodError = "Error grabaci�n comprobante en FELogComprobantes: " + B-CDOCU.coddoc + ' ' + B-CDOCU.nrodoc.
        DELETE FELogComprobantes NO-ERROR.
        RETURN 'ADM-ERROR'.
    END.
    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

