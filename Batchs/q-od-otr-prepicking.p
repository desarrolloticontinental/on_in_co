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

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR s-codalm AS CHAR NO-UNDO.

DEF VAR x-CodDoc AS CHAR INIT 'O/D,OTR' NO-UNDO.
DEF VAR x-CodAlm AS CHAR INIT '11,11e' NO-UNDO.
DEF VAR txtDesde AS DATE NO-UNDO.
DEF VAR txtHasta AS DATE NO-UNDO.
DEF VAR ChkChequeo AS LOG INIT YES NO-UNDO.
DEF VAR lMsgRetorno AS CHAR NO-UNDO.

ASSIGN
    txtDesde = TODAY - 15
    txtHasta = TODAY.

DEFINE STREAM REPORT.
DEFINE TEMP-TABLE ORDENES NO-UNDO LIKE FacCPedi.

&SCOPED-DEFINE CONDICION faccpedi.codcia = s-codcia ~
    AND faccpedi.coddoc = s-coddoc ~
    AND faccpedi.divdes = s-coddiv ~
    AND faccpedi.codalm = s-codalm ~
    AND (faccpedi.fchped >= ltxtDesde AND faccpedi.fchped <= ltxtHasta) 

&SCOPED-DEFINE Condicion2 faccpedi.flgest <> 'A'

/*
&SCOPED-DEFINE Condicion2 ((lChequeados = NO AND faccpedi.flgest <> 'A') ~
        OR (faccpedi.flgest = 'P' AND faccpedi.flgsit = 'T'))

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fAlmacen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAlmacen Procedure 
FUNCTION fAlmacen RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fDistribucion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDistribucion Procedure 
FUNCTION fDistribucion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fNroItm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNroItm Procedure 
FUNCTION fNroItm RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fPersonal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPersonal Procedure 
FUNCTION fPersonal RETURNS CHARACTER
     (INPUT cCodPer AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fPeso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPeso Procedure 
FUNCTION fPeso RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 8.23
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


/* Se van a generar 4 reportes:
    O/D del almacén 11
    O/D del almacén 11e
    OTR del almacén 11
    OTR del almacén 11e
    
*/
DEF VAR j AS INT NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR ltxtDesde AS DATE NO-UNDO.
DEF VAR ltxtHasta AS DATE NO-UNDO.
DEF VAR lChequeados AS LOG NO-UNDO.

DO j = 1 TO NUM-ENTRIES(x-CodDoc):
    s-CodDoc = ENTRY(j, x-CodDoc).
    DO k = 1 TO NUM-ENTRIES(x-CodAlm):
        s-CodAlm = ENTRY(k, x-CodAlm).
        FIND Almacen WHERE Almacen.codcia = s-codcia
            AND Almacen.codalm = s-codalm
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almacen THEN NEXT.
        s-CodDiv = Almacen.CodDiv.

        ltxtDesde = txtDesde.
        ltxtHasta = txtHasta.
        lChequeados = chkchequeo.
        RUN Carga-Temporal.
        RUN ue-envia-txt-detalle.
    END.
END.

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

EMPTY TEMP-TABLE ORDENES.

DEFINE VAR lOrdenCompra AS CHAR INIT ''.
DEFINE VAR lUserImpresion AS CHAR.

FOR EACH FacCPedi WHERE {&Condicion} USE-INDEX Llave09 NO-LOCK, FIRST GN-DIVI OF FacCPedi NO-LOCK:
    lOrdenCompra = "*".
    lUserImpresion = ENTRY(1, Faccpedi.Libre_c02, '|').
    lUserImpresion = IF (lUserImpresion = ?) THEN '' ELSE trim(lUserImpresion).

    CREATE ORDENES.
    BUFFER-COPY Faccpedi TO ORDENES.
    ASSIGN
        ORDENES.Libre_c01 = gn-divi.desdiv
        ORDENES.Libre_d01 = fPeso()
        ORDENES.Libre_c02 = fAlmacen()
        ORDENES.Libre_d02 = fNroItm()
        ORDENES.Libre_c03 = fDistribucion()
        ORDENES.UsrImpOD = ENTRY(1, Faccpedi.Libre_c02, '|')
        ORDENES.FchImpOD = (IF NUM-ENTRIES(Faccpedi.Libre_c02, '|') > 1 THEN DATETIME(ENTRY(2, Faccpedi.Libre_c02, '|'))
            ELSE ?)
        /*ORDENES.OrdCmp = IF (lOrdenCompra = '*') THEN '' ELSE lOrdenCompra*/.

    CASE Faccpedi.coddoc:
        WHEN 'O/D' OR WHEN 'O/M' THEN DO:
            ORDENES.Libre_c01 = gn-divi.desdiv.
        END.
        WHEN 'OTR' THEN DO:
            ORDENES.Libre_c01 = Faccpedi.codcli.
        END.
    END CASE.
END.
SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ue-envia-txt-detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue-envia-txt-detalle Procedure 
PROCEDURE ue-envia-txt-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 1.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE x-signo                 AS DECI.

DEFINE VARIABLE lsFechaEmision          AS CHARACTER.
DEFINE VARIABLE lsHoraEmision           AS CHARACTER.

DEFINE VARIABLE ldtFechaEmision         AS DATETIME.
DEFINE VARIABLE ldtFechaDistribucion    AS DATETIME.
DEFINE VARIABLE lsDifTempo_VtaAlm       AS CHARACTER.
DEFINE VARIABLE lsDifTempo_AlmDist      AS CHARACTER.
DEFINE VARIABLE lsDifTempo_VtaDist      AS CHARACTER.

DEFINE VAR lPeso AS DEC.
DEFINE VAR lNomUser AS CHAR.
DEFINE VAR lCodUser AS CHAR.
DEFINE VAR lZona AS CHAR.
DEFINE VAR lUbi AS CHAR.

DEFINE VARIABLE x-Archivo AS CHARACTER   NO-UNDO.

x-Archivo = "Y:\" + (IF s-coddoc = "O/D" THEN "OD" ELSE s-coddoc) + '-' + s-codalm + '-prepicking.txt'.

OUTPUT STREAM REPORT TO VALUE(x-Archivo).
PUT STREAM REPORT UNFORMATTED
    "Codigo|"
    "Numero|"
    "Emision|"
    "Hora|"
    "Origen|"
    "Cliente|"
    "Impreso por|"
    "Fecha/Hora Impresion|"
    "Fecha/Hora Distribucion|"
    "Items|"
    "Glosa|"
    "De Venta a Almacén|"
    "De Almacén a Distribución|"
    "De Venta a Distribución|"
    "Fech.Entrega|"
    "Peso|"
    "Articulo|"
    "Descripcion|"
    "Peso Unit|"
    "Marca|"
    "Cantidad|"
    "Unidad|"
    "Almacen|"
    "Zona|"
    "Ubicacion|"
    "Peso Tot|"
    "Usuario envio a Distribucion|"
    "Nombre Usuario|" SKIP.


FOR EACH ORDENES NO-LOCK,
    FIRST FacCPedi WHERE FacCPedi.CodCia = ORDENES.CodCia
    AND FacCPedi.CodDoc = ORDENES.CodDoc
    AND FacCPedi.NroPed = ORDENES.NroPed
    AND FacCPedi.CodDiv = ORDENES.CodDiv
    AND {&Condicion2} NO-LOCK:
    iCount = 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
       iCount = iCount + 1.
    END.

    lsFechaEmision = STRING(faccpedi.fchped, '99-99-9999').
    lsHoraEmision  = faccpedi.hora.

    ldtFechaEmision      = DATETIME(lsFechaEmision + ' ' + lsHoraEmision).
    ldtFechaDistribucion = DATETIME(IF(NUM-ENTRIES(Faccpedi.Libre_c03,'|') > 1) THEN ENTRY(2,Faccpedi.Libre_c03,'|') ELSE "").

    RUN lib\_time-passed.p (ldtFechaEmision, faccpedi.fchimpod, OUTPUT lsDifTempo_VtaAlm).
    RUN lib\_time-passed.p (faccpedi.fchimpod, ldtFechaDistribucion, OUTPUT lsDifTempo_AlmDist).
    RUN lib\_time-passed.p (ldtFechaEmision, ldtFechaDistribucion, OUTPUT lsDifTempo_VtaDist).

    lPeso = fPeso().

    FOR EACH facdpedi OF faccpedi NO-LOCK,FIRST almmmatg OF facdpedi NO-LOCK:
        lZona = ''.
        lUbi = ''.

        /* Detalle */
        FIND FIRST Almmmate WHERE Almmmate.CodCia = FacDPedi.CodCia
            AND Almmmate.CodAlm = FacDPedi.AlmDes
            AND Almmmate.codmat = FacDPedi.codmat NO-LOCK NO-ERROR.
        IF AVAILABLE Almmmate THEN DO:
            FIND FIRST almtubic OF Almmmate NO-LOCK NO-ERROR.
            IF AVAILABLE almtubic THEN lZona = almtubic.CodZona.
            lUbi = Almmmate.CodUbi.
        END.
        lCodUser = IF(NUM-ENTRIES(Faccpedi.Libre_c03,'|') > 2) THEN ENTRY(3,Faccpedi.Libre_c03,'|') ELSE "".
        lNomUser = fPersonal(lCodUser).

        PUT STREAM REPORT UNFORMATTED
            faccpedi.coddoc "|"
            faccpedi.nroped "|"
            faccpedi.fchped "|"
            faccpedi.hora "|"
            gn-divi.desdiv "|"
            faccpedi.nomcli "|"
            faccpedi.usrimpod "|"
            faccpedi.fchimpod "|"
            IF(NUM-ENTRIES(Faccpedi.Libre_c03,'|') > 1) THEN ENTRY(2,Faccpedi.Libre_c03,'|') ELSE "" "|"
            iCount "|"
            faccpedi.glosa "|"
            lsDifTempo_VtaAlm "|"
            lsDifTempo_AlmDist "|"
            lsDifTempo_VtaDist "|"
            faccpedi.fchent "|"
            lpeso "|"
            facdpedi.codmat "|"
            almmmatg.desmat "|"
            almmmatg.pesmat "|"
            almmmatg.desmar "|"
            facdpedi.canped "|"
            facdpedi.undvta "|"
            facdpedi.almdes "|"
            lzona "|"
            lUbi "|"                
            (FacDPedi.CanPed * FacDPedi.Factor * Almmmatg.Pesmat ) "|"
            IF(NUM-ENTRIES(Faccpedi.Libre_c03,'|') > 2) THEN ENTRY(3,Faccpedi.Libre_c03,'|') ELSE "" "|"
            lNomUser SKIP.
    END.
END.
OUTPUT STREAM REPORT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fAlmacen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAlmacen Procedure 
FUNCTION fAlmacen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NUM-ENTRIES(Faccpedi.Libre_c02,'|') > 1 THEN RETURN ENTRY(2,Faccpedi.Libre_c02,'|').
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fDistribucion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDistribucion Procedure 
FUNCTION fDistribucion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NUM-ENTRIES(Faccpedi.Libre_c03,'|') > 1 
      THEN RETURN ENTRY(2,Faccpedi.Libre_c03,'|').
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fNroItm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNroItm Procedure 
FUNCTION fNroItm RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR i AS INT.
FOR EACH facdpedi OF faccpedi NO-LOCK:
    i = i + 1.
END.
RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fPersonal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPersonal Procedure 
FUNCTION fPersonal RETURNS CHARACTER
     (INPUT cCodPer AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VAR lPersonal AS CHAR.

    lPersonal = "".
    FIND FIRST pl-pers WHERE pl-pers.codper = cCodPer NO-LOCK NO-ERROR.
    IF AVAILABLE pl-pers THEN DO:
        lPersonal = pl-pers.patper + ' ' + pl-pers.matper + ' ' + pl-pers.nomper.
    END.

  RETURN lPersonal.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fPeso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPeso Procedure 
FUNCTION fPeso RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VAR lPeso AS DEC.

    DEFINE BUFFER b-facdpedi FOR facdpedi.

    lPeso = 0.
    lMsgRetorno = ''.

    FOR EACH b-facdpedi OF faccpedi NO-LOCK,
        FIRST almmmatg OF b-facdpedi NO-LOCK :
        IF almmmatg.pesmat <> ? AND almmmatg.pesmat > 0 THEN DO:
            lPeso = lPeso + (b-facdpedi.canped * almmmatg.pesmat).
        END.       
        ELSE DO:
            IF lMsgRetorno = '' THEN DO:
                lMsgRetorno = almmmatg.codmat.
            END.
            ELSE DO:
                lMsgRetorno = lMsgRetorno + ", " + almmmatg.codmat.
            END.
        END.
    END.

  RETURN lPeso.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

