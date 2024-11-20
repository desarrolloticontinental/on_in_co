&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER b-mat1 FOR Almmmat1.
DEFINE BUFFER b-matg FOR Almmmatg.
DEFINE TEMP-TABLE T-MAT1 NO-UNDO LIKE Almmmat1.
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg.



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

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR pv-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN'.

DEF BUFFER MATG FOR almmmatg.

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
      TABLE: b-mat1 B "?" ? INTEGRAL Almmmat1
      TABLE: b-matg B "?" ? INTEGRAL Almmmatg
      TABLE: T-MAT1 T "?" NO-UNDO INTEGRAL Almmmat1
      TABLE: T-MATG T "?" NO-UNDO INTEGRAL Almmmatg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE chExcelApplication          AS COM-HANDLE.
DEFINE VARIABLE chWorkbook                  AS COM-HANDLE.
DEFINE VARIABLE chWorksheet                 AS COM-HANDLE.
DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INT64      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.

DEF VAR FILL-IN-Archivo AS CHAR NO-UNDO.
DEF VAR OKpressed AS LOG NO-UNDO.
                                      
/* RUTINA GENERAL */
SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls,*.xlsx)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN NO-APPLY.

/* CREAMOS LA HOJA EXCEL */
CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

SESSION:SET-WAIT-STATE('GENERAL').
RUN Carga-Temporal.
SESSION:SET-WAIT-STATE('').

/* CERRAMOS EL EXCEL */
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

/* Chequeo */
/* FOR EACH t-matg:                                   */
/*     DISPLAY T-MATG.codmat T-MATG.codbrr.           */
/*     FOR EACH t-mat1 OF t-matg:                     */
/*         DISPLAY t-mat1.barras[1] t-mat1.equival[1] */
/*             t-mat1.barras[2] t-mat1.equival[2]     */
/*             t-mat1.barras[3] t-mat1.equival[3]     */
/*             t-mat1.barras[4] t-mat1.equival[4]     */
/*             WITH 2 COL.                            */
/*     END.                                           */
/* END.                                               */
/* RETURN.                                            */

/* Consistencia final */

RUN Consistencia-Final.
IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN.


/* MESSAGE 'grabamos'.      */
/* RUN Actualiza-Productos. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ACTUALIZA-MAT-x-ALM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ACTUALIZA-MAT-x-ALM Procedure 
PROCEDURE ACTUALIZA-MAT-x-ALM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

     FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND
            Almacen.TdoArt:
         FIND Almmmate WHERE Almmmate.CodCia = Almmmatg.codcia AND 
              Almmmate.CodAlm = Almacen.CodAlm AND 
              Almmmate.CodMat = Almmmatg.CodMat NO-ERROR.
         IF NOT AVAILABLE Almmmate THEN DO:
            CREATE Almmmate.
            ASSIGN Almmmate.CodCia = Almmmatg.codcia
                   Almmmate.CodAlm = Almacen.CodAlm
                   Almmmate.CodMat = Almmmatg.CodMat.
         END.
         ASSIGN Almmmate.DesMat = Almmmatg.DesMat
                Almmmate.FacEqu = Almmmatg.FacEqu
                Almmmate.UndVta = Almmmatg.UndStk
                Almmmate.CodMar = Almmmatg.CodMar.
         FIND FIRST almautmv WHERE 
              almautmv.CodCia = Almmmatg.codcia AND
              almautmv.CodFam = Almmmatg.codfam AND
              almautmv.CodMar = Almmmatg.codMar AND
              almautmv.Almsol = Almmmate.CodAlm NO-LOCK NO-ERROR.
         IF AVAILABLE almautmv THEN 
            ASSIGN Almmmate.AlmDes = almautmv.Almdes
                   Almmmate.CodUbi = almautmv.CodUbi.
         RELEASE Almmmate.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Actualiza-Productos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Productos Procedure 
PROCEDURE Actualiza-Productos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-NroCor AS INT NO-UNDO.
DEF VAR x-ordmat AS INT NO-UNDO.
DEF VAR C-ALM AS CHAR NO-UNDO.

FOR EACH t-matg NO-LOCK WHERE t-matg.codcia = s-codcia AND t-matg.codmat > '':
    FIND Almmmatg WHERE Almmmatg.Codcia = S-CODCIA 
        AND Almmmatg.CodMat = T-MATG.CodMat
        NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    ASSIGN
        Almmmatg.CodBrr = T-MATG.CodBrr.
    IF CAN-FIND(FIRST T-MAT1 OF T-MATG NO-LOCK) THEN DO:
        FOR EACH Almmmat1 OF Almmmatg:
            DELETE Almmmat1.
        END.
        FOR EACH T-MAT1 OF T-MATG:
            CREATE Almmmat1.
            BUFFER-COPY T-MAT1 TO Almmmat1.
        END.
    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Temporal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal Procedure 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


EMPTY TEMP-TABLE T-MATG.
ASSIGN
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    cValue = STRING(INTEGER(cValue),'999999').
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = cValue
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    /* CODIGO */
    CREATE T-MATG.
    BUFFER-COPY Almmmatg TO T-MATG.
    /* EAN 13 */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    iValue = (IF cValue = ? THEN 0 ELSE INT64(cValue)).
    cValue = (IF iValue = 0 THEN '' ELSE STRING(iValue) ).
    ASSIGN
        T-MATG.codbrr = cValue.
    /* EAN 14-1 */
    FIND T-MAT1 OF T-MATG NO-ERROR.
    IF NOT AVAILABLE T-MAT1 THEN CREATE T-MAT1.
    ASSIGN
        T-MAT1.CodCia = T-MATG.CodCia
        T-MAT1.codmat = T-MATG.CodMat.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF NOT (TRUE <> (cValue > "")) THEN DO:
        iValue = (IF cValue = ? THEN 0 ELSE INT64(cValue)).
        cValue = (IF iValue = 0 THEN '' ELSE STRING(iValue) ).
        ASSIGN
            T-MAT1.Barras[1] = cValue.
    END.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MAT1.Equival[1] = DECIMAL(cValue).
    /* EAN 14-2 */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF NOT (TRUE <> (cValue > "")) THEN DO:
        iValue = (IF cValue = ? THEN 0 ELSE INT64(cValue)).
        cValue = (IF iValue = 0 THEN '' ELSE STRING(iValue) ).
        ASSIGN
            T-MAT1.Barras[2] = cValue.
    END.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MAT1.Equival[2] = DECIMAL(cValue).
    /* EAN 14-3 */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF NOT (TRUE <> (cValue > "")) THEN DO:
        iValue = (IF cValue = ? THEN 0 ELSE INT64(cValue)).
        cValue = (IF iValue = 0 THEN '' ELSE STRING(iValue) ).
        ASSIGN
            T-MAT1.Barras[3] = cValue.
    END.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MAT1.Equival[3] = DECIMAL(cValue).
    /* EAN 14-4 */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF NOT (TRUE <> (cValue > "")) THEN DO:
        iValue = (IF cValue = ? THEN 0 ELSE INT64(cValue)).
        cValue = (IF iValue = 0 THEN '' ELSE STRING(iValue) ).
        ASSIGN
            T-MAT1.Barras[4] = cValue.
    END.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MAT1.Equival[4] = DECIMAL(cValue).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Consistencia-Final) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consistencia-Final Procedure 
PROCEDURE Consistencia-Final :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Reportamos los errores y pasamos el resto. 
------------------------------------------------------------------------------*/

DEF VAR cError AS CHAR FORMAT 'x(60)' COLUMN-LABEL 'Error' NO-UNDO.
DEF VAR lError AS LOG INIT NO NO-UNDO.             
DEF VAR k AS INT NO-UNDO.
DEF VAR pError AS CHAR NO-UNDO.

OUTPUT TO d:\tmp\errores.txt.
FOR EACH T-MATG WHERE T-MATG.codcia = s-codcia AND T-MATG.codmat > '':
    IF NOT CAN-FIND(FIRST Almmmatg OF T-MATG NO-LOCK) THEN DO:
        cError = 'Producto NO registrado'.
        DISPLAY T-MATG.codmat cError WITH STREAM-IO NO-BOX WIDTH 80.
        lError = YES.
        DELETE T-MATG.
        NEXT.
    END.
    RUN gn/verifica-ean-repetido ( 'EAN13',0,T-MATG.codmat,T-MATG.codbrr,OUTPUT pError).
    IF pError > '' THEN DO:
        cError = 'Código EAN13 YA registrado en el material ' + T-MATG.codmat.
        cError = pError.
        DISPLAY
            T-MATG.codmat cError
            WITH STREAM-IO NO-BOX WIDTH 80.
        lError = YES.
        DELETE T-MATG.
        NEXT.
    END.
    FIND FIRST T-MAT1 OF T-MATG NO-LOCK NO-ERROR.
    IF NOT AVAILABLE T-MAT1 THEN NEXT.
    RUN gn/verifica-ean-repetido ( 'EAN14',1,T-MAT1.codmat,T-MAT1.Barras[1],OUTPUT pError).
    IF pError > '' THEN DO:
        cError = 'Código EAN14 YA registrado en el material ' + T-MATG.codmat.
        cError = pError.
        DISPLAY T-MATG.codmat cError WITH STREAM-IO NO-BOX WIDTH 80.
        lError = YES.
        DELETE T-MATG.
        DELETE T-MAT1.
        NEXT.
    END.
    RUN gn/verifica-ean-repetido ( 'EAN14',2,T-MAT1.codmat,T-MAT1.Barras[2],OUTPUT pError).
    IF pError > '' THEN DO:
        cError = 'Código EAN14 YA registrado en el material ' + T-MATG.codmat.
        cError = pError.
        DISPLAY T-MATG.codmat cError WITH STREAM-IO NO-BOX WIDTH 80.
        lError = YES.
        DELETE T-MATG.
        DELETE T-MAT1.
        NEXT.
    END.
    RUN gn/verifica-ean-repetido ( 'EAN14',3,T-MAT1.codmat,T-MAT1.Barras[3],OUTPUT pError).
    IF pError > '' THEN DO:
        cError = 'Código EAN14 YA registrado en el material ' + T-MATG.codmat.
        cError = pError.
        DISPLAY T-MATG.codmat cError WITH STREAM-IO NO-BOX WIDTH 80.
        lError = YES.
        DELETE T-MATG.
        DELETE T-MAT1.
        NEXT.
    END.
    RUN gn/verifica-ean-repetido ( 'EAN14',4,T-MAT1.codmat,T-MAT1.Barras[4],OUTPUT pError).
    IF pError > '' THEN DO:
        cError = 'Código EAN14 YA registrado en el material ' + T-MATG.codmat.
        cError = pError.
        DISPLAY T-MATG.codmat cError WITH STREAM-IO NO-BOX WIDTH 80.
        lError = YES.
        DELETE T-MATG.
        DELETE T-MAT1.
        NEXT.
    END.
END.
OUTPUT CLOSE.
IF lError = NO THEN RETURN 'OK'. ELSE RETURN 'ADM-ERROR'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

