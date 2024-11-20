&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER b-matg FOR Almmmatg.
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
      TABLE: b-matg B "?" ? INTEGRAL Almmmatg
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

OUTPUT TO d:\tmp\verificar.txt.
FOR EACH t-matg:
    PUT UNFORMATTED
        T-MATG.codcia '|'
        T-MATG.codmat '|'
        T-MATG.desmat '|'
        T-MATG.codmar '|'
        T-MATG.codfam '|'
        T-MATG.subfam '|'
        T-MATG.CodSSFam '|'
        T-MATG.CHR__02 '|'
        T-MATG.aftigv '|'
        T-MATG.tpomrg '|'
        T-MATG.undbas '|'
        T-MATG.undcmp '|'
        T-MATG.undstk '|'
        T-MATG.unda   '|'
        T-MATG.undb   '|'
        T-MATG.undc   '|'
        T-MATG.CHR__01 '|'
        T-MATG.undalt[1] '|'
        T-MATG.DEC__03 '|'
        T-MATG.stkmin       '|'
        T-MATG.canemp       '|'
        T-MATG.stkrep       '|'
        T-MATG.codpr1       '|'
        T-MATG.codpr2       '|'
        T-MATG.pesmat       '|'
        T-MATG.codbrr       '|'
        T-MATG.coddiges     '|'
        T-MATG.vtodigesa    '|'
        T-MATG.stkmax       '|'
        T-MATG.libre_d03    '|'
        SKIP.
END.
OUTPUT CLOSE.

/* Consistencia final */
RUN Consistencia-Final.
IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN.
/* Creamos los artículos en el catálogo de materiales */
RUN Crea-Productos.

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
    /* CODIGO */
    CREATE T-MATG.
    ASSIGN
        T-MATG.codcia = s-codcia
        T-MATG.codmat = STRING(t-Row - 1, '999999')    /* Valor Temporal */
        T-MATG.tpoart = "A".    /* ACTIVO por defecto */
    /* DESCRIPCION */
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.desmat = cValue.
    /* MARCA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.codmar = STRING(INT(cValue),'9999').
    /* LINEA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.codfam = STRING(INT(cValue),'999').
    /* SUB-LINEA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.subfam = cValue.
    /* SUBSBLINEA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.CodSSFam = STRING(INT(cValue),'999').
    /* PROPIOS / TERCEROS */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.CHR__02 = cValue.
    /* AFECTO */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.aftigv = IF LOOKUP(cValue, 'SI,Sí,YES,yes,TRUE') > 0 THEN YES ELSE NO.
    /* MAYORISTA /MINORISTA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.tpomrg = (IF cValue = ? THEN '' ELSE STRING(INT(cValue),'9')).
    /* UNIDAD BASICA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.undbas = cValue.
    /* UNIDAD COMPRAS */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.undcmp = cValue.
    /* UNIDAD STOCK */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.undstk = cValue.
    /* UNIDAD A */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.unda = cValue.
    /* UNIDAD B */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = ''.
    ASSIGN
        T-MATG.undb = cValue.
    /* UNIDAD C */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = ''.
    ASSIGN
        T-MATG.undc = cValue.
    /* UNIDAD OFICINA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = ''.
    ASSIGN
        T-MATG.CHR__01 = cValue.
    /* UNIDAD AL POR MENOR */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = ''.
    ASSIGN
        T-MATG.undalt[1] = cValue.
    /* MINIMO VTA MAY */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = '0'.
    ASSIGN
        T-MATG.DEC__03 = DEC(cValue).
    /* MINIMO VTA MIN */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = '0'.
    ASSIGN
        T-MATG.stkmin = DEC(cValue).
    /* EMP MASTER */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = '0'.
    ASSIGN
        T-MATG.canemp = DEC(cValue).
    /* EMP INNER */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = '0'.
    ASSIGN
        T-MATG.stkrep = DEC(cValue).
    /* PROVEEDOR A */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.codpr1 = (IF cValue = ? THEN '' ELSE STRING(INT(cValue)) ) NO-ERROR.
    /* PROVEEDOR B */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.codpr2 = (IF cValue = ? THEN '' ELSE STRING(INT(cValue)) ) NO-ERROR.
    /* PESO */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = '0'.
    ASSIGN
        T-MATG.pesmat = DEC(cValue).
    /* EAN 13 */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    iValue = (IF cValue = ? THEN 0 ELSE INT64(cValue)).
    cValue = (IF iValue = 0 THEN '' ELSE STRING(iValue) ).
    ASSIGN
        T-MATG.codbrr = cValue.
    /* DIGESA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = ''.
    ASSIGN
        T-MATG.coddiges = cValue.
    /* VCTO DIGESA */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.vtodigesa = DATE(cValue) NO-ERROR.
    /* MIN VTA EXPO */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = '0'.
    ASSIGN
        T-MATG.stkmax = DEC(cValue).
    /* EMPAQUE EXPO */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = ? THEN cValue = '0'.
    ASSIGN
        T-MATG.libre_d03 = DEC(cValue).
    /* REFERENCIA KITS */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.libre_c10 = cValue.
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
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH T-MATG WHERE T-MATG.codcia = s-codcia AND T-MATG.codmat > '':
    IF T-MATG.codmar > '' THEN DO:
        FIND almtabla WHERE almtabla.Tabla = "MK" AND
            almtabla.Codigo = T-MATG.CodMar NO-LOCK NO-ERROR.
        IF NOT AVAILABLE almtabla THEN DO:
            MESSAGE T-MATG.codmat "Codigo de Marca no Existe" VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
        T-MATG.desmar = almtabla.nombre.
    END.
    FIND Almtfami WHERE Almtfami.CodCia = S-CODCIA AND
         Almtfami.codfam = T-MATG.codfam NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtfami THEN DO:
       MESSAGE T-MATG.codmat "Codigo de Familia no existe" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".   
    END.
    FIND Almsfami WHERE Almsfami.CodCia = S-CODCIA AND
         Almsfami.codfam = T-MATG.codfam AND
         AlmSFami.subfam = T-MATG.subfam NO-LOCK NO-ERROR.
    IF NOT AVAILABLE AlmSFami THEN DO:
       MESSAGE T-MATG.codmat "Codigo de Familia no existe" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".   
    END.
    FIND Almssfami WHERE Almssfami.CodCia = S-CODCIA 
        AND Almssfami.codfam = T-MATG.codfam
        AND AlmsSFami.subfam = T-MATG.subfam
        AND AlmSSFami.CodSSFam = T-MATG.codssfam
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE AlmsSFami THEN DO:
       MESSAGE T-MATG.codmat "Codigo de Sub Sub Familia no existe" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".   
    END.
    FIND Unidades WHERE Unidades.Codunid = T-MATG.UndBas NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Unidades THEN DO:
       MESSAGE T-MATG.codmat "Unidad Basica no registrada ..." VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".   
    END.
    IF T-MATG.CodPr1 <> '' THEN DO:
        FIND gn-prov WHERE gn-prov.CodCia = pv-codcia 
            AND gn-prov.CodPro = T-MATG.CodPr1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE gn-prov THEN DO:
            MESSAGE T-MATG.codmat 'Proveedor no registrado' VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
    END.
    FIND Unidades WHERE Unidades.Codunid = T-MATG.UndA NO-LOCK NO-ERROR.
    IF T-MATG.UndA <> '' AND NOT AVAILABLE Unidades THEN DO:
       MESSAGE T-MATG.codmat "Unidad A no registrada ..." VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.
    FIND Unidades WHERE Unidades.Codunid = T-MATG.UndB NO-LOCK NO-ERROR.
    IF T-MATG.UndB <> '' AND NOT AVAILABLE Unidades THEN DO:
       MESSAGE T-MATG.codmat "Unidad B no registrada ..." VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.
    FIND Unidades WHERE Unidades.Codunid = T-MATG.UndC NO-LOCK NO-ERROR.
    IF T-MATG.UndC <> '' AND NOT AVAILABLE Unidades THEN DO:
       MESSAGE T-MATG.codmat "Unidad C no registrada ..." VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.
    IF T-MATG.UndA = T-MATG.UndB AND T-MATG.UndA <> ""
        THEN DO:
        MESSAGE T-MATG.codmat 'Las unidades de venta A-B NO pueden ser iguales' VIEW-AS ALERT-BOX ERROR.
        RETURN 'ADM-ERROR'.
    END.
   IF T-MATG.UndA = T-MATG.UndC AND T-MATG.UndA <> ""
       THEN DO:
       MESSAGE T-MATG.codmat 'Las unidades de venta A-C NO pueden ser iguales' VIEW-AS ALERT-BOX ERROR.
       RETURN 'ADM-ERROR'.
   END.
   IF T-MATG.UndB = T-MATG.UndC AND T-MATG.UndB <> ""
       THEN DO:
       MESSAGE T-MATG.codmat 'Las unidades de venta B-C NO pueden ser iguales' VIEW-AS ALERT-BOX ERROR.
       RETURN 'ADM-ERROR'.
   END.
   IF T-MATG.CodBrr <> '' THEN DO:
       FIND FIRST b-matg WHERE b-matg.codcia = s-codcia
           AND b-matg.codbrr = T-MATG.CodBrr
           NO-LOCK NO-ERROR.
       IF AVAILABLE b-matg THEN DO:
           MESSAGE T-MATG.codmat 'Código de barra YA registrado en el material' b-matg.codmat
               VIEW-AS ALERT-BOX WARNING.
           RETURN 'ADM-ERROR'.
       END.
   END.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Crea-Productos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Productos Procedure 
PROCEDURE Crea-Productos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-NroCor AS INT NO-UNDO.
DEF VAR x-ordmat AS INT NO-UNDO.
DEF VAR C-ALM AS CHAR NO-UNDO.

FOR EACH t-matg NO-LOCK WHERE t-matg.codcia = s-codcia AND t-matg.codmat > '':
    /* Capturamos Correlativo */
    FIND LAST MATG WHERE MATG.CodCia = S-CODCIA NO-LOCK NO-ERROR.
    IF AVAILABLE MATG THEN x-NroCor = INTEGER(MATG.codmat) + 1.
    ELSE x-NroCor = 1.
    FIND LAST MATG WHERE MATG.Codcia = S-CODCIA 
                    AND  MATG.CodFam = t-matg.Codfam
                   USE-INDEX Matg08 NO-LOCK NO-ERROR.
    IF AVAILABLE MATG 
    THEN x-ordmat = MATG.Orden + 3.
    ELSE x-ordmat = 1.
    CREATE Almmmatg.
    BUFFER-COPY t-matg TO Almmmatg
        ASSIGN
            Almmmatg.codmat = STRING(x-NroCor,"999999")
            Almmmatg.orden  = x-ordmat
            Almmmatg.ordlis = x-ordmat
            Almmmatg.tpoart = 'A'     /* Activo */
            Almmmatg.FchIng = TODAY
            Almmmatg.FchAct = TODAY.
    /* Actualizamos la lista de Almacenes */ 
    C-ALM = ''.
    FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt:
        IF C-ALM = "" THEN C-ALM = TRIM(Almacen.CodAlm).
        IF LOOKUP(TRIM(Almacen.CodAlm),C-ALM) = 0 THEN C-ALM = C-ALM + "," + TRIM(Almacen.CodAlm).
    END.
    ASSIGN 
        Almmmatg.almacenes = C-ALM.
    RUN ACTUALIZA-MAT-x-ALM.  
    RELEASE Almmmatg.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

