DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

DEFINE VARIABLE chExcelApplication  AS COM-HANDLE.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE.
DEFINE VARIABLE chWorksheet         AS COM-HANDLE.
DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cValueX          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.

DEF VAR s-codcia    AS INT  NO-UNDO.
DEF VAR s-user-id   AS CHAR INIT 'ADMIN' NO-UNDO.
DEF VAR s-codmon AS INT INIT 1 NO-UNDO.
DEF VAR s-nroitm AS INT NO-UNDO.
ASSIGN
    s-codcia = 001.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.


DEF TEMP-TABLE T-MATG LIKE Almmmatg.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

/* CARGAMOS EL TEMPORAL */
ASSIGN
    s-nroitm = 1
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */

REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    /* Descripción */
    CREATE T-MATG.
    ASSIGN 
        T-MATG.codcia = s-codcia
        T-MATG.codmat = STRING(s-nroitm, '999999')
        T-MATG.catconta = "MC"
        T-MATG.desmat = cValue.
    /* Linea */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.codfam = SUBSTRING(cValue,1,3).
    /* Sublinea*/
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.subfam = SUBSTRING(cValue,1,3).
    /* SubSublinea */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.CodSSFam = SUBSTRING(cValue,1,3).
    /* Proveedor */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.codpr1 = cValue.
    /* Marca */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.DesMar = cValue.
    /* Unidades */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.Chr__01 = cValue
        T-MATG.UndBas = cValue
        T-MATG.UndCmp = cValue
        T-MATG.UndStk = cValue.
    s-nroitm = s-nroitm + 1.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

FOR EACH T-MATG:
    IF codfam = ? THEN codfam = '999'.
    IF subfam = ? THEN subfam = '999'.
    IF codssfam = ? THEN codssfam = '999'.
    IF desmar = ? THEN desmar = ''.
    FIND FIRST almtabla WHERE almtabla.Tabla = "MK" AND
        almtabla.Nombre = T-MATG.desmar
        NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN T-MATG.codmar = almtabla.Codigo.
END.

RUN Aprobar.


PROCEDURE Aprobar:


    DEF VAR i AS INT NO-UNDO.
    DEF VAR x-NroCor AS INT NO-UNDO.
    DEF VAR x-OrdMat AS INTEGER NO-UNDO.
    DEF VAR C-ALM    AS CHAR NO-UNDO.

    DEFINE VAR lCodMat AS CHAR.

    DEF BUFFER MATG FOR Almmmatg.

    FOR EACH T-MATG:
            /* Capturamos Correlativo */
            FIND LAST MATG WHERE MATG.CodCia = S-CODCIA NO-LOCK NO-ERROR.
            IF AVAILABLE MATG THEN x-NroCor = INTEGER(MATG.codmat) + 1.
            ELSE x-NroCor = 1.
            FIND LAST MATG WHERE MATG.Codcia = S-CODCIA 
                AND  MATG.CodFam = T-MATG.Codfam
                USE-INDEX Matg08 NO-LOCK NO-ERROR.
            IF AVAILABLE MATG 
            THEN x-ordmat = MATG.Orden + 3.
            ELSE x-ordmat = 1.
            CREATE Almmmatg.
            BUFFER-COPY T-MATG 
                TO Almmmatg
                ASSIGN
                    Almmmatg.codmat = STRING(x-NroCor,"999999")
                    Almmmatg.orden  = x-ordmat
                    Almmmatg.ordlis = x-ordmat
                    Almmmatg.tpoart = 'A'     /* Activo */
                    Almmmatg.FchIng = TODAY
                    Almmmatg.FchAct = TODAY
                    Almmmatg.Libre_C05 = s-user-id + "|" + STRING(TODAY, '99/99/9999').
            lCodMat = lCodMat + '-' + Almmmatg.codmat.
            /* Actualizamos las SUBCATEGORIAS */
            FOR EACH webscmatt NO-LOCK WHERE webscmatt.CodCia = s-codcia
                AND webscmatt.codmat = T-MATG.codmat:
                CREATE webscmatg.
                ASSIGN
                    webscmatg.CodCia = webscmatt.CodCia 
                    webscmatg.codmat = Almmmatg.codmat
                    webscmatg.Subcategoria = webscmatt.Subcategoria.
            END.
            /* Actualizamos la lista de Almacenes */ 
            /*ALM = TRIM(Almmmatg.almacenes).*/
            C-ALM = ''.
            FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt:
                /* CONSISTENCIA POR PRODUCTO Y ALMACEN */
                IF Almmmatg.TpoMrg <> '' AND Almacen.Campo-c[2] <> '' THEN DO:
                    IF Almmmatg.TpoMrg <> Almacen.Campo-c[2] THEN NEXT.
                END.
                /* *********************************** */
                IF C-ALM = "" THEN C-ALM = TRIM(Almacen.CodAlm).
                IF LOOKUP(TRIM(Almacen.CodAlm),C-ALM) = 0 THEN C-ALM = C-ALM + "," + TRIM(Almacen.CodAlm).
            END.
            ASSIGN 
                Almmmatg.almacenes = C-ALM.
            /* RHC 24-09-2013 ACTUALIZAMOS MONEDA DE VENTA Y TIPO DE CAMBIO */
            FIND Almtfami OF Almmmatg NO-LOCK NO-ERROR.
            IF AVAILABLE Almtfami THEN DO:
               ASSIGN
                   Almmmatg.tpocmb = Almtfami.tpocmb.
            END.
            IF NOT (Almmmatg.MonVta = 1 OR Almmmatg.MonVta = 2)
                THEN Almmmatg.MonVta = 1.
            /* ************************************************************ */
            /* RHC 27.11.09 REPOSICIONES AUTOMATICAS */
            FIND Vtapmatg WHERE Vtapmatg.codcia = s-codcia
                AND Vtapmatg.codmat = 'T' + T-MATG.codmat
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE Vtapmatg THEN DO:
                ASSIGN
                    Vtapmatg.codmat = Almmmatg.codmat
                    NO-ERROR.
            END.

            RUN ACTUALIZA-MAT-x-ALM NO-ERROR.  
            IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.


            /* ACTUALIZA CISSAC */
            RUN Actualiza-Cissac NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.

    END.

    MESSAGE lCodMat.

END PROCEDURE.


PROCEDURE ACTUALIZA-MAT-x-ALM:

    FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt
        TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:

        /* CONSISTENCIA POR PRODUCTO Y ALMACEN */
        IF Almmmatg.TpoMrg <> '' AND Almacen.Campo-c[2] <> '' THEN DO:
            IF Almmmatg.TpoMrg <> Almacen.Campo-c[2] THEN NEXT.
        END.
        /* *********************************** */
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
    END.


END PROCEDURE.


PROCEDURE Actualiza-Cissac:


    RUN alm/p-catcontiacissac (ROWID(Almmmatg)).

END PROCEDURE.
