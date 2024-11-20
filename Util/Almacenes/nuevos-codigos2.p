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
DEFINE VARIABLE iValue          AS INT64      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.
DEFINE VAR lMsg AS LOG.

DEF VAR s-codcia    AS INT  NO-UNDO.
DEF VAR s-user-id   AS CHAR INIT 'ADMIN' NO-UNDO.
DEF VAR s-codmon AS INT INIT 1 NO-UNDO.
DEF VAR s-nroitm AS INT NO-UNDO.
ASSIGN
    s-codcia = 001.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xlsx)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.


DEF TEMP-TABLE T-MATG LIKE Almmmatg.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

DEFINE VAR iColumn AS INT.

ASSIGN
    s-nroitm = 1.
lmsg = NO.


REPEAT iColumn = 2 TO 65000:
    /* Descripción */
    cRange = "A" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.

    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */

    DISPLAY iColumn cValue FORMAT 'x(60)'.
    PAUSE 0.

    /* Descripción */
    IF lmsg = YES THEN DO:
        MESSAGE "Descripcion".
    END.
    
    cValue = TRIM(cValue).
    CREATE T-MATG.
    ASSIGN 
        T-MATG.codcia = s-codcia
        T-MATG.codmat = STRING(s-nroitm, '999999')
        T-MATG.catconta = "MC"
        T-MATG.desmat = cValue.

    /* Marca */
    IF lmsg = YES THEN DO:
        MESSAGE "Marca".
    END.

    cRange = "B" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.codmar = cValue
        T-MATG.desmar = "".

    /* Linea */
    IF lmsg = YES THEN DO:
        MESSAGE "Linea".
    END.
    cRange = "C" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.codfam = SUBSTRING(cValue,1,3).

    /* Sublinea*/
    IF lmsg = YES THEN DO:
        MESSAGE "SubLinea".
    END.
    cRange = "D" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.subfam = SUBSTRING(cValue,1,3).

    /* SubSublinea */
    IF lmsg = YES THEN DO:
        MESSAGE "SubSubLinea".
    END.
    cRange = "E" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.CodSSFam = SUBSTRING(cValue,1,3).

    /* Propios/Terceros */
    IF lmsg = YES THEN DO:
        MESSAGE "Propios/Terceros".
    END.
    cRange = "F" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.CHR__02 = SUBSTRING(cValue,1,1).

    /* Afecto al IGV */
    IF lmsg = YES THEN DO:
        MESSAGE "Afecto IGV".
    END.
    cRange = "G" + TRIM(STRING(iColumn)).
    cValue = CAPS(TRIM(chWorkSheet:Range(cRange):VALUE)).
    ASSIGN
        T-MATG.AftIGV = IF(cValue = 'AFECTO' OR cValue = 'SI' ) THEN YES ELSE NO.

    /* Para que Almacenes */
    IF lmsg = YES THEN DO:
        MESSAGE "Para que almacenes".
    END.
    cRange = "H" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    IF TRUE <> (cValue > '') THEN cValue = ''.
    ASSIGN
        T-MATG.TpoMrg = SUBSTRING(cValue,1,1).

    /* Unidad Base */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad Base".
    END.
    cRange = "I" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.UndBas = SUBSTRING(cValue,1,3).

    /* Unidad de Compra */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad de Compra".
    END.
    cRange = "J" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.UndCmp = SUBSTRING(cValue,1,3).

    /* Unidad de Stock */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad de Stock".
    END.
    cRange = "K" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.UndStk = SUBSTRING(cValue,1,3).

    /* Unidad Vta Mostrador "A" */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad Vta Mostrador A".
    END.
    cRange = "L" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):TEXT).
    ASSIGN
        T-MATG.UndA = SUBSTRING(cValue,1,3).

    /* Unidad Vta Mostrador "B" */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad Vta Mostrador B".
    END.
    cRange = "M" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):TEXT).
    ASSIGN
        T-MATG.UndB = SUBSTRING(cValue,1,3).

    /* Unidad Vta Mostrador "C" */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad Vta Mostrador C".
    END.
    cRange = "N" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):TEXT).
    ASSIGN
        T-MATG.UndC = SUBSTRING(cValue,1,3).

    /* Unidad Vta Oficina */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad Vta oficina".
    END.
    cRange = "O" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.CHR__01 = SUBSTRING(cValue,1,3).

    /* Unidad de Vta al por menor */
    IF lmsg = YES THEN DO:
        MESSAGE "Unidad Vta al por menor".
    END.
    cRange = "P" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.UndAlt[1] = SUBSTRING(cValue,1,3).

    /* Minimo Vta Mayorista */
    IF lmsg = YES THEN DO:
        MESSAGE "Minimo Vta Mayorista".
    END.
    cRange = "Q" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.DEC__03 = DEC(cValue).

    /* Minimo Vta Minorista */
    IF lmsg = YES THEN DO:
        MESSAGE "Minimo Vta Minorista".
    END.
    cRange = "R" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.StkMin = DEC(cValue).

    /* Empaque Master */
    IF lmsg = YES THEN DO:
        MESSAGE "Emapque Master".
    END.
    cRange = "S" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.CanEmp = DEC(cValue).

    /* Empaque Inner */
    IF lmsg = YES THEN DO:
        MESSAGE "Empaque Inner".
    END.
    cRange = "T" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.StkRep = DEC(cValue).

    /* Proveedor A */
    IF lmsg = YES THEN DO:
        MESSAGE "Proveedor A".
    END.
    cRange = "U" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    ASSIGN
        T-MATG.CodPr1 = STRING(INTEGER(cValue), '99999999').

    /* Proveedor B */
    IF lmsg = YES THEN DO:
        MESSAGE "Proveedor B".
    END.
    cRange = "V" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    IF NOT (TRUE <> (cValue > ''))  THEN cValue = STRING(INTEGER(cValue), '99999999').
    ELSE cValue = ''.
    ASSIGN
        T-MATG.CodPr2 = cValue.

    /* Peso */
    IF lmsg = YES THEN DO:
        MESSAGE "Peso".
    END.
    cRange = "W" + TRIM(STRING(iColumn)).
    /*cValue = TRIM(chWorkSheet:Range(cRange):VALUE).*/
    cValue = chWorkSheet:Range(cRange):VALUE.
    ASSIGN
        T-MATG.PesMat = DEC(cValue).

    /* Barras EAN13 */
    IF lmsg = YES THEN DO:
        MESSAGE "Barras".
    END.
    cRange = "X" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    iValue = INT64(CVALUE).
    ASSIGN
        T-MATG.CodBrr = STRING(iValue, '9999999999999').

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
    IF desmar = '' THEN DO:
        FIND FIRST almtabla WHERE almtabla.Tabla = "MK" AND
            almtabla.Codigo = T-MATG.codmar
            NO-LOCK NO-ERROR.
        IF AVAILABLE almtabla THEN ASSIGN T-MATG.desmar = almtabla.Nombre.
    END.

END.

/*
MESSAGE 'Continuamos con el proceso?' VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE rpta AS LOG.
IF rpta = YES THEN DO:
    RUN Aprobar.
END.
*/

RUN Aprobar.

RETURN.



PROCEDURE Aprobar:


    DEF VAR i AS INT NO-UNDO.
    DEF VAR x-NroCor AS INT NO-UNDO.
    DEF VAR x-OrdMat AS INTEGER NO-UNDO.
    DEF VAR C-ALM    AS CHAR NO-UNDO.

    DEFINE VAR lCodMat AS CHAR.

    DEF BUFFER MATG FOR Almmmatg.

    FOR EACH T-MATG:
            /* Capturamos Correlativo */
            DISPLAY T-MATG.desmat. 
            PAUSE 0.

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
                    Almmmatg.facequ = 1
                    Almmmatg.Libre_C05 = s-user-id + "|" + STRING(TODAY, '99/99/9999').
            FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
                AND  Almtconv.Codalter = Almmmatg.UndStk 
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almtconv THEN Almmmatg.FacEqu = Almtconv.Equival.

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
