DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00000'.

DEF VAR s-coddoc AS CHAR INIT 'PED' NO-UNDO.

DEF TEMP-TABLE Detalle 
    FIELD codmat LIKE integral.Almmmatg.codmat
    FIELD codcia LIKE integral.Almmmatg.codcia
    FIELD Saldo AS DEC
    FIELD StockConti AS DEC EXTENT 25
    FIELD StockCissac AS DEC EXTENT 25
    FIELD CtoUniConti AS DEC
    FIELD CtoUniCissac AS DEC.

DEF TEMP-TABLE CPEDI LIKE integral.Faccpedi.

DEF VAR x-AlmConti AS CHAR NO-UNDO.
DEF VAR x-AlmCissac AS CHAR NO-UNDO.

ASSIGN
    x-AlmConti = '11,22,22a,130,35a,36a,40,40a,15,17,03,03c,04,05,05c,42,42a,42b,83b'
    x-AlmCissac = '11,22,22a,130,40,40a,15,42'.
DEF VAR x-Orden AS INT NO-UNDO INIT 1.
DEF VAR x-CanPed AS DEC NO-UNDO.

RUN Excel.

PROCEDURE Carga-Temporal:
/* ********************* */

  FOR EACH Detalle:
      DELETE Detalle.
  END.
  INPUT FROM m:\tmp\codigos.prn.
  REPEAT:
      CREATE Detalle.
      IMPORT Detalle.
      ASSIGN Detalle.codcia = s-codcia.
  END.
  INPUT CLOSE.
  FOR EACH Detalle WHERE Detalle.codmat = '':
      DELETE Detalle.
  END.
  

  /* STOCKS CONTI */
  DO x-Orden = 1 TO NUM-ENTRIES(x-AlmConti):
      FOR EACH Detalle:
          FIND integral.Almmmate WHERE integral.Almmmate.codcia = s-codcia
              AND integral.Almmmate.codalm = ENTRY(x-Orden, x-AlmConti)
              AND integral.Almmmate.codmat = Detalle.codmat
              NO-LOCK NO-ERROR.
          IF AVAILABLE integral.Almmmate THEN Detalle.StockConti[x-Orden] = integral.Almmmate.stkact.
      END.
  END.
  /* STOCKS CISSAC */
  IF CONNECTED('cissac') THEN DO:
      DO x-Orden = 1 TO NUM-ENTRIES(x-AlmCissac):
          FOR EACH Detalle:
              FIND cissac.Almmmate WHERE cissac.Almmmate.codcia = s-codcia
                  AND cissac.Almmmate.codalm = ENTRY(x-Orden, x-AlmCissac)
                  AND cissac.Almmmate.codmat = Detalle.codmat
                  NO-LOCK NO-ERROR.
              IF AVAILABLE cissac.Almmmate THEN Detalle.StockCissac[x-Orden] = cissac.Almmmate.stkact.
          END.
      END.
  END.

  END PROCEDURE.


PROCEDURE Excel:
/* *********** */

    DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
    DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
    DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
    DEFINE VARIABLE chChart                 AS COM-HANDLE.
    DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
    DEFINE VARIABLE iCount                  AS INTEGER init 1.
    DEFINE VARIABLE iIndex                  AS INTEGER.
    DEFINE VARIABLE cColumn                 AS CHARACTER.
    DEFINE VARIABLE cRange                  AS CHARACTER.
    DEFINE VARIABLE t-Column                AS INTEGER.
    DEFINE VARIABLE t-Letra                 AS INTEGER.
    DEFINE VARIABLE cLetra                  AS CHAR.
    DEFINE VARIABLE cLetra2                 AS CHAR.
    DEFINE VARIABLE xOrden                  AS INT.

    RUN Carga-Temporal.

    /* create a new Excel Application object */
    CREATE "Excel.Application" chExcelApplication.

    /* create a new Workbook */
    chWorkbook = chExcelApplication:Workbooks:Add().

    /* get the active Worksheet */
    chWorkSheet = chExcelApplication:Sheets:Item(1).

    chWorkSheet:Range("A2"):Value = "Material".
    chWorkSheet:Range("B2"):Value = "Descripcion".
    chWorkSheet:Range("C2"):Value = "Marca".
    chWorkSheet:Range("D2"):Value = "Unidad".
    chWorkSheet:Range("E2"):Value = "Linea".
    chWorkSheet:Range("F2"):Value = "Sub-linea".
    chWorkSheet:Range("G2"):Value = "Saldo".
    t-Letra = ASC('H').
    t-column = 2.
    cLetra2 = ''.
    DO xOrden = 1 TO NUM-ENTRIES(x-AlmConti):
        cColumn = STRING(t-Column).
        cLetra  = CHR(t-Letra).
        cRange = cLetra2 + cLetra + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + ENTRY(xOrden, x-AlmConti).
        t-Letra = t-Letra + 1.
        IF t-Letra > ASC('Z') THEN DO:
            t-Letra = ASC('A').
            IF cLetra2 = '' 
            THEN cLetra2 = 'A'.
            ELSE cLetra2 = CHR(ASC(cLetra2) + 1).
        END.

    END.
    DO xOrden = 1 TO NUM-ENTRIES(x-AlmCissac):
        cColumn = STRING(t-Column).
        cLetra  = CHR(t-Letra).
        cRange = cLetra2 + cLetra + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + ENTRY(xOrden, x-AlmCissac).
        t-Letra = t-Letra + 1.
        IF t-Letra > ASC('Z') THEN DO:
            t-Letra = ASC('A').
            IF cLetra2 = '' 
            THEN cLetra2 = 'A'.
            ELSE cLetra2 = CHR(ASC(cLetra2) + 1).
        END.

    END.

    loopREP:
    FOR EACH Detalle, FIRST integral.Almmmatg OF Detalle NO-LOCK BREAK BY Detalle.CodMat:
        t-column = t-column + 1.
        /* DATOS DEL PRODUCTO */    
        cColumn = STRING(t-Column).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + Detalle.CodMat.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + integral.Almmmatg.DesMat.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + integral.Almmmatg.DesMar.
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + integral.Almmmatg.UndBas.
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + integral.Almmmatg.CodFam.
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + integral.Almmmatg.SubFam.
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = Detalle.Saldo.

        t-Letra = ASC('H').
        cLetra2 = ''.
        xOrden = 1.
        DO xOrden = 1 TO NUM-ENTRIES(x-AlmConti):
            cColumn = STRING(t-Column).
            cLetra  = CHR(t-Letra).
            cRange = cLetra2 + cLetra + cColumn.
            chWorkSheet:Range(cRange):Value = Detalle.StockConti[xOrden].
            t-Letra = t-Letra + 1.
            IF t-Letra > ASC('Z') THEN DO:
                t-Letra = ASC('A').
                IF cLetra2 = '' 
                THEN cLetra2 = 'A'.
                ELSE cLetra2 = CHR(ASC(cLetra2) + 1).
            END.
        END.
        DO xOrden = 1 TO NUM-ENTRIES(x-AlmCissac):
            cColumn = STRING(t-Column).
            cLetra  = CHR(t-Letra).
            cRange = cLetra2 + cLetra + cColumn.
            chWorkSheet:Range(cRange):Value = Detalle.StockCissac[xOrden].
            t-Letra = t-Letra + 1.
            IF t-Letra > ASC('Z') THEN DO:
                t-Letra = ASC('A').
                IF cLetra2 = '' 
                THEN cLetra2 = 'A'.
                ELSE cLetra2 = CHR(ASC(cLetra2) + 1).
            END.
        END.
    END.    
    iCount = iCount + 2.

    HIDE FRAME f-mensajes NO-PAUSE.

    /* launch Excel so it is visible to the user */
    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

END PROCEDURE.

