DEF VAR s-codcia AS INT INIT 001.
DEF TEMP-TABLE t-ped
    FIELD codmat AS CHAR FORMAT 'x(6)'
    FIELD canped AS DEC.


DEF TEMP-TABLE Detalle LIKE integral.Almmmatg
    FIELD canped AS DEC
    FIELD StockConti  AS DEC EXTENT 20
    FIELD StockCissac AS DEC EXTENT 20.

DEF VAR x-AlmConti AS CHAR NO-UNDO.
DEF VAR x-AlmCissac AS CHAR NO-UNDO.

ASSIGN
    x-AlmConti = '11,22,22a,130,35a,40,40a,15,17,03,03c,04,05,05c,35,42,42a,42b,83b'
    x-AlmCissac = '11,22,22a,130,40,40a,15,42'.

INPUT FROM m:\tmp\autoservicios.prn.
REPEAT:
    CREATE t-ped.
    IMPORT t-ped.
END.
INPUT CLOSE.

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
chWorkSheet:Range("g2"):Value = "Pedido".
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
FOR EACH Detalle BY Detalle.CodMat:
    t-column = t-column + 1.
    /* DATOS DEL PRODUCTO */    
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + Detalle.CodMat.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + Detalle.DesMat.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + Detalle.DesMar.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + Detalle.UndBas.
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + Detalle.CodFam.
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + Detalle.SubFam.
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.CanPed.
    
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



PROCEDURE carga-temporal:

  DEF VAR x-Orden AS INT NO-UNDO INIT 1.
  DEF VAR x-CanPed AS DEC NO-UNDO.

  /* STOCKS CONTI */
  DO x-Orden = 1 TO NUM-ENTRIES(x-AlmConti):
      FOR EACH integral.almmmate NO-LOCK WHERE integral.almmmate.codcia = s-codcia
          AND integral.almmmate.codalm = ENTRY(x-Orden, x-AlmConti),
          FIRST t-ped WHERE t-ped.codmat = integral.almmmate.codmat NO-LOCK,
          FIRST integral.almmmatg OF integral.almmmate NO-LOCK:
          FIND detalle OF integral.almmmatg EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE detalle THEN CREATE detalle.
          BUFFER-COPY integral.almmmatg TO detalle
              ASSIGN 
              detalle.canped = t-ped.canped
              Detalle.StockConti[x-Orden] = integral.Almmmate.stkact.
      END.
  END.
  /* STOCKS CISSAC */
  DO x-Orden = 1 TO NUM-ENTRIES(x-AlmCissac):
      FOR EACH cissac.almmmate NO-LOCK WHERE cissac.almmmate.codcia = s-codcia
          AND cissac.almmmate.codalm = ENTRY(x-Orden, x-AlmCissac),
          FIRST t-ped WHERE t-ped.codmat = cissac.almmmate.codmat NO-LOCK,
          FIRST cissac.almmmatg OF cissac.almmmate NO-LOCK:
          FIND detalle OF cissac.almmmatg EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE detalle THEN CREATE detalle.
          BUFFER-COPY cissac.almmmatg TO detalle
              ASSIGN 
              detalle.canped = t-ped.canped
              Detalle.StockCissac[x-Orden] = cissac.Almmmate.stkact.
      END.
  END.
      
END PROCEDURE.
