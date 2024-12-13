DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00000'.

DEF VAR s-coddoc AS CHAR INIT 'PED' NO-UNDO.

DEF TEMP-TABLE Detalle LIKE integral.Almmmatg
    FIELD CodCli LIKE integral.Faccpedi.codcli
    FIELD NomCli LIKE integral.Faccpedi.nomcli
    FIELD Saldo AS DEC
    FIELD StockConti AS DEC EXTENT 25
    FIELD StockCissac AS DEC EXTENT 25
    FIELD CtoUniConti AS DEC
    FIELD CtoUniCissac AS DEC
    INDEX LLave01 codcia codmat codcli.

DEF TEMP-TABLE CPEDI LIKE integral.Faccpedi.

DEF VAR x-AlmConti AS CHAR NO-UNDO.
DEF VAR x-AlmCissac AS CHAR NO-UNDO.

ASSIGN
    x-AlmConti = '11,22,22a,130,35a,36a,40,40a,15,17,03,03c,04,05,05c,42,42a,42b,83b'
    x-AlmCissac = '11,22,22a,130,40,40a,15,42'.
DEF VAR x-Orden AS INT NO-UNDO INIT 1.
DEF VAR x-CanPed AS DEC NO-UNDO.

FOR EACH integral.Faccpedi NO-LOCK WHERE codcia = s-codcia
    AND integral.Faccpedi.coddiv = s-coddiv
    AND integral.Faccpedi.coddoc = s-coddoc
    AND integral.Faccpedi.flgest = 'P'
    AND integral.Faccpedi.fchped >= 01/01/09
    AND integral.Faccpedi.codven = '151':
    CREATE CPEDI.
    BUFFER-COPY integral.Faccpedi TO CPEDI.
END.
RUN Excel.

PROCEDURE Carga-Temporal:
/* ********************* */

  FOR EACH Detalle:
      DELETE Detalle.
  END.
  /* SALDO DE PEDIDOS */
  FOR EACH CPEDI, FIRST integral.gn-clie NO-LOCK WHERE integral.gn-clie.codcia = 0
      AND integral.gn-clie.codcli = CPEDI.codcli
      BY CPEDI.NroPed:
      FOR EACH integral.Facdpedi OF CPEDI NO-LOCK WHERE CanPed - CanAte > 0, 
            FIRST integral.Almmmatg OF Facdpedi NO-LOCK:
          FIND Detalle WHERE Detalle.codmat = integral.Facdpedi.codmat
              AND Detalle.codcli = CPEDI.codcli
              EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE Detalle THEN DO:
              CREATE Detalle.
              BUFFER-COPY Almmmatg TO Detalle
                  ASSIGN Detalle.codcli = CPEDI.codcli
                        Detalle.nomcli = integral.gn-clie.nomcli.
          END.
          ASSIGN
              x-CanPed = (Facdpedi.canped - Facdpedi.canate) * Facdpedi.factor
              Detalle.Saldo = Detalle.Saldo + x-CanPed.
      END.
      x-Orden = x-Orden + 1.
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
    chWorkSheet:Range("H2"):Value = "Cliente".
    chWorkSheet:Range("I2"):Value = "Nombre del cliente".
    t-Letra = ASC('J').
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
    FOR EACH Detalle BREAK BY Detalle.CodMat:
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
        chWorkSheet:Range(cRange):Value = Detalle.Saldo.
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + Detalle.Codcli.
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = Detalle.Nomcli.

        t-Letra = ASC('J').
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

