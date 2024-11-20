DEF TEMP-TABLE DETALLE LIKE Almmmatg
    FIELD Stk03  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Ucayali */
    FIELD Stk03a AS DEC FORMAT '->>>,>>>,>>9.99'    /* Ucayali */
    FIELD Stk04  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Andahuaylas */
    FIELD Stk04a  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Andahuaylas */
    FIELD Stk05  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Paruro */
    FIELD Stk05a  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Paruro */
    FIELD Stk11  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Ate */
    FIELD Stk16  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Tda San Miguel */
    FIELD Stk83  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Andahuaylas */
    FIELD Stk130  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Andahuaylas */
    FIELD Stk131  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Andahuaylas */
    FIELD Stk22  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Andahuaylas */
    FIELD Stk22a  AS DEC FORMAT '->>>,>>>,>>9.99'    /* Andahuaylas */
    INDEX DETA01 AS PRIMARY CodCia CodMat.



  DEF VAR DFecha  AS DATE.
  DEF VAR F-Saldo  AS DEC NO-UNDO.
  DEF VAR F-CtoUni AS DEC NO-UNDO.
  DEF VAR x-Total  AS DEC NO-UNDO.
  DEF VAR s-codcia AS INT INIT 1.
  
  DFecha = TODAY.
  
  FOR EACH DETALLE:
    DELETE DETALLE.
  END.

  FOR EACH Almmmatg WHERE Almmmatg.codcia = s-codcia 
        AND Almmmatg.licencia[1] = '001'
        NO-LOCK:
    DISPLAY Almmmatg.CodMat LABEL "Codigo de Articulo "
        FORMAT "X(8)" WITH FRAME F-Proceso.
    PAUSE 0.
    FIND DETALLE OF Almmmatg EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE DETALLE 
    THEN CREATE DETALLE.
    BUFFER-COPY Almmmatg TO DETALLE.
    /* Saldo Logistico */
    FOR EACH Almacen WHERE almacen.codcia = s-codcia
            AND almacen.flgrep = YES NO-LOCK:
        IF Almacen.AlmCsg = YES THEN NEXT.
        ASSIGN
            F-Saldo  = 0.
        FIND LAST AlmStkAl WHERE almstkal.codcia = s-codcia
            AND almstkal.codalm = almacen.codalm
            AND almstkal.codmat = almmmatg.codmat
            AND almstkal.fecha <= DFecha
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almstkal THEN F-Saldo = almstkal.stkact.
        CASE Almacen.codalm:
            WHEN '03' THEN DETALLE.Stk03 = F-Saldo.
            WHEN '03a' THEN DETALLE.Stk03a = F-Saldo.
            WHEN '04' THEN DETALLE.Stk04 = F-Saldo.
            WHEN '04a' THEN DETALLE.Stk04a = F-Saldo.
            WHEN '05' THEN DETALLE.Stk05 = F-Saldo.
            WHEN '05a' THEN DETALLE.Stk05a = F-Saldo.
            WHEN '11' THEN DETALLE.Stk11 = F-Saldo.
            WHEN '16' THEN DETALLE.Stk16 = F-Saldo.
            WHEN '83' THEN DETALLE.Stk83 = F-Saldo.
            WHEN '130' THEN DETALLE.Stk130 = F-Saldo.
            WHEN '131' THEN DETALLE.Stk131 = F-Saldo.
            WHEN '22' THEN DETALLE.Stk22 = F-Saldo.
            WHEN '22a' THEN DETALLE.Stk22a = F-Saldo.
        END CASE.
    END.
  END.
  HIDE FRAME F-Proceso.

output to c:\tmp\disney.txt.
for each detalle:
    display detalle.codmat detalle.desmat detalle.undbas detalle.stk03 detalle.stk03a 
        detalle.stk04 detalle.stk04a detalle.stk05 detalle.stk05a
        detalle.stk11 detalle.stk16 detalle.stk83
        detalle.stk130 detalle.stk131 detalle.stk22 detalle.stk22a
        with stream-io no-labels no-box width 300.
end.
output close.
