def var x-coddoc as char init 'fac,bol,tck,n/c'.
def var i as int.
DEF VAR x-TpoCmbCmp AS DEC NO-UNDO.
DEF VAR x-TpoCmbVta AS DEC NO-UNDO.
DEF VAR x-Propios AS DEC NO-UNDO.
DEF VAR x-Terceros AS DEC NO-UNDO.
DEF VAR x-Fotocopias AS DEC NO-UNDO.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-nomcia AS CHAR INIT 'CONTINENTAL'.
DEF VAR x-Fecha-1 AS DATE.
DEF VAR x-Fecha-2 AS DATE.
DEF STREAM REPORT.

DEF TEMP-TABLE t-cdocu LIKE ccbcdocu
    FIELD Propios AS DEC
    FIELD Terceros AS DEC
    FIELD Fotocopias AS DEC
    FIELD Puntos_Old AS INT.
DEF TEMP-TABLE DETALLE LIKE t-cdocu.

DEF BUFFER B-CDOCU FOR t-cdocu.
DEF BUFFER B-DDOCU FOR ccbddocu.

ASSIGN
    x-Fecha-1 = 12/15/2009
    x-Fecha-2 = 04/20/2010.

/* paso 1 */
RUN Carga-inicial-temporal.

/* paso 2 */
RUN Carga-Puntos.

/* paso 3 */
RUN Carga-Temporal.

/* paso 4 */
OUTPUT STREAM REPORT TO c:\tmp\formato-1.txt.
RUN Imprime-Puntos-1.
OUTPUT CLOSE.

/* paso 5 */
OUTPUT STREAM REPORT TO c:\tmp\formato-2.txt.
RUN Imprime-Puntos-2.
OUTPUT CLOSE.

PROCEDURE Carga-inicial-temporal:
/* ***************************** */
    DEF VAR x-coddoc as char init 'fac,bol,tck,n/c'.
    DEF VAR i as int.

    FOR EACH gn-divi NO-LOCK WHERE codcia = 001 AND coddiv <> '00000':
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
            AND ccbcdocu.coddiv = gn-divi.coddiv
            AND LOOKUP(coddoc, x-coddoc) > 0
            AND fchdoc >= 12/15/2009:
            IF ccbcdocu.flgest = 'A' THEN NEXT.
            IF ccbcdocu.coddoc = 'N/C' AND ccbcdocu.cndcre = 'N' THEN NEXT. /* NO Otros */
            CREATE t-cdocu.
            BUFFER-COPY ccbcdocu TO t-cdocu.
            ASSIGN
                t-cdocu.puntos_old = t-cdocu.puntos.
        END.
    END.

END PROCEDURE.

PROCEDURE Carga-Puntos:
/* ******************* */

FOR EACH t-cdocu:
    puntos = 0.
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= t-cdocu.FchDoc NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= t-cdocu.FchDoc NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.
    ASSIGN
        x-Propios = 0
        x-Terceros = 0
        x-Fotocopias = 0.
    FOR EACH Ccbddocu OF t-cdocu NO-LOCK,
        FIRST Almmmatg OF Ccbddocu NO-LOCK:
        /* Fotocopias */
        IF Almmmatg.CodFam = '011' THEN DO: 
            x-Fotocopias = x-Fotocopias + Ccbddocu.ImpLin.
            NEXT.
        END.
        IF Almmmatg.CHR__02 = 'P' THEN x-Propios = x-Propios + Ccbddocu.ImpLin.
        ELSE x-Terceros = x-Terceros + Ccbddocu.ImpLin.
    END.
    ASSIGN
        t-cdocu.Fotocopias = x-Fotocopias * ( 1 - t-cdocu.pordto / 100 )
        t-cdocu.Propios = x-Propios * ( 1 - t-cdocu.pordto / 100 )
        t-cdocu.Terceros = x-Terceros * ( 1 - t-cdocu.pordto / 100 ).
    IF t-cdocu.codmon = 2 
    THEN ASSIGN
            x-Propios = x-Propios * x-TpoCmbVta
            x-Terceros = x-Terceros * x-TpoCmbVta.
    /* PUNTOS BONUS */
/*     ASSIGN                                                                                        */
/*         t-cdocu.puntos = TRUNCATE(x-Propios * 1 / 100, 0) + TRUNCATE(x-Terceros *  0.5 / 100, 0). */
    ASSIGN
        t-cdocu.puntos = TRUNCATE(x-Propios / 10, 0) * 3 +
                        TRUNCATE(x-Terceros / 10, 0) * 1.
END.            

END PROCEDURE.

PROCEDURE Carga-Temporal:
/* ********************* */

    FOR EACH t-cdocu WHERE t-cdocu.codcia = s-codcia
          AND LOOKUP(TRIM(t-cdocu.coddoc), 'FAC,BOL,TCK') > 0
          AND t-cdocu.nrocard <> ''
          AND t-cdocu.fchdoc >= x-fecha-1
          AND t-cdocu.fchdoc <= x-fecha-2
          AND t-cdocu.flgest <> 'A' NO-LOCK,
          FIRST GN-CARD WHERE gn-card.NroCard = t-cdocu.nrocard NO-LOCK:
      IF ( t-cdocu.puntos + t-cdocu.puntos_old ) <= 0 THEN NEXT.
      CREATE DETALLE.
      BUFFER-COPY t-cdocu TO DETALLE.
      /* buscamos la nota de credito por devolucion */
      FOR EACH B-CDOCU WHERE B-CDOCU.codcia = s-codcia
              AND B-CDOCU.coddoc = 'N/C'
              AND B-CDOCU.codref = t-cdocu.coddoc
              AND B-CDOCU.nroref = t-cdocu.nrodoc
              AND B-CDOCU.flgest <> 'A'
              AND B-CDOCU.CndCre <> 'N'       /* NO por Otros conceptos */
              NO-LOCK:
          CREATE DETALLE.
          BUFFER-COPY B-CDOCU TO DETALLE
              ASSIGN
                  DETALLE.NroCard = t-cdocu.nrocard
                  DETALLE.Puntos = B-CDOCU.Puntos * -1
                  DETALLE.Puntos_Old = B-CDOCU.Puntos_Old * -1
                  DETALLE.Imptot = B-CDOCU.ImpTot * -1
                  DETALLE.Fotocopias = B-CDOCU.Fotocopias * -1
                  DETALLE.Propios = B-CDOCU.Propios * -1
                  DETALLE.Terceros = B-CDOCU.Terceros * -1.
      END.        
    END.

END PROCEDURE.


PROCEDURE Imprime-puntos-1:
/* ************************ */

    DEF VAR x-ImpTot AS DEC FORMAT '>>>,>>>,>>9.99'.
    DEF VAR x-ImpPro AS DEC FORMAT '>>>,>>>,>>9.99'.
    DEF VAR x-ImpTer AS DEC FORMAT '>>>,>>>,>>9.99'.
    DEF VAR x-ImpFot AS DEC FORMAT '>>>,>>>,>>9.99'.

    DEF VAR x-NomCli AS CHAR.

    DEFINE FRAME F-REPORTE
      DETALLE.nrocard                             COLUMN-LABEL 'Cliente'
      x-nomcli            FORMAT 'x(45)'          COLUMN-LABEL 'Nombre'
      DETALLE.coddiv      FORMAT 'x(5)'           COLUMN-LABEL 'Division'
      x-ImpTot            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Importe S/.'
      x-ImpPro            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Propios S/.'
      x-ImpTer            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Terceros S/.'
      x-ImpFot            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Fotocopias S/.'
      DETALLE.Puntos      FORMAT '->>>,>>9'       COLUMN-LABEL 'ContiPuntos Nuevo'
      DETALLE.Puntos_Old  FORMAT '->>>,>>9'       COLUMN-LABEL 'ContiPuntos Viejo'
      DETALLE.codcli      FORMAT 'x(11)'          COLUMN-LABEL 'RUC'
      HEADER
           S-NOMCIA FORMAT "X(50)" SKIP
           "Pagina : " TO 90 PAGE-NUMBER(REPORT) TO 100 FORMAT "ZZZZZ9" SKIP
           "CONTIPUNTOS" AT 30
           " Fecha : " TO 90 TODAY TO 100 FORMAT "99/99/9999" SKIP
           "  Hora : " TO 90 STRING(TIME,"HH:MM:SS") TO 100 SKIP
           "DESDE EL" x-Fecha-1 "HASTA EL" x-Fecha-2 SKIP(1)
    WITH WIDTH 200 NO-BOX STREAM-IO DOWN.

    FOR EACH DETALLE BREAK BY DETALLE.CodCia BY DETALLE.NroCard BY DETALLE.CodDiv:
        FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= DETALLE.FchDoc NO-LOCK NO-ERROR.
        IF NOT AVAIL Gn-Tcmb THEN 
            FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= DETALLE.FchDoc NO-LOCK NO-ERROR.
        IF AVAIL Gn-Tcmb THEN 
            ASSIGN
                x-TpoCmbCmp = Gn-Tcmb.Compra
                x-TpoCmbVta = Gn-Tcmb.Venta.
      ASSIGN
          x-ImpTot = 0
          x-ImpPro = 0
          x-ImpTer = 0
          x-ImpFot = 0.
      ASSIGN
          x-ImpTot = DETALLE.Imptot
          x-ImpPro = DETALLE.Propios
          x-ImpTer = DETALLE.Terceros
          x-ImpFot = DETALLE.Fotocopias.
      IF DETALLE.CodMon = 2
      THEN ASSIGN
          x-ImpTot = x-ImpTot * x-TpoCmbVta
          x-ImpPro = x-ImpPro * x-TpoCmbVta
          x-ImpTer = x-ImpTer * x-TpoCmbVta
          x-ImpFot = x-ImpFot * x-TpoCmbVta.
      ACCUMULATE x-ImpTot (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE x-ImpPro (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE x-ImpTer (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE x-ImpFot (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE DETALLE.Puntos (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE DETALLE.Puntos_Old (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      IF LAST-OF(DETALLE.CodDiv) THEN DO:
          x-NomCli = ''.
          FIND GN-CARD WHERE GN-CARD.NroCard = DETALLE.NroCard NO-LOCK NO-ERROR.
          IF AVAILABLE GN-CARD THEN x-NomCli = GN-CARD.NomCli[1].
          DISPLAY STREAM REPORT
              DETALLE.nrocard
              x-NomCli
              DETALLE.CodDiv
              DETALLE.codcli
              ACCUM TOTAL BY DETALLE.CodDiv x-ImpTot @ x-ImpTot
              ACCUM TOTAL BY DETALLE.CodDiv x-ImpPro @ x-ImpPro
              ACCUM TOTAL BY DETALLE.CodDiv x-ImpTer @ x-ImpTer
              ACCUM TOTAL BY DETALLE.CodDiv x-ImpFot @ x-ImpFot
              ACCUM TOTAL BY DETALLE.CodDiv DETALLE.Puntos @ DETALLE.Puntos
              ACCUM TOTAL BY DETALLE.CodDiv DETALLE.Puntos_Old @ DETALLE.Puntos_Old
              WITH FRAME F-REPORTE.
      END.
      IF LAST-OF(DETALLE.NroCard) THEN DO:
          UNDERLINE STREAM REPORT
              x-ImpTot
              x-ImpPro
              x-ImpTer
              x-ImpFot
              WITH FRAME F-REPORTE.
          DISPLAY STREAM REPORT
              ACCUM TOTAL BY DETALLE.NroCard x-ImpTot @ x-ImpTot
              ACCUM TOTAL BY DETALLE.NroCard x-ImpPro @ x-ImpPro
              ACCUM TOTAL BY DETALLE.NroCard x-ImpTer @ x-ImpTer
              ACCUM TOTAL BY DETALLE.NroCard x-ImpFot @ x-ImpFot
              ACCUM TOTAL BY DETALLE.NroCard DETALLE.Puntos @ DETALLE.Puntos
              ACCUM TOTAL BY DETALLE.NroCard DETALLE.Puntos_Old @ DETALLE.Puntos_Old
              WITH FRAME F-REPORTE.
          DOWN STREAM REPORT 1 WITH FRAME F-REPORTE.
      END.
    END.

END PROCEDURE.



PROCEDURE Imprime-puntos-2:
/* *********************** */

    DEF VAR x-ImpTot AS DEC FORMAT '>>>,>>>,>>9.99'.
    DEF VAR x-ImpPro AS DEC FORMAT '>>>,>>>,>>9.99'.
    DEF VAR x-ImpTer AS DEC FORMAT '>>>,>>>,>>9.99'.
    DEF VAR x-ImpFot AS DEC FORMAT '>>>,>>>,>>9.99'.
    DEF VAR x-NomCli AS CHAR.

    FOR EACH DETALLE  BREAK BY DETALLE.NroCard BY DETALLE.CodDiv BY DETALLE.FchDoc:
      x-NomCli = ''.
      FIND GN-CARD WHERE GN-CARD.NroCard = DETALLE.NroCard NO-LOCK NO-ERROR.
      IF AVAILABLE GN-CARD THEN x-NomCli = GN-CARD.NomCli[1].
      DEFINE FRAME F-REPORTE
          DETALLE.fchdoc       FORMAT '99/99/99'  COLUMN-LABEL 'Fecha'
          DETALLE.CodCli       FORMAT 'x(11)'     COLUMN-LABEL 'Codigo'
          DETALLE.nomcli       FORMAT 'x(40)'     COLUMN-LABEL 'Cliente'
          DETALLE.coddiv       FORMAT 'x(5)'      COLUMN-LABEL 'Division'
          DETALLE.coddoc                          COLUMN-LABEL 'Doc'
          DETALLE.nrodoc                          COLUMN-LABEL 'Numero'
          x-ImpTot            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Importe S/.'
          x-ImpPro            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Propios S/.'
          x-ImpTer            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Terceros S/.'
          x-ImpFot            FORMAT '->,>>>,>>9.99'  COLUMN-LABEL 'Fotocopias S/.'
          DETALLE.Puntos      FORMAT '->>>,>>9'       COLUMN-LABEL 'ContiPuntos Nuevos'
          DETALLE.Puntos_Old  FORMAT '->>>,>>9'       COLUMN-LABEL 'ContiPuntos Viejos'
        HEADER
             S-NOMCIA FORMAT "X(50)" SKIP
             "Pagina : " TO 90 PAGE-NUMBER(REPORT) TO 100 FORMAT "ZZZZZ9" SKIP
             "CONTIPUNTOS" AT 30
             " Fecha : " TO 90 TODAY TO 100 FORMAT "99/99/9999" SKIP
             "  Hora : " TO 90 STRING(TIME,"HH:MM:SS") TO 100 SKIP
             "CLIENTE:" DETALLE.NroCard FORMAT 'x(11)' x-NomCli FORMAT 'x(50)' SKIP
             "DESDE EL" x-Fecha-1 "HASTA EL" x-Fecha-2 SKIP(1)
      WITH WIDTH 200 NO-BOX STREAM-IO DOWN.

      FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= DETALLE.FchDoc NO-LOCK NO-ERROR.
      IF NOT AVAIL Gn-Tcmb THEN 
          FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= DETALLE.FchDoc NO-LOCK NO-ERROR.
      IF AVAIL Gn-Tcmb THEN 
          ASSIGN
              x-TpoCmbCmp = Gn-Tcmb.Compra
              x-TpoCmbVta = Gn-Tcmb.Venta.
      ASSIGN
          x-ImpTot = 0
          x-ImpPro = 0
          x-ImpTer = 0
          x-ImpFot.
      ASSIGN
          x-ImpTot = DETALLE.Imptot
          x-ImpPro = DETALLE.Propios
          x-ImpTer = DETALLE.Terceros
          x-ImpFot = DETALLE.Fotocopias.
      IF DETALLE.CodMon = 2
      THEN ASSIGN
          x-ImpTot = x-ImpTot * x-TpoCmbVta
          x-ImpPro = x-ImpPro * x-TpoCmbVta
          x-ImpTer = x-ImpTer * x-TpoCmbVta
          x-ImpFot = x-ImpFot * x-TpoCmbVta.
      ACCUMULATE x-ImpTot (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE x-ImpPro (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE x-ImpTer (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE x-ImpFot (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE DETALLE.Puntos (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      ACCUMULATE DETALLE.Puntos_Old (TOTAL BY DETALLE.NroCard BY DETALLE.CodDiv).
      DISPLAY STREAM REPORT
          DETALLE.fchdoc                          
          DETALLE.codcli
          DETALLE.nomcli
          DETALLE.coddiv
          DETALLE.coddoc                          
          DETALLE.nrodoc                          
          x-ImpTot
          x-ImpPro
          x-ImpTer
          x-ImpFot
          DETALLE.Puntos  
          DETALLE.Puntos_Old
          WITH FRAME F-REPORTE.
      IF LAST-OF(DETALLE.CodDiv) THEN DO:
          UNDERLINE STREAM REPORT
              x-ImpTot
              x-ImpPro
              x-ImpTer
              x-ImpFot
              DETALLE.Puntos
              DETALLE.Puntos_Old
              WITH FRAME F-REPORTE.
          DISPLAY STREAM REPORT
              'TOTAL DIVISION >>>' @ DETALLE.nomcli
              ACCUM TOTAL BY DETALLE.CodDiv x-ImpTot @ x-ImpTot
              ACCUM TOTAL BY DETALLE.CodDiv x-ImpPro @ x-ImpPro
              ACCUM TOTAL BY DETALLE.CodDiv x-ImpTer @ x-ImpTer
              ACCUM TOTAL BY DETALLE.CodDiv DETALLE.Puntos @ DETALLE.Puntos
              ACCUM TOTAL BY DETALLE.CodDiv DETALLE.Puntos_Old @ DETALLE.Puntos_Old
              WITH FRAME F-REPORTE.
      END.
      IF LAST-OF(DETALLE.NroCard) THEN DO:
          UNDERLINE STREAM REPORT
              x-ImpTot
              x-ImpPro
              x-ImpTer
              x-ImpFot
              DETALLE.Puntos
              DETALLE.Puntos_Old
              WITH FRAME F-REPORTE.
          DISPLAY STREAM REPORT
              'TOTAL GENERAL >>>' @ DETALLE.nomcli
              ACCUM TOTAL BY DETALLE.NroCard x-ImpTot @ x-ImpTot
              ACCUM TOTAL BY DETALLE.NroCard x-ImpPro @ x-ImpPro
              ACCUM TOTAL BY DETALLE.NroCard x-ImpTer @ x-ImpTer
              ACCUM TOTAL BY DETALLE.NroCard x-ImpFot @ x-ImpFot
              ACCUM TOTAL BY DETALLE.NroCard DETALLE.Puntos @ DETALLE.Puntos
              ACCUM TOTAL BY DETALLE.NroCard DETALLE.Puntos_Old @ DETALLE.Puntos_Old
              WITH FRAME F-REPORTE.
          PAGE STREAM REPORT.
      END.
    END.


END PROCEDURE.
