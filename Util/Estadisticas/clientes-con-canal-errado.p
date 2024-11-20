DEFINE VAR s-codcia AS INT INIT 001 NO-UNDO.
DEFINE VAR cl-codcia AS INT INIT 000 NO-UNDO.

DEFINE VAR I         AS INTEGER   NO-UNDO.

DEFINE VAR F-Salida  AS DECI INIT 0.
DEFINE VAR T-Vtamn   AS DECI INIT 0.
DEFINE VAR T-Vtame   AS DECI INIT 0.
DEFINE VAR T-Ctomn   AS DECI INIT 0.
DEFINE VAR T-Ctome   AS DECI INIT 0.
DEFINE VAR T-Vta     AS DECI INIT 0.
DEFINE VAR T-Cto     AS DECI INIT 0.

DEFINE VAR X-FECHA AS DATE.
DEFINE VAR X-CODDIA AS INTEGER INIT 1.
DEFINE VAR X-CODANO AS INTEGER .
DEFINE VAR X-CODMES AS INTEGER .

DEFINE VAR DesdeF AS DATE NO-UNDO.
DEFINE VAR HastaF AS DATE NO-UNDO.

DEF TEMP-TABLE Detalle
    FIELD codcli LIKE gn-clie.codcli
    FIELD nomcli LIKE gn-clie.nomcli
    FIELD codven LIKE gn-clie.codven
    FIELD coddept LIKE gn-clie.coddept
    FIELD codprov LIKE gn-clie.codprov
    FIELD coddist LIKE gn-clie.coddist
    FIELD canal LIKE gn-clie.canal
    INDEX llave01 AS PRIMARY codcli.

ASSIGN
    DesdeF = DATE(12,01,2008)
    HastaF = TODAY - 1.

/* VENTAS POR CANAL */
OUTPUT TO c:\tmp\canal.txt.
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = S-CODCIA:
    FOR EACH EvtClArti NO-LOCK WHERE EvtClArti.CodCia = S-CODCIA
        AND EvtClArti.CodDiv = '00000'
        AND EvtClArti.Codmat = Almmmatg.codmat
        AND ( EvtClArti.Nrofch >= INTEGER(STRING(YEAR(DesdeF),"9999") + STRING(MONTH(DesdeF),"99"))
        AND EvtClArti.Nrofch <= INTEGER(STRING(YEAR(HastaF),"9999") + STRING(MONTH(HastaF),"99")) ),
        FIRST Gn-Clie NO-LOCK WHERE Gn-Clie.codcia = cl-codcia
        AND Gn-Clie.codcli = EvtClArti.codcli:
       CASE Gn-clie.canal:
           WHEN '0005' THEN DO:
           END.
           WHEN '0008' THEN DO:
           END.
           WHEN '0001' THEN DO:
           END.
           WHEN '0003' THEN DO:
           END.
           OTHERWISE DO:
               FIND Detalle WHERE Detalle.codcli = gn-clie.codcli NO-ERROR.
               IF NOT AVAILABLE Detalle THEN DO:
                   CREATE Detalle.
                   BUFFER-COPY gn-clie TO detalle.
               END.
           END.
          END CASE.
    END.
END.
FOR EACH Detalle:
    DISPLAY
        detalle
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

