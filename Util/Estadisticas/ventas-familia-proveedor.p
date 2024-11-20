DEF VAR x-NroFch-1 LIKE EvtALL01.Nrofch NO-UNDO.
DEF VAR x-NroFch-2 LIKE EvtALL01.Nrofch NO-UNDO.
DEF VAR X-CODDIA AS INT INIT 1 NO-UNDO.
DEF VAR X-CODANO AS INT NO-UNDO.
DEF VAR X-CODMES AS INT NO-UNDO.
DEF VAR X-FECHA AS DATE NO-UNDO.
DEF VAR X-FECHA-I AS DATE NO-UNDO.
DEF VAR I AS INT NO-UNDO.
DEF VAR T-Vtamn   AS DECI INIT 0 NO-UNDO.
DEF VAR T-Vtame   AS DECI INIT 0 NO-UNDO.
DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
DEF VAR x-FChDoc-2 AS DATE NO-UNDO.

ASSIGN
    x-FchDoc-1 = 03/01/2009
    x-FchDoc-2 = 03/01/2010
    x-NroFch-1 = YEAR(x-FchDoc-1) * 100 + MONTH(x-FchDoc-1)
    x-NroFch-2 = YEAR(x-FchDoc-2) * 100 + MONTH(x-FchDoc-2).

DEF TEMP-TABLE detalle
    FIELD codfam LIKE evtall01.codfam
    FIELD subfam LIKE evtall01.subfam
    FIELD ventas AS DEC EXTENT 7 FORMAT '(>>>,>>>,>>9.99)'
    INDEX llave01 AS UNIQUE PRIMARY codfam subfam.

FOR EACH evtall01 NO-LOCK WHERE codcia = 1
    AND codfam = '001'
    AND nrofch >= x-NroFch-1
    AND nrofch <= x-NroFch-2:
/*     DISPLAY                    */
/*         coddiv                 */
/*         nrofch                 */
/*         codmat                 */
/*         codfam                 */
/*         subfam                 */
/*         codpro                 */
/*         WITH STREAM-IO NO-BOX. */
/*         PAUSE 0.               */

    /*********************** Calculo Para Obtener los datos diarios ************/
    /*****************Capturando el Mes siguiente *******************/
    IF Evtall01.Codmes < 12 THEN DO:
      ASSIGN
          X-CODMES = Evtall01.Codmes + 1
          X-CODANO = Evtall01.Codano .
    END.
    ELSE DO: 
      ASSIGN
          X-CODMES = 01
          X-CODANO = Evtall01.Codano + 1 .
    END.
    T-Vtamn   = 0.
    T-Vtame   = 0.
    x-Fecha-I = DATE(STRING(X-CODDIA,"99") + "/" + STRING(X-CODMES,"99") + "/" + STRING(X-CODANO,"9999")).
    DO I = 1 TO DAY( x-Fecha-I - 1 ) :        
        X-FECHA = DATE(STRING(I,"99") + "/" + STRING(EvtAll01.Codmes,"99") + "/" + STRING(EvtAll01.Codano,"9999")).
        IF X-FECHA >= x-FchDoc-1 AND X-FECHA <= x-FchDoc-2 THEN DO:
            FIND Gn-tcmb WHERE Gn-tcmb.Fecha = DATE(STRING(I,"99") + "/" + STRING(EvtAll01.Codmes,"99") + "/" + STRING(EvtAll01.Codano,"9999")) NO-LOCK NO-ERROR.
            IF AVAILABLE Gn-tcmb THEN DO: 
                T-Vtamn   = T-Vtamn   + EvtAll01.Vtaxdiamn[I].
                T-Vtame   = T-Vtame   + EvtAll01.Vtaxdiame[I].
            END.
        END.
    END.         
    FIND detalle WHERE detalle.codfam = evtall01.codfam
        AND detalle.subfam = evtall01.subfam
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codfam = evtall01.codfam
            detalle.subfam = evtall01.subfam.
    END.
    CASE evtall01.codpro:
    WHEN '10005035' THEN detalle.ventas[1] = detalle.ventas[1] + T-Vtame.
    WHEN '10006732' THEN detalle.ventas[2] = detalle.ventas[2] + T-Vtame.
    WHEN '10065402' THEN detalle.ventas[3] = detalle.ventas[3] + T-Vtame.
    WHEN '10006929' THEN detalle.ventas[4] = detalle.ventas[4] + T-Vtame.
    WHEN '10031028' THEN detalle.ventas[5] = detalle.ventas[5] + T-Vtame.
    WHEN '10230128' THEN detalle.ventas[6] = detalle.ventas[6] + T-Vtame.
        OTHERWISE detalle.ventas[7] = detalle.ventas[7] + T-Vtame.
    END CASE.
    RELEASE detalle.
END.

OUTPUT TO c:\tmp\familia.txt.
FOR EACH detalle NO-LOCK,
    FIRST almsfam WHERE almsfam.codcia = 1
    AND AlmSFami.codfam = detalle.codfam
    AND AlmSFami.subfam = detalle.subfam:
    DISPLAY
        detalle.codfam
        detalle.subfam
        AlmSFami.dessub
        detalle.ventas[1]
        detalle.ventas[2]
        detalle.ventas[3]
        detalle.ventas[4]
        detalle.ventas[5]
        detalle.ventas[6]
        detalle.ventas[7]
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

