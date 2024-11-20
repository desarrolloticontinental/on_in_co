DEF TEMP-TABLE detalle
    FIELD codano LIKE evtarti.codano
    FIELD codfam LIKE almmmatg.codfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD enero-marzo AS DEC        FORMAT '(>>>,>>>,>>9.99)'
    FIELD abril-diciembre AS DEC    FORMAT '(>>>,>>>,>>9.99)'
    INDEX llave01 AS PRIMARY codano codfam subfam.
    
FOR EACH gn-divi NO-LOCK WHERE codcia = 1:
    FOR EACH evtarti NO-LOCK WHERE evtarti.codcia = 1
        AND EvtArti.Nrofch >= 200901 AND nrofch <= 200903
        AND evtarti.coddiv = gn-divi.coddiv,
        FIRST almmmatg OF evtarti NO-LOCK WHERE codfam = '001':
        DISPLAY coddiv evtarti.nrofch evtarti.codmat.
        PAUSE 0.
        FIND detalle WHERE detalle.codano = evtarti.codano
            AND detalle.codfam = almmmatg.codfam
            AND detalle.subfam = almmmatg.subfam
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.codano = evtarti.codano
                detalle.codfam = almmmatg.codfam
                detalle.subfam = almmmatg.subfam.
        END.
        IF EvtArti.Codmes >= 1 AND evtarti.codmes <= 3 
            THEN detalle.enero-marzo = detalle.enero-marzo + EvtArti.VtaxMesMe.
        ELSE detalle.abril-diciembre = detalle.abril-diciembre + EvtArti.VtaxMesMe.
    END.
END.


OUTPUT TO m:\tmp\ventas-por-familia-subfamilia2009.txt.
FOR EACH detalle:
    DISPLAY
        detalle.codano
        detalle.codfam
        detalle.subfam
        detalle.enero-marzo
        detalle.abril-diciembre
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.
