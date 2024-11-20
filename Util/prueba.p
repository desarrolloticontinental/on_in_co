DEF VAR pCodCia AS INT.
DEF VAR pAnoDesde AS INT.
DEF VAR pMesDesde AS INT.
DEF VAR pAnoHasta AS INT.
DEF VAR pMesHasta AS INT.

ASSIGN
    pcodcia = 001
    panodesde = 2008
    panohasta = 2008
    pmesdesde = 01
    pmeshasta = 12.

/* Definimos la tabla con la información necesaria */
DEF TEMP-TABLE Detalle
    FIELD CodCia AS INT
    FIELD CodDiv AS CHAR FORMAT 'x(5)' COLUMN-LABEL 'Division'
    FIELD DesDiv AS CHAR FORMAT 'x(30)' COLUMN-LABEL 'DesDivision'
    FIELD CodAno AS INT FORMAT '9999' COLUMN-LABEL 'Año'
    FIELD CodMes AS INT FORMAT '99' COLUMN-LABEL 'Mes'
    FIELD CodMat AS CHAR FORMAT 'x(6)' COLUMN-LABEL 'Material'
    FIELD DesMat AS CHAR FORMAT 'x(60)' COLUMN-LABEL 'Descripcion'
    FIELD UndBas AS CHAR FORMAT 'x(4)' COLUMN-LABEL 'Unidad'
    FIELD CodFam AS CHAR FORMAT 'x(3)' COLUMN-LABEL 'Familia'
    FIELD DesFam AS CHAR FORMAT 'x(30)' COLUMN-LABEL 'DesFamilia'
    FIELD SubFam AS CHAR FORMAT 'x(3)' COLUMN-LABEL 'SubFamilia'
    FIELD DesSub AS CHAR FORMAT 'x(30)' COLUMN-LABEL 'DesSubFamilia'
    FIELD ImpMn  AS DEC FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL 'Importe S/.'
    FIELD ImpMe  AS DEC FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL 'Importe US$'
    FIELD Cantidad AS DEC FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL 'Cantidad'.


FOR EACH gn-divi NO-LOCK WHERE codcia = pCodCia:
    FOR EACH EvtArti NO-LOCK WHERE Evtarti.codcia = pCodCia
        AND EvtArti.CodDiv = Gn-divi.coddiv
        AND EvtArti.Nrofch >= pAnoDesde * 100 + pMesDesde
        AND EvtArti.Nrofch <= pAnoHasta * 100 + pMesHasta,
        FIRST Almmmatg OF Evtarti NO-LOCK,
        FIRST Almtfami OF Almmmatg NO-LOCK,
        FIRST Almsfami OF Almmmatg NO-LOCK:
        DISPLAY codano codmes almmmatg.codmat evtarti.coddiv.
        PAUSE 0.
        FIND Detalle WHERE Detalle.codcia = pCodCia
            AND Detalle.coddiv = gn-divi.coddiv
            AND Detalle.codmat = Evtarti.codmat
            AND Detalle.codano = Evtarti.codano
            AND Detalle.codmes = Evtarti.codmes
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Detalle THEN CREATE Detalle.
        BUFFER-COPY Almmmatg TO Detalle
            ASSIGN
                Detalle.Coddiv = Evtarti.coddiv
                Detalle.Codano = Evtarti.codano
                Detalle.Codmes = Evtarti.codmes.
        ASSIGN
            Detalle.desmat = Detalle.codmat + ' - ' + Almmmatg.desmat
            Detalle.desfam = Detalle.codfam + ' - ' + Almtfami.desfam
            Detalle.dessub = Detalle.subfam + ' - ' + AlmSFami.dessub
            Detalle.desdiv = Gn-divi.coddiv + ' - ' + GN-DIVI.DesDiv
            Detalle.ImpMn = Detalle.ImpMn + EvtArti.VtaxMesMn
            Detalle.ImpMe = Detalle.ImpMe + EvtArti.VtaxMesMe
            Detalle.Cantidad = Detalle.Cantidad + EvtArti.CanxMes.
    END.
END.

OUTPUT TO c:\tmp\prueba.txt.
FOR EACH Detalle NO-LOCK:
    PUT 
        Detalle.codano '|'
        Detalle.codmes '|'
        Detalle.desdiv '|'
        Detalle.desmat '|'
        Detalle.desfam '|'
        Detalle.dessub '|'
        Detalle.impmn '|'
        Detalle.impme '|'
        Detalle.cantidad SKIP.
END.
OUTPUT CLOSE.

