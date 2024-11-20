DEF TEMP-TABLE Resumen
    FIELD codcia AS INT
    FIELD codven LIKE gn-ven.codven
    FIELD nomven LIKE gn-ven.nomven
    FIELD codcli LIKE gn-clie.codcli
    FIELD nomcli LIKE gn-clie.nomcli
    FIELD codpais LIKE evtall01.codpais
    FIELD coddept LIKE evtall01.coddept
    FIELD codprov LIKE evtall01.codprov
    FIELD coddist LIKE evtall01.coddist
    FIELD nomdistr LIKE tabdistr.nomdistr
    FIELD codmat LIKE evtall01.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD desmar LIKE evtall01.desmar
    FIELD codfam LIKE almtfami.codfam
    FIELD desfam LIKE almtfami.desfam
    FIELD canxmes LIKE evtall01.canxmes
    FIELD vtaxmesmn LIKE EvtALL01.VtaxMesMn
    INDEX Llave01 AS PRIMARY codcia nomven codcli codmat.

DEF VAR x-nomven LIKE gn-ven.nomven NO-UNDO.
DEF VAR x-codven AS CHAR NO-UNDO.
x-codven = '164,205,332,726,809,362,812,361,742,744,731'.
DISPLAY 'buscando en la estadistica'.
PAUSE 0.
FOR EACH evtall01 NO-LOCK WHERE codcia = 1
    AND nrofch >= 200901 AND nrofch <= 200903
    AND coddiv = '00014'
    AND LOOKUP(codven, x-codven) > 0:
    CASE evtall01.codven:
        WHEN '164' OR WHEN '205' OR WHEN '332' OR WHEN '726' OR WHEN '809' 
            THEN x-nomven = 'PRISCA BACA'.
        WHEN '362' OR WHEN '812' THEN x-nomven = 'LIDIA CARRASCO'.
        WHEN '361' OR WHEN '742' THEN x-nomven = 'PERCY VILLAR'.
        WHEN '744' THEN x-nomven = 'NORMA PEREZ'.
        WHEN '731' THEN x-nomven = 'INES TRIBEÑOS'.
    END CASE.
    FIND Resumen WHERE Resumen.codcia = evtall01.codcia
        AND Resumen.nomven = x-nomven
        AND Resumen.codcli = evtall01.codcli
        AND Resumen.codmat = evtall01.codmat
        NO-ERROR.
    IF NOT AVAILABLE Resumen THEN DO:
        CREATE Resumen.
        BUFFER-COPY evtall01 TO Resumen
            ASSIGN
                Resumen.nomven = x-nomven.
    END.
    ELSE ASSIGN
            Resumen.canxmes = Resumen.canxmes + evtall01.canxmes
            Resumen.vtaxmesmn = Resumen.vtaxmesmn + evtall01.vtaxmesmn.
END.
DISPLAY 'completando informacion'.
PAUSE 0.
OUTPUT TO c:\tmp\ferias.txt.
FOR EACH Resumen,
    FIRST Almmmatg OF Resumen NO-LOCK,
    FIRST Almtfami OF Almmmatg NO-LOCK:
    ASSIGN
        Resumen.desmat = Almmmatg.desmat
        Resumen.desfam = Almtfami.desfam.
    FIND gn-clie WHERE gn-clie.codcia = 000
        AND gn-clie.codcli = Resumen.codcli
        NO-LOCK.
    ASSIGN
        Resumen.nomcli = gn-clie.nomcli
        Resumen.coddept = gn-clie.coddept
        Resumen.codprov = gn-clie.codprov
        Resumen.coddist = gn-clie.coddist.
    FIND Tabdistr WHERE Tabdistr.coddepto = gn-clie.coddept
        AND Tabdistr.codprovi = gn-clie.codprov
        AND Tabdistr.coddistr = gn-clie.coddist
        NO-LOCK NO-ERROR.
    IF AVAILABLE Tabdistr THEN Resumen.nomdistr = Tabdistr.nomdistr.
    ELSE Resumen.nomdistr = 'SIN DISTRITO DEFINIDO'.
    DISPLAY
        Resumen.nomven 
        '|'
        Resumen.nomcli 
        '-'
        Resumen.codcli 
        '|'
        Resumen.nomdistr 
        '|'
        Resumen.codmat 
        '-'
        Resumen.desmat 
        '|'
        Resumen.desmar 
        '|'
        Resumen.codfam 
        '-'
        Resumen.desfam 
        '|'
        Resumen.canxmes 
        '|'
        Resumen.vtaxmesmn 
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.
