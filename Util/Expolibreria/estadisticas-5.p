DEF TEMP-TABLE detalle LIKE faccpedi.

/* EXPO OCT 2010 */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND coddiv = '00015'
    AND fchped >= 10/25/2010
    AND flgest = 'P':
    CREATE detalle.
    BUFFER-COPY faccpedi TO detalle
        ASSIGN 
        detalle.TipVta = "EXPO OCT 2010"
        detalle.Libre_d01 = 0
        detalle.Libre_d02 = 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK WHERE canped > canate,
        FIRST almmmatg OF facdpedi NO-LOCK:
        IF LOOKUP(codfam, '010,011,012,013') > 0 
        THEN detalle.libre_d01 = detalle.libre_d01 + ( canped - canate) * ( implin / canped ).
        ELSE detalle.libre_d02 = detalle.libre_d02 + ( canped - canate) * ( implin / canped ).
    END.
END.
/* EXPO ENE 2011 */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND coddiv = '00025'
    AND fchped >= 01/06/2011
    AND flgest = 'P':
    CREATE detalle.
    BUFFER-COPY faccpedi TO detalle
        ASSIGN 
        detalle.TipVta = "EXPO ENE 2011"
        detalle.Libre_d01 = 0
        detalle.Libre_d02 = 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK WHERE canped > canate,
        FIRST almmmatg OF facdpedi NO-LOCK:
        IF LOOKUP(codfam, '010,011,012,013') > 0 
        THEN detalle.libre_d01 = detalle.libre_d01 + ( canped - canate) * ( implin / canped ).
        ELSE detalle.libre_d02 = detalle.libre_d02 + ( canped - canate) * ( implin / canped ).
    END.
END.

DEF VAR x-nomdepto LIKE tabdepto.nomdepto.
DEF VAR x-nomprovi LIKE TabProvi.NomProvi.
DEF VAR x-nomdistr LIKE TabDistr.NomDistr.

OUTPUT TO c:\tmp\despachos.txt.
FOR EACH detalle NO-LOCK, 
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = detalle.codcli,
    FIRST gn-ven OF detalle NO-LOCK:
    ASSIGN
        x-nomdepto = ''
        x-nomprovi = ''
        x-nomdistr = ''.
    FIND tabdepto WHERE tabdepto.coddepto = gn-clie.CodDept NO-LOCK NO-ERROR.
    IF AVAILABLE tabdepto THEN x-nomdepto = TabDepto.NomDepto.
    FIND tabprovi WHERE TabProvi.CodDepto = gn-clie.coddept
        AND TabProvi.CodProvi = gn-clie.CodProv NO-LOCK NO-ERROR.
    IF AVAILABLE tabprovi THEN x-nomprovi = TabProvi.NomProvi.
    FIND tabdistr WHERE TabDistr.CodDepto = gn-clie.coddept
        AND TabDistr.CodDistr = gn-clie.CodDist
        AND TabDistr.CodProvi = gn-clie.CodProv NO-LOCK NO-ERROR.
    IF AVAILABLE tabdistr THEN x-nomdistr = TabDistr.NomDistr.
    DISPLAY
        detalle.tipvta FORMAT 'x(15)'
        '|'
        detalle.nroped
        '|'
        detalle.fchped
        '|'
        detalle.fchent
        '|'
        detalle.codven     FORMAT 'x(3)'
        gn-ven.nomven       FORMAT 'x(30)'
        '|'
        detalle.codcli
        detalle.nomcli
        '|'
        gn-clie.coddept
        x-nomdepto
        '|'
        gn-clie.codprov
        x-nomprovi
        '|'
        gn-clie.coddist
        x-nomdistr
        '|'
        detalle.libre_d01
        '|'
        detalle.libre_d02
        '|'
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.

