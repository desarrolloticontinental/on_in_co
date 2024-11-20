DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '30060'.
DEF NEW SHARED VAR cl-codcia AS INTE INIT 000.
DEF VAR x-Fecha-1 AS DATE NO-UNDO.
DEF VAR x-Fecha-2 AS DATE NO-UNDO.

x-Fecha-1 = DATE(01,12,2024).
x-Fecha-2 = DATE(01,15,2024).

DEF TEMP-TABLE Detalle NO-UNDO
    FIELD codcli AS CHAR FORMAT 'x(15)' LABEL 'Cod Cliente'
    FIELD nomcli AS CHAR FORMAT 'x(80)' LABEL 'Nom Cliente'
    FIELD codven AS CHAR FORMAT 'x(12)' LABEL 'Cod Vend'
    FIELD nomven AS CHAR FORMAT 'x(40)' LABEL 'Nom Vend'
    FIELD distrito AS CHAR FORMAT 'x(40)' LABEL 'Distrito'
    FIELD provincia AS CHAR FORMAT 'x(40)' LABEL 'Provicia'
    FIELD departamento AS CHAR FORMAT 'x(40)' LABEL 'Departamento'
    FIELD linea001 AS DECI FORMAT '>>>,>>9.99' LABEL 'Lin 001'
    FIELD linea010 AS DECI FORMAT '>>>,>>9.99' LABEL 'Lin 010'
    FIELD linea011 AS DECI FORMAT '>>>,>>9.99' LABEL 'Lin 011'
    FIELD linea012 AS DECI FORMAT '>>>,>>9.99' LABEL 'Lin 012'
    FIELD linea013 AS DECI FORMAT '>>>,>>9.99' LABEL 'Lin 013'
    FIELD imptot AS DECI FORMAT '>>>,>>9.99' LABEL 'Total cotizado'
    FIELD impbd  AS DECI FORMAT '>>>,>>9.99' LABEL 'Total bd y ar'
    FIELD porcentaje AS INTE FORMAT '->>>,>>>,>>9' LABEL '%'
    FIELD f12 AS DECI FORMAT '>>>,>>9.99' LABEL '12 Ene'
    FIELD f13 AS DECI FORMAT '>>>,>>9.99' LABEL '13 Ene'
    FIELD f14 AS DECI FORMAT '>>>,>>9.99' LABEL '14 Ene'
    FIELD f15 AS DECI FORMAT '>>>,>>9.99' LABEL '15 Ene'
    FIELD f16 AS DECI FORMAT '>>>,>>9.99' LABEL '16 Ene'
    FIELD f17 AS DECI FORMAT '>>>,>>9.99' LABEL '17 Ene'
    FIELD f18 AS DECI FORMAT '>>>,>>9.99' LABEL '18 Ene'
    FIELD f19 AS DECI FORMAT '>>>,>>9.99' LABEL '19 Ene'
    INDEX Llave00 AS PRIMARY codcli
    .

FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia AND
    coddiv = s-coddiv AND
    coddoc = 'COT' AND
    fchped >= x-Fecha-1 AND
    fchped <= x-Fecha-2 AND
    flgest <> 'A',
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia AND
    gn-clie.codcli = faccpedi.codcli,
    FIRST gn-ven NO-LOCK WHERE gn-ven.codcia = faccpedi.codcia AND
    gn-ven.codven = faccpedi.codven,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg NO-LOCK WHERE almmmatg.codcia = s-codcia AND
    almmmatg.codmat = facdpedi.codmat:
    FIND detalle WHERE detalle.codcli = faccpedi.codcli NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codcli = faccpedi.codcli
            detalle.nomcli = gn-clie.nomcli.
    END.
    ASSIGN
        detalle.codven = faccpedi.codven
        detalle.nomven = gn-ven.NomVen
        .
    CASE almmmatg.codfam:
        WHEN "001" THEN detalle.linea001 = detalle.linea001 + facdpedi.implin.
        WHEN "010" THEN detalle.linea010 = detalle.linea010 + facdpedi.implin.
        WHEN "011" THEN detalle.linea011 = detalle.linea011 + facdpedi.implin.
        WHEN "012" THEN detalle.linea012 = detalle.linea012 + facdpedi.implin.
        WHEN "013" THEN detalle.linea013 = detalle.linea013 + facdpedi.implin.
    END CASE.
    ASSIGN
        detalle.imptot = detalle.imptot + facdpedi.implin.
    FIND gn-clied WHERE Gn-ClieD.CodCia = cl-codcia AND
        Gn-ClieD.CodCli = faccpedi.codcli AND
        Gn-ClieD.Sede = faccpedi.sede
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clied THEN DO:
        ASSIGN
            detalle.distrito     = Gn-ClieD.CodDist 
            detalle.provincia    = Gn-ClieD.CodProv 
            detalle.departamento = Gn-ClieD.CodDept
            .
        FIND TabDepto WHERE TabDepto.CodDepto = Gn-ClieD.CodDept NO-LOCK NO-ERROR.
        IF AVAILABLE TabDepto THEN 
            detalle.departamento = TabDepto.CodDepto + ' ' + TabDepto.NomDepto.
        FIND TabProvi WHERE TabProvi.CodDepto = Gn-ClieD.CodDept AND
            TabProvi.CodProvi = Gn-ClieD.CodProv NO-LOCK NO-ERROR.
        IF AVAILABLE TabProvi THEN 
            detalle.provincia = TabProvi.CodProvi + ' ' + TabProvi.NomProvi.
        FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
            TabDistr.CodProvi = Gn-ClieD.CodProv AND
            TabDistr.CodDistr = Gn-ClieD.CodDist
            NO-LOCK NO-ERROR.
        IF AVAILABLE TabDistr THEN 
            detalle.distrito = TabDistr.CodDistr + ' ' + TabDistr.NomDistr.
    END.
END.

FOR EACH detalle:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
        ccbcdocu.codcli = detalle.codcli AND
        ccbcdocu.coddoc = 'bd' AND
        ccbcdocu.flgest <> 'A' AND
        ccbcdocu.fchdoc >= x-Fecha-1 AND
        ccbcdocu.fchdoc <= x-Fecha-2:
        CASE ccbcdocu.fchdoc:
            WHEN DATE(01,12,2024) THEN detalle.f12 = detalle.f12 + ccbcdocu.imptot.
            WHEN DATE(01,13,2024) THEN detalle.f13 = detalle.f13 + ccbcdocu.imptot.
            WHEN DATE(01,14,2024) THEN detalle.f14 = detalle.f14 + ccbcdocu.imptot.
            WHEN DATE(01,15,2024) THEN detalle.f15 = detalle.f15 + ccbcdocu.imptot.
            WHEN DATE(01,16,2024) THEN detalle.f16 = detalle.f16 + ccbcdocu.imptot.
            WHEN DATE(01,17,2024) THEN detalle.f17 = detalle.f17 + ccbcdocu.imptot.
            WHEN DATE(01,18,2024) THEN detalle.f18 = detalle.f18 + ccbcdocu.imptot.
            WHEN DATE(01,19,2024) THEN detalle.f19 = detalle.f19 + ccbcdocu.imptot.
        END CASE.
        detalle.impbd = detalle.impbd + ccbcdocu.imptot.
    END.
    IF detalle.imptot <> 0 THEN detalle.porcentaje = detalle.impbd / detalle.imptot * 100.
END.
FOR EACH detalle:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
        ccbcdocu.codcli = detalle.codcli AND
        ccbcdocu.coddoc = 'a/r' AND
        ccbcdocu.flgest <> 'A' AND
        ccbcdocu.fchdoc >= x-Fecha-1 AND
        ccbcdocu.fchdoc <= x-Fecha-2:
        CASE ccbcdocu.fchdoc:
            WHEN DATE(01,12,2024) THEN detalle.f12 = detalle.f12 + ccbcdocu.imptot.
            WHEN DATE(01,13,2024) THEN detalle.f13 = detalle.f13 + ccbcdocu.imptot.
            WHEN DATE(01,14,2024) THEN detalle.f14 = detalle.f14 + ccbcdocu.imptot.
            WHEN DATE(01,15,2024) THEN detalle.f15 = detalle.f15 + ccbcdocu.imptot.
            WHEN DATE(01,16,2024) THEN detalle.f16 = detalle.f16 + ccbcdocu.imptot.
            WHEN DATE(01,17,2024) THEN detalle.f17 = detalle.f17 + ccbcdocu.imptot.
            WHEN DATE(01,18,2024) THEN detalle.f18 = detalle.f18 + ccbcdocu.imptot.
            WHEN DATE(01,19,2024) THEN detalle.f19 = detalle.f19 + ccbcdocu.imptot.
        END CASE.
        detalle.impbd = detalle.impbd + ccbcdocu.imptot.
    END.
    IF detalle.imptot <> 0 THEN detalle.porcentaje = detalle.impbd / detalle.imptot * 100.
END.

OUTPUT TO d:\arequipa.txt.
PUT UNFORMATTED 'Cod Cliente|Nom Cliente|Cod Vend|Nom Vend|Distrito|Provicia|Departamento|~
    Lin 001|Lin 010|Lin 011|Lin 012|Lin 013|Total cotizado|~
    Total bd y ar|%|12 Ene|13 Ene|14 Ene|15 Ene|16 Ene|17 Ene|18 Ene|19 Ene'
    SKIP.
FOR EACH detalle NO-LOCK:
    PUT UNFORMATTED
        detalle.codcli         "|"
        detalle.nomcli         "|"
        detalle.codven         "|"
        detalle.nomven         "|"
        detalle.distrito       "|"
        detalle.provincia      "|"
        detalle.departamento   "|"
        detalle.linea001       "|"
        detalle.linea010       "|"
        detalle.linea011       "|"
        detalle.linea012       "|"
        detalle.linea013       "|"
        detalle.imptot         "|"
        detalle.impbd          "|"
        detalle.porcentaje     "|"
        detalle.f12            "|"
        detalle.f13            "|"
        detalle.f14            "|"
        detalle.f15            "|"
        detalle.f16            "|"
        detalle.f17            "|"
        detalle.f18            "|"
        detalle.f19            "|"
        SKIP
        .
END.
