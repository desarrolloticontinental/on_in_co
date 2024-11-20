

OUTPUT TO c:\tmp\arequipa.txt.
PUT UNFORMATTED
    'DIVI|DIV ORIG|FECHA|DOC|NUMERO|FMAPGO|CLIENTE|NOMBRE|MON|ARTICULO|CANTIDAD|UNIDAD|UNITARIO|%DTO1|%DTO2|%DTO3|TOTAL'
    SKIP.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'fac,bol') > 0
    AND LOOKUP(coddiv, '00060,00061') > 0
    AND flgest <> 'a',
    EACH ccbddocu OF ccbcdocu NO-LOCK WHERE codmat = '005206':
    PUT UNFORMATTED
        ccbcdocu.coddiv '|' 
        ccbcdocu.divori '|' 
        ccbcdocu.fchdoc '|' 
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fmapgo '|'
        ccbcdocu.codcli '|' 
        ccbcdocu.nomcli '|'
        ccbcdocu.codmon '|'
        ccbddocu.codmat '|'
        ccbddocu.candes '|'
        ccbddocu.undvta '|'
        ccbddocu.preuni '|'
        CcbDDocu.Por_Dsctos[1] '|'
        CcbDDocu.Por_Dsctos[2] '|'
        CcbDDocu.Por_Dsctos[3] '|'
        ccbddocu.implin
        SKIP.
END.
