DEF VAR s-undvta AS CHAR NO-UNDO.
DEF VAR f-Factor AS DEC DECIMALS 4 NO-UNDO.
DEF VAR f-PreBas AS DEC DECIMALS 4 NO-UNDO.
DEF VAR f-PreVta AS DEC DECIMALS 4 NO-UNDO.
DEF VAR f-Dsctos AS DEC DECIMALS 4 NO-UNDO.
DEF VAR y-Dsctos AS DEC DECIMALS 4 NO-UNDO.
DEF VAR z-Dsctos AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-TipDto AS CHAR NO-UNDO.


RUN vta2/preciolistaxmayorcredito (
    "",
    "00060",
    "11111111111",
    1,
    INPUT-OUTPUT S-UNDVTA,
    OUTPUT f-Factor,
    "019931",
    "000",
    1,
    4,
    OUTPUT F-PREBAS,
    OUTPUT F-PREVTA,
    OUTPUT F-DSCTOS,
    OUTPUT Y-DSCTOS,
    OUTPUT Z-DSCTOS,
    OUTPUT X-TIPDTO,
    "",
    YES
    ).

MESSAGE f-prebas f-prevta x-tipdto f-dsctos y-dsctos.
