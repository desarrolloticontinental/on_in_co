FIND FIRST vtacatventa.
FIND FIRST AlmCatVtaC WHERE AlmCatVtaC.CodCia = 001
    AND AlmCatVtaC.CodDiv = '00015'
    AND AlmCatVtaC.CodPro = vtacatventa.codpro.
DEF VAR x-codmat AS CHAR FORMAT 'x(6)' NO-UNDO.
DEF VAR x-item AS INT NO-UNDO.

x-item = 1.
INPUT FROM c:\tmp\expolibreria.prn.
REPEAT:
    IMPORT x-codmat.
    IF x-codmat = '' THEN LEAVE.
    CREATE AlmCatVtaD.
    BUFFER-COPY 
        AlmCatVtaC
        EXCEPT AlmCatVtaC.libre_d05 AlmCatVtaC.libre_d04 AlmCatVtaC.libre_d03
        TO AlmCatVtaD
        ASSIGN AlmCatVtaD.codmat = x-codmat AlmCatVtaD.NroSec = x-item.
    x-item = x-item + 1.
END.
INPUT CLOSE.


