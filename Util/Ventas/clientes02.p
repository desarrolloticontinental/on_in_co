DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR x-ubigeo AS CHAR NO-UNDO.
DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM d:\tmp\clientes.prn.
OUTPUT TO d:\tmp\clientes.txt.
PUT UNFORMATTED
    'CODIGO|NOMBRE|DIRECCION|RUC|TELEFONO|TELEFONO|UBIGEO'
    SKIP.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = SUBSTRING(x-linea,1,11)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    x-ubigeo = ''.
    FIND TabDepto WHERE TabDepto.CodDepto =  gn-clie.CodDept NO-LOCK NO-ERROR.
    IF AVAILABLE TabDepto THEN DO:
        FIND TabProvi WHERE TabProvi.CodDepto = TabDepto.CodDepto
            AND TabProvi.CodProvi = gn-clie.CodProv
            NO-LOCK NO-ERROR.
        IF AVAILABLE TabProvi THEN DO:
            FIND TabDistr WHERE TabDistr.CodDepto = gn-clie.CodDept
                AND TabDistr.CodProvi = gn-clie.CodProv
                AND TabDistr.CodDistr = gn-clie.CodDist
                NO-LOCK NO-ERROR.
            IF AVAILABLE TabDistr THEN x-Ubigeo = TRIM(TabDepto.NomDepto) + ' - ' +
                TRIM(TabProvi.NomProvi) + ' - ' + TabDistr.NomDistr.
        END.
    END.
    PUT UNFORMATTED
        codcli '|'
        nomcli '|'
        dircli '|'
        ruc '|'
        Telfnos[1] '|'
        Telfnos[2] '|'
        x-Ubigeo
        SKIP.
END.
OUTPUT CLOSE.
INPUT CLOSE.

