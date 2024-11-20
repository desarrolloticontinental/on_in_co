
DEFINE TEMP-TABLE tmp-tabla NO-UNDO
    FIELD t-CodAlm LIKE Almacen.codalm  FORMAT 'x(3)'
    FIELD t-CodDoc LIKE FacDPedi.CodDoc FORMAT "XXX"
    FIELD t-Nroped LIKE FacDPedi.NroPed FORMAT "XXX-XXXXXXXX"
    FIELD t-CodDiv LIKE FacCPedi.CodDiv FORMAT 'x(5)'
    FIELD t-FchPed LIKE FacDPedi.FchPed
    FIELD t-NomCli LIKE FacCPedi.NomCli COLUMN-LABEL "Cliente" FORMAT "x(35)"
    FIELD t-CodMat LIKE FacDPedi.codmat
    FIELD t-Canped LIKE FacDPedi.CanPed
    INDEX t-CodDoc t-NroPed.

DEFINE VAR s-codcia AS INT INIT 001.
DEFINE VAR s-codalm AS CHAR INIT '11i'.
DEFINE VAR s-codmat AS CHAR INIT '000996'.
DEFINE VAR x-Total AS DEC.

DEFINE VAR x-linea AS CHAR.

INPUT FROM d:\codigos.txt.
REPEAT:
    IMPORT UNFORMATTE x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    s-codmat = SUBSTRING(x-linea,1,6).
    s-codalm = SUBSTRING(x-linea,8).

    RUN alm/p-articulo-en-transito (s-codcia,
                                    s-codalm,
                                    s-codmat,
                                    INPUT-OUTPUT TABLE tmp-tabla,
                                    OUTPUT x-Total).
    DISPLAY s-codalm s-codmat x-total.
END.
INPUT CLOSE.

