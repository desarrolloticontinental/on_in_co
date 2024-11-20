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

DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF VAR x-codmat AS CHAR INIT '098266'.
DEF VAR x-codalm AS CHAR INIT '11'.
DEF VAR pComprometido AS DECI.
DEF VAR x-time-1 AS INTE LABEL 'Nuevo'.
DEF VAR x-time-2 AS INTE LABEL 'Viejo'.

OUTPUT TO d:\velocidad.txt.
PUT UNFORMATTED 'NUEVO|VIEJO' SKIP.
DEF VAR X AS INTE.
DO X = 1 TO 1000:
    ETIME(YES).
    RUN alm/prueba-new (s-codcia,
			x-codalm,
			x-codmat,
			input-output table tmp-tabla,
			output pComprometido).

    x-time-1 = ETIME.

    ETIME(YES).
    RUN alm/prueba-old (s-codcia,
			x-codalm,
			x-codmat,
			input-output table tmp-tabla,
			output pComprometido).
    x-time-2 = ETIME.
    PUT x-time-1 '|' x-time-2 SKIP.
END.
OUTPUT CLOSE.
