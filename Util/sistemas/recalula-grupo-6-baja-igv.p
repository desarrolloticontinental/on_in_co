
DISABLE TRIGGERS FOR LOAD OF integral.faccpedi.
DISABLE TRIGGERS FOR LOAD OF integral.facdpedi.

DEF VAR s-nrodec AS INT INIT 4.
DEF VAR s-porigv AS DECI.
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.

DEF VAR s-codcli AS CHAR.
DEF VAR s-cmpbnte AS CHAR.
DEF VAR x-articulo-ICBPer AS CHAR INIT '099268'.

FOR EACH integral.faccpedi EXCLUSIVE-LOCK WHERE integral.faccpedi.codcia = 1
    AND integral.faccpedi.coddiv = '00519'
    AND integral.faccpedi.coddoc = 'cot'
    AND integral.faccpedi.deliverygroup = "6":
    s-porigv = faccpedi.porigv.
    s-codcli = faccpedi.codcli.
    s-cmpbnte = faccpedi.cmpbnte.
    FOR EACH facdpedi OF faccpedi EXCLUSIVE-LOCK,
        FIRST Almmmatg OF Facdpedi NO-LOCK,
        FIRST Almsfami OF Almmmatg NO-LOCK:
        ASSIGN
            facdpedi.preuni = round(facdpedi.preuni / 1.18, s-nrodec).
        ASSIGN
            facdpedi.prebas = facdpedi.preuni
            facdpedi.prevta[1] = facdpedi.preuni.

        /* ***************************************************************** */
        {vtagn/CalculoDetalleMayorCredito.i &Tabla="Facdpedi" }
        /* ***************************************************************** */
    END.

    RUN Totales.

END.

PROCEDURE Totales:

    /* ****************************************************************************************** */
    {vtagn/totales-cotizacion-unificada.i &Cabecera="FacCPedi" &Detalle="FacDPedi"}
    /* ****************************************************************************************** */

END PROCEDURE.

