DEF VAR x-linea AS CHAR.
DEF VAR s-undvta AS CHAR.
DEF VAR f-factor AS DEC DECIMALS 4.
DEF VAR f-prebas AS DEC DECIMALS 4.
DEF VAR f-prevta AS DEC DECIMALS 4.
DEF VAR f-dsctos AS DEC DECIMALS 4.
DEF VAR y-dsctos AS DEC DECIMALS 4.
DEF VAR z-dsctos AS DEC DECIMALS 4.
DEF VAR x-tipdto AS CHAR.

DEF TEMP-TABLE detalle
    FIELD codmat AS CHAR FORMAT 'x(6)'
    FIELD ctouni AS DEC
    FIELD preuti AS DEC
    FIELD preins AS DEC
    FIELD dsctos1 AS DEC
    FIELD dsctos2 AS DEC
    FIELD dsctos3 AS DEC.

INPUT FROM c:\tmp\huarac.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        codmat = STRING(INTEGER(x-linea), '999999').
END.
INPUT CLOSE.

FOR EACH detalle:
    FIND almmmatg WHERE codcia = 1
        AND almmmatg.codmat = detalle.codmat NO-LOCK.
    ASSIGN
        detalle.ctouni =  Almmmatg.CtoTot.
    FIND VtaListaMinGn WHERE VtaListaMinGn.CodCia = 001
        AND VtaListaMinGn.codmat = almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE VtaListaMinGn THEN DO:
        detalle.preuti = VtaListaMinGn.PreOfi.
        IF almmmatg.monvta = 2 THEN detalle.preuti = VtaListaMinGn.PreOfi * almmmatg.tpocmb.
    END.

    RUN vta2/PrecioListaxMayorCredito (
        "I",
        "00024",
        "11111111111",
        1,
        INPUT-OUTPUT s-undvta,
        OUTPUT f-factor,
        almmmatg.codmat,
        "130",
        1,
        4,
        OUTPUT f-prebas,
        OUTPUT f-prevta,
        OUTPUT f-dsctos,
        OUTPUT y-dsctos,
        OUTPUT x-tipdto,
        OUTPUT z-dsctos,
        "",
        NO
        ).
    ASSIGN
        detalle.preins = f-prevta
        detalle.dsctos1 = f-dsctos
        detalle.dsctos2 = y-dsctos
        detalle.dsctos3 = z-dsctos.
END.
OUTPUT TO c:\tmp\resultado.txt.
FOR EACH detalle:
    DISPLAY detalle WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
END.
OUTPUT CLOSE.

/*

DEF INPUT PARAMETER s-TpoPed AS CHAR.
DEF INPUT PARAMETER S-CODDIV AS CHAR.
DEF INPUT PARAMETER S-CODCLI AS CHAR.
DEF INPUT PARAMETER S-CODMON AS INT.
DEF INPUT-OUTPUT PARAMETER S-UNDVTA AS CHAR.
DEF OUTPUT PARAMETER f-Factor AS DEC.
DEF INPUT PARAMETER S-CODMAT AS CHAR.
DEF INPUT PARAMETER S-CNDVTA AS CHAR.
DEF INPUT PARAMETER X-CANPED AS DEC.
DEF INPUT PARAMETER x-NroDec AS INT.
DEF OUTPUT PARAMETER F-PREBAS AS DEC DECIMALS 4.
DEF OUTPUT PARAMETER F-PREVTA AS DEC DECIMALS 4.
DEF OUTPUT PARAMETER F-DSCTOS AS DEC.       /* Descuento incluido en el precio unitario base */
DEF OUTPUT PARAMETER Y-DSCTOS AS DEC.       /* Descuento por Volumen y/o Promocional */
DEF OUTPUT PARAMETER Z-DSCTOS AS DEC.       /* Descuento por evento */
DEF OUTPUT PARAMETER X-TIPDTO AS CHAR.      /* Tipo de descuento aplicado (PROM, VOL) */ 
DEF INPUT  PARAMETER s-TipVta AS CHAR.      /* Lista "A" o "B" */
DEF INPUT PARAMETER pError AS LOG.          /* Mostrar el error en pantalla */


**/
