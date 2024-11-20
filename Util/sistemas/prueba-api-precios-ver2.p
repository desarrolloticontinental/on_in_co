DEF NEW SHARED VAR s-coddiv AS CHAR INIT '10015'.
DEF VAR  pMonVta AS INTE NO-UNDO.
DEF VAR  pTpoCmb AS DECI NO-UNDO.
DEF VAR  pPrecioDescontado AS DECI NO-UNDO.
DEF VAR  pMessage AS CHAR NO-UNDO.
DEF VAR pCodMat AS CHAR INIT '601095' NO-UNDO.
DEF NEW SHARED VAR  s-CodCia AS INTE INIT 001.
DEF VAR pCodDiv AS CHAR INIT '10060' NO-UNDO.
DEF VAR pCodAlm AS CHAR INIT '03' NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.
DEF VAR F-PREBAS AS DEC.
DEF VAR F-PREVTA AS DEC.
DEF VAR F-DSCTOS AS DEC.
DEF VAR Y-DSCTOS AS DEC.
DEF VAR Z-DSCTOS AS DEC.
DEF VAR X-TIPDTO AS CHAR.
DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR s-undvta AS CHAR.
DEF VAR f-factor AS DECI.
DEF VAR f-FleteUnitario AS DECI NO-UNDO.

FIND Almmmatg WHERE codcia = s-codcia AND codmat = pCodMat NO-LOCK.

RUN web/PrecioFinalCreditoMayorista (
        "E",
        pCodDiv,
        '11111111111',
        1,
        INPUT-OUTPUT s-UndVta,
        OUTPUT f-Factor,
        pCodMat,
        '001',
        1,
        4,
        OUTPUT f-PreBas,
        OUTPUT f-PreVta,
        OUTPUT f-Dsctos,
        OUTPUT y-Dsctos,
        OUTPUT z-Dsctos,
        OUTPUT x-TipDto,
        "",     /* ClfCli: lo ingresamos solo si se quiere forzar la clasificacion */
        OUTPUT f-FleteUnitario,
        "",
        YES,
        OUTPUT pMensaje).

MESSAGE f-prevta SKIP 
    f-prebas SKIP
    pmensaje SKIP RETURN-VALUE.
